source("install_and_load_INLA.R")

#parameters
kappa = 1
phi_1 = c(1, 1.25) # "Local" version: (sigma, phi) First value is not used

WALS <- read_csv("data/complexity_data_WALS.csv") %>%
  dplyr::select("Name" = lang, roundComp, logpop2, "ISO_639" = silCode) %>%
  dplyr::mutate(ISO_639 = str_to_lower(ISO_639)) %>%
  inner_join(read_csv("output_tables/WALS_high_coverage.csv"),
             by = c("ISO_639"))

min_val <- min(WALS$roundComp)
max_val <- max(WALS$roundComp)

# Perform the rescaling
WALS$roundComp <- (WALS$roundComp - min_val) / (max_val - min_val)

pop_file_fn <-
  "data_wrangling/ethnologue_pop_SM_morph_compl_reanalysis.tsv"
L1 <-
  read_tsv(pop_file_fn, show_col_types = F) %>% dplyr::select(ISO_639, L1_log10_scaled)

glottolog_df <-
  read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", col_types = cols()) %>%
  dplyr::select(
    Glottocode,
    Name,
    Language_ID,
    "ISO_639" = ISO639P3code,
    Language_level_ID,
    level,
    Family_ID,
    Longitude,
    Latitude
  ) %>%
  mutate(Language_level_ID = if_else(is.na(Language_level_ID), Glottocode, Language_level_ID))  %>%
  mutate(Family_ID = ifelse(is.na(Family_ID), Language_level_ID, Family_ID)) %>%
  dplyr::select(
    Glottocode,
    Name,
    Language_ID,
    ISO_639,
    Language_level_ID,
    level,
    Family_ID,
    Longitude,
    Latitude
  )

WALS_df <- WALS %>%
  inner_join(L1,
             by = c("ISO_639")) %>%
  inner_join(glottolog_df, by = "ISO_639") %>%
  filter(!is.na(Latitude),!is.na(Longitude)) %>%
  dplyr::select(Language_ID = Glottocode,
                Name,
                roundComp,
                ISO_639,
                L1_log10_scaled,
                Longitude,
                Latitude)

# jitter points locations
WALS_df$Latitude <- jitter(WALS_df$Latitude, amount = 0.001)
WALS_df$Longitude <- jitter(WALS_df$Longitude, amount = 0.001)

tree <- read.tree(file.path("data_wrangling/wrangled.tree"))

#dropping tips not in Grambank
WALS_df <- WALS_df[WALS_df$Language_ID %in% tree$tip.label, ]
WALS_df <- WALS_df[!duplicated(WALS_df$Language_ID),]
tree <- keep.tip(tree, WALS_df$Language_ID)

x <-
  assert_that(all(tree$tip.label %in% WALS_df$Language_ID), msg = "The data and phylogeny taxa do not match")

## Building standardized phylogenetic precision matrix
tree_scaled <- tree

tree_vcv = vcv.phylo(tree_scaled)
typical_phylogenetic_variance = exp(mean(log(diag(tree_vcv))))

#We opt for a sparse phylogenetic precision matrix (i.e. it is quantified using all nodes and tips), since sparse matrices make the analysis in INLA less time-intensive
tree_scaled$edge.length <-
  tree_scaled$edge.length / typical_phylogenetic_variance
phylo_prec_mat <- MCMCglmm::inverseA(tree_scaled,
                                     nodes = "ALL",
                                     scale = FALSE)$Ainv

WALS_df = WALS_df[order(match(WALS_df$Language_ID, rownames(phylo_prec_mat))),]

#calculate geo_dists
lat_long_matrix <- WALS_df %>% 
  column_to_rownames("Language_ID") %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

rdist.earth_dists <- fields::rdist.earth(lat_long_matrix, miles = FALSE)

rdist.earth_dists[upper.tri(rdist.earth_dists, diag = TRUE)] <- NA

# dists_vector <- as.vector(rdist.earth_dists) 
dists_vector = rdist.earth_dists[lower.tri(rdist.earth_dists)]


#"local" set of parameters
## Create spatial covariance matrix using the matern covariance function
spatial_covar_mat_1 = varcov.spatial(dists.lowertri = rdist.earth_dists[lower.tri(rdist.earth_dists)] / 100,
                                     cov.pars = phi_1, kappa = kappa)$varcov
# Calculate and standardize by the typical variance
typical_variance_spatial_1 = exp(mean(log(diag(
  spatial_covar_mat_1
))))
spatial_cov_std_1 = spatial_covar_mat_1 / typical_variance_spatial_1
spatial_prec_mat_1 = solve(spatial_cov_std_1)
dimnames(spatial_prec_mat_1) = list(WALS_df$Language_ID, WALS_df$Language_ID)

## Since we are using a sparse phylogenetic matrix, we are matching taxa to rows in the matrix
phy_id <-  match(tree$tip.label, rownames(phylo_prec_mat))
if (length(phy_id) != nrow(WALS_df)) {
  stop("The number of phylogenetic IDs does not match the number of rows in WALS_df.")
}

WALS_df$phy_id <-  phy_id

## Other effects are in the same order they appear in the dataset
WALS_df$sp_id = 1:nrow(spatial_prec_mat_1)


formula <- as.formula(
  paste(
    "roundComp ~",
    "L1_log10_scaled +",
    "f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat,
    constr = TRUE, hyper = pcprior_hyper) +
f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1,
constr = TRUE, hyper = pcprior_hyper)"
  )
)

result <- inla(
  formula,
  family = "gaussian",
  control.family = list(hyper = pcprior_hyper),
  data = WALS_df,
  control.compute = list(waic = TRUE)
)
summary(result)

save(result, file = "output_models/model_WALS_high_coverage.RData")

#mean estimate of L1_Users:  with credible intervals not crossing zero ()

social_effects_controlled_coverage <-
  c(
    "morphological complexity ~ L1 + phylogenetic effect + spatial effect",
    round(
      c(
        result$summary.fixed[2, ]$`0.025quant`,
        result$summary.fixed[2, ]$`0.5quant`,
        result$summary.fixed[2, ]$`0.975quant`,
        nrow(WALS_df)
      ),
      2
    ),
    "35%"
  )

save(social_effects_controlled_coverage, file = "output_models/social_effects_controlled_coverage.RData")

load("output_models/social_effects_uncontrolled.RData")
load("output_models/social_effects_controlled.RData")
load("output_models/social_effects_controlled_coverage.RData")

effects_morph_comp <-
  as.data.frame(
    rbind(
      social_effects_uncontrolled,
      social_effects_controlled,
      social_effects_controlled_coverage
    )
  )
colnames(effects_morph_comp) <-
  c("model",
    "2.5%",
    "50%",
    "97.5%",
    "sample size",
    "feature coverage threshold")

rownames(effects_morph_comp) <- NULL

effects_morph_comp %>% 
  write_csv("output_tables/WALS_morph_compl_effects.csv")


effects_morph_comp_flex <- effects_morph_comp %>%
  flextable() %>%
  autofit() %>%
  fix_border_issues()

save_as_docx("Reanalysis of WALS morphological complexity" = effects_morph_comp_flex,
             path = "output_tables_reanalysis/Table_WALS_reanalysis.docx")
