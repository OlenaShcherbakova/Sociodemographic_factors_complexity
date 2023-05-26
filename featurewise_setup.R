source("requirements.R")

#parameters
kappa = 1 
phi_1 = c(1, 1.25) # "Local" version: (sigma, phi) First value is not used
phi_2 = c(1, 17) # "Regional" version: (sigma, phi) First value is not used

GB_wide <-
  read_tsv(file.path("data", "GB_wide", "GB_wide_strict.tsv"),
           col_types = WIDE_COLSPEC)

GB_langs <- read_tsv("data/GB_wide/GB_wide_strict.tsv", col_types = WIDE_COLSPEC) %>% 
  dplyr::select(Language_ID)

glottolog_df <- read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", show_col_types = F) %>% 
  inner_join(GB_langs, by =  "Language_ID") %>% 
  mutate(Family_ID = ifelse(is.na(Family_ID), Language_level_ID, Family_ID)) %>% 
  mutate(Longitude = round(Longitude, 3)) %>% # let's cut down the precision of the lat/long to make the process go quicker. See stack exchange thread where they say "The third decimal place is worth up to 110 m: it can identify a large agricultural field or institutional campus." https://gis.stackexchange.com/questions/8650/measuring-accuracy-of-latitude-and-longitude
  mutate(Latitude = round(Latitude, 3)) 


#taking care of duplicate coordinates by finding them and jittering them
glottolog_df <- glottolog_df %>% 
  mutate(longlat = paste0(Longitude, " - ", Latitude)) %>% 
  mutate(longlat = ifelse(longlat == "NA - NA", NA, longlat)) %>% 
  mutate(dup_longlat = (duplicated(longlat) + duplicated(longlat, fromLast = TRUE))) %>% 
  mutate(dup_longlat = ifelse(is.na(Longitude), NA, dup_longlat )) %>% 
  mutate(Longitude = ifelse(dup_longlat >= 1, jitter(Longitude, factor = 1), Longitude)) %>% 
  mutate(Latitude = ifelse(dup_longlat >= 1, jitter(Latitude, factor = 1), Latitude))


fusion_features <-
  data.table::fread(
    file.path("data", "GB_wide", "parameters.csv"),
    encoding = 'UTF-8',
    quote = "\"",
    header = TRUE,
    sep = ","
  ) %>%
  dplyr::select(Parameter_ID = ID, Fusion = boundness, informativity) %>%
  mutate(Fusion = as.numeric(Fusion)) %>%
  filter(Fusion == 1) %>%
  filter(!is.na(Fusion)) %>%
  pull(Parameter_ID)

GB_wide_filtered <- GB_wide %>%
  filter(na_prop <= 0.25) %>% #exclude languages with more than 25% missing data
  dplyr::select(-na_prop)

GB_wide_filtered <-
  GB_wide_filtered[, c("Language_ID", fusion_features)]

#fusion_features <- fusion_features[c(1, 19)]

sample <- "reduced"
if (sample == "reduced") {
  pop_file_fn <- "data_wrangling/pop_reduced.tsv"
} else {
  pop_file_fn <- "data_wrangling/pop_full.tsv"
}

GB_fusion <- GB_wide_filtered %>%
  inner_join(
    read_tsv(pop_file_fn, show_col_types = F) %>% dplyr::select(Language_ID, L1_log10_st),
    by = "Language_ID"
  ) %>%  #discarding languages with no or low numbers of L1 speakers and L2 speakers resulting primarily from language revival efforts
  filter(!Language_ID == "gami1243") %>%
  filter(!Language_ID == "tuni1252") %>%
  filter(!Language_ID == "yuch1247") %>%
  filter(!Language_ID == "mari1424") %>%
  filter(!Language_ID == "natc1249") %>%
  filter(!Language_ID == "poli1260") %>%   #removing Polish due to problems with coding
  inner_join(glottolog_df, by = "Language_ID") 

tree <- read.tree(file.path("data_wrangling/wrangled.tree")) 

#dropping tips not in Grambank
GB_fusion <- GB_fusion[GB_fusion$Language_ID %in% tree$tip.label, ]
tree <- keep.tip(tree, GB_fusion$Language_ID)

x <- assert_that(all(tree$tip.label %in% GB_fusion$Language_ID), msg = "The data and phylogeny taxa do not match")

## Building standardized phylogenetic precision matrix
tree_scaled <- tree

tree_vcv = vcv.phylo(tree_scaled)
typical_phylogenetic_variance = exp(mean(log(diag(tree_vcv))))

#We opt for a sparse phylogenetic precision matrix (i.e. it is quantified using all nodes and tips), since sparse matrices make the analysis in INLA less time-intensive
tree_scaled$edge.length <- tree_scaled$edge.length/typical_phylogenetic_variance
phylo_prec_mat <- MCMCglmm::inverseA(tree_scaled,
                                     nodes = "ALL",
                                     scale = FALSE)$Ainv

GB_fusion = GB_fusion[order(match(GB_fusion$Language_ID, rownames(phylo_prec_mat))),]

#"local" set of parameters
## Create spatial covariance matrix using the matern covariance function
spatial_covar_mat_1 = varcov.spatial(GB_fusion[,c("Longitude", "Latitude")], 
                                     cov.pars = phi_1, kappa = kappa)$varcov
# Calculate and standardize by the typical variance
typical_variance_spatial_1 = exp(mean(log(diag(spatial_covar_mat_1))))
spatial_cov_std_1 = spatial_covar_mat_1 / typical_variance_spatial_1
spatial_prec_mat_1 = solve(spatial_cov_std_1)
dimnames(spatial_prec_mat_1) = list(GB_fusion$Language_ID, GB_fusion$Language_ID)

## Since we are using a sparse phylogenetic matrix, we are matching taxa to rows in the matrix
phy_id = match(tree$tip.label, rownames(phylo_prec_mat))
GB_fusion$phy_id = phy_id

## Other effects are in the same order they appear in the dataset
GB_fusion$sp_id = 1:nrow(spatial_prec_mat_1)

