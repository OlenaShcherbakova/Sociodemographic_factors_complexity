source("requirements.R")

#parameters
kappa = 1 
phi_1 = c(1, 1.25) # "Local" version: (sigma, phi) First value is not used
phi_2 = c(1, 17) # "Regional" version: (sigma, phi) First value is not used

#GB langs for subsetting
GB_langs <- read_tsv("data/GB_wide/GB_wide_strict.tsv", col_types = WIDE_COLSPEC) %>% 
  dplyr::select(Language_ID)

areas <- read_tsv("data_wrangling/glottolog_AUTOTYPE_areas.tsv", show_col_types = F) %>% 
  dplyr::select(Language_ID, AUTOTYP_area)

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

# #checking closest languoids
# glottolog_matrix <- glottolog_df %>% 
#   column_to_rownames("Language_ID") %>% 
#   dplyr::select(Longitude, Latitude) %>% 
#   as.matrix()
# 
# dists <- rdist.earth(glottolog_matrix, miles = F)
# 
# diag(dists) <- NA
# 
# dists %>% 
#   reshape2::melt() %>% 
#   arrange(value) %>%
#   .[1:10,]

sample <- "reduced_L2"
pop_file_fn <- "data_wrangling/pop_reduced_L2.tsv"

metrics_joined <- read_tsv(pop_file_fn, show_col_types = F ) %>% 
  full_join(read_tsv("output/Informativity/informativity_score.tsv", show_col_types = F), by = "Language_ID" ) %>% 
  full_join(read_tsv("output/Bound_morph/bound_morph_score.tsv", show_col_types = F), by = "Language_ID" ) %>%
  full_join(glottolog_df, by = "Language_ID") %>%
  #full_join(mean_size, by = "Language_ID") %>%
  #full_join(hierarchy, by = "Language_ID") %>%
  full_join(areas, by = "Language_ID") 

metrics_joined <- metrics_joined %>% 
  dplyr::select(Language_ID, Name, Family_ID, Macroarea, Latitude, Longitude, 
                  boundness_st, informativity_st, boundness, Informativity, 
                  L1_log10_st, L2_prop, Vehicularity, Education, Official, 
                  neighboring_languages_st, AUTOTYP_area)

metrics_joined <- metrics_joined %>% 
  #discarding languages with no or low numbers of L1 speakers and L2 speakers resulting primarily from language revival efforts
  filter(!Language_ID == "gami1243") %>%
  filter(!Language_ID == "tuni1252") %>%
  filter(!Language_ID == "yuch1247") %>%
  filter(!Language_ID == "mari1424") %>%
  filter(!Language_ID == "natc1249") %>%
  filter(!Language_ID == "poli1260")  #removing Polish due to problems with coding

tree <- read.tree(file.path("data_wrangling/wrangled.tree"))   

metrics_joined <- metrics_joined %>% 
  filter(!is.na(L1_log10_st)) %>%
  rename(L1_log_st = L1_log10_st) %>%
  mutate(L1_copy = L1_log_st) %>%
  filter(!is.na(L2_prop)) %>%
  mutate(L2_copy = L2_prop) %>%
  filter(!is.na(neighboring_languages_st)) %>%
  filter(!is.na(Official)) %>%
  filter(!is.na(Education)) %>%
  filter(!is.na(boundness_st)) %>%
  filter(!is.na(informativity_st))

#dropping tips not in Grambank
metrics_joined <- metrics_joined[metrics_joined$Language_ID %in% tree$tip.label, ]
tree <- keep.tip(tree, metrics_joined$Language_ID)

x <- assert_that(all(tree$tip.label %in% metrics_joined$Language_ID), msg = "The data and phylogeny taxa do not match")

## Building standardized phylogenetic precision matrix
tree_scaled <- tree

tree_vcv = vcv.phylo(tree_scaled)
typical_phylogenetic_variance = exp(mean(log(diag(tree_vcv))))

#We opt for a sparse phylogenetic precision matrix (i.e. it is quantified using all nodes and tips), since sparse matrices make the analysis in INLA less time-intensive
tree_scaled$edge.length <- tree_scaled$edge.length/typical_phylogenetic_variance
phylo_prec_mat <- MCMCglmm::inverseA(tree_scaled,
                                     nodes = "ALL",
                                     scale = FALSE)$Ainv

metrics_joined = metrics_joined[order(match(metrics_joined$Language_ID, rownames(phylo_prec_mat))),]

#"local" set of parameters
## Create spatial covariance matrix using the matern covariance function
spatial_covar_mat_1 = varcov.spatial(metrics_joined[,c("Longitude", "Latitude")], 
                                     cov.pars = phi_1, kappa = kappa)$varcov
# Calculate and standardize by the typical variance
typical_variance_spatial_1 = exp(mean(log(diag(spatial_covar_mat_1))))
spatial_cov_std_1 = spatial_covar_mat_1 / typical_variance_spatial_1
spatial_prec_mat_1 = solve(spatial_cov_std_1)
dimnames(spatial_prec_mat_1) = list(metrics_joined$Language_ID, metrics_joined$Language_ID)

View(spatial_covar_mat_1)

#calculate geo_dists
lat_long_matrix <- metrics_joined %>% 
  column_to_rownames("Language_ID") %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

rdist.earth_dists <- fields::rdist.earth(lat_long_matrix, miles = FALSE)

rdist.earth_dists[upper.tri(rdist.earth_dists, diag = TRUE)] <- NA

dists_vector <- as.vector(rdist.earth_dists) %>% na.omit()

#"local" set of parameters
## Create spatial covariance matrix using the matern covariance function
spatial_covar_mat_2 = varcov.spatial(dists.lowertri = dists_vector, 
                                     cov.pars = phi_1, kappa = kappa)$varcov
# Calculate and standardize by the typical variance
typical_variance_spatial_2 = exp(mean(log(diag(spatial_covar_mat_2))))
spatial_cov_std_2 = spatial_covar_mat_2 / typical_variance_spatial_2
spatial_prec_mat_2 = solve(spatial_cov_std_2)
dimnames(spatial_prec_mat_2) = list(metrics_joined$Language_ID, metrics_joined$Language_ID)

View(spatial_covar_mat_2)



# ## Since we are using a sparse phylogenetic matrix, we are matching taxa to rows in the matrix
# phy_id = match(tree$tip.label, rownames(phylo_prec_mat))
# metrics_joined$phy_id = phy_id
# 
# ## Other effects are in the same order they appear in the dataset
# metrics_joined$sp_id = 1:nrow(spatial_prec_mat_1)
# 
