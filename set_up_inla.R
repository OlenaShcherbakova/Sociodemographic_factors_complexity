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

if(sample == "reduced"){
  pop_file_fn <- "data_wrangling/pop_reduced.tsv" 
} else {
  pop_file_fn <- "data_wrangling/pop_full.tsv"
}

metrics_joined <- read_tsv(pop_file_fn, show_col_types = F ) %>% 
  full_join(read_tsv("output/Informativity/informativity_score.tsv", show_col_types = F), by = "Language_ID" ) %>% 
  full_join(read_tsv("output/Bound_morph/bound_morph_score.tsv", show_col_types = F), by = "Language_ID" ) %>%
  full_join(glottolog_df, by = "Language_ID") %>%
  #full_join(mean_size, by = "Language_ID") %>%
  #full_join(hierarchy, by = "Language_ID") %>%
  full_join(areas, by = "Language_ID") 

if(sample == "reduced"){
  metrics_joined <- metrics_joined %>% 
    dplyr::select(Language_ID, Name, Family_ID, Macroarea, Latitude, Longitude, boundness_st, informativity_st, boundness, Informativity, L1_log10_st, L2_prop, Education, Official, neighboring_languages_st, AUTOTYP_area)
} else {
  metrics_joined <- metrics_joined %>% 
    dplyr::select(Language_ID, Name, Family_ID, Macroarea, Latitude, Longitude, boundness_st, informativity_st, boundness, Informativity, L1_log10_st, L1_log10, L2_prop, Education, Official, neighboring_languages_st, AUTOTYP_area)
}

metrics_joined <- metrics_joined %>% 
  #discarding languages with no or low numbers of L1 speakers and L2 speakers resulting primarily from language revival efforts
  filter(!Language_ID == "gami1243") %>%
  filter(!Language_ID == "tuni1252") %>%
  filter(!Language_ID == "yuch1247") %>%
  filter(!Language_ID == "mari1424") %>%
  filter(!Language_ID == "natc1249") %>%
  filter(!Language_ID == "poli1260")  #removing Polish due to problems with coding

tree <- read.tree(file.path("data_wrangling/wrangled.tree"))   
         