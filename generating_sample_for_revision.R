# glottolog_df <- read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", show_col_types = F) %>% 
#   dplyr::select(ISO_639 = ISO639P3code, Glottocode, Language_level_ID) %>% 
#   mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID))

glottolog_df <-
  read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", col_types = cols()) %>%
  dplyr::select(
    Glottocode,
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
    Language_ID,
    ISO_639,
    Language_level_ID,
    level,
    Family_ID,
    Longitude,
    Latitude
  )

areas <- read_tsv("data_wrangling/glottolog_AUTOTYPE_areas.tsv", show_col_types = F) %>% 
  dplyr::select(Language_ID, AUTOTYP_area)

social_vars <-
  readxl::read_xlsx(
    "data/lang_endangerment_predictors.xlsx",
    sheet = "Supplementary data 1",
    skip = 1,
    col_types = "text",
    na = "NA"
  ) %>%
  left_join(glottolog_df, by = c("ISO" = "ISO_639")) %>%
  rename("ISO_639" = "ISO") %>%
  dplyr::select(
    Language_ID = Glottocode,
    ISO_639,
    official_status,
    language_of_education,
    bordering_language_richness
  ) %>%
  rename(Official = official_status) %>%
  #  naniar::replace_with_na(replace = list(L1_log10 = -Inf, L2_log10 = -Inf)) #removing for now
  dplyr::mutate(neighboring_languages = bordering_language_richness, Education =
                  language_of_education) %>%
  dplyr::mutate(neighboring_languages = as.numeric(neighboring_languages)) %>%
  #dplyr::mutate(neighboring_languages_log10 = log10(neighboring_languages+1)) %>%
  dplyr::mutate(neighboring_languages_st  = scale(neighboring_languages)[, 1]) %>%
  #dplyr::mutate(neighboring_languages_log10_st  = scale(neighboring_languages_log10)[,1]) %>%
  dplyr::select(Language_ID, Education, Official, neighboring_languages_st)


current <- read_tsv("data_wrangling/ethnologue_pop_full.tsv") %>% 
  left_join(social_vars, by=c("Language_ID")) %>% 
  full_join(read_tsv("output/Informativity/informativity_score.tsv", show_col_types = F), by = "Language_ID" ) %>% 
  full_join(read_tsv("output/Bound_morph/bound_morph_score.tsv", show_col_types = F), by = "Language_ID" ) %>%
  full_join(glottolog_df, by = "Language_ID") %>%
  #full_join(mean_size, by = "Language_ID") %>%
  #full_join(hierarchy, by = "Language_ID") %>%
  full_join(areas, by = "Language_ID") %>% 
  dplyr::select(Language_ID, ISO_639, Family_ID, Latitude, Longitude, 
                boundness_st, informativity_st, boundness, Informativity, 
                L1_log10_st, Vehicularity, L1_log10, Education, Official, 
                neighboring_languages_st, AUTOTYP_area) %>% 
  #discarding languages with no or low numbers of L1 speakers and L2 speakers resulting primarily from language revival efforts
  filter(!Language_ID == "gami1243") %>%
  filter(!Language_ID == "kaur1267") %>% 
  filter(!Language_ID == "klam1254") %>% 
  filter(!Language_ID == "waka1274") %>% 
  filter(!Language_ID == "tuni1252") %>%
  filter(!Language_ID == "yuch1247") %>%
  filter(!Language_ID == "mari1424") %>%
  filter(!Language_ID == "natc1249") %>%
  filter(!Language_ID == "poli1260")  #removing Polish due to problems with coding

tree <- read.tree(file.path("data_wrangling/wrangled.tree")) 

current <- current %>% 
  filter(!is.na(L1_log10_st)) %>%
  rename(L1_log_st = L1_log10_st) %>%
  mutate(L1_copy = L1_log_st) %>%
  # filter(!is.na(L2_prop)) %>%
  # mutate(L2_copy = L2_prop) %>%
  filter(!is.na(neighboring_languages_st)) %>%
  filter(!is.na(Official)) %>%
  filter(!is.na(Education)) %>%
  filter(!is.na(boundness_st)) %>%
  filter(!is.na(informativity_st))

#dropping tips not in Grambank
current <- current[current$Language_ID %in% tree$tip.label, ]

current <- current %>% 
  dplyr::mutate(used_in_reanalysis = "yes") %>% 
  dplyr::select(Language_ID, ISO_639, used_in_reanalysis)




