#create_pop_table

OUTPUTDIR_data_wrangling <- here("data_wrangling")
# create output dir if it does not exist.
if (!dir.exists(OUTPUTDIR_data_wrangling)) {
  dir.create(OUTPUTDIR_data_wrangling)
}


#Glottolog df for ISO_639 merging
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


if (sample == "full") {
  data_ethnologue <-
    read_tsv("data_wrangling/ethnologue_pop_full.tsv", show_col_types = F)
}

if (sample == "full_L2") {
  data_ethnologue <-
    read_tsv("data_wrangling/ethnologue_pop_L2_full.tsv", show_col_types = F)
}

if (sample == "reduced") {
  data_ethnologue <-
    read_tsv("data_wrangling/ethnologue_pop_SM.tsv", show_col_types = F)
}

if (sample == "reduced_L2") {
  data_ethnologue <-
    read_tsv("data_wrangling/ethnologue_pop_L2_SM.tsv", show_col_types = F)
}



# if (sample == "reduced") {
#   #double check if the file below needs to be changed
#   data_ethnologue <-
#     read_tsv("data_wrangling/ethnologue_pop_SM.tsv", show_col_types = F) %>%
#     dplyr::select(ISO_639, Language_ID, L1_log10_st, L2_prop)
# }

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


if (sample == "reduced_L2") {
  social_vars  %>%
    left_join(data_ethnologue, by = c("Language_ID")) %>%
    dplyr::select(
      Language_ID,
      L1_log10_st,
      Vehicularity,
      L2_prop,
      Education,
      Official,
      neighboring_languages_st
    ) %>%
    write_tsv(here(OUTPUTDIR_data_wrangling, "pop_reduced_L2.tsv"))
} else if (sample == "reduced") {
  social_vars  %>%
    left_join(data_ethnologue, by = c("Language_ID")) %>%
    dplyr::select(
      Language_ID,
      L1_log10_st,
      Vehicularity,
      Education,
      Official,
      neighboring_languages_st
    ) %>%
    write_tsv(here(OUTPUTDIR_data_wrangling, "pop_reduced.tsv"))
} else if (sample == "full_L2") {
  social_vars  %>%
    left_join(data_ethnologue, by = c("Language_ID")) %>%
    dplyr::select(
      Language_ID,
      L1_log10_st,
      L1_log10,
      Vehicularity,
      L2_prop,
      Education,
      Official,
      neighboring_languages_st
    ) %>%
    write_tsv(here(OUTPUTDIR_data_wrangling, "pop_full_L2.tsv"))
} else if (sample == "full") {
  social_vars  %>%
    left_join(data_ethnologue, by = c("Language_ID")) %>%
    dplyr::select(
      Language_ID,
      L1_log10_st,
      L1_log10,
      Vehicularity,
      Education,
      Official,
      neighboring_languages_st
    ) %>%
    write_tsv(here(OUTPUTDIR_data_wrangling, "pop_full.tsv"))
} else {
  cat("Not appropriate sample specification. Sample can be full, full_L2,
      reduced or reduced_L2.\n")
}



# if (sample == "full") {
#   social_vars  %>%
#     left_join(data_ethnologue, by = c("Language_ID")) %>%
#     dplyr::select(
#       Language_ID,
#       L1_log10_st,
#       L1_log10,
#       L2_prop,
#       Education,
#       Official,
#       neighboring_languages_st
#     ) %>%
#     write_tsv(here(OUTPUTDIR_data_wrangling, "pop_full.tsv"))
# }	else{
#   social_vars  %>%
#     left_join(data_ethnologue, by = c("Language_ID")) %>%
#     dplyr::select(Language_ID,
#                   L1_log10_st,
#                   L2_prop,
#                   Education,
#                   Official,
#                   neighboring_languages_st) %>%
#     write_tsv(here(OUTPUTDIR_data_wrangling, "pop_reduced.tsv"))
# }

glottolog_df_ISO <- glottolog_df %>% 
dplyr::select("Language_ID", "ISO_639")

if (sample == "reduced") {
  social_vars  %>%
    left_join(data_ethnologue, by = c("Language_ID")) %>%
    dplyr::select(Language_ID,
                  L1_log10_st,
                  Vehicularity,
                  Education,
                  Official,
                  neighboring_languages_st) %>%
    left_join(glottolog_df_ISO,
              by = c("Language_ID")) %>%
    write_tsv(here(OUTPUTDIR_data_wrangling, "pop_reduced_with_ISO.tsv"))
}

if (sample == "reduced_L2") {
  social_vars  %>%
    left_join(data_ethnologue, by = c("Language_ID")) %>%
    dplyr::select(Language_ID,
                  L1_log10_st,
                  Vehicularity,
                  L2_prop,
                  Education,
                  Official,
                  neighboring_languages_st) %>%
    left_join(glottolog_df_ISO,
              by = c("Language_ID")) %>%
    write_tsv(here(OUTPUTDIR_data_wrangling, "pop_L2_reduced_with_ISO.tsv"))
}
