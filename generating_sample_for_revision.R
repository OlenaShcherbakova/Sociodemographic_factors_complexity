

### Currect approach ###
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

areas <-
  read_tsv("data_wrangling/glottolog_AUTOTYPE_areas.tsv",
           show_col_types = F) %>%
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
  left_join(social_vars, by = c("Language_ID")) %>%
  full_join(read_tsv(
    "output/Informativity/informativity_score.tsv",
    show_col_types = F
  ),
  by = "Language_ID") %>%
  full_join(read_tsv("output/Bound_morph/bound_morph_score.tsv", show_col_types = F),
            by = "Language_ID") %>%
  full_join(glottolog_df, by = "Language_ID") %>%
  #full_join(mean_size, by = "Language_ID") %>%
  #full_join(hierarchy, by = "Language_ID") %>%
  full_join(areas, by = "Language_ID") %>%
  dplyr::select(
    Language_ID,
    ISO_639,
    Family_ID,
    Latitude,
    Longitude,
    boundness_st,
    informativity_st,
    boundness,
    Informativity,
    L1_log10_st,
    Vehicularity,
    L1_log10,
    Education,
    Official,
    neighboring_languages_st,
    AUTOTYP_area
  ) %>%
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
current <- current[current$Language_ID %in% tree$tip.label,]

current <- current %>%
  dplyr::mutate(used_in_reanalysis = "yes") %>%
  dplyr::select(Language_ID, ISO_639, used_in_reanalysis)




### Old approach ###
glottolog_df <-
  read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv",
           show_col_types = F) %>%
  dplyr::select(ISO_639 = ISO639P3code, Glottocode, Language_level_ID) %>%
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID))

GB <-
  read_tsv("data/GB_wide/GB_wide_strict.tsv", show_col_types = F) %>%
  dplyr::select(Glottocode = "Language_ID")

#this script needs the Table_of_languages.tab file to exists, which is only available to people with an SIL lisence
data_ethnologue <-
  read_tsv("data/Table_of_Languages.tab", show_col_types = F) %>%
  filter(!is.na("All_Users")) %>% #remove rows with missing data for pop of all users
  filter(!is.na("L1_Users")) %>%
  left_join(glottolog_df, by = "ISO_639") %>%
  dplyr::select(-Glottocode) %>% #removing old Glottocode column
  rename(Glottocode = Language_level_ID) %>%
  group_by(Glottocode) %>%
  summarise(All_Users = sum(All_Users, na.rm = T),
            L1_Users = sum(L1_Users, na.rm = T),
            ISO_639 = paste0(ISO_639, collapse = "; "))
  # summarise(
  #   All_Users = sum(All_Users),
  #   L1_Users = sum(L1_Users),
  #   ISO_639 = paste0(ISO_639, collapse = "; ")
  # )


#do some the subsettting to GB and log10 and L2 prop
data_ethnologue <- data_ethnologue %>%
  inner_join(GB, by = "Glottocode") %>%
  dplyr::mutate(
    L2 = All_Users - L1_Users,
    #calculating the number of L2 users by subtracting the number of L1 from All users
    L2_prop = L2 / All_Users,
    #calculating the proportion of L2 users out of the entire population
    L1_log10 = log10(L1_Users + 1),
    All_Users_log10 = log10(All_Users + 1)
  ) %>% #adding a 1 for cases where pop is 0
  mutate(L2_prop = ifelse(All_Users == 0, 0, L2_prop)) %>% #if All users is 0, L2_prop would be NA if we didn't do this (can't divide by 0). It should be 0
  dplyr::select(Glottocode,
                ISO_639,
                L1_log10,
                L2_prop,
                L1_Users,
                All_Users_log10,
                All_Users)

#do the scaling
data_ethnologue$L1_scaled <- scale(data_ethnologue$L1_Users)[, 1]
data_ethnologue$L1_log10_scaled <-
  scale(data_ethnologue$L1_log10)[, 1]

data_ethnologue$All_Users_scaled <-
  scale(data_ethnologue$All_Users)[, 1]
data_ethnologue$All_Users_log10_scaled <-
  scale(data_ethnologue$All_Users_log10)[, 1]

old <- data_ethnologue %>%
  dplyr::select(
    Glottocode,
    L1_log10,
    L2_prop,
    L1_Users,
    All_Users_log10,
    All_Users,
    L1_st = L1_scaled,
    L1_log10_st = L1_log10_scaled,
    All_Users_scaled,
    All_Users_log10_scaled
  )




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

areas <-
  read_tsv("data_wrangling/glottolog_AUTOTYPE_areas.tsv",
           show_col_types = F) %>%
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


old <- old %>%
  rename(Language_ID = Glottocode) %>%
  left_join(social_vars, by = c("Language_ID")) %>%
  full_join(read_tsv(
    "output/Informativity/informativity_score.tsv",
    show_col_types = F
  ),
  by = "Language_ID") %>%
  full_join(read_tsv("output/Bound_morph/bound_morph_score.tsv", show_col_types = F),
            by = "Language_ID") %>%
  full_join(glottolog_df, by = "Language_ID") %>%
  #full_join(mean_size, by = "Language_ID") %>%
  #full_join(hierarchy, by = "Language_ID") %>%
  full_join(areas, by = "Language_ID") %>%
  dplyr::select(
    Language_ID,
    ISO_639,
    Family_ID,
    Latitude,
    Longitude,
    boundness_st,
    informativity_st,
    boundness,
    Informativity,
    L1_st,
    L1_log10,
    #L1_log10_st = L1_log10_scaled,
    L1_log10_st,
    L2_prop,
    Education,
    Official,
    neighboring_languages_st,
    AUTOTYP_area
  ) %>%
  #discarding languages with no or low numbers of L1 speakers and L2 speakers resulting primarily from language revival efforts
  filter(!Language_ID == "gami1243") %>%
  filter(!Language_ID == "tuni1252") %>%
  filter(!Language_ID == "yuch1247") %>%
  filter(!Language_ID == "mari1424") %>%
  filter(!Language_ID == "natc1249") %>%
  filter(!Language_ID == "poli1260")  #removing Polish due to problems with coding

tree <- read.tree(file.path("data_wrangling/wrangled.tree"))


old <- old %>%
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
old <- old[old$Language_ID %in% tree$tip.label,]

old <- old %>%
  dplyr::mutate(used_in_old_analysis = "yes") %>%
  dplyr::select(Language_ID, ISO_639, used_in_old_analysis)

both <- current %>%
  full_join(old, by = "Language_ID")
