#create_pop_table

OUTPUTDIR_data_wrangling<- here("data_wrangling")		
# create output dir if it does not exist.		
if (!dir.exists(OUTPUTDIR_data_wrangling)) { dir.create(OUTPUTDIR_data_wrangling) }	


  #Glottolog df for ISO_639 merging
  glottolog_df <- read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", col_types = cols()) %>%
    dplyr::select(Glottocode, Language_ID, "ISO_639"= ISO639P3code, Language_level_ID, level, Family_ID, Longitude, Latitude) %>% 
    mutate(Language_level_ID = if_else(is.na(Language_level_ID), Glottocode, Language_level_ID))  %>% 
    mutate(Family_ID = ifelse(is.na(Family_ID), Language_level_ID, Family_ID)) %>%
    dplyr::select(Glottocode, Language_ID, ISO_639, Language_level_ID, level, Family_ID, Longitude, Latitude) 
  
  if(full_or_reduced == "full"){
    data_ethnologue <- read_tsv("data/Table_of_Languages.tab", show_col_types = F) %>%
    dplyr::select(!c(Longitude, Latitude)) %>%
    left_join(glottolog_df, by = "ISO_639") %>% 
    rename(ISO = ISO_639) %>%
    dplyr::select(Language_ID = Glottocode, ISO, L1 = L1_Users, All_Users, EGIDS, Is_Written, Institutional) %>% 
    dplyr::mutate(L2 = All_Users - L1, 
                  #calculating the number of L2 users by subtracting the number of L1 from All users
                  L2_prop = L2/ All_Users, 
                  #calculating the proportion of L2 users out of the entire population
                  L1_log10 = log10(L1+1)) %>%
    dplyr::select("Language_ID", "ISO", "L1_log10", "L2_prop", "L1", "L2")
  
  }
  
social_vars <- readxl::read_xlsx("data/lang_endangerment_predictors.xlsx", sheet = "Supplementary data 1", skip=1, col_types ="text", na = "NA") %>%
#    plyr::join(data_ethnologue, by = "ISO") %>%
#    rename(ISO_639 = "ISO") %>%
    left_join(glottolog_df, by = "ISO_639") %>% 
    dplyr::select(Language_ID = Glottocode, ISO_639, L1_log10, L2_prop, EGIDS, official_status, language_of_education, bordering_language_richness, pop_density) %>% 
    rename(Official = official_status) %>% 
    dplyr::mutate(L1_log10_st = scale(L1_log10)[,1]) %>% 
    #  naniar::replace_with_na(replace = list(L1_log10 = -Inf, L2_log10 = -Inf)) #removing for now
    mutate(Official_EGIDS = if_else(str_detect(EGIDS, "0") | #making a new variable for every language that has EGIDS of 0, 1 or 2
                                str_detect(EGIDS, "1") |  
                                str_detect(EGIDS, "2"), "1", "0" )) %>% 
    mutate(Official_EGIDS = if_else(str_detect(EGIDS, "10"), "0", Official_EGIDS)) %>% #making it so that cases where EGIDS is 10 are set to "Not Official"
    #  mutate(EGIDS = str_replace_all(EGIDS, "a", "" )) %>% 
    #  mutate(EGIDS = str_replace_all(EGIDS, "b", "" )) %>% 
    #  mutate(EGIDS = str_replace_all(EGIDS, "x", "" )) %>% 
    #  mutate(EGIDS = as.numeric(EGIDS)) %>%
    mutate(Shifting_or_Extinct = if_else(str_detect(EGIDS, "7") | 
                                           str_detect(EGIDS, "8a") |
                                           str_detect(EGIDS, "8b") |
                                           str_detect(EGIDS, "9") |
                                           str_detect(EGIDS, "10"), "1", "0")) %>%
    dplyr::mutate(neighboring_languages=bordering_language_richness, population_density=pop_density, Education=language_of_education) %>%
    dplyr::mutate(neighboring_languages = as.numeric(neighboring_languages)) %>% 
    dplyr::mutate(population_density = as.numeric(population_density)) %>%
    dplyr::mutate(population_density_log10 = log10(population_density+1)) %>% 
    dplyr::mutate(density_log10_st  = scale(population_density_log10 )[,1]) %>% 
    #dplyr::mutate(neighboring_languages_log10 = log10(neighboring_languages+1)) %>% 
    dplyr::mutate(neighboring_languages_st  = scale(neighboring_languages)[,1]) %>%
    #dplyr::mutate(neighboring_languages_log10_st  = scale(neighboring_languages_log10)[,1]) %>%
    dplyr::select(Language_ID, L1_log10_st, L1_log10, L2_prop, Education, Official, neighboring_languages_st, Shifting_or_Extinct)
  
  if(full_or_reduced == "full"){
    social_vars  %>% 		
      left_join(ethnologue) %>% 

            write_tsv(here(OUTPUTDIR_data_wrangling, "pop.tsv")) }	else{
        social_vars  %>% 		
                    write_tsv(here(OUTPUTDIR_data_wrangling, "pop_reduced.tsv"))
        
        }