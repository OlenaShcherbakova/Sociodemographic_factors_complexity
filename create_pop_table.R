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
  
  
  if(sample == "full"){
    data_ethnologue <- read_tsv("data/Table_of_Languages.tab", show_col_types = F) %>%
    dplyr::select(!c(Longitude, Latitude)) %>%
    left_join(glottolog_df, by = "ISO_639") %>% 
    rename(ISO = ISO_639) %>%
    dplyr::select(Language_ID = Glottocode, ISO, L1 = L1_Users, All_Users) %>% 
    dplyr::mutate(L2 = All_Users - L1, 
                  #calculating the number of L2 users by subtracting the number of L1 from All users
                  L2_prop = L2/ All_Users, 
                  #calculating the proportion of L2 users out of the entire population
                  L1_log10 = log10(L1+1)) %>%
    dplyr::select("Language_ID", "ISO", "L1_log10", "L2_prop", "L1", "L2") %>% 
    dplyr::mutate(L1_log10_st = scale(L1_log10)[,1])
  
  }
  
  
  if(sample == "reduced"){
    #double check if the file below needs to be changed
    data_ethnologue <- read_tsv("data_wrangling/ethnologue_pop_SM.tsv", show_col_types = F) %>% 
      rename(L1_log10_st = L1_log10_scaled) %>%
      dplyr::select(ISO_639, Glottocode, L1_log10_st, L2_prop) %>% 
      distinct(Glottocode, .keep_all=TRUE)
      
    
  }
  
social_vars <- readxl::read_xlsx("data/lang_endangerment_predictors.xlsx", sheet = "Supplementary data 1", skip=1, col_types ="text", na = "NA") %>%
    left_join(glottolog_df, by = c("ISO" = "ISO_639")) %>% 
    rename("ISO_639" = "ISO") %>% 
    dplyr::select(Language_ID = Glottocode, ISO_639, official_status, language_of_education, bordering_language_richness) %>% 
    rename(Official = official_status) %>% 
    #  naniar::replace_with_na(replace = list(L1_log10 = -Inf, L2_log10 = -Inf)) #removing for now
    dplyr::mutate(neighboring_languages=bordering_language_richness, Education=language_of_education) %>%
    dplyr::mutate(neighboring_languages = as.numeric(neighboring_languages)) %>% 
    #dplyr::mutate(neighboring_languages_log10 = log10(neighboring_languages+1)) %>%
    dplyr::mutate(neighboring_languages_st  = scale(neighboring_languages)[,1]) %>%
    #dplyr::mutate(neighboring_languages_log10_st  = scale(neighboring_languages_log10)[,1]) %>%
    dplyr::select(Language_ID, Education, Official, neighboring_languages_st)
  
  if(sample == "full"){
    social_vars  %>% 		
      left_join(data_ethnologue, by=c("Language_ID")) %>% 
      dplyr::select(Language_ID, L1_log10_st, L1_log10, L2_prop, Education, Official, neighboring_languages_st) %>% 
            write_tsv(here(OUTPUTDIR_data_wrangling, "pop_full.tsv")) }	else{
        social_vars  %>% 	
                left_join(data_ethnologue, by=c("Language_ID" = "Glottocode")) %>% 
                dplyr::select(Language_ID, L1_log10_st, L2_prop, Education, Official, neighboring_languages_st) %>% 
                    write_tsv(here(OUTPUTDIR_data_wrangling, "pop_reduced.tsv"))
            }
