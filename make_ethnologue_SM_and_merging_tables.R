source("requirements.R")

#this script necessitates that grambank and glottolog files exist. if they do not, run generate_GB_input_file.R
#this script generates the Ethnologue file that can be published based on the raw ethnologue file from SIL. The raw file CANNOT be shared publicly, but derived information that cannot be transformed back to the original values can, such as scaled and log-transformed values

glottolog_df <- read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", show_col_types = F) %>% 
  dplyr::select(ISO_639 = ISO639P3code, Glottocode, Language_level_ID)

GB <- read_tsv("data/GB_wide/GB_wide_strict.tsv", show_col_types = F) %>% 
  dplyr::select(Glottocode = "Language_ID")

#this script needs the Table_of_languages.tab file to exists, which is only available to people with an SIL lisence
data_ethnologue <- read_tsv("data/Table_of_Languages.tab", show_col_types = F) %>%
  filter(!is.na("All_Users")) %>% #remove rows with missing data for pop of all users
  filter(!is.na("L1_Users")) %>% 
  left_join(glottolog_df, by = "ISO_639" ) %>% 
  dplyr::select(-Glottocode) %>% #removing old Glottocode column
  rename(Glottocode = Language_level_ID) %>% 
  group_by(Glottocode) %>% 
  summarise(All_Users = sum(All_Users, na.rm = T),
            L1_Users = sum(L1_Users, na.rm = T), 
            ISO_639 = paste0(ISO_639, collapse = "; "))
            
#do some the subsettting to GB and log10 and L2 prop
data_ethnologue <- data_ethnologue %>% 
  inner_join(GB, by = "Glottocode" ) %>% 
    dplyr::mutate(L2 = All_Users - L1_Users, 
                #calculating the number of L2 users by subtracting the number of L1 from All users
                L2_prop = L2/ All_Users, 
                #calculating the proportion of L2 users out of the entire population
                L1_log10 = log10(L1_Users+1),
                All_Users_log10 = log10(All_Users+1)) %>% #adding a 1 for cases where pop is 0
  mutate(L2_prop = ifelse(All_Users == 0, 0, L2_prop)) %>% #if All users is 0, L2_prop would be NA if we didn't do this (can't divide by 0). It should be 0
  dplyr::select(Glottocode, ISO_639, L1_log10, L2_prop, L1_Users, All_Users_log10, All_Users)

#do the scaling
data_ethnologue$L1_scaled <- scale(data_ethnologue$L1_Users)[,1]
data_ethnologue$L1_log10_scaled <- scale(data_ethnologue$L1_log10)[,1]

data_ethnologue$All_Users_scaled <- scale(data_ethnologue$All_Users)[,1]
data_ethnologue$All_Users_log10_scaled <- scale(data_ethnologue$All_Users_log10)[,1]

#write to file: Ethnologue data for supplementary materials and merging into "reduced" version of the final dataset with social variables (excluding L1_log10)
data_ethnologue %>% 
  dplyr::select(ISO_639, Language_ID=Glottocode, L2_prop, L1_scaled, L1_log10_scaled, All_Users_scaled, All_Users_log10_scaled) %>% 
  write_tsv("data_wrangling/ethnologue_pop_SM.tsv")

#write to file: Ethnologue data for merging into "full" version of the final dataset with social variables (including L1_log10) - won't be available to public
data_ethnologue %>% 
  dplyr::select(ISO_639, Language_ID=Glottocode, L2_prop, L1_st = L1_scaled, L1_log10_st=L1_log10_scaled, L1_log10 ) %>% 
  write_tsv("data_wrangling/ethnologue_pop_full.tsv")