source("requirements.R")

#Script was written by Hedvig Skirgård, Hannah J. Haynie and Olena Shcherbakova.

#this script necessitates that grambank and glottolog files exist. if they do not, run generate_GB_input_file.R
#this script generates the Ethnologue file that can be published based on the raw ethnologue file from SIL. The raw file CANNOT be shared publicly, but derived information that cannot be transformed back to the original values can, such as scaled and log-transformed values

glottolog_df <- read_tsv("data_wrangling/glottolog_cldf_wide_df.tsv", show_col_types = F) %>% 
  dplyr::select(ISO_639 = ISO639P3code, Glottocode, Language_level_ID) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Glottocode, Language_level_ID))

GB <- read_tsv("data/GB_wide/GB_wide_strict.tsv", show_col_types = F) %>% 
  dplyr::select(Glottocode = "Language_ID")

#colnames(data_ethnologue)

#this script needs the Table_of_LICs.tab file to exists, which is only available to people with an SIL lisence
data_ethnologue <- read_tsv("data/Table_of_LICs.tab", show_col_types = F) %>%
  filter(!is.na(`EGIDS`)) %>%
  dplyr::mutate(Vehicularity = ifelse(EGIDS == "0" | EGIDS == "1" |
                                        EGIDS == "2" |
                                        EGIDS == "3", "1", "0")) %>% 
  left_join(glottolog_df, by = "ISO_639" ) %>% 
  dplyr::select(-Glottocode) %>% #removing old Glottocode column
  rename(Glottocode = Language_level_ID) 

#Making different df's for each count. This is because sometimes there is missing values for a language in a country for L1 OR L2, so it's better to calcualte them separately and then join them
L2_pop_df <- data_ethnologue %>%
  filter(!is.na(L2_Users)) %>% 
  dplyr::filter(L2_Users >= 0) %>%
  group_by(Glottocode) %>% 
  summarise(
  ISO_639_l2 = paste0(ISO_639, collapse = "; "),
L2_Users = sum(L2_Users), 
.groups = "drop"
)

L1_pop_df <- data_ethnologue %>%
  filter(!is.na(L1_Users)) %>% 
  dplyr::filter(L1_Users >= 0) %>% 
  group_by(Glottocode) %>% 
  summarise(
    ISO_639_L1 = paste0(ISO_639, collapse = "; "),
    L1_Users = sum(L1_Users), 
    .groups = "drop"
  )

All_pop_df <- data_ethnologue %>%
  filter(!is.na(All_Users)) %>% 
  dplyr::filter(All_Users >= 0) %>% 
  group_by(Glottocode) %>% 
  summarise(
    ISO_639_all = paste0(ISO_639, collapse = "; "),
    All_Users = sum(All_Users), 
    .groups = "drop"
  )

Vehicularity_df  <- data_ethnologue %>%
  filter(!is.na(Vehicularity)) %>% 
  group_by(Glottocode) %>% 
  summarise(
    ISO_639_vehic = paste0(ISO_639, collapse = "; "),
    Vehicularity = max(as.numeric(Vehicularity)), 
      .groups = "drop") 


###

joined_df <- L2_pop_df %>% 
  full_join(L1_pop_df, by = "Glottocode") %>% 
  full_join(All_pop_df, by = "Glottocode") %>% 
  full_join(Vehicularity_df, by = "Glottocode") %>% 
  inner_join(GB, by = "Glottocode" )
  


  

#this dataset will serve as the basis for the reanalysis on the large sample
data_ethnologue_reanalysis <- joined_df %>% 
  dplyr::mutate(L1_log10 = log10(L1_Users+1),
                All_Users_log10 = log10(All_Users+1)) %>% #adding a 1 for cases where pop is 0
  dplyr::select(Glottocode, L1_log10, L1_Users, All_Users_log10, All_Users, Vehicularity)




#this dataset will serve as the basis for the reanalysis on the small sample with available L2           
data_ethnologue_reanalysis_L2 <- joined_df %>%
  dplyr::mutate(L2_prop = L2_Users/ All_Users, 
                #calculating the proportion of L2 users out of the entire population
                L1_log10 = log10(L1_Users+1),
                All_Users_log10 = log10(All_Users+1)) %>% #adding a 1 for cases where pop is 0
  mutate(L2_prop = ifelse(All_Users == 0, 0, L2_prop)) %>% #if All users is 0, L2_prop would be NA if we didn't do this (can't divide by 0). It should be 0
  dplyr::select(Glottocode, L1_log10, L2_prop, L1_Users, All_Users_log10, All_Users, Vehicularity)



#do some the subsettting to GB and log10 and L2 prop
# data_ethnologue <- data_ethnologue %>% 
#   inner_join(GB, by = "Glottocode" ) %>% 
#     dplyr::mutate(L2_prop = L2_Users/ All_Users, 
#                 #calculating the proportion of L2 users out of the entire population
#                 L1_log10 = log10(L1_Users+1),
#                 All_Users_log10 = log10(All_Users+1)) %>% #adding a 1 for cases where pop is 0
#   mutate(L2_prop = ifelse(All_Users == 0, 0, L2_prop)) %>% #if All users is 0, L2_prop would be NA if we didn't do this (can't divide by 0). It should be 0
#   dplyr::select(Glottocode, ISO_639, L1_log10, L2_prop, L1_Users, All_Users_log10, All_Users)

#do the scaling
data_ethnologue_reanalysis$L1_scaled <- scale(data_ethnologue_reanalysis$L1_Users)[,1]
data_ethnologue_reanalysis_L2$L1_scaled <- scale(data_ethnologue_reanalysis_L2$L1_Users)[,1]

data_ethnologue_reanalysis$L1_log10_scaled <- scale(data_ethnologue_reanalysis$L1_log10)[,1]
data_ethnologue_reanalysis_L2$L1_log10_scaled <- scale(data_ethnologue_reanalysis_L2$L1_log10)[,1]

data_ethnologue_reanalysis$All_Users_scaled <- scale(data_ethnologue_reanalysis$All_Users)[,1]
data_ethnologue_reanalysis_L2$All_Users_scaled <- scale(data_ethnologue_reanalysis_L2$All_Users)[,1]

data_ethnologue_reanalysis$All_Users_log10_scaled <- scale(data_ethnologue_reanalysis$All_Users_log10)[,1]
data_ethnologue_reanalysis_L2$All_Users_log10_scaled <- scale(data_ethnologue_reanalysis_L2$All_Users_log10)[,1]


#write to file: Ethnologue data for supplementary materials and merging into "reduced" version of the final dataset with social variables (excluding L1_log10)
#large sample
data_ethnologue_reanalysis %>% 
  dplyr::select(ISO_639, Language_ID=Glottocode, L1_scaled, L1_log10_st=L1_log10_scaled, Vehicularity, All_Users_scaled, All_Users_log10_scaled) %>% 
  write_tsv("data_wrangling/ethnologue_pop_SM.tsv")

#small L2 sample
data_ethnologue_reanalysis_L2 %>% 
  dplyr::select(ISO_639, Language_ID=Glottocode, L2_prop, L1_scaled, L1_log10_st=L1_log10_scaled, Vehicularity, All_Users_scaled, All_Users_log10_scaled) %>% 
  write_tsv("data_wrangling/ethnologue_pop_L2_SM.tsv")

#write to file: Ethnologue data for merging into "full" version of the final dataset with social variables (including L1_log10) - won't be available to public
#large sample
data_ethnologue_reanalysis %>% 
  dplyr::select(ISO_639, Language_ID=Glottocode, L1_st = L1_scaled, L1_log10_st=L1_log10_scaled, L1_log10, Vehicularity ) %>% 
  write_tsv("data_wrangling/ethnologue_pop_full.tsv")

#small L2 sample
data_ethnologue_reanalysis_L2 %>% 
  dplyr::select(ISO_639, Language_ID=Glottocode, L2_prop, L1_st = L1_scaled, L1_log10_st=L1_log10_scaled, L1_log10, Vehicularity ) %>% 
  write_tsv("data_wrangling/ethnologue_pop_L2_full.tsv")
