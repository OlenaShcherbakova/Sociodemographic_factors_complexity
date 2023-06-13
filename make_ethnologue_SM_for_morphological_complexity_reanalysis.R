source("requirements.R")

#Script was written by Hedvig Skirgård and modified by Olena Shcherbakova

#this script generates the Ethnologue file that can be published based on the raw ethnologue file from SIL. The raw file CANNOT be shared publicly, but derived information that cannot be transformed back to the original values can, such as scaled and log-transformed values

#this script needs the Table_of_languages.tab file to exists, which is only available to people with an SIL lisence
data_ethnologue <- read_tsv("data/Table_of_Languages.tab", show_col_types = F) %>%
  filter(!is.na("All_Users")) %>% #remove rows with missing data for pop of all users
  filter(!is.na("L1_Users")) %>% 
  dplyr::select(ISO_639, L1_Users, All_Users)


#do some the subsettting to GB and log10 and L2 prop
data_ethnologue <- data_ethnologue %>% 
  dplyr::mutate(L2 = All_Users - L1_Users, 
                #calculating the number of L2 users by subtracting the number of L1 from All users
                L2_prop = L2/ All_Users, 
                #calculating the proportion of L2 users out of the entire population
                L1_log10 = log10(L1_Users+1),
                All_Users_log10 = log10(All_Users+1)) %>% #adding a 1 for cases where pop is 0
  mutate(L2_prop = ifelse(All_Users == 0, 0, L2_prop)) %>% #if All users is 0, L2_prop would be NA if we didn't do this (can't divide by 0). It should be 0
  dplyr::select(ISO_639, L1_log10, L2_prop, L1_Users, All_Users_log10, All_Users)

#do the scaling
data_ethnologue$L1_scaled <- scale(data_ethnologue$L1_Users)[,1]
data_ethnologue$L1_log10_scaled <- scale(data_ethnologue$L1_log10)[,1]

data_ethnologue$All_Users_scaled <- scale(data_ethnologue$All_Users)[,1]
data_ethnologue$All_Users_log10_scaled <- scale(data_ethnologue$All_Users_log10)[,1]

#write to file: Ethnologue data for supplementary materials and merging into "reduced" version of the final dataset with social variables (excluding L1_log10)
data_ethnologue_file <- data_ethnologue %>% 
  dplyr::select(ISO_639, L2_prop, L1_scaled, L1_log10_scaled, All_Users_scaled, All_Users_log10_scaled)

data_ethnologue_file <- data_ethnologue_file %>% 
  write_tsv("data_wrangling/ethnologue_pop_SM_morph_compl_reanalysis.tsv")
