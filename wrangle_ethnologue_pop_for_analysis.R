source("requirements.R")

read_tsv("data_wrangling/ethnologue_pop_SM.tsv", show_col_types = F) %>% 
  group_by(Glottocode) %>% #join dialects
   summarise(L2_prop = mean(L2_prop, na.rm = T),
             L1_scaled = mean(L1_scaled, na.rm = T),
             L1_log10_scaled = mean(L1_log10_scaled, na.rm = T),
             All_users_scaled = mean(All_Users_scaled, na.rm = T),
             All_Users_log10_scaled = mean(All_Users_log10_scaled, na.rm = T),
             number_of_lgs_in_ethnologue = n(),
             ISO_639 = paste0(ISO_639, collapse = ", ")) %>%
  write_tsv("data_wrangling/ethnologue_pop_SM_dialects_joined.tsv") 

