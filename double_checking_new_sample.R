source("set_up_general.R")

#checking whether the languages in final dataframes used in the analysis are the same 

sample <- "reduced"

if(sample == "reduced"){
  source("set_up_inla.R")
  
  metrics_joined <- metrics_joined %>% 
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
  metrics_joined <- metrics_joined[metrics_joined$Language_ID %in% tree$tip.label, ]
  
  metrics_joined %>% write_tsv("data_wrangling/reduced_analysis_sample.tsv")

}
  
sample <-  "full"
if(sample == "full"){
  source("set_up_inla.R")
  
  metrics_joined <- metrics_joined %>% 
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
  metrics_joined <- metrics_joined[metrics_joined$Language_ID %in% tree$tip.label, ]
  
  metrics_joined %>% write_tsv("data_wrangling/full_analysis_sample.tsv")
}

full <- read_tsv("data_wrangling/full_analysis_sample.tsv")
reduced <- read_tsv("data_wrangling/reduced_analysis_sample.tsv")

ab <- data.frame(a=c(full$Language_ID, rep(NA, times=8)), 
                 b=reduced$Language_ID,
                 boundness_full=c(full$boundness_st, rep(NA, times=8)),
                 boundness_reduced=reduced$boundness_st)

#the discrepancies are found in extra languages present in the reduced sample
in_full_not_in_reduced <- ab$a[!(ab$a %in% ab$b)]
in_reduced_not_in_full <-ab$b[!(ab$a %in% ab$b)]

non_overlapping_languages <- c(in_full_not_in_reduced, in_reduced_not_in_full)

#Are transformed scores the same?
reduced[!(reduced$Language_ID %in% 
                        full$Language_ID)]$Language_ID

#checking for suspicious L2 proportions in languages with no or few L1 speakers & not detecting any
reduced <- reduced %>% 
  mutate(suspicious = ifelse(L1_log_st < -2 & L2_prop > 0, 1, 0))
         
         
      
pop_full <- read_tsv("data_wrangling/pop_full.tsv")
pop_reduced <- read_tsv("data_wrangling/pop_reduced.tsv")
