#generate summary tables of WAIC values and effects for all boundness and informativity models (including social-only models and models including non-linear effects)

#for random effects only, the access folder is output_tables rather than output_tables_reduced 
effs_B_SP <- read.csv("output_tables/ effects Boundness_phylogenetic_spatial_models .csv") 
effs_B_social <- read.csv("output_tables_reduced/ effects Boundness_social_models .csv")
effs_B_social_only <- read.csv("output_tables_reduced/ effects Boundness_social_only_models .csv")

effs_B <- as.data.frame(rbind(effs_B_SP, effs_B_social, effs_B_social_only)) %>%
  mutate(response="boundness")

effs_I_SP <- read.csv("output_tables/ effects Informativity_phylogenetic_spatial_models .csv")
effs_I_social <- read.csv("output_tables_reduced/ effects Informativity_social_models .csv")
effs_I_social_only <- read.csv("output_tables_reduced/ effects Informativity_social_only_models .csv")

effs_I <- as.data.frame(rbind(effs_I_SP, effs_I_social, effs_I_social_only)) %>%
  mutate(response="informativity") 

all_effs <- as.data.frame(rbind(effs_B, effs_I)) %>%
  rename("2.5%"=2,
         "50%" = 3,
         "97.5%" = 4) %>%
  relocate(effect, .after = model) %>%
  relocate(response, .after = model) 

write.csv(all_effs, "output_tables_reduced/Table_INLA_all_models.csv")

all_effs <- all_effs %>% 
  flextable() %>%
  autofit() %>%
  merge_v(j=c("response", "model")) %>%
  fix_border_issues() 

save_as_docx(
  "Summary of all fitted INLA models" = all_effs, 
  path = "output_tables_reduced/Table_INLA_all_models.docx")
