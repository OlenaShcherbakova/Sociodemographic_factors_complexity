#generate summary tables of WAIC values and effects for all boundness and informativity models (including social-only models and models including non-linear effects)

effs_B_SP <-
  read.csv("output_tables_reanalysis/ effects Boundness_reanalysis_phylogenetic_spatial_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)
effs_B_social <-
  read.csv("output_tables_reanalysis/ effects Boundness_reanalysis_social_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)
effs_B_social_only <-
  read.csv("output_tables_reanalysis/ effects Boundness_reanalysis_social_only_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_B <-
  as.data.frame(rbind(effs_B_SP, effs_B_social, effs_B_social_only)) %>%
  dplyr::mutate(response = "fusion") %>% 
  dplyr::mutate(`sample` = "entire")



effs_B_SP_L2 <-
  read.csv("output_tables_reanalysis/ effects Boundness_L2_phylogenetic_spatial_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)
effs_B_social_L2 <-
  read.csv("output_tables_reanalysis/ effects Boundness_L2_social_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)
effs_B_social_only_L2 <-
  read.csv("output_tables_reanalysis/ effects Boundness_L2_social_only_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_B_L2 <-
  as.data.frame(rbind(effs_B_SP_L2, effs_B_social_L2, effs_B_social_only_L2)) %>%
  dplyr::mutate(response = "fusion") %>% 
  dplyr::mutate(`sample` = "L2")



effs_I_SP <-
  read.csv("output_tables_reanalysis/ effects Informativity_reanalysis_phylogenetic_spatial_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)
effs_I_social <-
  read.csv("output_tables_reanalysis/ effects Informativity_reanalysis_social_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)
effs_I_social_only <-
  read.csv("output_tables_reanalysis/ effects Informativity_reanalysis_social_only_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_I <-
  as.data.frame(rbind(effs_I_SP, effs_I_social, effs_I_social_only)) %>%
  dplyr::mutate(response = "informativity") %>% 
  dplyr::mutate(`sample` = "entire")


effs_I_SP_L2 <-
  read.csv("output_tables_reanalysis/ effects Informativity_L2_phylogenetic_spatial_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)
effs_I_social_L2 <-
  read.csv("output_tables_reanalysis/ effects Informativity_L2_social_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)
effs_I_social_only_L2 <-
  read.csv("output_tables_reanalysis/ effects Informativity_L2_social_only_models .csv") %>%
  rename(lower = 2,
         upper = 4,
         mean = 3)

effs_I_L2 <-
  as.data.frame(rbind(effs_I_SP_L2, effs_I_social_L2, effs_I_social_only_L2)) %>%
  dplyr::mutate(response = "informativity") %>% 
  dplyr::mutate(`sample` = "L2")

all_effs <- as.data.frame(rbind(effs_B, effs_I, effs_B_L2, effs_I_L2)) %>%
  rename("2.5%" = 2,
         "50%" = 3,
         "97.5%" = 4) %>%
  relocate(effect, .after = model) %>%
  relocate(response, .after = model)

write.csv(all_effs, "output_tables_reanalysis/Table_INLA_all_models.csv")

all_effs <- all_effs %>%
  flextable() %>%
  autofit() %>%
  merge_v(j = c("response", "model")) %>%
  fix_border_issues()

save_as_docx("Summary of all fitted INLA models" = all_effs,
             path = "output_tables_reanalysis/Table_INLA_all_models.docx")
