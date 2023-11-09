#picking the top 2 models of boundness (with social predictors that improve the fit of the random-effects-only model) and top 4 models of informativity (with WAIC difference from the top model no lower than 10) to calculate the percentages of variance of random effects

# script was written by Sam Passmore and Olena Shcherbakova

n_samples = 100

load("output_models_reanalysis_reduced/models_Boundness_reanalysis_social.RData")

#boundness_st ~ phy+spa+L1
posterior = inla.hyperpar.sample(n_samples, result[[1]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

B_L1 <- c("fusion", "L1 speakers", h2[[1]], h2[[2]], h2[[3]], result[[1]]$waic$waic)


#boundness_st ~ phy+spa+L1+Vehicularity
posterior = inla.hyperpar.sample(n_samples, result[[4]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

B_L1_V <- c("fusion", "L1 speakers + Vehicularity", h2[[1]], h2[[2]], h2[[3]], result[[4]]$waic$waic)


#boundness_st ~ phy+spa+Vehicularity
posterior = inla.hyperpar.sample(n_samples, result[[3]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

B_V <- c("fusion", "Vehicularity", h2[[1]], h2[[2]], h2[[3]], result[[3]]$waic$waic)



load("output_models_reanalysis_reduced/models_Informativity_reanalysis_social.RData")

#informativity_st ~ phy+spa+L1
posterior = inla.hyperpar.sample(n_samples, result[[1]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

I_L1 <- c("informativity", "L1 speakers", h2[[1]], h2[[2]], h2[[3]], result[[1]]$waic$waic)


#informativity_st ~ phy+spa+L1+Vehicularity
posterior = inla.hyperpar.sample(n_samples, result[[4]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

I_L1_V <- c("informativity", "L1 speakers + Vehicularity", h2[[1]], h2[[2]], h2[[3]], result[[4]]$waic$waic)


#informativity_st ~ phy+spa+Vehicularity
posterior = inla.hyperpar.sample(n_samples, result[[3]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

I_V <- c("informativity", "Vehicularity", h2[[1]], h2[[2]], h2[[3]], result[[3]]$waic$waic)


variance <- as.data.frame(rbind(B_L1, B_L1_V, 
                                B_V,
                                I_L1, I_L1_V,
                                I_V))
colnames(variance) <- c("response", "sociodemographic predictor(s)", "variance for the Gaussian observations", "variance for phy_id", "variance for sp_id", "WAIC")

variance_csv <- variance %>% 
  group_by(response) %>% 
  arrange(WAIC) %>% 
  ungroup(response) %>% 
  mutate(across(.cols=c(3:6), as.numeric)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(across(.cols=c(3:5), function(x) x*100)) %>% 
  rename_with(.cols=c(3:5), ~ paste0(.x, " in %"))

write.csv(variance_csv, "output_tables_reanalysis_reduced/Table_variance_top_ranking_models_reduced.csv")

variance_flextable <- variance %>% 
  group_by(response) %>% 
  arrange(WAIC) %>% 
  ungroup(response) %>% 
  mutate(across(.cols=c(3:6), as.numeric)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(across(.cols=c(3:5), function(x) x*100)) %>% 
  rename_with(.cols=c(3:5), ~ paste0(.x, " in %")) %>% 
  flextable() %>%
  autofit() %>%
  merge_v(j=c("response")) %>%
  fix_border_issues() 

save_as_docx(
  "Variance in top-ranking models of fusion and informativity (reduced)" = variance_flextable, 
  path = "output_tables_reanalysis_reduced/table_SM_variance_top_ranking_models_reduced.docx")

