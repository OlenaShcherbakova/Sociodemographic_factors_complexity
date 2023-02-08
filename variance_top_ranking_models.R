#picking the top 2 models of boundness (with social predictors that improve the fit of the random-effects-only model) and top 4 models of informativity (with WAIC difference from the top model no lower than 10) to calculate the percentages of variance of random effects

n_samples = 100

load("output_models/models_Boundness_social.RData")

#boundness_st ~ phy+spa+L1
posterior = inla.hyperpar.sample(n_samples, result[[1]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

B_L1 <- c("boundness", "L1 speakers", h2[[1]], h2[[2]], h2[[3]], result[[1]]$waic$waic)


#boundness_st ~ phy+spa+L1+L1 proportion
posterior = inla.hyperpar.sample(n_samples, result[[6]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

B_L1_L2_prop <- c("boundness", "L1 speakers + L2 proportion", h2[[1]], h2[[2]], h2[[3]], result[[6]]$waic$waic)


load("output_models/models_Informativity_social.RData")

#informativity_st ~ phy+spa+L1
posterior = inla.hyperpar.sample(n_samples, result[[1]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

I_L1 <- c("informativity", "L1 speakers", h2[[1]], h2[[2]], h2[[3]], result[[1]]$waic$waic)

#informativity_st ~ phy+spa+L1+L1 proportion
posterior = inla.hyperpar.sample(n_samples, result[[6]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

I_L1_L2_prop <- c("informativity", "L1 speakers + L2 proportion", h2[[1]], h2[[2]], h2[[3]], result[[6]]$waic$waic)


variance <- as.data.frame(rbind(B_L1, B_L1_L2_prop, I_L1, I_L1_L2_prop))
colnames(variance) <- c("response", "sociodemographic predictor(s)", "variance for the Gaussian observations", "variance for phy_id", "variance for sp_id", "WAIC")

variance_csv <- variance %>% 
  mutate(across(.cols=c(3:6), as.numeric)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(across(.cols=c(3:5), function(x) x*100)) %>% 
  rename_with(.cols=c(3:5), ~ paste0(.x, " in %"))
  

write.csv(variance_csv, "output_tables/Table_variance_top_ranking_models.csv")

variance_flextable <- variance %>% 
  mutate(across(.cols=c(3:6), as.numeric)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(across(.cols=c(3:5), function(x) x*100)) %>% 
  rename_with(.cols=c(3:5), ~ paste0(.x, " in %")) %>% 
  flextable() %>%
  autofit() %>%
  merge_v(j=c("response")) %>%
  fix_border_issues() 

save_as_docx(
  "Variance in top-ranking models of boundness and informativity" = variance_flextable, 
  path = "output_tables/table_SM_variance_top_ranking_models.docx")

