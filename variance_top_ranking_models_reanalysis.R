#picking the top models of fusion and informativity
#to calculate the percentages of variance of random effects

# script was written by Sam Passmore and Olena Shcherbakova

n_samples = 100

load("output_models_reanalysis/models_Boundness_reanalysis_social.RData")

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


#boundness_st ~ phy+spa+L1*Vehicularity
posterior = inla.hyperpar.sample(n_samples, result[[5]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

B_L1_V_interaction <- c("fusion", "L1 speakers*Vehicularity", h2[[1]], h2[[2]], h2[[3]], result[[5]]$waic$waic)



load("output_models_reanalysis/models_Informativity_reanalysis_social.RData")

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


#informativity_st ~ phy+spa+L1*Vehicularity
posterior = inla.hyperpar.sample(n_samples, result[[5]])
h2 = (1 / posterior) / rowSums(1 / posterior)
h2 <- h2 %>%
  as.data.frame() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

I_L1_V_interaction <- c("informativity", "L1 speakers*Vehicularity", h2[[1]], h2[[2]], h2[[3]], result[[5]]$waic$waic)



variance <- as.data.frame(rbind(B_L1, B_L1_V, 
                                B_V, B_L1_V_interaction,
                                I_L1, I_L1_V,
                                I_V, I_L1_V_interaction))
colnames(variance) <- c("response", "sociodemographic predictor(s)", "variance for the Gaussian observations", "variance for phy_id", "variance for sp_id", "WAIC")

variance_csv <- variance %>% 
  group_by(response) %>% 
  arrange(WAIC) %>% 
  ungroup(response) %>% 
  mutate(across(.cols=c(3:6), as.numeric)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(across(.cols=c(3:5), function(x) x*100)) %>% 
  rename_with(.cols=c(3:5), ~ paste0(.x, " in %"))


write.csv(variance_csv, "output_tables_reanalysis/Table_variance_top_ranking_models.csv")

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
  "Variance in top-ranking models of boundness and informativity" = variance_flextable, 
  path = "output_tables_reanalysis/table_SM_variance_top_ranking_models.docx")

