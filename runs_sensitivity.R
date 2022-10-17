source('sensitivity_testing_B_001.R')
source('sensitivity_testing_B_05.R')
source('sensitivity_testing_B_099.R')

WAIC_0.1 <- read.csv("output_tables/ waics Boundness_social_models .csv") %>%
  mutate(prior = "0.1")

WAIC_0.01 <- read.csv("output_tables/ waics Boundness_social_models prior_0.01 .csv") %>%
  mutate(prior = "0.01")

WAIC_0.5 <- read.csv("output_tables/ waics Boundness_social_models prior_0.5 .csv") %>%
  mutate(prior = "0.5")

WAIC_0.99 <- read.csv("output_tables/ waics Boundness_social_models prior_0.99 .csv") %>%
  mutate(prior = "0.99")

sensitivity_B <- as.data.frame(rbind(WAIC_0.1, WAIC_0.01, WAIC_0.5, WAIC_0.99)) %>%
  mutate(response="boundness")

source('check_I_lacking_family_priors.R')
source('check_B_lacking_family_priors.R')
source('sensitivity_testing_I_001.R')
source('sensitivity_testing_I_05.R')
source('sensitivity_testing_I_099.R')

WAIC_0.1 <- read.csv("output_tables/ waics Informativity_social_models .csv") %>%
  mutate(prior = "0.1")

WAIC_0.01 <- read.csv("output_tables/ waics Informativity_social_models prior_0.01 .csv") %>%
  mutate(prior = "0.01")

WAIC_0.5 <- read.csv("output_tables/ waics Informativity_social_models prior_0.5 .csv") %>%
  mutate(prior = "0.5")

WAIC_0.99 <- read.csv("output_tables/ waics Informativity_social_models prior_0.99 .csv") %>%
  mutate(prior = "0.99")

sensitivity_I <- as.data.frame(rbind(WAIC_0.1, WAIC_0.01, WAIC_0.5, WAIC_0.99)) %>%
  mutate(response="informativity")

sensitivity_all <- as.data.frame(rbind(sensitivity_B, sensitivity_I))

write.csv(sensitivity_all, "output_tables/Table_sensitivity.csv")

sensitivity_all <- sensitivity_all %>% 
  flextable() %>%
  autofit() %>%
  merge_v(j=c("response", "prior")) %>%
  fix_border_issues() 

save_as_docx(
  "Summary of sensitivity" = sensitivity_all, 
  path = "output_tables/Table_sensitivity.docx")
