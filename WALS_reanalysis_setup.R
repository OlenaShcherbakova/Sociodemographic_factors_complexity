source("install_and_load_INLA.R")

#parameters
kappa = 1 
phi_1 = c(1, 1.25) # "Local" version: (sigma, phi) First value is not used

WALS <- read_csv("data/complexity_data_WALS.csv") %>% 
  dplyr::select("Name"=lang, roundComp, logpop2, "ISO_639"=silCode) %>% 
  dplyr::mutate(ISO_639 = str_to_lower(ISO_639))

min_val <- min(WALS$roundComp)
max_val <- max(WALS$roundComp)

# Perform the rescaling
WALS$roundComp <- (WALS$roundComp - min_val) / (max_val - min_val)

pop_file_fn <- "data_wrangling/ethnologue_pop_SM_morph_compl_reanalysis.tsv"
L1 <-
  read_tsv(pop_file_fn, show_col_types = F) %>% dplyr::select(ISO_639, L1_log10_scaled)

WALS_df <- WALS %>%
  inner_join(L1,
             by = c("ISO_639")) 

formula <- as.formula(paste("roundComp ~", "L1_log10_scaled"))
result <- inla(formula, family = "gaussian",  
                         data = WALS_df, control.compute = list(waic = TRUE)) 
summary(result)

save(result, file = "output_models/models_WALS_uncontrolled.RData")

social_effects_uncontrolled <- c("morphological complexity ~ L1",
  round(c(
    result$summary.fixed[2,]$`0.025quant`,
    result$summary.fixed[2,]$`0.5quant`,
    result$summary.fixed[2,]$`0.975quant`, nrow(WALS_df)), 2), "default (~10%)")

save(social_effects_uncontrolled, file = "output_models/social_effects_uncontrolled.RData")


