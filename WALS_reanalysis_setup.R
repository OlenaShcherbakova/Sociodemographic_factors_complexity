source("requirements.R")

#parameters
kappa = 1 
phi_1 = c(1, 1.25) # "Local" version: (sigma, phi) First value is not used
phi_2 = c(1, 17) # "Regional" version: (sigma, phi) First value is not used

WALS <- read_csv("data/complexity_data_WALS.csv") %>% 
  dplyr::select("Name"=lang, roundComp, logpop2, "ISO_639"=silCode) %>% 
  dplyr::mutate(ISO_639 = str_to_lower(ISO_639))

min_val <- min(WALS$roundComp)
max_val <- max(WALS$roundComp)

# Perform the rescaling
WALS$roundComp <- (WALS$roundComp - min_val) / (max_val - min_val)

pop_file_fn <- "data/Table_of_Languages.tab"
L1 <- read_tsv(pop_file_fn, show_col_types = F) %>% dplyr::select(ISO_639, L1_Users)

WALS_df <- WALS %>%
  inner_join(L1,
    by = c("ISO_639")
  ) %>%  #discarding languages with no or low numbers of L1 speakers and L2 speakers resulting primarily from language revival efforts
  filter(!is.na(L1_Users)) %>% 
  dplyr::mutate(L1_Users = log10(L1_Users+1)) %>% 
  dplyr::mutate(L1_Users_scaled = scale(L1_Users)[,1])

formula <- as.formula(paste("roundComp ~", "L1_Users_scaled"))
result <- inla(formula, family = "gaussian", 
                         #control.family = list(hyper = pcprior_hyper), 
                         data = WALS_df, control.compute = list(waic = TRUE)) 
summary(result)

#mean estimate of L1_Users: -0.021 with credible intervals not crossing zero (-0.035 - -0.008)
