#predictors: random effects - phylogenetic and spatial
source("models_Boundness_phylogenetic_spatial.R")
source("models_Informativity_phylogenetic_spatial.R")

#predictors: phylogenetic and spatial random effects + sociodemograhic variables as fixed effects (on reduced set of social variables: without log10 transformed L1 speakers)
source("models_Boundness_reduced_social.R")
source("models_Informativity_reduced_social.R")

#predictors: sociodemographic variables as fixed effects
source("models_Boundness_social_only.R")
source("models_Informativity_social_only.R")



