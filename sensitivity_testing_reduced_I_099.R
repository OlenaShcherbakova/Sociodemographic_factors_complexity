#model fitting: Informativity predicted by social effects on top of random phylogenetic and spatial factors

source("requirements.R")

source("install_and_load_INLA.R")

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
tree <- keep.tip(tree, metrics_joined$Language_ID)

x <- assert_that(all(tree$tip.label %in% metrics_joined$Language_ID), msg = "The data and phylogeny taxa do not match")

## Building standardized phylogenetic precision matrix
tree_scaled <- tree

tree_vcv = vcv.phylo(tree_scaled)
typical_phylogenetic_variance = exp(mean(log(diag(tree_vcv))))

tree_scaled$edge.length <- tree_scaled$edge.length/typical_phylogenetic_variance
phylo_prec_mat <- MCMCglmm::inverseA(tree_scaled,
                                     nodes = "ALL",
                                     scale = FALSE)$Ainv

metrics_joined = metrics_joined[order(match(metrics_joined$Language_ID, rownames(phylo_prec_mat))),]

#"local" set of parameters
## Create spatial covariance matrix using the matern covariance function
spatial_covar_mat_1 = varcov.spatial(metrics_joined[,c("Longitude", "Latitude")], 
                                     cov.pars = phi_1, kappa = kappa)$varcov
# Calculate and standardize by the typical variance
typical_variance_spatial_1 = exp(mean(log(diag(spatial_covar_mat_1))))
spatial_cov_std_1 = spatial_covar_mat_1 / typical_variance_spatial_1
spatial_prec_mat_1 = solve(spatial_cov_std_1)
dimnames(spatial_prec_mat_1) = list(metrics_joined$Language_ID, metrics_joined$Language_ID)

## Since we are using a sparse phylogenetic matrix, we are matching taxa to rows in the matrix
phy_id = match(tree$tip.label, rownames(phylo_prec_mat))
metrics_joined$phy_id = phy_id

## Other effects are in the same order they appear in the dataset
metrics_joined$sp_id = 1:nrow(spatial_prec_mat_1)

pcprior_hyper_0.99 = list(prec =list(prior="pc.prec", param = c(1, 0.99)))

#Preparing the formulas for 10 competing models to be used in inla() call
listcombo <- list( 
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "L1_log_st"), 
  
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(inla.group(L1_copy), model='rw2', scale.model = TRUE)"), 
  
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "L2_prop"), 
  
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(inla.group(L2_copy), model='rw2', scale.model = TRUE)"),
  
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(inla.group(L1_copy), model='rw2', scale.model = TRUE)", "f(inla.group(L2_copy), model='rw2', scale.model = TRUE)"),
  
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "L1_log_st", "L2_prop"), 
  
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)",  "neighboring_languages_st"), 
  
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "Official"),
  
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "Education"))


predterms <- lapply(listcombo, function(x) paste(x, collapse="+"))

predterms <- t(as.data.frame(predterms))

predterms_short <- predterms
predterms_short <- gsub("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper_0.99)", "Phylogenetic", predterms_short, fixed=TRUE)
predterms_short <- gsub("f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper_0.99)", "Spatial: local", predterms_short, fixed=TRUE)

predterms_short <- gsub("f(inla.group(L1_copy), model='rw2', scale.model = TRUE)", "L1 speakers (nonlinear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("L1_log_st", "L1 speakers (linear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("f(inla.group(L2_copy), model='rw2', scale.model = TRUE)", "L2 proportion (nonlinear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("L2_prop", "L2 proportion (linear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("neighboring_languages_st", "Neighbours", predterms_short, fixed=TRUE)


phylogenetic_element <- data.frame("judgement" = grepl("Phylogenetic", predterms_short),
                                   number = 1:length(predterms_short))
phylogenetic_element <- phylogenetic_element[phylogenetic_element$judgement == TRUE,]$number

spatial_element_local <- data.frame("judgement" = grepl("local", predterms_short),
                                    number = 1:length(predterms_short))
spatial_element_local <- spatial_element_local[spatial_element_local$judgement == TRUE,]$number

spatial_element_regional <- data.frame("judgement" = grepl("regional", predterms_short),
                                       number = 1:length(predterms_short))
spatial_element_regional <- spatial_element_regional[spatial_element_regional$judgement == TRUE,]$number

spatial_element <- c(spatial_element_local, spatial_element_regional)

L1_element <- data.frame("judgement" = grepl("L1 speakers (linear)", predterms_short, fixed=TRUE),
                         number = 1:length(predterms_short))
L1_element <- L1_element[L1_element$judgement == TRUE,]$number


L1_nl_element <- data.frame("judgement" = grepl("L1 speakers (nonlinear)", predterms_short, fixed=TRUE),
                            number = 1:length(predterms_short))
L1_nl_element <- L1_nl_element[L1_nl_element$judgement == TRUE,]$number

L2_prop_element <- data.frame("judgement" = grepl("L2 proportion (linear)", predterms_short, fixed=TRUE),
                              number = 1:length(predterms_short))
L2_prop_element <- L2_prop_element[L2_prop_element$judgement == TRUE,]$number

L2_prop_nl_element <- data.frame("judgement" = grepl("L2 proportion (nonlinear)", predterms_short, fixed=TRUE),
                                 number = 1:length(predterms_short))
L2_prop_nl_element <- L2_prop_nl_element[L2_prop_nl_element$judgement == TRUE,]$number

neighbour_element <- data.frame("judgement" = grepl("Neighbours", predterms_short),
                                number = 1:length(predterms_short))
neighbour_element <- neighbour_element[neighbour_element$judgement == TRUE,]$number

official_element <- data.frame("judgement" = grepl("Official", predterms_short),
                               number = 1:length(predterms_short))
official_element <- official_element[official_element$judgement == TRUE,]$number

education_element <- data.frame("judgement" = grepl("Education", predterms_short),
                                number = 1:length(predterms_short))
education_element <- education_element[education_element$judgement == TRUE,]$number



#preparing empty matrices to be filled with effect estimates (quantiles), model name, and WAIC value
phy_effects_matrix <- matrix(NA, 10, 5)
colnames(phy_effects_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
spa_effects_matrix <- matrix(NA, 10, 5)
colnames(spa_effects_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

intercept_matrix <- matrix(NA, 10, 5)
colnames(intercept_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

social_effects_matrix_L1 <- matrix(NA, 10, 5)
colnames(social_effects_matrix_L1) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_L1_nl <- matrix(NA, 10, 5)
colnames(social_effects_matrix_L1_nl) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_L2_prop <- matrix(NA, 10, 5)
colnames(social_effects_matrix_L2_prop) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_L2_prop_nl <- matrix(NA, 10, 5)
colnames(social_effects_matrix_L2_prop_nl) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_N <- matrix(NA, 10, 5)
colnames(social_effects_matrix_N) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_O <- matrix(NA, 10, 5)
colnames(social_effects_matrix_O) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_E <- matrix(NA, 10, 5)
colnames(social_effects_matrix_E) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_L1_L2_prop <- matrix(NA, 10, 5)
colnames(social_effects_matrix_L1_L2_prop) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

#fitted values
fitted_list <- vector("list", 10)
names(fitted_list) <- predterms_short

#marginals of hyperparameters
marginals_hyperpar_list_gaussian <- vector("list", 10)
names(marginals_hyperpar_list_gaussian) <- predterms_short

marginals_hyperpar_list_phy <- vector("list", 10)
names(marginals_hyperpar_list_phy) <- predterms_short

marginals_hyperpar_list_spa <- vector("list", 10)
names(marginals_hyperpar_list_spa) <- predterms_short

marginals_hyperpar_list_social_L1_nl <- vector("list", 10)
names(marginals_hyperpar_list_social_L1_nl) <- predterms_short

marginals_hyperpar_list_social_L2_prop_nl <- vector("list", 10)
names(marginals_hyperpar_list_social_L2_prop_nl) <- predterms_short


#marginals of fixed effects
marginals_fixed_list_Intercept <- vector("list", 10)
names(marginals_fixed_list_Intercept) <- predterms_short

marginals_fixed_list_L1 <- vector("list", 10)
names(marginals_fixed_list_L1) <- predterms_short

marginals_fixed_list_L2_prop <- vector("list", 10)
names(marginals_fixed_list_L2_prop) <- predterms_short

marginals_fixed_list_O <- vector("list", 10)
names(marginals_fixed_list_O) <- predterms_short

marginals_fixed_list_N <- vector("list", 10)
names(marginals_fixed_list_N) <- predterms_short

marginals_fixed_list_E <- vector("list", 10)
names(marginals_fixed_list_E) <- predterms_short

marginals_fixed_list_L1_L2_prop <- vector("list", 10)
names(marginals_fixed_list_L1_L2_prop) <- predterms_short




#summary statistics of random effects
summary_random_list_phy <- vector("list", 10)
names(summary_random_list_phy) <- predterms_short

summary_random_list_spa <- vector("list", 10)
names(summary_random_list_spa) <- predterms_short

summary_random_list_social_L1_nl <- vector("list", 10)
names(summary_random_list_social_L1_nl) <- predterms_short

summary_random_list_social_L2_prop_nl <- vector("list", 10)
names(summary_random_list_social_L2_prop_nl) <- predterms_short


coefm <- matrix(NA,10,1)
result <- vector("list",10)

for(i in 1:10){
  formula <- as.formula(paste("informativity_st ~ ",predterms[[i]]))
  result[[i]] <- inla(formula, family="gaussian", 
                      control.family = list(hyper = pcprior_hyper_0.99), 
                      #control.inla = list(tolerance = 1e-8, h = 0.0001), 
                      #tolerance: the tolerance for the optimisation of the hyperparameters
                      #h: the step-length for the gradient calculations for the hyperparameters.
                      data=metrics_joined, control.compute=list(waic=TRUE)) 
  
  coefm[i,1] <- round(result[[i]]$waic$waic, 2)
  
  intercept_matrix[i, 1:3] <- c(result[[i]]$summary.fixed[1,]$`0.025quant`, result[[i]]$summary.fixed[1,]$`0.5quant`, result[[i]]$summary.fixed[1,]$`0.975quant`) 
  intercept_matrix[i, 4] <- predterms_short[[i]]
  intercept_matrix[i, 5] <- result[[i]]$waic$waic
  
  marginals_fixed_list_Intercept[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][["(Intercept)"]]))
  colnames(marginals_fixed_list_Intercept[[i]]) <- c("x for Intercept", "y for Intercept")
  
  if(i %in% phylogenetic_element) {
    phy_effects_matrix[i, 1:3] <- inla.tmarginal(function(x) 1/sqrt(x),
                                                 result[[i]]$marginals.hyperpar$`Precision for phy_id`,
                                                 method = "linear") %>%
      inla.qmarginal(c(0.025, 0.5, 0.975), .)
    phy_effects_matrix[i, 4] <- predterms_short[[i]]
    phy_effects_matrix[i, 5] <- result[[i]]$waic$waic
  } 
  
  if(i %in% spatial_element) {
    spa_effects_matrix[i, 1:3] <- inla.tmarginal(function(x) 1/sqrt(x),
                                                 result[[i]]$marginals.hyperpar$`Precision for sp_id`,
                                                 method = "linear") %>%
      inla.qmarginal(c(0.025, 0.5, 0.975), .)
    spa_effects_matrix[i, 4] <- predterms_short[[i]]
    spa_effects_matrix[i, 5] <- result[[i]]$waic$waic
  }
  
  
  if(i %in% L1_nl_element){
    social_effects_matrix_L1_nl[i, 1:3] <- inla.tmarginal(function(x) 1/sqrt(x),
                                                          result[[i]]$marginals.hyperpar$`Precision for inla.group(L1_copy)`,
                                                          method = "linear") %>%
      inla.qmarginal(c(0.025, 0.5, 0.975), .)
    social_effects_matrix_L1_nl[i, 4] <- predterms_short[[i]]
    social_effects_matrix_L1_nl[i, 5] <- result[[i]]$waic$waic
  } 
  
  if(i %in% L2_prop_nl_element){
    social_effects_matrix_L2_prop_nl[i, 1:3] <- inla.tmarginal(function(x) 1/sqrt(x),
                                                               result[[i]]$marginals.hyperpar$`Precision for inla.group(L2_copy)`,
                                                               method = "linear") %>%
      inla.qmarginal(c(0.025, 0.5, 0.975), .)
    social_effects_matrix_L2_prop_nl[i, 4] <- predterms_short[[i]]
    social_effects_matrix_L2_prop_nl[i, 5] <- result[[i]]$waic$waic
  } 
  
  if(i %in% L1_element) {
    social_effects_matrix_L1[i, 1:3] <- c(result[[i]]$summary.fixed["L1_log_st",]$`0.025quant`, result[[i]]$summary.fixed["L1_log_st",]$`0.5quant`, result[[i]]$summary.fixed["L1_log_st",]$`0.975quant`)
    social_effects_matrix_L1[i, 4] <- predterms_short[[i]]
    social_effects_matrix_L1[i, 5] <- result[[i]]$waic$waic
    
    marginals_fixed_list_L1[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][["L1_log_st"]]))
    colnames(marginals_fixed_list_L1[[i]]) <- c("x for L1", "y for L1")
  }
  
  if(i %in% L2_prop_element) {
    social_effects_matrix_L2_prop[i, 1:3] <- c(result[[i]]$summary.fixed["L2_prop",]$`0.025quant`, result[[i]]$summary.fixed["L2_prop",]$`0.5quant`, result[[i]]$summary.fixed["L2_prop",]$`0.975quant`)
    social_effects_matrix_L2_prop[i, 4] <- predterms_short[[i]]
    social_effects_matrix_L2_prop[i, 5] <- result[[i]]$waic$waic
    
    marginals_fixed_list_L2_prop[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][["L2_prop"]]))
    colnames(marginals_fixed_list_L2_prop[[i]]) <- c("x for L2 proportion", "y for L2 proportion")
  }
  
  if(i %in% neighbour_element) {
    social_effects_matrix_N[i, 1:3] <- c(result[[i]]$summary.fixed[2,]$`0.025quant`, result[[i]]$summary.fixed[2,]$`0.5quant`, result[[i]]$summary.fixed[2,]$`0.975quant`)
    social_effects_matrix_N[i, 4] <- predterms_short[[i]]
    social_effects_matrix_N[i, 5] <- result[[i]]$waic$waic
    
    marginals_fixed_list_N[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][[2]]))
    colnames(marginals_fixed_list_N[[i]]) <- c("x for Neighbours", "y for Neighbours")
  }
  
  if(i %in% official_element) {
    social_effects_matrix_O[i, 1:3] <- c(result[[i]]$summary.fixed[2,]$`0.025quant`, result[[i]]$summary.fixed[2,]$`0.5quant`, result[[i]]$summary.fixed[2,]$`0.975quant`)
    social_effects_matrix_O[i, 4] <- predterms_short[[i]]
    social_effects_matrix_O[i, 5] <- result[[i]]$waic$waic
    
    marginals_fixed_list_O[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][[2]]))
    colnames(marginals_fixed_list_O[[i]]) <- c("x for Official", "y for Official")
  }
  
  if(i %in% education_element) {
    social_effects_matrix_E[i, 1:3] <- c(result[[i]]$summary.fixed[2,]$`0.025quant`, result[[i]]$summary.fixed[2,]$`0.5quant`, result[[i]]$summary.fixed[2,]$`0.975quant`)
    social_effects_matrix_E[i, 4] <- predterms_short[[i]]
    social_effects_matrix_E[i, 5] <- result[[i]]$waic$waic
    
    marginals_fixed_list_E[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][[2]]))
    colnames(marginals_fixed_list_E[[i]]) <- c("x for Education", "y for Education")
  }
  
  fitted_list[[i]] <- result[[i]]$summary.fitted.values
  fitted_list[[i]] <- fitted_list[[i]] %>%
    mutate(across(where(is.numeric), round, 2))
  
  marginals_hyperpar_list_gaussian[[i]] <- as.data.frame(cbind(result[[i]]$marginals.hyperpar[["Precision for the Gaussian observations"]]))
  colnames(marginals_hyperpar_list_gaussian[[i]]) <- c("x for the Gaussian observations", "y for the Gaussian observations")
  
  if(i %in% phylogenetic_element){
    marginals_hyperpar_list_phy[[i]] <- as.data.frame(cbind(result[[i]]$marginals.hyperpar[["Precision for phy_id"]]))
    colnames(marginals_hyperpar_list_phy[[i]]) <- c("x for phy_id", "y for phy_id")
  }
  
  if(i %in% spatial_element){
    marginals_hyperpar_list_spa[[i]] <- as.data.frame(cbind(result[[i]]$marginals.hyperpar[["Precision for sp_id"]]))
    colnames(marginals_hyperpar_list_spa[[i]]) <- c("x for sp_id", "y for sp_id")
  }
  
  if(i %in% L1_nl_element){
    marginals_hyperpar_list_social_L1_nl[[i]] <- as.data.frame(cbind(result[[i]]$marginals.hyperpar[["Precision for inla.group(L1_copy)"]]))
    colnames(marginals_hyperpar_list_social_L1_nl[[i]]) <- c("x for inla.group(L1_copy)", "y for inla.group(L1_copy)")
  }
  
  if(i %in% L2_prop_nl_element){
    marginals_hyperpar_list_social_L2_prop_nl[[i]] <- as.data.frame(cbind(result[[i]]$marginals.hyperpar[["Precision for inla.group(L2_copy)"]]))
    colnames(marginals_hyperpar_list_social_L2_prop_nl[[i]]) <- c("x for inla.group(L2_copy)", "y for inla.group(L2_copy)")
  }
  
  if(i %in% phylogenetic_element){
    summary_random_list_phy[[i]] <- cbind(result[[i]]$summary.random$phy_id) %>%
      rename(phy_id = ID) %>%
      as.data.frame() %>%
      mutate(across(where(is.numeric), round, 2))
  }
  
  if(i %in% spatial_element){
    summary_random_list_spa[[i]] <- cbind(result[[i]]$summary.random$sp_id) %>%
      rename(sp_id = ID) %>%
      as.data.frame() %>%
      mutate(across(where(is.numeric), round, 2))
  }
}

#beepr::beep(5)

save(result, file = "output_models_reduced/models_Informativity_social_0.99.RData")


coefm <- as.data.frame(cbind(predterms_short, coefm))
colnames(coefm) <- c("model", "WAIC")
coefm <- coefm %>%
  mutate(across(.cols=2, as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  arrange(WAIC)

coefm$WAIC <- as.numeric(coefm$WAIC)
coefm <- coefm[order(coefm$WAIC),]

coefm_path <- paste("output_tables_reduced/", "waics", "Informativity_social_models", "prior_0.99", ".csv", collapse = "")
write.csv(coefm, coefm_path, row.names=FALSE)


phy_effects<-as.data.frame(phy_effects_matrix)
spa_effects<-as.data.frame(spa_effects_matrix)
intercept_effects <- as.data.frame(intercept_matrix)
L1_effects <- as.data.frame(social_effects_matrix_L1)
L1_nl_effects <- as.data.frame(social_effects_matrix_L1_nl)
L2_prop_effects <- as.data.frame(social_effects_matrix_L2_prop)
L2_prop_nl_effects <- as.data.frame(social_effects_matrix_L2_prop_nl)
N_effects<-as.data.frame(social_effects_matrix_N)
E_effects<-as.data.frame(social_effects_matrix_E)
O_effects<-as.data.frame(social_effects_matrix_O)

phy_effects$effect <- "phylogenetic SD"
spa_effects$effect <- "spatial SD"
intercept_effects$effect <- "Intercept"
L1_effects$effect <- "L1"
L1_nl_effects$effect <- "social SD:\nL1"
L2_prop_effects$effect <- "L2 proportion"
L2_prop_nl_effects$effect <- "social SD:\nL2 proportion"
N_effects$effect <- "Neighbours"
E_effects$effect <- "Education"
O_effects$effect <- "Official status"

effs <- as.data.frame(rbind(phy_effects, spa_effects, intercept_effects, L1_effects, L1_nl_effects, L2_prop_effects, L2_prop_nl_effects, N_effects, O_effects, E_effects))
effs <- effs %>%
  mutate(across(.cols=c(1:3, 5), as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  na.omit() %>%
  arrange(WAIC) %>%
  relocate(model)

effs_path <- paste("output_tables_reduced/", "effects", "Informativity_social_models", "prior_0.99", ".csv", collapse = "")
write.csv(effs, effs_path, row.names=FALSE)

