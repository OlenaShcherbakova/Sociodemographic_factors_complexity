#model fitting: Boundness predicted by combinations of random phylogenetic and spatial factors

#Script was written by Sam Passmore and modified by Olena Shcherbakova

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

#We opt for a sparse phylogenetic precision matrix (i.e. it is quantified using all nodes and tips), since sparse matrices make the analysis in INLA less time-intensive
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

#"regional" set of parameters
spatial_covar_mat_2 = varcov.spatial(metrics_joined[,c("Longitude", "Latitude")], cov.pars = phi_2, kappa = kappa)$varcov
typical_variance_spatial_2 = exp(mean(log(diag(spatial_covar_mat_2))))
spatial_cov_std_2 = spatial_covar_mat_2 / typical_variance_spatial_2
spatial_prec_mat_2 = solve(spatial_cov_std_2)
dimnames(spatial_prec_mat_2) = list(metrics_joined$Language_ID, metrics_joined$Language_ID)

## Since we are using a sparse phylogenetic matrix, we are matching taxa to rows in the matrix
phy_id = match(tree$tip.label, rownames(phylo_prec_mat))
metrics_joined$phy_id = phy_id

## Other effects are in the same order they appear in the dataset
metrics_joined$sp_id = 1:nrow(spatial_prec_mat_1)

#Preparing the formulas for 7 competing models to be used in inla() call
listcombo <- list(#phylogenetic and spatial effects in isolation
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper)"), 
  c("f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper)"), 
  c("f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_2, constr = TRUE, hyper = pcprior_hyper)"), 
  c("f(AUTOTYP_area, model = 'iid', hyper = pcprior_hyper)"), 
  #phylogenetic and distinct spatial effects in combination
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper)", 
    "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper)"), 
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper)", 
    "f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_2, constr = TRUE, hyper = pcprior_hyper)"), 
  c("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper)", 
    "f(AUTOTYP_area, model = 'iid', hyper = pcprior_hyper)"))

predterms <- lapply(listcombo, function(x) paste(x, collapse="+"))

predterms <- t(as.data.frame(predterms))

predterms_short <- predterms
predterms_short <- gsub("f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper)", "Phylogenetic", predterms_short, fixed=TRUE)
predterms_short <- gsub("f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper)", "Spatial: local", predterms_short, fixed=TRUE)
predterms_short <- gsub("f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_2, constr = TRUE, hyper = pcprior_hyper)", "Spatial: regional", predterms_short, fixed=TRUE)
predterms_short <- gsub("f(AUTOTYP_area, model = 'iid', hyper = pcprior_hyper)", "Areal", predterms_short, fixed=TRUE)

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

areal_element <- data.frame("judgement" = grepl("Areal", predterms_short),
                            number = 1:length(predterms_short))
areal_element <- areal_element[areal_element$judgement == TRUE,]$number


#preparing empty matrices to be filled with effect estimates (quantiles), model name, and WAIC value
phy_effects_matrix <- matrix(NA, 7, 5)
colnames(phy_effects_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
spa_effects_matrix <- matrix(NA, 7, 5)
colnames(spa_effects_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
area_effects_matrix <- matrix(NA, 7, 5)
colnames(area_effects_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

intercept_matrix <- matrix(NA, 7, 5)
colnames(intercept_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

#fitted values
fitted_list <- vector("list", 7)
names(fitted_list) <- predterms_short

#marginals of hyperparameters
marginals_hyperpar_list_gaussian <- vector("list", 7)
names(marginals_hyperpar_list_gaussian) <- predterms_short

marginals_hyperpar_list_phy <- vector("list", 7)
names(marginals_hyperpar_list_phy) <- predterms_short

marginals_hyperpar_list_spa <- vector("list", 7)
names(marginals_hyperpar_list_spa) <- predterms_short

marginals_hyperpar_list_area <- vector("list", 7)
names(marginals_hyperpar_list_area) <- predterms_short


#marginals of fixed effects
marginals_fixed_list_Intercept <- vector("list", 7)
names(marginals_fixed_list_Intercept) <- predterms_short


#summary statistics of random effects
summary_random_list_phy <- vector("list", 7)
names(summary_random_list_phy) <- predterms_short

summary_random_list_spa <- vector("list", 7)
names(summary_random_list_spa) <- predterms_short

summary_random_list_area <- vector("list", 7)
names(summary_random_list_area) <- predterms_short

coefm <- matrix(NA,7,1)
result <- vector("list",7)

for(i in 1:7){
  formula <- as.formula(paste("boundness_st ~ ",predterms[[i]]))
  result[[i]] <- inla(formula, family="gaussian", 
                 control.family = list(hyper = pcprior_hyper), 
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
  
  if(i %in% areal_element){
    area_effects_matrix[i, 1:3] <- inla.tmarginal(function(x) 1/sqrt(x),
                                                  result[[i]]$marginals.hyperpar$`Precision for AUTOTYP_area`,
                                                  method = "linear") %>%
      inla.qmarginal(c(0.025, 0.5, 0.975), .)
    area_effects_matrix[i, 4] <- predterms_short[[i]]
    area_effects_matrix[i, 5] <- result[[i]]$waic$waic
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
  
  if(i %in% areal_element){
    marginals_hyperpar_list_area[[i]] <- as.data.frame(cbind(result[[i]]$marginals.hyperpar[["Precision for AUTOTYP_area"]]))
    colnames(marginals_hyperpar_list_area[[i]]) <- c("x for AUTOTYP_area", "y for AUTOTYP_area")
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
  
  if(i %in% areal_element){
    summary_random_list_area[[i]] <- cbind(result[[i]]$summary.random$AUTOTYP_area) %>%
      rename(AUTOTYP_area = ID) %>%
      as.data.frame() %>%
      mutate(across(where(is.numeric), round, 2))
  } 
}

#beepr::beep(5)

save(result, file = "output_models/models_Boundness_phylogenetic_spatial.RData")

coefm <- as.data.frame(cbind(predterms_short, coefm))
colnames(coefm) <- c("model", "WAIC")
coefm <- coefm %>%
  mutate(across(.cols=2, as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  arrange(WAIC)

coefm$WAIC <- as.numeric(coefm$WAIC)
coefm <- coefm[order(coefm$WAIC),]

coefm_path <- paste("output_tables/", "waics", "Boundness_phylogenetic_spatial_models", ".csv", collapse = "")
write.csv(coefm, coefm_path, row.names=FALSE)

for (i in 1:length(fitted_list)) {
  fitted_list[[i]]$model <- names(fitted_list)[i]
}
fitted_list <- dplyr::bind_rows(fitted_list)
fitted_list_path <- paste("output_tables/", "fitted_list", "Boundness_phylogenetic_spatial_models", ".csv", collapse = "")
write.csv(fitted_list, fitted_list_path) 

phy_effects<-as.data.frame(phy_effects_matrix)
spa_effects<-as.data.frame(spa_effects_matrix)
area_effects <- as.data.frame(area_effects_matrix)
intercept_effects <- as.data.frame(intercept_matrix)

phy_effects$effect <- "phylogenetic SD"
spa_effects$effect <- "spatial SD"
area_effects$effect <- "areal SD"
intercept_effects$effect <- "Intercept"

effs <- as.data.frame(rbind(phy_effects, spa_effects, area_effects, intercept_effects))
effs <- effs %>%
  mutate(across(.cols=c(1:3, 5), as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  na.omit() %>%
  arrange(WAIC) %>%
  relocate(model)

effs_path <- paste("output_tables/", "effects", "Boundness_phylogenetic_spatial_models", ".csv", collapse = "")
write.csv(effs, effs_path, row.names=FALSE)

effs <- read.csv("output_tables/ effects Boundness_phylogenetic_spatial_models .csv")

effs_table_SM <- effs %>%
  mutate(effect = 
           dplyr::recode(effect,
                  "areal SD" = "spatial SD")) %>%
  rename("2.5%"=2,
         "50%" = 4,
         "97.5%" = 3) %>%
  flextable() %>%
  autofit() %>%
  merge_v(j=c("model", "WAIC")) %>%
  fix_border_issues() %>%
  border_inner_h()

save_as_docx(
  "Effects in boundess models with random effects" = effs_table_SM, 
  path = "output_tables/table_SM_effects_Boundness_phylogenetic_spatial_models.docx")

effs_plot <- effs %>%
  #filter(WAIC <= top_9) %>%
  rename(lower=2,
         upper = 4,
         mean = 3) %>% #mean here refers to 0.5 quantile 
  #filter(!effect == "Intercept") %>%
  mutate(effect = 
           dplyr::recode(effect,
                  "areal SD" = "spatial SD")) %>%
  mutate(effect = factor(effect, levels=c("phylogenetic SD", "spatial SD", "areal SD", "Intercept"))) %>%
  mutate(WAIC = round(WAIC, 2)) %>%
  unite("model", model, WAIC, sep = ",\nWAIC: ", remove=FALSE) %>%
  group_by(WAIC) %>%
  arrange(WAIC) %>%
  mutate(model = forcats::fct_reorder(as.factor(model), WAIC)) %>% #reordering levels within model based on WAIC values
  mutate(model = factor(model, levels=rev(levels(model)))) #reversing the order



#plot modified from function ggregplot::Efxplot
cols = c(brewer.pal(12, "Paired"))
cols = c(cols[c(12, 10)], "gray50")

show_col(cols)

plot_1 <- ggplot(effs_plot,
                 aes(y = as.factor(model),
                     x = mean,
                     group = effect,
                     colour = effect)) +
  geom_pointrangeh(aes(xmin = lower, xmax = upper), position = position_dodge(w = 0.9), size = 1.5) +
  geom_vline(aes(xintercept = 0),lty = 2) + labs(x = NULL) + #coord_flip() +
  scale_color_manual(values=cols) +
  ylab("Model of boundness") + xlab("Estimate") + labs(color = "Effect") + theme_classic() +
  theme(axis.text=element_text(size=50),
        legend.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.title=element_text(size=50),
        legend.spacing.y = unit(1.5, 'cm')) +
  guides(color = guide_legend(reverse = TRUE, byrow = TRUE))


#plot_1
ggsave(filename = 'output/SP_models_plot_Boundness_phylogenetic_spatial_models.jpg',
       plot_1, height = 29, width = 33)


#saving hyperparameters: Gaussian observations
for (i in 1:length(marginals_hyperpar_list_gaussian)) {
  marginals_hyperpar_list_gaussian[[i]]$model <- names(marginals_hyperpar_list_gaussian)[i]
}
marginals_hyperpar_list_gaussian <- dplyr::bind_rows(marginals_hyperpar_list_gaussian)

write.csv(marginals_hyperpar_list_gaussian, "output_tables/Boundness_phylogenetic_spatial_models_marginals_hyperpar_gaussian.csv") 

#saving hyperparameters: phylogenetic
for (i in 1:length(marginals_hyperpar_list_phy)) {
  marginals_hyperpar_list_phy[[i]]$model <- names(marginals_hyperpar_list_phy)[i]
}
marginals_hyperpar_list_phy <- dplyr::bind_rows(marginals_hyperpar_list_phy)

write.csv(marginals_hyperpar_list_phy, "output_tables/Boundness_phylogenetic_spatial_models_marginals_hyperpar_phylogenetic.csv")

#saving hyperparameters: spatial
for (i in 1:length(marginals_hyperpar_list_spa)) {
  marginals_hyperpar_list_spa[[i]]$model <- names(marginals_hyperpar_list_spa)[i]
}
marginals_hyperpar_list_spa <- dplyr::bind_rows(marginals_hyperpar_list_spa)

write.csv(marginals_hyperpar_list_spa, "output_tables/Boundness_phylogenetic_spatial_models_marginals_hyperpar_spatial.csv")

#saving hyperparameters: areas
for (i in 1:length(marginals_hyperpar_list_area)) {
  marginals_hyperpar_list_area[[i]]$model <- names(marginals_hyperpar_list_area)[i]
}
marginals_hyperpar_list_area <- dplyr::bind_rows(marginals_hyperpar_list_area)

write.csv(marginals_hyperpar_list_area, "output_tables/Boundness_phylogenetic_spatial_models_marginals_hyperpar_areal.csv")


#saving summaries of random effects: phylogenetic
for (i in 1:length(summary_random_list_phy)) {
  summary_random_list_phy[[i]]$model <- names(summary_random_list_phy)[i]
}
summary_random_list_phy <- dplyr::bind_rows(summary_random_list_phy)

write.csv(summary_random_list_phy, "output_tables/Boundness_phylogenetic_spatial_models_summary_random_phy.csv")

#saving summaries of random effects: spatial
for (i in 1:length(summary_random_list_spa)) {
  summary_random_list_spa[[i]]$model <- names(summary_random_list_spa)[i]
}
summary_random_list_spa <- dplyr::bind_rows(summary_random_list_spa)

write.csv(summary_random_list_spa, "output_tables/Boundness_phylogenetic_spatial_models_summary_random_spa.csv")

#saving summaries of random effects: areas
for (i in 1:length(summary_random_list_area)) {
  summary_random_list_area[[i]]$model <- names(summary_random_list_area)[i]
}
summary_random_list_area <- dplyr::bind_rows(summary_random_list_area)

write.csv(summary_random_list_area, "output_tables/Boundness_phylogenetic_spatial_models_summary_random_areas.csv")
