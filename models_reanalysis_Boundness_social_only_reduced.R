#model fitting: reduced sample setting
#Boundness predicted by social effects

#Script was written by Sam Passmore and modified by Olena Shcherbakova

source("requirements.R")

source("install_and_load_INLA.R")

source("set_up_inla.R")

metrics_joined <- metrics_joined %>% 
  filter(!is.na(L1_log10_st)) %>%
  rename(L1_log_st = L1_log10_st) %>%
  mutate(L1_copy = L1_log_st) %>%
  # filter(!is.na(L2_prop)) %>%
  # mutate(L2_copy = L2_prop) %>%
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

#calculate geo_dists
lat_long_matrix <- metrics_joined %>% 
  column_to_rownames("Language_ID") %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

rdist.earth_dists <- fields::rdist.earth(lat_long_matrix, miles = FALSE)

rdist.earth_dists[upper.tri(rdist.earth_dists, diag = TRUE)] <- NA

# dists_vector <- as.vector(rdist.earth_dists) 
dists_vector = rdist.earth_dists[lower.tri(rdist.earth_dists)]

#"local" set of parameters
## Create spatial covariance matrix using the matern covariance function
spatial_covar_mat_1 = varcov.spatial(dists.lowertri = rdist.earth_dists[lower.tri(rdist.earth_dists)] / 100, 
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

#Preparing the formulas for 10 competing models to be used in inla() call
listcombo <- list( 
  c("L1_log_st"), 
  
  c("f(inla.group(L1_copy), model='rw2', scale.model = TRUE)"), 
  
  c("Vehicularity"), 
  
  c("L1_log_st", "Vehicularity"), 
  
  #unavailable
  #c("L1_log10:Vehicularity"),
  
  c("neighboring_languages_st"), 
  
  c("Official"),
  
  c("Education"))


predterms <- lapply(listcombo, function(x) paste(x, collapse="+"))

predterms <- t(as.data.frame(predterms))

predterms_short <- predterms

predterms_short <- gsub("f(inla.group(L1_copy), model='rw2', scale.model = TRUE)", "L1 speakers (nonlinear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("L1_log_st", "L1 speakers (linear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("neighboring_languages_st", "Neighbours", predterms_short, fixed=TRUE)

L1_element <- data.frame("judgement" = grepl("L1 speakers (linear)", predterms_short, fixed=TRUE),
                         number = 1:length(predterms_short))
L1_element <- L1_element[L1_element$judgement == TRUE,]$number


L1_nl_element <- data.frame("judgement" = grepl("L1 speakers (nonlinear)", predterms_short, fixed=TRUE),
                            number = 1:length(predterms_short))
L1_nl_element <- L1_nl_element[L1_nl_element$judgement == TRUE,]$number

L2_prop_element <- data.frame("judgement" = grepl("Vehicularity", predterms_short, fixed=TRUE),
                              number = 1:length(predterms_short))
L2_prop_element <- L2_prop_element[L2_prop_element$judgement == TRUE,]$number
#unnecessary
#L2_prop_element <- L2_prop_element[-length(L2_prop_element)] #making sure that the interaction term (introduced below) is not treated as belonging to this isolated element

#unnecessary
#can use only part of the interaction term within grepl() function
# interaction_element <- data.frame("judgement" = grepl(":Vehicularity", predterms_short),
#                                   number = 1:length(predterms_short))
# interaction_element <- interaction_element[interaction_element$judgement == TRUE,]$number

neighbour_element <- data.frame("judgement" = grepl("Neighbours", predterms_short),
                                number = 1:length(predterms_short))
neighbour_element <- neighbour_element[neighbour_element$judgement == TRUE,]$number

official_element <- data.frame("judgement" = grepl("Official", predterms_short),
                               number = 1:length(predterms_short))
official_element <- official_element[official_element$judgement == TRUE,]$number

education_element <- data.frame("judgement" = grepl("Education", predterms_short),
                                number = 1:length(predterms_short))
education_element <- education_element[education_element$judgement == TRUE,]$number


n_models <- length(listcombo)
#preparing empty matrices to be filled with effect estimates (quantiles), model name, and WAIC value
intercept_matrix <- matrix(NA, n_models, 5)
colnames(intercept_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

social_effects_matrix_L1 <- matrix(NA, n_models, 5)
colnames(social_effects_matrix_L1) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_L1_nl <- matrix(NA, n_models, 5)
colnames(social_effects_matrix_L1_nl) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_L2_prop <- matrix(NA, n_models, 5)
colnames(social_effects_matrix_L2_prop) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_L2_prop_nl <- matrix(NA, n_models, 5)
colnames(social_effects_matrix_L2_prop_nl) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_N <- matrix(NA, n_models, 5)
colnames(social_effects_matrix_N) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_O <- matrix(NA, n_models, 5)
colnames(social_effects_matrix_O) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
social_effects_matrix_E <- matrix(NA, n_models, 5)
colnames(social_effects_matrix_E) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
#unnecessary
# social_effects_matrix_L1_L2_prop <- matrix(NA, n_models, 5)
# colnames(social_effects_matrix_L1_L2_prop) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

#fitted values
fitted_list <- vector("list", n_models)
names(fitted_list) <- predterms_short

#marginals of hyperparameters
marginals_hyperpar_list_gaussian <- vector("list", n_models)
names(marginals_hyperpar_list_gaussian) <- predterms_short

marginals_hyperpar_list_social_L1_nl <- vector("list", n_models)
names(marginals_hyperpar_list_social_L1_nl) <- predterms_short

#marginals of fixed effects
marginals_fixed_list_Intercept <- vector("list", n_models)
names(marginals_fixed_list_Intercept) <- predterms_short

marginals_fixed_list_L1 <- vector("list", n_models)
names(marginals_fixed_list_L1) <- predterms_short

marginals_fixed_list_L2_prop <- vector("list", n_models)
names(marginals_fixed_list_L2_prop) <- predterms_short

marginals_fixed_list_O <- vector("list", n_models)
names(marginals_fixed_list_O) <- predterms_short

marginals_fixed_list_N <- vector("list", n_models)
names(marginals_fixed_list_N) <- predterms_short

marginals_fixed_list_E <- vector("list", n_models)
names(marginals_fixed_list_E) <- predterms_short

#unnecessary
# marginals_fixed_list_L1_L2_prop <- vector("list", n_models)
# names(marginals_fixed_list_L1_L2_prop) <- predterms_short


#summary statistics of random effects
summary_random_list_social_L1_nl <- vector("list", n_models)
names(summary_random_list_social_L1_nl) <- predterms_short

summary_random_list_social_L2_prop_nl <- vector("list", n_models)
names(summary_random_list_social_L2_prop_nl) <- predterms_short


coefm <- matrix(NA,n_models,1)
result <- vector("list",n_models)

for(i in 1:n_models){
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
  
  if(i %in% L1_nl_element){
    social_effects_matrix_L1_nl[i, 1:3] <- inla.tmarginal(function(x) 1/sqrt(x),
                                                          result[[i]]$marginals.hyperpar$`Precision for inla.group(L1_copy)`,
                                                          method = "linear") %>%
      inla.qmarginal(c(0.025, 0.5, 0.975), .)
    social_effects_matrix_L1_nl[i, 4] <- predterms_short[[i]]
    social_effects_matrix_L1_nl[i, 5] <- result[[i]]$waic$waic
  } 
  
  if(i %in% L1_element) {
    social_effects_matrix_L1[i, 1:3] <- c(result[[i]]$summary.fixed["L1_log_st",]$`0.025quant`, result[[i]]$summary.fixed["L1_log_st",]$`0.5quant`, result[[i]]$summary.fixed["L1_log_st",]$`0.975quant`)
    social_effects_matrix_L1[i, 4] <- predterms_short[[i]]
    social_effects_matrix_L1[i, 5] <- result[[i]]$waic$waic
    
    marginals_fixed_list_L1[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][["L1_log_st"]]))
    colnames(marginals_fixed_list_L1[[i]]) <- c("x for L1", "y for L1")
  }
  
  if(i %in% L2_prop_element) {
    social_effects_matrix_L2_prop[i, 1:3] <- c(result[[i]]$summary.fixed["Vehicularity",]$`0.025quant`, result[[i]]$summary.fixed["Vehicularity",]$`0.5quant`, result[[i]]$summary.fixed["Vehicularity",]$`0.975quant`)
    social_effects_matrix_L2_prop[i, 4] <- predterms_short[[i]]
    social_effects_matrix_L2_prop[i, 5] <- result[[i]]$waic$waic
    
    marginals_fixed_list_L2_prop[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][["Vehicularity"]]))
    colnames(marginals_fixed_list_L2_prop[[i]]) <- c("x for Vehicularity", "y for Vehicularity")
  }
  
  #unnecessary
  # if(i %in% interaction_element) {
  #   social_effects_matrix_L1_L2_prop[i, 1:3] <- c(result[[i]]$summary.fixed["L1_log10:Vehicularity",]$`0.025quant`, result[[i]]$summary.fixed["L1_log10:Vehicularity",]$`0.5quant`, result[[i]]$summary.fixed["L1_log10:Vehicularity",]$`0.975quant`)
  #   social_effects_matrix_L1_L2_prop[i, 4] <- predterms_short[[i]]
  #   social_effects_matrix_L1_L2_prop[i, 5] <- result[[i]]$waic$waic
  #   
  #   marginals_fixed_list_L1_L2_prop[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][["L1_log10:Vehicularity"]]))
  #   colnames(marginals_fixed_list_L1_L2_prop[[i]]) <- c("x for L1*Vehicularity", "y for L1*Vehicularity")
  # }
  
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
  
  if(i %in% L1_nl_element){
    marginals_hyperpar_list_social_L1_nl[[i]] <- as.data.frame(cbind(result[[i]]$marginals.hyperpar[["Precision for inla.group(L1_copy)"]]))
    colnames(marginals_hyperpar_list_social_L1_nl[[i]]) <- c("x for inla.group(L1_copy)", "y for inla.group(L1_copy)")
  }
}

#beepr::beep(5)

save(result, file = "output_models_reanalysis_reduced/models_Boundness_reanalysis_social_only.RData")
#load("output_models_reanalysis_reduced/models_Boundness_reanalysis_social_only.RData")

coefm <- as.data.frame(cbind(predterms_short, coefm))
colnames(coefm) <- c("model", "WAIC")
coefm <- coefm %>%
  mutate(across(.cols=2, as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  arrange(WAIC)

coefm$WAIC <- as.numeric(coefm$WAIC)
coefm <- coefm[order(coefm$WAIC),]

coefm_path <- paste("output_tables_reanalysis_reduced/", "waics", "Boundness_reanalysis_social_only_models", ".csv", collapse = "")
write.csv(coefm, coefm_path, row.names=FALSE)

for (i in 1:length(fitted_list)) {
  fitted_list[[i]]$model <- names(fitted_list)[i]
}

fitted_list <- dplyr::bind_rows(fitted_list)
fitted_list_path <- paste("output_tables_reanalysis_reduced/", "fitted_list", "Boundness_reanalysis_social_only_models", ".csv", collapse = "")
write.csv(fitted_list, fitted_list_path) 

intercept_effects <- as.data.frame(intercept_matrix)
L1_effects <- as.data.frame(social_effects_matrix_L1)
L1_nl_effects <- as.data.frame(social_effects_matrix_L1_nl)
L2_prop_effects <- as.data.frame(social_effects_matrix_L2_prop)
L2_prop_nl_effects <- as.data.frame(social_effects_matrix_L2_prop_nl)
N_effects<-as.data.frame(social_effects_matrix_N)
E_effects<-as.data.frame(social_effects_matrix_E)
O_effects<-as.data.frame(social_effects_matrix_O)
#unnecessary
#interaction_effects <- as.data.frame(social_effects_matrix_L1_L2_prop)


intercept_effects$effect <- "Intercept"
L1_effects$effect <- "L1"
L1_nl_effects$effect <- "social SD:\nL1"
L2_prop_effects$effect <- "Vehicularity"
N_effects$effect <- "Neighbours"
E_effects$effect <- "Education"
O_effects$effect <- "Official status"
#unnecessary
#interaction_effects$effect <- "L1*Vehicularity"

effs <- as.data.frame(rbind(intercept_effects, L1_effects, L1_nl_effects, L2_prop_effects, N_effects, O_effects, E_effects))
effs <- effs %>%
  mutate(across(.cols=c(1:3, 5), as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  na.omit() %>%
  arrange(WAIC) %>%
  relocate(model)

effs_path <- paste("output_tables_reanalysis_reduced/", "effects", "Boundness_reanalysis_social_only_models", ".csv", collapse = "")
write.csv(effs, effs_path, row.names=FALSE)

effs <- read.csv("output_tables_reanalysis_reduced/ effects Boundness_reanalysis_social_only_models .csv")

effs_table_SM <- effs %>%
  rename("2.5%"=2,
         "50%" = 4,
         "97.5%" = 3) %>%
  flextable() %>%
  autofit() %>%
  merge_v(j=c("model", "WAIC")) %>%
  fix_border_issues() %>%
  border_inner_h()

save_as_docx(
  "Effects in boundness models with fixed and random effects (including non-linear implementations of some fixed effects)" = effs_table_SM, 
  path = "output_tables_reanalysis_reduced/table_SM_effects_Boundness_reanalysis_social_only_models.docx")

effs_table_Main <- effs %>%
  rename("2.5%"=2,
         "50%" = 4,
         "97.5%" = 3) %>%
  filter(!grepl("nonlinear", model))

effs_table_Main$model <- gsub("(\\s*\\(\\w+\\))", "", effs_table_Main$model)

effs_table_Main <- effs_table_Main %>%
  flextable() %>%
  autofit() %>%
  merge_v(j=c("model", "WAIC")) %>%
  fix_border_issues() %>%
  border_inner_h()

save_as_docx(
  "Effects in boundness models with fixed and random effects" = effs_table_Main, 
  path = "output_tables_reanalysis_reduced/table_Main_effects_Boundness_reanalysis_social_only_models.docx")


effs_plot <- effs %>%
  #filter(WAIC <= top_9) %>%
  rename(lower=2,
         upper = 4,
         mean = 3) %>% #mean here refers to 0.5 quantile 
  #filter(!effect == "Intercept") %>%
  mutate(WAIC = round(WAIC, 2)) %>%
  unite("model", model, WAIC, sep = ",\nWAIC: ", remove=FALSE) %>%
  group_by(WAIC) %>%
  arrange(WAIC) %>%
  mutate(model = forcats::fct_reorder(as.factor(model), WAIC)) %>% #reordering levels within model based on WAIC values
  mutate(model = factor(model, levels=rev(levels(model)))) #reversing the order


#plot modified from function ggregplot::Efxplot
cols = c(brewer.pal(12, "Paired"))
cols = c("gray50", cols[c(1:8)])

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
ggsave(filename = 'output_reanalysis_reduced/SP_models_plot_Boundness_reanalysis_social_only_models.jpg',
       plot_1, height = 20, width = 45)


#saving hyperparameters: Gaussian observations
for (i in 1:length(marginals_hyperpar_list_gaussian)) {
  marginals_hyperpar_list_gaussian[[i]]$model <- names(marginals_hyperpar_list_gaussian)[i]
}
marginals_hyperpar_list_gaussian <- dplyr::bind_rows(marginals_hyperpar_list_gaussian)

write.csv(marginals_hyperpar_list_gaussian, "output_tables_reanalysis_reduced/Boundness_reanalysis_social_only_models_marginals_hyperpar_gaussian.csv") 