#model fitting: Informativity predicted by social effects on top of random phylogenetic and spatial factors

#small sample with data on L2 speakers

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

#Preparing the formulas for 7 competing models to be used in inla() call
listcombo <- list( 
  c("L1_log_st"), 
  
  c("f(inla.group(L1_copy), model='rw2', scale.model = TRUE)"), 
  
  c("L2_prop"), 
  
  c("f(inla.group(L2_copy), model='rw2', scale.model = TRUE)"),
  
  c("f(inla.group(L1_copy), model='rw2', scale.model = TRUE)", 
    "f(inla.group(L2_copy), model='rw2', scale.model = TRUE)"),
  
  c("L1_log_st", "L2_prop"), 
  
  c("L1_log10:L2_prop"))

predterms <- lapply(listcombo, function(x) paste(x, collapse="+"))

predterms <- t(as.data.frame(predterms))

predterms_short <- predterms

predterms_short <- gsub("f(inla.group(L1_copy), model='rw2', scale.model = TRUE)", "L1 speakers (nonlinear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("L1_log_st", "L1 speakers (linear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("f(inla.group(L2_copy), model='rw2', scale.model = TRUE)", "L2 proportion (nonlinear)", predterms_short, fixed=TRUE)
predterms_short <- gsub("L2_prop", "L2 proportion (linear)", predterms_short, fixed=TRUE)



L1_element <- data.frame("judgement" = grepl("L1 speakers (linear)", predterms_short, fixed=TRUE),
                         number = 1:length(predterms_short))
L1_element <- L1_element[L1_element$judgement == TRUE,]$number


L1_nl_element <- data.frame("judgement" = grepl("L1 speakers (nonlinear)", predterms_short, fixed=TRUE),
                            number = 1:length(predterms_short))
L1_nl_element <- L1_nl_element[L1_nl_element$judgement == TRUE,]$number

L2_prop_element <- data.frame("judgement" = grepl("L2 proportion (linear)", predterms_short, fixed=TRUE),
                              number = 1:length(predterms_short))
L2_prop_element <- L2_prop_element[L2_prop_element$judgement == TRUE,]$number
L2_prop_element <- L2_prop_element[-length(L2_prop_element)] #making sure that the interaction term (introduced below) is not treated as belonging to this isolated element

L2_prop_nl_element <- data.frame("judgement" = grepl("L2 proportion (nonlinear)", predterms_short, fixed=TRUE),
                                 number = 1:length(predterms_short))
L2_prop_nl_element <- L2_prop_nl_element[L2_prop_nl_element$judgement == TRUE,]$number

#can use only part of the interaction term within grepl() function
interaction_element <- data.frame("judgement" = grepl(":L2 proportion", predterms_short),
                                  number = 1:length(predterms_short))
interaction_element <- interaction_element[interaction_element$judgement == TRUE,]$number


n_models <- length(listcombo)
#preparing empty matrices to be filled with effect estimates (quantiles), model name, and WAIC value
phy_effects_matrix <- matrix(NA, n_models, 5)
colnames(phy_effects_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")
spa_effects_matrix <- matrix(NA, n_models, 5)
colnames(spa_effects_matrix) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

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
social_effects_matrix_L1_L2_prop <- matrix(NA, n_models, 5)
colnames(social_effects_matrix_L1_L2_prop) <- c("2.5%", "50%", "97.5%", "model", "WAIC")

#fitted values
fitted_list <- vector("list", n_models)
names(fitted_list) <- predterms_short

#marginals of hyperparameters
marginals_hyperpar_list_gaussian <- vector("list", n_models)
names(marginals_hyperpar_list_gaussian) <- predterms_short

marginals_hyperpar_list_social_L1_nl <- vector("list", n_models)
names(marginals_hyperpar_list_social_L1_nl) <- predterms_short

marginals_hyperpar_list_social_L2_prop_nl <- vector("list", n_models)
names(marginals_hyperpar_list_social_L2_prop_nl) <- predterms_short


#marginals of fixed effects
marginals_fixed_list_Intercept <- vector("list", n_models)
names(marginals_fixed_list_Intercept) <- predterms_short

marginals_fixed_list_L1 <- vector("list", n_models)
names(marginals_fixed_list_L1) <- predterms_short

marginals_fixed_list_L2_prop <- vector("list", n_models)
names(marginals_fixed_list_L2_prop) <- predterms_short

marginals_fixed_list_L1_L2_prop <- vector("list", n_models)
names(marginals_fixed_list_L1_L2_prop) <- predterms_short

#summary statistics of random effects
summary_random_list_social_L1_nl <- vector("list", n_models)
names(summary_random_list_social_L1_nl) <- predterms_short

summary_random_list_social_L2_prop_nl <- vector("list", n_models)
names(summary_random_list_social_L2_prop_nl) <- predterms_short


coefm <- matrix(NA,n_models,1)
result <- vector("list",n_models)

for(i in 1:n_models){
  formula <- as.formula(paste("informativity_st ~ ",predterms[[i]]))
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
  
  if(i %in% interaction_element) {
    social_effects_matrix_L1_L2_prop[i, 1:3] <- c(result[[i]]$summary.fixed["L1_log10:L2_prop",]$`0.025quant`, result[[i]]$summary.fixed["L1_log10:L2_prop",]$`0.5quant`, result[[i]]$summary.fixed["L1_log10:L2_prop",]$`0.975quant`)
    social_effects_matrix_L1_L2_prop[i, 4] <- predterms_short[[i]]
    social_effects_matrix_L1_L2_prop[i, 5] <- result[[i]]$waic$waic
    
    marginals_fixed_list_L1_L2_prop[[i]] <- as.data.frame(cbind(result[[i]][["marginals.fixed"]][["L1_log10:L2_prop"]]))
    colnames(marginals_fixed_list_L1_L2_prop[[i]]) <- c("x for L1*L2 proportion", "y for L1*L2 proportion")
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
  
  if(i %in% L2_prop_nl_element){
    marginals_hyperpar_list_social_L2_prop_nl[[i]] <- as.data.frame(cbind(result[[i]]$marginals.hyperpar[["Precision for inla.group(L2_copy)"]]))
    colnames(marginals_hyperpar_list_social_L2_prop_nl[[i]]) <- c("x for inla.group(L2_copy)", "y for inla.group(L2_copy)")
  }
}

beepr::beep(5)

save(result, file = "output_models_reanalysis/models_Informativity_L2_social_only.RData")
load("output_models_reanalysis/models_Informativity_L2_social_only.RData")


coefm <- as.data.frame(cbind(predterms_short, coefm))
colnames(coefm) <- c("model", "WAIC")
coefm <- coefm %>%
  mutate(across(.cols=2, as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  arrange(WAIC)

coefm$WAIC <- as.numeric(coefm$WAIC)
coefm <- coefm[order(coefm$WAIC),]

coefm_path <- paste("output_tables_reanalysis/", "waics", "Informativity_L2_social_only_models", ".csv", collapse = "")
write.csv(coefm, coefm_path, row.names=FALSE)

for (i in 1:length(fitted_list)) {
  fitted_list[[i]]$model <- names(fitted_list)[i]
}
fitted_list <- dplyr::bind_rows(fitted_list)
fitted_list_path <- paste("output_tables_reanalysis/", "fitted_list", "Informativity_L2_social_only_models", ".csv", collapse = "")
write.csv(fitted_list, fitted_list_path) 


intercept_effects <- as.data.frame(intercept_matrix)
L1_effects <- as.data.frame(social_effects_matrix_L1)
L1_nl_effects <- as.data.frame(social_effects_matrix_L1_nl)
L2_prop_effects <- as.data.frame(social_effects_matrix_L2_prop)
L2_prop_nl_effects <- as.data.frame(social_effects_matrix_L2_prop_nl)
interaction_effects <- as.data.frame(social_effects_matrix_L1_L2_prop)

intercept_effects$effect <- "Intercept"
L1_effects$effect <- "L1"
L1_nl_effects$effect <- "social SD:\nL1"
L2_prop_effects$effect <- "L2 proportion"
L2_prop_nl_effects$effect <- "social SD:\nL2 proportion"
interaction_effects$effect <- "L1*L2 proportion"

effs <- as.data.frame(rbind(phy_effects, spa_effects, intercept_effects, L1_effects, L1_nl_effects, L2_prop_effects, L2_prop_nl_effects, interaction_effects))
effs <- effs %>%
  mutate(across(.cols=c(1:3, 5), as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  na.omit() %>%
  arrange(WAIC) %>%
  relocate(model)

effs_path <- paste("output_tables_reanalysis/", "effects", "Informativity_L2_social_only_models", ".csv", collapse = "")
write.csv(effs, effs_path, row.names=FALSE)

effs <- read.csv("output_tables_reanalysis/ effects Informativity_L2_social_only_models .csv")

effs_table_Main <- effs %>%
  rename("2.5%"=2,
         "50%" = 3,
         "97.5%" = 4) %>%
  filter(!grepl("nonlinear", model))

effs_table_Main$model <- gsub("(\\s*\\(\\w+\\))", "", effs_table_Main$model)

effs_table_Main <- effs_table_Main %>%
  relocate(effect, .after = model) %>%
  flextable() %>%
  flextable::bold(~ (`2.5%` > 0 & `97.5%` > 0) | (`2.5%` < 0 & `97.5%` < 0), 2) %>%
  autofit() %>%
  merge_v(j=c("model", "WAIC")) %>%
  fix_border_issues() %>%
  border_inner_h()

save_as_docx(
  "Effects in boundness models with fixed and random effects" = effs_table_Main, 
  path = "output_tables_reanalysis/table_Main_effects_Informativity_L2_social_only_models.docx")


effs_plot <- effs %>%
  #filter(WAIC <= top_9) %>%
  rename(lower=2,
         upper = 4,
         mean = 3) %>% #mean here refers to 0.5 quantile 
  #filter(!effect == "Intercept") %>%
  mutate(effect = factor(effect, levels=c("Intercept", "social SD:\nL1", "L1", "social SD:\nL2 proportion", "L2 proportion", "Neighbours", "Education", "Official status", "L1*L2 proportion"))) %>%
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
ggsave(filename = 'output_reanalysis/SP_models_plot_Informativity_L2_social_only_models.jpg',
       plot_1, height = 20, width = 45) 


#saving hyperparameters: Gaussian observations
for (i in 1:length(marginals_hyperpar_list_gaussian)) {
  marginals_hyperpar_list_gaussian[[i]]$model <- names(marginals_hyperpar_list_gaussian)[i]
}
marginals_hyperpar_list_gaussian <- dplyr::bind_rows(marginals_hyperpar_list_gaussian)

write.csv(marginals_hyperpar_list_gaussian, "output_tables_reanalysis/Informativity_L2_social_only_models_marginals_hyperpar_gaussian.csv") 
