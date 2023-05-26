source("featurewise_setup.R")

social_effects_matrix_L1 <- matrix(NA, 55, 5)
colnames(social_effects_matrix_L1) <-
  c("feature", "2.5%", "50%", "97.5%", "correlation")

index <- 0

for (feature in fusion_features) {
  # Remove the 'c()' function around fusion_features
  index <- index + 1
  cat(paste0("I'm at feature ", feature, " which is index ", index, ".\n"))
  
  # Construct the formula using the actual feature name
  formula <-
    as.formula(
      paste0(
        feature,
        " ~ f(phy_id, model = 'generic0', Cmatrix = phylo_prec_mat, constr = TRUE, hyper = pcprior_hyper) + f(sp_id, model = 'generic0', Cmatrix = spatial_prec_mat_1, constr = TRUE, hyper = pcprior_hyper) + L1_log10_st"
      )
    )
  
  binomial_model <- INLA::inla(
    formula = formula,
    family = "binomial",
    control.inla = list(tolerance = 1e-7, h = 0.0001),
    control.predictor = list(link = 1, compute = TRUE),
    data = GB_fusion,
    num.threads = 6
  )
  
  qs::qsave(
    x = binomial_model,
    file = paste0("output_models", "/INLA_obj_", index , "_" , feature, ".qs")
  )
  
  social_effects_matrix_L1[index, 2:4] <-
    c(
      binomial_model$summary.fixed[2, ]$`0.025quant`,
      binomial_model$summary.fixed[2, ]$`0.5quant`,
      binomial_model$summary.fixed[2, ]$`0.975quant`
    )
  
  social_effects_matrix_L1[index, 1] <- feature
  social_effects_matrix_L1[index, 5] <-
    ifelse((
      binomial_model$summary.fixed[2, ]$`0.025quant` > 0 &
        binomial_model$summary.fixed[2, ]$`0.975quant` > 0
    ) |
      (
        binomial_model$summary.fixed[2, ]$`0.025quant` < 0 &
          binomial_model$summary.fixed[2, ]$`0.975quant` < 0
      ),
    "correlation",
    "no correlation"
    )
  
}

social_effects_matrix_L1_copy <- social_effects_matrix_L1

social_effects_matrix_L1 <- as.data.frame(social_effects_matrix_L1)

social_effects_matrix_L1 <- social_effects_matrix_L1 %>%
  rename("lower" = 2,
        "value" = 3,
         "upper" = 4) %>%
  mutate(
    correlation = case_when(
      correlation == "no correlation" ~ "no correlation",
      correlation == "correlation" &
        value > 0 ~ "positive",
      correlation == "correlation" &
        value < 0 ~ "negative"
    )
  ) %>%
  left_join(parameters, by=c("feature"="ID"))

social_effects_matrix_L1 <- social_effects_matrix_L1 %>% 
  mutate(lower = as.numeric(lower),
         value = as.numeric(value),
         upper = as.numeric(upper)) %>% 
  mutate(across(where(is.numeric), ~round(., 2)))



  write.csv(social_effects_matrix_L1,
            "output_tables/featurewise_INLA_analysis_results.csv")
  
  social_effects_matrix_L1_doc <- social_effects_matrix_L1 %>%
    flextable() %>%
    autofit() %>%
    fix_border_issues()
  
  save_as_docx("Summary of L1 coefficients for individual fusion features" = social_effects_matrix_L1_doc,
               path = "output_tables/Featurewise_analyses.docx")
