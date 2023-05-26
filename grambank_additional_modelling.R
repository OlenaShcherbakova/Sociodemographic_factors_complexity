source("wals_additional_modelling.R")
source("gb_additional_setup.R")

dataframe_list <- list(gb_1_joined, gb_2_joined)

listcombo <- list(c("Alignment"),
                  
                  c("Person_marking"))


predterms <- lapply(listcombo, function(x)
  paste(x, collapse = "+"))

predterms <- t(as.data.frame(predterms))

predterms_short <- predterms

element_1 <-
  data.frame(
    "judgement" = grepl("Alignment", predterms_short),
    number = 1:length(predterms_short)
  )
element_1 <-
  element_1[element_1$judgement == TRUE, ]$number

element_2 <-
  data.frame(
    "judgement" = grepl("Person_marking", predterms_short),
    number = 1:length(predterms_short)
  )
element_2 <-
  element_2[element_2$judgement == TRUE, ]$number

models_number <- length(predterms_short)

#preparing empty matrices to be filled with effect estimates (quantiles) (L1 coefficients) for each response feature

matrix_1 <- matrix(NA, models_number, 5)
colnames(matrix_1) <-
  c("2.5%", "50%", "97.5%", "feature", "sample")
matrix_1[1, 5] <- nrow(dataframe_list[[1]])
matrix_2 <- matrix(NA, models_number, 5)
colnames(matrix_2) <-
  c("2.5%", "50%", "97.5%", "feature", "sample")
matrix_2[2, 5] <- nrow(dataframe_list[[2]])

result <- vector("list", models_number)

for (i in 1:models_number) {
  formula <- as.formula(paste(predterms[[i]], "~ L1_log10_st"))
  result[[i]] <- inla(
    formula,
    family = "binomial",
    #control.family = list(hyper = pcprior_hyper),
    #control.inla = list(tolerance = 1e-8, h = 0.0001),
    #tolerance: the tolerance for the optimisation of the hyperparameters
    #h: the step-length for the gradient calculations for the hyperparameters.
    data = dataframe_list[[i]],
    control.compute = list(waic = TRUE)
  )
  
  
  if (i %in% element_1) {
    matrix_1[i, 1:3] <-
      c(
        result[[i]]$summary.fixed[2, ]$`0.025quant`,
        result[[i]]$summary.fixed[2, ]$`0.5quant`,
        result[[i]]$summary.fixed[2, ]$`0.975quant`
      )
    matrix_1[i, 4] <- predterms_short[[i]]
    
  }
  
  if (i %in% element_2) {
    matrix_2[i, 1:3] <-
      c(
        result[[i]]$summary.fixed[2, ]$`0.025quant`,
        result[[i]]$summary.fixed[2, ]$`0.5quant`,
        result[[i]]$summary.fixed[2, ]$`0.975quant`
      )
    matrix_2[i, 4] <- predterms_short[[i]]
    
  }
  
}

#beepr::beep(5)

save(result, file = "output_models/models_GB.RData")

matrix_1 <- as.data.frame(matrix_1)
matrix_2 <- as.data.frame(matrix_2)

effs <-
  as.data.frame(rbind(matrix_1, matrix_2))

effs_gb <- effs %>%
  mutate(across(.cols = c(1:3, 5), as.numeric)) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  na.omit() %>%
  mutate(dataset = "GB") %>%
  relocate(feature)

additional_effs_combined <- as.data.frame(rbind(effs_wals, effs_gb))

effs_path <-
    paste("output_tables/",
          "effects",
          "WALS_GB_models",
          ".csv",
          collapse = "")
  write.csv(additional_effs_combined, effs_path, row.names = FALSE)
  
  effs_table_SM <- additional_effs_combined %>%
    rename("2.5%" = 2,
           "50%" = 3,
           "97.5%" = 4) %>%
    flextable() %>%
    autofit() %>%
    merge_v(j = c("dataset")) %>%
    fix_border_issues() %>%
    border_inner_h()

  save_as_docx(
    "_WALS_GB" = effs_table_SM,
    path = "output_tables/table_SM_effects_WALS_GB_models.docx"
  )

