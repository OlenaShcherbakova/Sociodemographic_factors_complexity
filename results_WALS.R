#inflectional morphology, inflectional future is inversely correlated
#question particle is positively correlated (also in line with LNH prediction)


formula <-
  as.formula(
    paste(
      predterms[[2]],
      "~ L1_log10_st + f(phy_id, model = 'generic0', 
      Cmatrix = phylo_prec_mat, constr = TRUE, 
      hyper = pcprior_hyper)"
    )
  )
result_test <- inla(
  formula,
  family = "binomial",
  #control.family = list(hyper = pcprior_hyper),
  #control.inla = list(tolerance = 1e-8, h = 0.0001),
  #tolerance: the tolerance for the optimisation of the hyperparameters
  #h: the step-length for the gradient calculations for the hyperparameters.
  data = wals_joined,
  control.compute = list(waic = TRUE)
)

summary(result_test)
