#global tree
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

#measuring phylogenetic signal of boundness/fusion
boundness<-setNames(metrics_joined$boundness_st, metrics_joined$Language_ID)
physig_boundness_l <- phytools::phylosig(tree, boundness, method="lambda", test=TRUE)
lambda_boundness_l <- physig_boundness_l[1][["lambda"]]
LR_boundness_l <- 2*(physig_boundness_l$logL-physig_boundness_l$logL0) #performing likelihood ratio test
P_lambda_boundness_l <- physig_boundness_l$P

#measuring phylogenetic signal of informativity
informativity<-setNames(metrics_joined$informativity_st, metrics_joined$Language_ID)
physig_informativity_l <- phytools::phylosig(tree, informativity, method="lambda", test=TRUE)
lambda_informativity_l <- physig_informativity_l[1][["lambda"]]
LR_informativity_l <- 2*(physig_informativity_l$logL-physig_informativity_l$logL0) #performing likelihood ratio test
P_lambda_informativity_l <- physig_informativity_l$P

boundness_signal <- c(physig_boundness_l$logL, physig_boundness_l$logL0, LR_boundness_l, lambda_boundness_l, P_lambda_boundness_l)
informativity_signal <- c(physig_informativity_l$logL, physig_informativity_l$logL0, LR_informativity_l, lambda_informativity_l, P_lambda_informativity_l)


#Making a table out of two measures of phylogenetic signal
physig <- as.data.frame(rbind(boundness_signal, informativity_signal))
colnames(physig) <- c("logL", "logL0", "LR (lambda)", "lambda", "p-value")
physig <- round(physig, digits=2)
physig$`p-value` <- ifelse(physig$`p-value` < 0.001, "< 0.001", physig$`p-value`)
features <- as.data.frame(c("fusion", "informativity"))
colnames(features) <- "Feature"
physig <- cbind(features, physig)
write.csv(physig, file=here("output_tables", "Table_phylosig.csv"), row.names = FALSE)
