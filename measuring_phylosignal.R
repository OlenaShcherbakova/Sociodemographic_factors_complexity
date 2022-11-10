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

physig_boundness_K <- phytools::phylosig(tree, boundness, method="K", test=TRUE)
K_boundness_K<- physig_boundness_K[1][["K"]]
P_boundness_K <- physig_boundness_K$P

#measuring phylogenetic signal of informativity
informativity<-setNames(metrics_joined$informativity_st, metrics_joined$Language_ID)
physig_informativity_l <- phytools::phylosig(tree, informativity, method="lambda", test=TRUE)
lambda_informativity_l <- physig_informativity_l[1][["lambda"]]
LR_informativity_l <- 2*(physig_informativity_l$logL-physig_informativity_l$logL0) #performing likelihood ratio test
P_lambda_informativity_l <- physig_informativity_l$P

physig_informativity_K <- phytools::phylosig(tree, informativity, method="K", test=TRUE)
K_informativity_K<- physig_informativity_K[1][["K"]]
P_informativity_K <- physig_informativity_K$P

boundness_signal <- c(physig_boundness_l$logL, physig_boundness_l$logL0, LR_boundness_l, lambda_boundness_l, P_lambda_boundness_l, K_boundness_K, P_boundness_K)
informativity_signal <- c(physig_informativity_l$logL, physig_informativity_l$logL0, LR_informativity_l, lambda_informativity_l, P_lambda_informativity_l, K_informativity_K, P_informativity_K)


#Making a table out of two measures of phylogenetic signal
physig <- as.data.frame(rbind(boundness_signal, informativity_signal))
colnames(physig) <- c("logL", "logL0", "LR (lambda)", "lambda", "p-value (lambda)", "K", "p-value (K)")
physig <- round(physig, digits=2)
physig$`p-value (lambda)` <- ifelse(physig$`p-value (lambda)` < 0.001, "< 0.001", physig$`p-value (lambda)`)
features <- as.data.frame(c("fusion", "informativity"))
colnames(features) <- "Feature"
physig <- cbind(features, physig)
write.csv(physig, file=here("output_tables", "Table_phylosig.csv"), row.names = FALSE)















gb <- load_data_final_short()

#loading ASJP file (v. 17) for world tree or a taxa file for a corresponding phylogeny
taxa <- read.csv("data/phylogenies/world/taxa.csv")

gb$taxon <- taxa$taxon[match(gb$Glottocode, taxa$glottocode)]
gb %>%
  dplyr::select(Glottocode, sem_classes, agr_patterns, taxon) %>%
  filter(!is.na(taxon)) -> gb.subset

gb.subset$sem_classes <- as.numeric(as.character(gb.subset$sem_classes))
gb.subset$agr_patterns <- as.numeric(as.character(gb.subset$agr_patterns))

gb.subset <- gb.subset[complete.cases(gb.subset),]

#load tree
TREEFILE <- "data/phylogenies/world/"
tree <- sample(load_trees(TREEFILE), 1)[[1]]

#merge data with tree and set up tree
gb_geo.subset <- gb.subset[gb.subset$taxon %in% tree$tip.label, ]
to_remove <- setdiff(tree$tip.label, gb_geo.subset$taxon)
tree <- drop.tip(tree, to_remove)

gb_geo.subset<-gb_geo.subset[match(tree$tip.label, gb_geo.subset$taxon),]
rownames(gb_geo.subset) <- gb_geo.subset$taxon

#checking if duplicates are present
dupes <- gb_geo.subset[duplicated(gb_geo.subset$Glottocode), ]
head(dupes)


#measuring phylogenetic signal (global):
tree.sem_classes <- drop.tip(tree, as.vector(gb_geo.subset[is.na(gb_geo.subset$sem_classes), 'Glottocode']))
sem_classes <- get_trait_vector(tree.sem_classes, gb_geo.subset, 'sem_classes')
physig_sem_classes_world_l <- phytools::phylosig(tree.sem_classes, sem_classes, method="lambda", test=TRUE)
lambda_sem_classes_world_l <- physig_sem_classes_world_l[1][["lambda"]]
LR_sem_classes_world_l <- 2*(physig_sem_classes_world_l$logL-physig_sem_classes_world_l$logL0) #performing likelihood ratio test
P_lambda_sem_classes_world_l <- physig_sem_classes_world_l$P

physig_sem_classes_world_K <- phytools::phylosig(tree.sem_classes, sem_classes, method="K", test=TRUE)
K_sem_classes_world_K<- physig_sem_classes_world_K[1][["K"]]
P_sem_classes_world_K <- physig_sem_classes_world_K$P

tree.agr_patterns <- drop.tip(tree, as.vector(gb_geo.subset[is.na(gb_geo.subset$agr_patterns), 'Glottocode']))
agr_patterns <- get_trait_vector(tree.agr_patterns, gb_geo.subset, 'agr_patterns')
physig_agr_patterns_world_l <- phytools::phylosig(tree.agr_patterns, agr_patterns, method="lambda", test=TRUE)
lambda_agr_patterns_world_l <- physig_agr_patterns_world_l[1][["lambda"]]
LR_agr_patterns_world_l <- 2*(physig_agr_patterns_world_l$logL-physig_agr_patterns_world_l$logL0) #performing likelihood-ratio test
P_lambda_agr_patterns_world_l <- physig_agr_patterns_world_l$P

physig_agr_patterns_world_K <- phytools::phylosig(tree.agr_patterns, agr_patterns, method="K", test=TRUE)
K_agr_patterns_world_K <- physig_agr_patterns_world_K[1][["K"]]
P_agr_patterns_world_K <- physig_agr_patterns_world_K$P

sem_classes_world <- c(physig_sem_classes_world_l$logL, physig_sem_classes_world_l$logL0, LR_sem_classes_world_l, P_lambda_sem_classes_world_l, K_sem_classes_world_K, P_sem_classes_world_K)
agr_patterns_world <- c(physig_agr_patterns_world_l$logL, physig_agr_patterns_world_l$logL0, LR_agr_patterns_world_l, P_lambda_agr_patterns_world_l, K_agr_patterns_world_K, P_agr_patterns_world_K)

#Making a table out of two measures of phylogenetic signal
physig <- as.data.frame(rbind(sem_classes_a, agr_patterns_a, sem_classes_b, agr_patterns_b, sem_classes_dr, agr_patterns_dr, sem_classes_ie, agr_patterns_ie, sem_classes_world, agr_patterns_world))
colnames(physig) <- c("logL", "logL0", "LR (lambda)", "p-value (lambda)", "K", "p-value (K)")
physig <- round(physig, digits=2)
#rownames(physig) <- c("Semantic rules (Austronesian)", "Agreement patterns (Austronesian)", "Semantic rules (Bantu)", "Agreement patterns (Bantu)", "Semantic rules (Dravidian)", "Agreement patterns (Dravidian)", "Semantic rules (Indo-European)", "Agreement patterns (Indo-European)", "Semantic rules (World)", "Agreement patterns (World)")
phylogeny <- as.data.frame(c(rep(c("Austronesian"), times=2), rep(c("Bantu"), times=2), rep(c("Dravidian"), times=2), rep(c("Indo-European"), times=2), rep(c("World"), times=2)))
colnames(phylogeny) <- "Phylogeny"
features <- as.data.frame(c(rep(c("Semantic rules", "Agreement patterns"), times=5)))
colnames(features) <- "Feature"
physig <- cbind(phylogeny, features, physig)
write.csv(physig, file=here("output_tables", "Table_SI_phylosig_continuous.csv"), row.names = FALSE)

rownames(physig) <- NULL

physig_latex <- physig %>%
  kbl(caption="Phylogenetic signal",
      format="latex") %>% #,
  kable_minimal(full_width = F) %>%
  kable_styling(latex_options = c("scale_down"))  %>%
  column_spec(1, width = "8em")
