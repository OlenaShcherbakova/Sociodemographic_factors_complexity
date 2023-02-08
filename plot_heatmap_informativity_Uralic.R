#heat map with a tree plot: boundness

#first, obtaining the actual max and min values of informativity in the entire sample
source("set_up_inla.R")

metrics_joined <- metrics_joined %>%
  filter(!is.na(L1_log10_st)) %>%
  rename(L1_log_st = L1_log10_st) %>%
  mutate(L1_copy = L1_log_st) %>%
  filter(!is.na(L2_prop)) %>%
  dplyr::mutate(L2_prop  = scale(L2_prop)[, 1]) %>%
  mutate(L2_copy = L2_prop) %>%
  filter(!is.na(neighboring_languages_st)) %>%
  filter(!is.na(Official)) %>%
  filter(!is.na(Education)) %>%
  filter(!is.na(boundness_st)) %>%
  filter(!is.na(informativity_st))

#dropping tips not in Grambank
metrics_joined <-
  metrics_joined[metrics_joined$Language_ID %in% tree$tip.label,]

max <- max(metrics_joined$informativity_st)
min <- min(metrics_joined$informativity_st)

#now plotting Uralic languages only
source("set_up_inla.R")

metrics_joined <- metrics_joined %>%
  filter(!is.na(L1_log10_st)) %>%
  rename(L1_log_st = L1_log10_st) %>%
  mutate(L1_copy = L1_log_st) %>%
  filter(!is.na(L2_prop)) %>%
  dplyr::mutate(L2_prop  = scale(L2_prop)[, 1]) %>%
  mutate(L2_copy = L2_prop) %>%
  filter(!is.na(neighboring_languages_st)) %>%
  filter(!is.na(Official)) %>%
  filter(!is.na(Education)) %>%
  filter(!is.na(boundness_st)) %>%
  filter(!is.na(informativity_st)) %>%
  filter(Family_ID == "ural1272")

#dropping tips not in Grambank
metrics_joined <-
  metrics_joined[metrics_joined$Language_ID %in% tree$tip.label,]
tree <- keep.tip(tree, metrics_joined$Language_ID)

x <-
  assert_that(all(tree$tip.label %in% metrics_joined$Language_ID), msg = "The data and phylogeny taxa do not match")

metrics_joined <-
  metrics_joined %>% mutate(Language_ID_2 = Language_ID) %>% column_to_rownames(var = "Language_ID_2")

df1 <- metrics_joined %>%
  dplyr::select(informativity_st) %>%
  rename(informativity = informativity_st)

uralic_fam_tree <-
  ggtree(tree, layout = 'rect', branch.length = 'none')
uralic_fam_tree <-
  uralic_fam_tree %<+% metrics_joined + geom_tiplab(aes(label = Name), size =
                                                      5)

p1 <-
  gheatmap(
    uralic_fam_tree,
    df1,
    offset = 6,
    width = .2,
    colnames_angle = 0,
    colnames_position = "top",
    colnames = F,
    #removing column names
    font.size = 5,
    hjust = 0.5,
    color = FALSE
  ) + #ylim(-5, 27) + #ylim(-5, 1450)
  scale_fill_viridis_c(
    option = "viridis",
    direction = -1,
    limits = c(min, max),
    guide = "none"
  ) +
  theme(legend.position = c(1, 1)) + theme(
    legend.position = "right",
    legend.box = "vertical",
    text = element_text(size = 50),
    legend.key.size = unit(0.9, 'cm')
  )

svg(
  "output/heatmap_Uralic_informativity.svg",
  width = 7,
  height = 7
)
p1
dev.off()

triple_plot <-
  b_labelled / i_labelled | p1 #+ plot_annotation(tag_levels = '1')

svg(
  "output/triple_plot.svg",
  width = 12,
  height = 7
)
triple_plot
dev.off()
