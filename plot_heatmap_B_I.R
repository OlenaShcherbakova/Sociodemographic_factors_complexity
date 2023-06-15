#heat map with a tree plot: boundness + informativity
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
tree <- keep.tip(tree, metrics_joined$Language_ID)

x <-
  assert_that(all(tree$tip.label %in% metrics_joined$Language_ID), msg = "The data and phylogeny taxa do not match")

metrics_joined <-
  metrics_joined %>% mutate(Language_ID_2 = Language_ID) %>% column_to_rownames(var = "Language_ID_2")

df1 <-
  metrics_joined %>% dplyr::select(boundness_st) %>% rename(boundness = boundness_st)
df2 <-
  metrics_joined %>% dplyr::select(informativity_st) %>% rename(informativity = informativity_st)

### Adding colored branches of the biggest families in the dataset

metrics_joined %>%
  group_by(Family_ID) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(n)) %>%
  filter(!Family_ID == "") %>%
  top_n(12, freq) -> table

biggest_families <- table$Family_ID
metrics_joined$family_status <- NA
metrics_joined$family_status <-
  ifelse(metrics_joined$Family_ID %in% biggest_families,
         metrics_joined$Family_ID,
         "other")

#double-checking if all families indeed converted to names and none is left with "NA"
unique(metrics_joined$family_status)

metrics_joined <- metrics_joined %>%
  mutate(
    family =
      dplyr::recode(
        family_status,
        "aust1307"  = "Austronesian",
        "aust1305"  = "Austroasiatic",
        "indo1319"  = "Indo-European",
        "atla1278" = "Atlantic-Congo",
        "utoa1244" = "Uto-Aztecan",
        "sino1245" = "Sino-Tibetan",
        "afro1255"  = "Afro-Asiatic",
        "nucl1709" = "Nuclear Trans New Guinea",
        "maya1287" = "Mayan",
        "pano1259" = "Pano-Tacanan",
        "otom1299" = "Otomanguean",
        "chib1249" = "Chibchan ",
        "nakh1245" = "Nakh-Daghestanian",
        "cent2225" = "Central Sudanic",
        "drav1251" = "Dravidian",
        "ural1272" = "Uralic",
        "pama1250" = "Pama-Nyungan",
        "other" = "other"
      )
  )

#double-checking if all families indeed converted to names and none is left with "NA"
unique(metrics_joined$family)

#ordering the families in the desired way
#metrics_joined$family <- factor(metrics_joined$family, order = TRUE, levels = c("other", "Austronesian", "Austroasiatic", "Sino-Tibetan",  "Indo-European", "Atlantic-Congo", "Afro-Asiatic", "Uto-Aztecan", "Nuclear Trans New Guinea"))

tips_lists <- vector(mode = "list", length = 12)

for (f in 1:length(biggest_families)) {
  tips_lists[[f]] <-
    metrics_joined[metrics_joined$Family_ID == biggest_families[f], ]$Language_ID
  tips_lists[[f]] <- na.omit(tips_lists[[f]])
  
  }

#the correct order within biggest families is preserved and the Glottocodes are replaced with suitable family name labels
biggest_families_verbose <- dplyr::recode(
  biggest_families,
  "aust1307"  = "Austronesian",
  "aust1305"  = "Austroasiatic",
  "indo1319"  = "Indo-European",
  "atla1278" = "Atlantic-Congo",
  "utoa1244" = "Uto-Aztecan",
  "sino1245" = "Sino-Tibetan",
  "afro1255"  = "Afro-Asiatic",
  "nucl1709" = "Nuclear Trans New Guinea",
  "maya1287" = "Mayan",
  "pano1259" = "Pano-Tacanan",
  "otom1299" = "Otomanguean",
  "chib1249" = "Chibchan ",
  "nakh1245" = "Nakh-Daghestanian",
  "cent2225" = "Central Sudanic",
  "drav1251" = "Dravidian",
  "ural1272" = "Uralic",
  "pama1250" = "Pama-Nyungan",
  "other" = "other"
)

names(tips_lists) <- biggest_families_verbose

nodes <- vector(mode = "character", length = length(biggest_families))

for (tips in 1:length(tips_lists)) {
  nodes[tips] <- getMRCA(tree, tips_lists[[tips]])
}

#test
#nodes <- vector(mode="character", length=1)
#nodes[1] <- getMRCA(tree, tips_lists[[4]])

nodes <- as.numeric(nodes)

coloured_branches <- groupClade(tree, nodes)
coloured_branches <-
  ggtree(
    coloured_branches,
    layout = 'rect',
    branch.length = 'none',
    size = 0.5
  )

p1 <-
  gheatmap(
    coloured_branches,
    df1,
    offset = -1,
    width = .1,
    colnames_angle = 0,
    colnames_offset_y = 25,
    colnames_position = "top",
    colnames = F,
    #removing column names
    font.size = 20,
    hjust = 0.5,
    color = FALSE
  ) + ylim(-5, 1480) + #ylim(-5, 1450)
  scale_fill_viridis_c(option = "magma", direction = -1) + labs(fill = "fusion") + theme(legend.position = "bottom",
                                                                                         legend.key.size = unit(1.4, 'cm')) +
  geom_cladelabel(
    node = nodes[1],
    label = biggest_families_verbose[1],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[2],
    label = biggest_families_verbose[2],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[3],
    label = biggest_families_verbose[3],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[4],
    label = biggest_families_verbose[4],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[5],
    label = biggest_families_verbose[5],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[6],
    label = biggest_families_verbose[6],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[7],
    label = biggest_families_verbose[7],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[8],
    label = biggest_families_verbose[8],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[9],
    label = biggest_families_verbose[9],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[10],
    label = biggest_families_verbose[10],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[11],
    label = biggest_families_verbose[11],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) +
  geom_cladelabel(
    node = nodes[12],
    label = biggest_families_verbose[12],
    offset = 6,
    align = TRUE,
    fontsize = 13
  ) + labs(fill = "fusion")

p2 <- p1 + new_scale_fill()

p3 <- gheatmap(
  p2,
  df2,
  offset = 2,
  width = .1,
  colnames_angle = 0,
  colnames_offset_y = 25,
  colnames_position = "top",
  font.size = 20,
  hjust = 0.5,
  color = FALSE,
  colnames = FALSE
) + ylim(-5, 1400) +
  xlim(-1, 55) +
  scale_fill_viridis_c(option = "viridis", direction = -1)  +
  labs(fill = "informativity") +
  theme(
    legend.box = "horizontal",
    legend.position = "bottom",
    text = element_text(size = 55),
    legend.key.size = unit(1.6, 'cm')
  )
p3

ggsave(
  file = "output/plot_heatmap_B_I.svg",
  plot = p3,
  width = 25,
  height = 27,
  dpi = 300
)

ggsave(
  file = "output/plot_heatmap_B_I.pdf",
  plot = p3,
  width = 25,
  height = 27,
  dpi = 300
)

ggsave(
  file = "output/plot_heatmap_B_I.jpeg",
  plot = p3,
  width = 25,
  height = 27,
  dpi = 300
)
