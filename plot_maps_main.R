#plotting maps
source("set_up_inla.R")

metrics_joined <- metrics_joined %>%
  filter(!is.na(L1_log10_st)) %>%
  rename(L1_log_st = L1_log10_st) %>%
  mutate(L1_copy = L1_log_st) %>%
  # filter(!is.na(L2_prop)) %>%
  # dplyr::mutate(L2_prop  = scale(L2_prop)[, 1]) %>%
  # mutate(L2_copy = L2_prop) %>%
  filter(!is.na(neighboring_languages_st)) %>%
  filter(!is.na(Official)) %>%
  filter(!is.na(Education)) %>%
  filter(!is.na(boundness_st)) %>%
  filter(!is.na(informativity_st))

#dropping tips not in Grambank
metrics_joined <-
  metrics_joined[metrics_joined$Language_ID %in% tree$tip.label,]
tree <- keep.tip(tree, metrics_joined$Language_ID)

#an overview of area and family coverage
tab_areas <- as.data.frame(table(metrics_joined$AUTOTYP_area)) %>%
  arrange(desc(Freq))

tab_families <- as.data.frame(table(metrics_joined$Family_ID)) %>%
  arrange(desc(Freq))

x <-
  assert_that(all(tree$tip.label %in% metrics_joined$Language_ID), msg = "The data and phylogeny taxa do not match")

world <-
  map_data(
    'world',
    wrap = c(-25, 335),
    ylim = c(-56, 80),
    margin = T
  )

lakes <-
  map_data(
    "lakes",
    wrap = c(-25, 335),
    col = "white",
    border = "gray",
    ylim = c(-55, 65),
    margin = T
  )

#shifting the longlat of the dataframe to match the pacific centered map
combination <- metrics_joined %>%
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))

#duplicates?

combination %>%
  group_by(Language_ID) %>%
  mutate(dupe = n() > 1) -> combination_dup
dupes <- combination_dup[combination_dup$dupe == "TRUE", ]


#Basemap
basemap <- ggplot(combination) +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    colour = "gray87",
    fill = "gray87",
    size = 0.5
  ) +
  geom_polygon(
    data = lakes,
    aes(x = long, y = lat, group = group),
    colour = "gray87",
    fill = "white",
    size = 0.3
  )  +
  theme(
    panel.grid.major = element_blank(),
    #all of these lines are just removing default things like grid lines, axises etc
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )   +
  coord_map(projection = "vandergrinten", ylim = c(-56, 67))

#plotting informativity map
i <-
  basemap + geom_point(
    data = combination,
    aes(x = Longitude, y = Latitude, colour = informativity_st, fill = informativity_st),
    pch = 21,
    size = 1.1,
    alpha = 1,
    position = position_jitter(
      width = 1,
      height = 1,
      seed = 123
    )
  ) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  scale_color_viridis_c(option = "viridis",
                        guide = "none",
                        direction = -1) +
  theme(
    plot.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.5, units = "cm"),
    legend.direction = "horizontal",
    legend.position = c(.4, .07)
  ) +
  guides(fill = guide_colourbar()) +
  labs(title = "Informativity", fill = "score")
i

ggsave(
  file = "output_reanalysis/map_informativity.svg",
  plot = i,
  width = 10,
  height = 9, 
  dpi=300
)

ggsave(
  file = "output_reanalysis/map_informativity.tiff",
  plot = i,
  width = 10,
  height = 9, 
  dpi=600
)


#plotting boundness map
b <-
  basemap + geom_point(
    data = combination,
    aes(x = Longitude, y = Latitude, colour = boundness_st, fill = boundness_st),
    pch = 21,
    size = 1.1,
    alpha = 1,
    position = position_jitter(
      width = 1,
      height = 1,
      seed = 123
    )
  ) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_color_viridis_c(option = "magma",
                        guide = "none",
                        direction = -1) +
  theme(
    plot.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.5, units = "cm"),
    legend.direction = "horizontal",
    legend.position = c(.4, .07)
  ) +
  guides(fill = guide_colourbar()) +
  labs(title = "Boundness", fill = "score")

ggsave(
  file = "output_reanalysis/map_boundness.svg",
  plot = b,
  width = 10,
  height = 9, 
  dpi=300
)

ggsave(
  file = "output_reanalysis/map_boundness.tiff",
  plot = b,
  width = 10,
  height = 9, 
  dpi=600
)



i <-
  basemap + geom_point(
    data = combination,
    aes(
      x = Longitude,
      y = Latitude,
      fill = informativity_st,
      color = informativity_st
    ),
    pch = 21,
    size = 1.1,
    alpha = 1,
    position = position_jitter(
      width = 1,
      height = 1,
      seed = 123
    )
  ) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.5, units = "cm"),
    legend.direction = "horizontal",
    legend.position = c(.4, .07)
  ) +
  guides(fill = guide_colourbar(), color = "none") +
  labs(fill = "informativity")

b <-
  basemap + geom_point(
    data = combination,
    aes(
      x = Longitude,
      y = Latitude,
      fill = boundness_st,
      color = boundness_st
    ),
    pch = 21,
    size = 1.1,
    alpha = 1,
    position = position_jitter(
      width = 1,
      height = 1,
      seed = 123
    )
  ) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  scale_color_viridis_c(option = "magma", direction = -1) +
  theme(
    text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.5, units = "cm"),
    legend.direction = "horizontal",
    legend.position = c(.4, .07)
  ) +
  guides(fill = guide_colourbar(), color = "none") +
  labs(fill = "fusion")

two_maps <- b / i

ggsave(
  file = "output_reanalysis/maps.tiff",
  plot = two_maps,
  width = 5,
  height = 7, 
  dpi=300
)

ggsave(
  file = "output_reanalysis/maps.svg",
  plot = two_maps,
  width = 5,
  height = 7, 
  dpi=300
)

ggsave(
  file = "output_reanalysis/maps.pdf",
  plot = two_maps,
  width = 5,
  height = 7, 
  dpi=300
)

ggsave(
  file = "output_reanalysis/maps.jpeg",
  plot = two_maps,
  width = 5,
  height = 7, 
  dpi=300
)



#histograms
# hist_b <- ggplot(metrics_joined, aes(x = boundness_st)) +
#   geom_histogram(color = "black", aes(fill = ..x..)) +
#   scale_fill_viridis_c(option = "magma", direction = -1) +
#   labs(x = "fusion", fill = "score") +
#   theme_classic(base_size = 22)
# 
# hist_i <- ggplot(metrics_joined, aes(x = informativity_st)) +
#   geom_histogram(color = "black", aes(fill = ..x..)) +
#   scale_fill_viridis_c(option = "viridis", direction = -1) +
#   labs(x = "informativity", fill = "score") +
#   theme_classic(base_size = 22)
# 
# ggsave(
#   file = "output/hist_boundness.svg",
#   plot = hist_b,
#   width = 7,
#   height = 5, 
#   dpi=300
# )
# ggsave(
#   file = "output/hist_informativity.svg",
#   plot = hist_i,
#   width = 7,
#   height = 5, 
#   dpi=300
# )
