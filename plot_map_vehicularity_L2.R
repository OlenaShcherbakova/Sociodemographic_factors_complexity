#plot a map of vehicularity

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

table(metrics_joined$Vehicularity)

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

combination <- combination %>% 
  dplyr::mutate(Vehicularity = as.factor(Vehicularity))

colors_binary <- c("#1abc9c", "#e67e22")

#plotting informativity map
i <-
  basemap + geom_point(
    data = combination,
    aes(x = Longitude, y = Latitude, 
        colour = Vehicularity, fill = Vehicularity, size=L2_prop),
    pch = 21,
    #size = 0.8, #1.1
    alpha = 1,
    position = position_jitter(
      width = 1,
      height = 1,
      seed = 123
    )
  ) +
  scale_fill_manual(name = "Vehicularity", 
                    values = c("#1abc9c", "#e67e22")) +
  scale_color_manual(name = "Vehicularity",
                     values = colors_binary
  ) +
  scale_size_continuous(range = c(0.3, 3)) +
    # values = c(0.3, 0.6, 0.9, 1.2),
    #                 breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme(
    plot.title = element_text(size = 15),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.5, units = "cm"),
    legend.direction = "horizontal",
    legend.position = c(.4, .07)
  ) +
  #guides(fill = guide_colourbar()) +
  labs(title = "Vehicularity", fill = "Value")
i

ggsave(
  file = "output_reanalysis/map_Vehicularity_L2.jpeg",
  plot = i,
  width = 5,
  height = 3.5, 
  dpi=300
)

i <- i + geom_text_repel(
  aes(x = Longitude, y = Latitude, label = Name),
  #size=5,
  data  = subset(combination, (
    L2_prop > 0.5)),
  size          = 3,
  box.padding   = 0.6,
  max.overlaps = Inf,
  point.padding = 0.1,
  segment.color = "gray10",  # Adjust the segment color here
  segment.size = 0.5  # Adjust the segment thickness here
)

ggsave(
  file = "output_reanalysis/map_Vehicularity_L2_labelled.jpeg",
  plot = i,
  width = 5,
  height = 3.5, 
  dpi=300
)
