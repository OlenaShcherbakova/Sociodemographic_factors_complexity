effs_I <- read.csv("output_tables/ effects Informativity_social_models .csv")
effs_I$variable <- "informativity"

effs_B <- read.csv("output_tables/ effects Boundness_social_models .csv")
effs_B$variable <- "fusion"

effs_1 <- as.data.frame(rbind(effs_B, effs_I))

effs_1$control <- "yes"

effs_I <- read.csv("output_tables/ effects Informativity_social_only_models .csv")
effs_I$variable <- "informativity"

effs_B <- read.csv("output_tables/ effects Boundness_social_only_models .csv")
effs_B$variable <- "fusion"

effs_2 <- as.data.frame(rbind(effs_B, effs_I))
effs_2$control <- "no"

effs <- as.data.frame(rbind(effs_1, effs_2))

effs_main <- effs %>%
  rename(lower=2,
         upper = 4,
         mean = 3) %>% 
  filter(!grepl("nonlinear", model)) %>%
  filter(!grepl("SD", effect)) %>%
  filter(!grepl("Intercept", effect)) 


#removing "(linear)" part within the model column
effs_main$model <- gsub("(\\s*\\(\\w+\\))", "", effs_main$model)

#adding "combined" after L1/L1 proportion where applicable
effs_main$effect <- ifelse(grepl("L1 speakers\\+", effs_main$model), paste(effs_main$effect, "(combined)"), effs_main$effect)

effs_main <- effs_main %>%
  mutate(effect=recode(effect,
                       "L2 proportion (combined)" = "L2 (combined)",
                       "L1*L2 proportion" = "L1*L2",
                       "L2 proportion" = "L2"))

effs_main_plot_bw <- effs_main %>%
  as.data.frame() %>%
  mutate_if(is.character, as.factor) %>%
  mutate(effect = factor(effect, levels=c("Education", "Neighbours", "Official status", "L1*L2", "L2 (combined)", "L1 (combined)", "L2", "L1"))) %>%
  mutate(importance = case_when((lower > 0 & upper > 0) | (lower < 0 & upper < 0)  ~ "1", 
                                (lower < 0 & upper > 0) | 
                                (lower < 0 & upper == 0) | (lower == 0 & upper > 0) ~ "0")) %>%
  mutate(importance = as.factor(importance)) %>%
  mutate(importance = factor(importance, levels=c("0", "1"), ordered = TRUE)) %>%
  mutate(control = factor(control, levels=c("yes", "no"), ordered = TRUE)) %>%
  ggplot(.,
         aes(y = effect,
             x = mean, color = as.factor(importance), linetype=control)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width=0.6, size=3, position = position_dodge(w = 0.8)) + 
  geom_point(size = 10, position=position_dodge(w = 0.8)) +
  geom_line(position=position_dodge(w = 0.8)) +
  geom_vline(aes(xintercept = 0),lty = 2) + 
  scale_color_manual(values=c("black", "red3")) +
  #geom_line(linetype = 2) +
  #scale_linetype_manual(values=c("yes"="solid","no"="dashed")) +
  ylab("") + 
  xlab("Estimate: 95% credible interval") + 
  theme_classic() + facet_grid(. ~ variable, scales="free_x", space="free") +
  theme(axis.text=element_text(size=65),
        legend.text=element_text(size=65),
        axis.title=element_text(size=65),
        legend.title=element_text(size=65),
        strip.text.x = element_text(size = 65),
        legend.spacing.y = unit(2.7, 'cm'), 
        legend.key.size = unit(2, 'cm'),
        legend.direction="horizontal",
        legend.position="bottom",
        panel.spacing.x = unit(15, "mm")) +
  guides(color="none")

effs_main_plot_bw
ggsave(file="output/effects_plot.svg", plot=effs_main_plot_bw, height = 22, width = 36)
