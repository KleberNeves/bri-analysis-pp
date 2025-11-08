# Código do Pedro

# Output path -------------------------------------------------------------

# generate output dir path named data
output.dir <- file.path(paste0("output/", results_path, "/survey_processed_data"))

# if source output dir does not exist, create it
if (!dir.exists(output.dir)) {
  dir.create(output.dir)
} else{
  print("Output folder already exists")
}

# Definitions -------------------------------------------------------------

factor_summary <- function(object, maxsum = 20) {
  object %>% mutate_if(is.character, as.factor) %>% summary(maxsum = maxsum)
}

theme_Publication_bri <- function(base_size=11, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(fill = "white", colour = NA),
            plot.background = element_rect(fill = "white", colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(size = 11), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour=NA, fill="white"),
            strip.text = element_text()
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# Load data ---------------------------------------------------------------

summarized_surveys_combined <- read.csv2("other-data/survey-data/BRI_summarized_surveys_combined.csv")

study_code_correspondence <- read.csv2("other-data/survey-data/BRI Study number and experimental code correspondence.csv")

summarized_surveys_combined_f <- summarized_surveys_combined %>% filter(!Excluded)
summarized_surveys_combined %>% factor_summary()

summarized_surveys_combined_f %>% factor_summary()
summarized_surveys_combined_f %>% filter(Technique == "EPM") %>% factor_summary()
summarized_surveys_combined_f %>% filter(Technique == "PCR") %>% factor_summary()
summarized_surveys_combined_f %>% filter(Technique == "MTT") %>% factor_summary()


get_survey_summary <- function(survey_df,
                               columns = c("Replication_probability",
                                           "Replication_rel_effect_size",
                                           "Difficulty"),
                               Technique = "All") {
  
  exp_summary <- do.call(cbind, lapply(survey_df %>%
                                         select(all_of(columns)), summary)) %>%
    as.data.frame() %>%
    mutate_all(as.numeric) %>%
    round(2) %>%
    rownames_to_column("Summary") %>%
    mutate(Technique = Technique, n = nrow(survey_df))
  
  exp_summary
  
}

survey_summary_info <- get_survey_summary(summarized_surveys_combined_f) %>%
  rbind(get_survey_summary(summarized_surveys_combined_f %>% filter(Technique == "EPM"), Technique = "EPM"))%>%
  rbind(get_survey_summary(summarized_surveys_combined_f %>% filter(Technique == "PCR"), Technique = "PCR"))%>%
  rbind(get_survey_summary(summarized_surveys_combined_f %>% filter(Technique == "MTT"), Technique = "MTT"))

survey_summary_info %>% write_csv2("survey_summary_info.csv")

# Plot --------------------------------------------------------------------

plot_height = 4
plot_width = 5

color_epm <- "#6e86bf"
color_mtt <- "#61c89f"
color_pcr <- "#e48f4e"

survey_replication_probability_boxplots <- summarized_surveys_combined_f %>%
  ggplot(aes(x = Technique, y = Replication_probability, col = Technique)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() + theme_Publication_bri() +
  labs(y = "Replication Probability") +
  expand_limits(y = 0) +
  guides(col = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_color_manual(values = c(color_epm, color_mtt, color_pcr))

ggsave(file.path(output.dir, "Survey_replication_probability_boxplots.png"), plot = survey_replication_probability_boxplots, height = plot_height, width = plot_width, bg = "white")

survey_replication_ES_boxplots <- summarized_surveys_combined_f %>%
  ggplot(aes(x = Technique, y = Replication_rel_effect_size, col = Technique)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() + theme_Publication_bri() +
  labs(y = "Replication Effect Size") +
  expand_limits(y = 0) +
  guides(col = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_color_manual(values = c(color_epm, color_mtt, color_pcr)) +
  scale_y_continuous(breaks = c(seq(0, 80, by = 20)))

ggsave(file.path(output.dir, "Survey_replication_ES_boxplots.png"), plot = survey_replication_ES_boxplots, height = plot_height, width = plot_width, bg = "white")

survey_replication_difficulty_boxplots <- summarized_surveys_combined_f %>%
  ggplot(aes(x = Technique, y = Difficulty, col = Technique)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() + theme_Publication_bri() +
  expand_limits(y = 0)  +
  guides(col = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10))) +
  scale_color_manual(values = c(color_epm, color_mtt, color_pcr))

ggsave(file.path(output.dir, "Survey_replication_difficulty_boxplots.png"), plot = survey_replication_difficulty_boxplots, height = plot_height, width = plot_width, bg = "white")

plot_height = 5
plot_width = 5

survey_replication_probability_ES_scatterplot <- summarized_surveys_combined_f %>%
  ggplot(aes(x = Replication_probability, y = Replication_rel_effect_size)) +
  geom_point(aes(col = Technique)) + theme_Publication_bri() +
  labs(x = "Replication Probability", y = "Replication Effect Size") +
  geom_smooth(method = "lm", linetype = "dashed", col = "gray", se = FALSE) + stat_cor(size = 4, label.x.npc = 0.02, label.y.npc = 0.98) +
  labs(col = "")  +
  scale_color_manual(values = c(color_epm, color_mtt, color_pcr)) +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(breaks = c(seq(0, 80, by = 20))) +
  theme(legend.text = element_text(size=10, margin = margin(r = 7, l = 2, unit = "pt")),
        legend.spacing.x = unit(1, 'cm'),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave(file.path(output.dir, "Survey_replication_probability_ES_scatterplot.png"), plot = survey_replication_probability_ES_scatterplot, height = plot_height, width = plot_width, bg = "white")


survey_replication_difficulty_probability_scatterplot <- summarized_surveys_combined_f %>%
  ggplot(aes(x = Difficulty, y = Replication_probability)) +
  geom_point(aes(col = Technique)) + theme_Publication_bri() +
  labs(x = "Difficulty", y = "Replication Probability") +
  geom_smooth(method = "lm", linetype = "dashed", col = "gray", se = FALSE) + stat_cor(size = 4, label.x.npc = 0.02, label.y.npc = 0.98) +
  labs(col = "")  +
  scale_color_manual(values = c(color_epm, color_mtt, color_pcr)) +
  expand_limits(x = 0, y = 0)  +
  theme(legend.text = element_text(size=10, margin = margin(r = 7, l = 2, unit = "pt")),
        legend.spacing.x = unit(1, 'cm'),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave(file.path(output.dir, "Survey_replication_difficulty_probability_scatterplot.png"), plot = survey_replication_difficulty_probability_scatterplot, height = plot_height, width = plot_width, bg = "white")

survey_replication_difficulty_ES_scatterplot <- summarized_surveys_combined_f %>%
  ggplot(aes(x = Difficulty , y = Replication_rel_effect_size)) +
  geom_point(aes(col = Technique)) + theme_Publication_bri() +
  labs(x = "Difficulty", y = "Replication Effect Size") +
  geom_smooth(method = "lm", linetype = "dashed", col = "gray", se = FALSE) + stat_cor(size = 4, label.x.npc = 0.02, label.y.npc = 0.98) +
  labs(col = "") +
  scale_color_manual(values = c(color_epm, color_mtt, color_pcr)) +
  expand_limits(x = 0, y = 0)  +
  scale_y_continuous(breaks = c(seq(0, 80, by = 20))) +
  theme(legend.text = element_text(size=10, margin = margin(r = 7, l = 2, unit = "pt")),
        legend.spacing.x = unit(1, 'cm'),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave(file.path(output.dir, "Survey_replication_difficulty_ES_scatterplot.png"), plot = survey_replication_difficulty_ES_scatterplot, height = plot_height, width = plot_width, bg = "white")


plot_survey <- plot_grid(plotlist = list(survey_replication_probability_boxplots,
               survey_replication_ES_boxplots,
               survey_replication_difficulty_boxplots,
               survey_replication_probability_ES_scatterplot,
               survey_replication_difficulty_probability_scatterplot, 
               survey_replication_difficulty_ES_scatterplot),
          labels = "AUTO",
          label_size = 12,
          align = "v")


ggsave(file.path(output.dir, "Figure S7.png"), plot = plot_survey, height = 9, width = 15, bg = "white"
       )


