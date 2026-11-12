library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggpubr)

#Read data
butterflies <- read.csv("butterflies.csv")

#Creating a theme I can use in both plots
My_Theme <- theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  plot.tag = element_text(size = 14, face = "bold"),
  plot.tag.position = c(0.05, 0.95))

xxddfhgfdgfn

#Plotting developmental time

p1 <- butterflies |>
  #Take the mean growth rate, SD and count for each combination of maternal and larval host plant
  group_by(MaternalHost, LarvalHost) |>
  summarise(mean_time = mean(DevelopmentTime), 
            sd_time = sd(DevelopmentTime),
            n_time = n(),
            .groups = 'drop') |>
  #plot
  ggplot(aes(x = LarvalHost, y = mean_time, fill = MaternalHost, group = MaternalHost)) + 
  geom_point(shape = 21, size = 4, colour = "black", 
             position = position_dodge(width = 0.2)) +
  geom_line(aes(colour = MaternalHost, linetype = MaternalHost), size = 1,
            position = position_dodge(width = 0.2), 
            show.legend = FALSE) +
  #add standard error as errorbars
  geom_errorbar(aes(ymin = mean_time - sd_time/sqrt(n_time), 
                    ymax = mean_time + sd_time/sqrt(n_time)), 
                width = 0.2,
                position = position_dodge(width = 0.2)) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.85)) +
  My_Theme +
  labs(
    x = "Larval host",
    y = "Developmental time (days)",
    fill = "Maternal host",
    tag = "A"
  ) +
  scale_fill_manual(values = c("skyblue", "#882255")) +
  scale_colour_manual(values = c("skyblue", "#882255")) +
  scale_y_continuous(breaks = seq(20, 34, by = 2))



#Plotting growth rate

p2 <- butterflies |>
  #Take the mean growth rate, SD and count for each combination of maternal and larval host plant
  group_by(MaternalHost, LarvalHost) |>
  summarise(mean_growth = mean(GrowthRate), 
            sd_growth = sd(GrowthRate),
            n_growth = n(),
            .groups = 'drop') |>
  #plot
  ggplot(aes(x = LarvalHost, y = mean_growth, fill = MaternalHost, group = MaternalHost)) + 
    geom_point(shape = 21, size = 4, colour = "black", 
               position = position_dodge(width = 0.2)) +
    geom_line(aes(colour = MaternalHost, linetype = MaternalHost), size = 1,
              position = position_dodge(width = 0.2), 
              show.legend = FALSE) +
    #add standard error as errorbars
    geom_errorbar(aes(ymin = mean_growth - sd_growth/sqrt(n_growth), 
                      ymax = mean_growth + sd_growth/sqrt(n_growth)), 
                  width = 0.2,
                  position = position_dodge(width = 0.2)) +
    theme_classic() +
    theme(legend.position = c(0.8, 0.85)) +
    My_Theme +
    labs(
      x = "Larval host",
      y = "Growth rate (mg/day)",
      fill = "Maternal host",
      tag = "B"
    ) +
    scale_fill_manual(values = c("skyblue", "#882255")) +
    scale_colour_manual(values = c("skyblue", "#882255"))





#Arrange both plots in same window, save the plot as a png
arrange <- ggarrange(p1, p2, ncol = 2, nrow = 1)
ggsave("Butterflies.png", arrange, width = 12, height = 5)  



