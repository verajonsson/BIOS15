

library(ggplot2)
library(tidyverse)
library(gridExtra)

#Creating three separate dataframes, holding the effort constant at mean, the forest cover 
#as a sequence from min to max and holding MAP constant at three different values, 
#i.e.mean, mean + SD and mean - SD

df_mean <- data.frame(effort = mean(dat$effort),
                      forest. = seq(min(dat$forest), max(dat$forest), length.out = 178),
                      MAP = mean(dat$MAP))

plus_SD <- data.frame(effort = mean(dat$effort),
                      forest. = seq(min(dat$forest), max(dat$forest), length.out = 178),
                      MAP = mean(dat$MAP) + sd(dat$MAP))

minus_SD <- data.frame(effort = mean(dat$effort),
                       forest. = seq(min(dat$forest), max(dat$forest), length.out = 178),
                       MAP = mean(dat$MAP) - sd(dat$MAP))


#Predicting the corresponding y values for the three different dataframes, these 
#will be used for plotting later on

y_hat <- predict(m1, newdata = df_mean, type = "response", se.fit = T)

y_hat_plus <- predict(m1, newdata = plus_SD, type = "response", se.fit = T)

y_hat_minus <- predict(m1, newdata = minus_SD, type = "response", se.fit = T)



#Gathering all the predicted y-values, i.e. Eulaema abundance as well as the
#true abundance and the MAT sequence

plot_data <- data.frame(forest = df_mean$forest, y_mean_MAP = y_hat$fit,
                        y_plusSD_MAP = y_hat_plus$fit,
                        y_minusSD_MAP = y_hat_minus$fit,
                        true_count = dat$Eulaema_nigrita,
                        true_x = dat$forest.) 



#Using the same theme as I used in the last write up
My_Theme <- theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  plot.tag = element_text(size = 14, face = "bold"),
  plot.tag.position = c(0.05, 0.95))



#Plotting the data
plot_data |>
  #pivot longer to put all the predicted y-values in the same column
  pivot_longer(cols = c(y_mean_MAP, y_plusSD_MAP, y_minusSD_MAP),
               names_to = "MAP",
               values_to = "bee_count") |>
  #changing the legend names
  mutate( 
    MAP = factor(MAP,
                      levels = c("y_mean_MAP", "y_plusSD_MAP", "y_minusSD_MAP"),
                      labels = c("MAP = Mean", "MAP = Mean + SD", "MAP = Mean - SD"))
  )|>
  ggplot() +
    geom_line(aes(x = forest, y = bee_count, colour = MAP, linetype = MAP), lwd = 1) + 
    geom_point(aes(x = true_x, y = true_count), pch = 21, size = 3, alpha = 0.2) +
    theme_bw() +
    labs(x = "Proportion forest cover",
        y = substitute(paste("Abundance ", italic("Eulaema nigrita"))),
        colour = NULL,
        linetype = NULL
    ) +
    scale_colour_manual(values = c("#2c95ff","#004392", "#b8dbff")) +
    My_Theme


#Save the graph
ggsave("Bees.png", width = 8, height = 5)  
  
  
  
