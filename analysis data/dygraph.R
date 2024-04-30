library(ggplot2)
library(ggthemes)


# Format 3: Several variables for each date
jobsAFAM1 <- data.frame(
  data_date = data$year,
  Percent.Change = data$India
)

jobsAFAM2 <- data.frame(
  data_date = data$year,
  Percent.Change = data$Kerala
)

p <- ggplot() + 
  geom_line(data = jobsAFAM1, aes(x = data_date, y = Percent.Change, color = "jobsAFAM1"), color = "red") +
  geom_point(data = jobsAFAM1, aes(x = data_date, y = Percent.Change, color = "jobsAFAM1"), shape = 21, fill = "#69b3a2", size = 3) +
  geom_smooth(data = jobsAFAM1, aes(x = data_date, y = Percent.Change, color = "jobsAFAM1"), 
              method = 'lm', formula = y ~ x, linetype = "dotted", color = 'grey50', se = FALSE) +  # Specify linetype and color
   geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change, color = "jobsAFAM2"), color = "blue") +
  geom_point(data = jobsAFAM2, aes(x = data_date, y = Percent.Change, color = "jobsAFAM2"), shape = 21, fill = "#018c65", size = 3) +
  geom_smooth(data = jobsAFAM2, aes(x = data_date, y = Percent.Change, color = "jobsAFAM1"), 
              method = 'lm', formula = y ~ x, linetype = "dotted", color = 'grey50', se = FALSE) +  # Specify linetype and color
  xlab('Year') +
  ylab('Production (’000 MT)') +
  scale_y_continuous("Production (’000 MT)",limits=c(0,80)) +
  theme_bw()
ggsave("ppro.png", width = 5, height =5)
