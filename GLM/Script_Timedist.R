#Supervised ML 

#SIMPLE AND MULTIPLE REGRESSION ANALYSIS

#Isabela Pereira Lima Dias

set_packages <-  c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
                   "splines","reshape2","PerformanceAnalytics","correlation","see",
                   "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
                   "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
                   "equatiomatic")

options(rgl.debug = TRUE)

if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  install_packages <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

load(file = "timedist.rdata")

time_distance <- tempodist
rm(tempodist)

colnames(time_distance)[1] <- "time"
colnames(time_distance)[2] <- "distance"

time_distance %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

glimpse(time_distance)


ggplotly(
  ggplot(time_distance, aes(x = distance, y = time)) +
    geom_point(color = "deeppink4", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    xlim(0,max(time_distance$distance))+
           ylim(0,max(time_distance$time))+
    labs(x = "Distance",
         y = "Time",
         title = paste("R²:",
                       round(((cor(time_distance$time, time_distance$distance))^2),4))) +
    scale_color_manual("Legend:",
                       values = "grey50") +
    theme_classic()
)

time_distance_model <- lm(formula = time ~ distance, data = time_distance)
summary(time_distance_model)

anova(time_distance_model)

summ(time_distance_model, confint = T, digits = 4, ci.width=.95)
export_summs(time_distance_model,scale=F, digits=4)

remotes::install_github("datalorax/equatiomatic", force = TRUE)


extract_eq(time_distance_model, use_coefs=T) %>% 
  kable() %>%
  kable_styling(bootstrap_options="striped", 
                full_width = F,
                font_size=14)

time_distance$yhat <- time_distance_model$fitted.values
time_distance$error <- time_distance_model$residuals
time_distance %>%
  select(time,distance, yhat,error) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)
ggplotly(
  ggplot(time_distance, aes(x = distance, y = time)) +
    geom_point(color = "deeppink4", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    geom_hline(yintercept = 30, color = "grey50", size = .5) +
    geom_segment(aes(color = "Yhat - Yaverage", x = distance, xend = distance,
                     y = yhat, yend = mean(time)), size = 0.7, linetype = 2) +
    geom_segment(aes(color = "Error = Y - Yhat", x = distance, xend = distance,
                     y = time, yend = yhat), size = 0.7, linetype = 3) +
    labs(x = "Distance",
         y = "Time") +
    scale_color_manual("Legend:",
                       values = c("#55C667FF", "grey50", "deeppink2")) +
    theme_classic()
)
