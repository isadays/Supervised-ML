#Supervised ML 

#SIMPLE REGRESSION ANALYSIS

#Isabela Pereira Lima Dias

set_packages <-  c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
                   "splines","reshape2","PerformanceAnalytics","correlation","see",
                   "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
                   "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
                   "equatiomatic")

options(rgl.debug = TRUE)
#remotes::install_github("datalorax/equatiomatic", force = TRUE) Force installation


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

# Manual evaluation of R2
R2 <- (sum((time_distance$yhat-mean(time_distance$time))^2))/
  ((sum((time_distance$yhat-mean(time_distance$time))^2)) + (sum((time_distance$error)^2)))
round(R2,digits = 4)
#correlation coefficient
cor(time_distance[1:2])

# Now, we can test the auxiliary model
aux_model <- lm(formula= yhat ~distance,
                data = time_distance)
summary(aux_model)


my_plot <-
  ggplot(time_distance, aes(x = distance, y = yhat)) +
  geom_point(color = "deeppink3", size = 5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  labs(x = "Distance",
       y = "Time") +
  scale_color_manual("Legend:",
                     values = "grey50") +
  theme_cowplot()
my_plot


# GLOBAL : F-statistic- F-distribution ( Fisher-Snedecor), pf -> critical p-value qf -> critical f 

#LOCAL : P-value of t -> T -STUDENT SQRT(f) critical pt(p-value)

#Significance level (5% -> critical P-value (H1)) and confidence level (95%) (H0)


# Plot of confidence level 95%
ggplotly(
  ggplot(time_distance, aes(x = distance, y = time)) +
    geom_point(color = "deeppink3") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x,
                level = 0.95) +
    labs(x = "Distance",
         y = "Time") +
    scale_color_manual("Legend:",
                       values = "grey50") +
    theme_bw()
)

#Confidence level  (Analysing betha and alpha parameters)
confint(time_distance_model, leve=0.90) #significance level 10% OK BETHA

#Confidence level  
confint(time_distance_model, leve=0.95) #significance level 5% OK BETHA 

#Confidence level  
confint(time_distance_model, leve=0.98) #significance level 1% OK BETHA 

#Confidence level  
confint(time_distance_model, leve=0.999999) #significance level 0.001% -> contains 0


#Predictions ->  time to cover the distance of 25 km 
predict(object=time_distance_model,
        data.frame(distance=25))
#Predictions confidence level 
predict(object=time_distance_model,
        data.frame(distance=25),
        interval = "confidence", level=0.95)
#New dataset with replications 
# Function slice 
new_time_distance <- time_distance %>%
  slice(rep(1:n(), each=3))
new_time_distance_model  <- lm(formula = time ~distance,
                               data = new_time_distance)
# The aim is to make new predictions for our model with more data
summary(new_time_distance_model)

confint(new_time_distance_model, level=0.95) # significance 5%

ggplotly(
  ggplot(new_time_distance, aes(x = distance, y = time)) +
    geom_point(color = "deeppink3") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x,
                level = 0.95) +
    labs(x = "Distance",
         y = "Time") +
    scale_color_manual("Legend:",
                       values = "grey50") +
    theme_bw()
)
