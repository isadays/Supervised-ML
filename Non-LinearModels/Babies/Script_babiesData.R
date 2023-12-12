#Nonlinear Models and Box-Cox Transformations


#Isabela Pereira Lima Dias

set_packages <-  c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
                   "splines","reshape2","PerformanceAnalytics","correlation","see",
                   "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
                   "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
                   "equatiomatic")

options(rgl.debug = TRUE)
#remotes::install_github("abalgo/ggraph", force = TRUE) Force installation
#install.packages("ggraph", type = "source")

if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  install_packages <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

load(file="babies.RData")

babies <-bebes
rm(bebes)


colnames(babies)[1] <- "length"
colnames(babies)[2] <- "age" #

summary(babies)

ggplotly(
  babies %>%
    ggplot() +
    geom_point(aes(x= age, y=length),
               color="grey20", alpha=0.6,size=2)+
    labs(x= "Age (weeks)", y="Length (cm)")+
    theme_bw())


ggplotly(
  babies %>%
    ggplot(aes(x= age, y=length, label= emoji("baby")))+
             geom_text(family="EmojiOne", size=5, color="black") +
    labs(x= "Age (weeks)", y="Length (cm)")+
    theme_bw()
    )
#Scatter graph with non-linear and linear fits


ggplotly(
  babies %>% 
    ggplot() +
    geom_point(aes(x = age, y = length),
               color = "grey20", alpha = 0.6, size = 2) +
    geom_smooth(aes(x = age, y = length),
                method = "lm", formula = y ~ x,
                color = "#ADE525FF", se = F) +
    geom_smooth(aes(x = age, y = length),
                method = "loess", formula = y ~ x,
                color = "#710458FF", se = F) +
    labs(x= "Age (weeks)", 
         y="Length (cm)") +
    theme_bw()
)

# LINEAR MODEL 
linear_model <- lm(formula = length  ~ age,data = babies)
summary(linear_model)

#"Shapiro-Francia normality test"-> test of adherence  
#is a statistical test for the normality of a population, based on sample data
sf.test(linear_model$residuals) #package 'nortest', function 'sf.test'

#histogram 
babies %>%
  mutate(residuals = linear_model$residuals) %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(linear_model$residuals),
                            sd = sd(linear_model$residuals)),
                aes(color = "Normal curve"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Residuals",
       y = "Frequency") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

babies %>%
  ggplot(aes(x = linear_model$fitted.values, y = linear_model$residuals)) +
  geom_point(color = "#FDE725FF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  geom_xsidedensity(aes(y = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  geom_ysidedensity(aes(x = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)

#Box-Cox Y* = (Y^{lambda} - 1)/lambda = alpha + beta_1 x_1 + .... beta_k x_k + u_i , 
#lambda of Box cox 
#lambda = 1 - linear , lambda = 2 quadratic equation and etc...
lambda_bc <- powerTransform(babies$length)
lambda_bc

#inserting into the dataset
babies$length_bc <- (((babies$length^lambda_bc$lambda)-1)/lambda_bc$lambda)
bc_model <- lm(formula= length_bc ~ age,data=babies)

#length^2,65-1/2,65 = alpha + beta_age * age <- our model 
summary(bc_model)
#COMPARING
export_summs(linear_model,bc_model,
             model.names = c("Linear Model", "Cox-Box Model"),
             scale=F, digits=4)

data.frame("R2 Linear" = round(summary(linear_model)$r.squared,4),
           "R2 Box-Cox" = round(summary(bc_model)$r.squared,4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F,
                font_size = 14  )

#SF test
sf.test(bc_model$residuals)

babies %>%
  mutate(residuals = bc_model$residuals) %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(bc_model$residuals),
                            sd = sd(bc_model$residuals)),
                aes(color = "Normal Curve"),
                size = 2) +
  scale_color_manual("Legend:",
                     values = "#440154FF") +
  labs(x = "Residuals",
       y = "Frequency") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

babies %>%
  ggplot(aes(x = bc_model$fitted.values, y = bc_model$residuals)) +
  geom_point(color = "#440154FF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  geom_xsidedensity(aes(y = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  geom_ysidedensity(aes(x = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)

#Predicting with the two different models
#linear model
predict(object = linear_model,
        data.frame(age=52), 
        interval = "confidence", level=0.95)
#non-linear model 
predict(object = bc_model,
        data.frame(age=52),
        interval = "confidence", level=0.95)
(((54251.12 * 2.659051) + 1)) ^ (1 / 2.659051)
 
#inserting
babies$yhat_linear <- linear_model$fitted.values
babies$yhat_bc_model <- (((bc_model$fitted.values*(lambda_bc$lambda))+
                            1))^(1/(lambda_bc$lambda))

babies %>%
  select(age, length, yhat_linear, yhat_bc_model) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

babies %>%
  ggplot() +
  geom_smooth(aes(x = length, y = yhat_linear, color = "Linear"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_point(aes(x = length, y = yhat_linear),
             color = "#FDE725FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = length, y = yhat_bc_model, color = "Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_point(aes(x = length, y = yhat_bc_model),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = length, y = length), method = "lm", 
              color = "gray30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Models:", 
                     values = c("#440154FF", "#FDE725FF")) +
  labs(x = "Length", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")
