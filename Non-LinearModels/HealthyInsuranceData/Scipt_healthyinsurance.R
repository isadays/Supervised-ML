set_packages <-  c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
                   "splines","reshape2","PerformanceAnalytics","correlation","see",
                   "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
                   "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
                   "equatiomatic","metan")
#install.packages("metan")

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
load(file = "healthyinsurance.Rdata")

# data -  preliminary avaliation
glimpse(planosaude)
summary(planosaude)
levels(factor(planosaude$plano))
# absolute frequency
table(planosaude$plano)

chart.Correlation((planosaude[2:5]), histogram = TRUE)

# dummy variables
healthy_insurance_dummy <- dummy_columns(.data = planosaude,
                                         select_columns = "plano",
                                         remove_selected_columns = T,
                                         remove_most_frequent_dummy = T
)
healthy_insurance_dummy %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)

#Multiple linear regression
model_healthy_insurance <- lm(despmed ~. -id ,healthy_insurance_dummy)
summary(model_healthy_insurance)
k <- qchisq(p=0.05,df=1,lower.tail = F)
#stepwise
step_healthyinsurance <- step(model_healthy_insurance,k=k )
summary(step_healthyinsurance)
#Shapiro-Francio test

sf.test(step_healthyinsurance$residuals)
healthy_insurance_dummy %>%
  ggplot() +
  geom_density(aes(x = step_healthyinsurance$residuals), fill = "lightblue") +
  labs(x = "Residuals of Stepwise",
       y = "Density") +
  theme_bw()
#Heteroscedasticity test
ols_test_breusch_pagan(step_healthyinsurance)

# we can add fitted values and residuals of the stepwise model into the dataset
healthy_insurance_dummy$fitted_step <- step_healthyinsurance$fitted.values
healthy_insurance_dummy$residuals_step <- step_healthyinsurance$residuals


healthy_insurance_dummy %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y= residuals_step), fill = "lightblue") +
  labs(x = "Residuals of Stepwise",
       y = "Density") +
  theme_bw()

#Box-cox transf.
lambda_bc <- powerTransform(planosaude$despmed)
healthy_insurance_dummy$bcdespmed <- (((planosaude$despmed^lambda_bc$lambda)-1)/lambda_bc$lambda)


# new model with dummies
model_bc_hi <- lm(formula = bcdespmed ~ . -id -despmed -fitted_step
                           -residuals_step, 
                           data = healthy_insurance_dummy)
summary(model_bc_hi)

#stepwise
step_bc_hi <- step(model_bc_hi,k=k)
#Shapiro-Francia test
sf.test(step_bc_hi$residuals)


healthy_insurance_dummy %>%
  mutate(residuals = step_bc_hi$residuals) %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_hi$residuals),
                            sd = sd(step_bc_hi$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Residuals",
       y = "Frequency") +
  theme_bw()

healthy_insurance_dummy %>%
  ggplot() +
  geom_density(aes(x = step_bc_hi$residuals), fill = "#440154FF") +
  labs(x = "Residuals",
       y = "Density") +
  theme_bw()
ols_test_breusch_pagan(step_bc_hi)


healthy_insurance_dummy$fitted_step_new <- step_bc_hi$fitted.values
healthy_insurance_dummy$residuals_step_new<- step_bc_hi$residuals


healthy_insurance_dummy %>%
  ggplot() +
  geom_point(aes(x = fitted_step_new, y = residuals_step_new),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values",
       y = "Residuals") +
  theme_bw()
