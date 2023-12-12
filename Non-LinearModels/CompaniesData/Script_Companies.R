#MULTIPLE REGRESSION ANALYSIS
#algorithm
#Linear model
#stepwise
#shapiro (residuals)
#box-cox

#Isabela Pereira Lima Dias


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

# to english
load(file="companies.RData")

companies <-empresas
rm(empresas)


colnames(companies)[1] <- "company"
colnames(companies)[2] <- "return" 
colnames(companies)[3] <- "disclosure"
colnames(companies)[4] <- "debt"
colnames(companies)[5] <- "assets"
colnames(companies)[6] <- "liquidity"
summary(companies)

companies %>%
  correlation(method = "pearson") %>%
  plot()

chart.Correlation((companies[2:6]),histogram = TRUE)

pairs.panels(companies[2:6],
             smooth = TRUE,
             lm = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "pearson",
             pch = 1,
             cor = TRUE,
             hist.col = "lightblue",
             breaks = 12,
             stars = TRUE,       
             ci = TRUE, alpha = 0.05)
library(metan)
companies %>%
  corr_plot(return, disclosure, debt, assets, liquidity,
            shape.point = 21,
            col.point = "black",
            fill.point = "lightblue3",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#840154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#840154FF",
            pan.spacing = 0,
            lab.position = "bl")

companies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
#model
companies_model <- lm(formula= return ~. -company,data=companies)
summary(companies_model)

#Stepwise Procedure 
#optimal betas
k <- qchisq(p=0.05,df=1,lower.tail = F)
#best configuration between betas (only statistically significant betas)
step_companies <- step(companies_model, k = k)
round(pchisq(k,df=1,lower.tail = F),7)
summary(step_companies)

export_summs(step_companies, scale=F, digits=5)

confint(step_companies,level=0.95)
plot_summs(step_companies,colors = "#440184")
# standardize variables liquidy and assets
plot_summs(step_companies,scale=TRUE,colors = "#440184")

plot_summs(step_companies, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440184")

plot_summs(companies_model, step_companies, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("lightblue3", "#440154FF"))
#SHAPIRO-FRANCIA test
sf.test(step_companies$residuals)

companies %>%
  mutate(residuals = step_companies$residuals) %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Residuals",
       y = "Frequency") + 
  theme_bw()

companies %>%
  mutate(residuals = step_companies$residuals) %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_companies$residuals),
                            sd = sd(step_companies$residuals)),
                size = 2, color = "lightblue3") +
  scale_color_manual(values = "lightblue1") +
  labs(x = "Residuals",
       y = "Frequency") +
  theme_bw()
#box-cox tranformation
lambda_bc <- powerTransform(companies$return)
lambda_bc
companies$returnbc <- (((companies$return ^lambda_bc$lambda)-1)/lambda_bc$lambda)

bc_model <- lm(formula= returnbc ~. -company -return ,data=companies)
summary(bc_model)

step_bc_model <- step(bc_model, k=k)
summary(step_bc_model)
sf.test(step_bc_model$residuals)

companies %>%
  mutate(residuals = step_bc_model$residuals) %>%
  ggplot(aes(x = residuals)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 fill = "#287D8EFF",
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_model$residuals),
                            sd = sd(step_bc_model$residuals)),
                size = 2, color = "deeppink4") +
  scale_color_manual(values = "deeppink3") +
  labs(x = "Residuals",
       y = "Frequency") +
  theme_bw()


#Summary of the models


export_summs(step_companies, step_bc_model,
             model.names = c("Linear Model ","Box-Cox Model "),
             scale = F, digits = 6)

confint(step_bc_model, level = 0.95) #  5%

#Plots 
plot_summs(step_bc_model, colors = "deeppink4") 

plot_summs(step_bc_model, scale = TRUE, colors = "deeppink4")

plot_summs(step_bc_model, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "deeppink4")


plot_summs(step_companies, step_bc_model, scale = T, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("deeppink4", "lightblue2"))

predict(object = step_bc_model, 
        data.frame(disclosure = 50,
                   liquidity = 14,
                   assets = 4000),
        interval = "confidence", level=0.95)
(((3.702015 * -0.02256414) + 1)) ^ (1 / -0.02256414)


companies$yhat_step_companies <- step_companies$fitted.values
companies$yhat_step_bc_model <- (((step_bc_model$fitted.values*(lambda_bc$lambda))+
                                    1))^(1/(lambda_bc$lambda))
companies %>%
  select(company, return, yhat_step_companies, yhat_step_bc_model) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)


companies %>%
  ggplot() +
  geom_smooth(aes(x = return, y = yhat_step_companies, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = return, y = yhat_step_companies),
             color = "deeppink3", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = return, y = yhat_step_bc_model, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = return, y = yhat_step_bc_model),
             color = "lightblue2", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = return, y = return), method = "lm", formula = y ~ x,
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Models:", 
                     values = c("lightblue2", "deeppink3")) +
  labs(x = "Return", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

