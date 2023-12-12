#Supervised ML 

# MULTIPLE REGRESSION ANALYSIS

#Isabela Pereira Lima Dias

set_packages <-  c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
                   "splines","reshape2","PerformanceAnalytics","correlation","see",
                   "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
                   "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
                   "equatiomatic")

options(rgl.debug = TRUE)
#remotes::install_github("abalgo/ggraph", force = TRUE) Force installation
install.packages("ggraph", type = "source")

if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  install_packages <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

load(file="countries.RData")
countries <-paises
rm(paises)
summary(countries)

colnames(countries)[1] <- "Country"
colnames(countries)[2] <- "CPI" #- corruption index 
colnames(countries)[3] <- "Age" #of billionaires 
colnames(countries)[4] <- "Hours"# of work/ week 

# OUR MODEL IS TO PREDICT THE CPI 
# hatcpi = alpha + beta_age* age + beta_hours* hours 

#Modelling the points
scatter3d(CPI ~ Age + Hours,
          data = countries,
          surface = F,
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))

countries %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)
#Using the package correlation 
countries %>%
  correlation(method="pearson") %>%
  plot()

chart.Correlation((countries[,2:4]),histogram = TRUE)

countries_model <-lm(formula = CPI ~ . - Country,
                     data = countries)
summary(countries_model)
confint(countries_model,level=0.95)

summ(countries_model, confint = T,digits=3, ci.width = .95)
export_summs(countries_model,scale=F, digits =5)

countries$cpifit <- countries_model$fitted.values

scatter3d(CPI ~ Age + Hours,
          data = countries,
          surface = T, fit = "linear",
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))

