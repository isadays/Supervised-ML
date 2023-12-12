#Supervised ML 

# MULTIPLE REGRESSION ANALYSIS

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

load(file="countries.RData")

summary(countries)

#colnames(countries)[1] <- "Country"
#colnames(countries)[2] <- "CPI" - corruption index 
#colnames(countries)[3] <- "Age" of billionaires 
#colnames(countries)[4] <- "Hours" of work/ week 

# OUR MODEL IS TO PREDICT THE CPI 
# hatcpi = alpha + beta_age* age + beta_hours* hours 

