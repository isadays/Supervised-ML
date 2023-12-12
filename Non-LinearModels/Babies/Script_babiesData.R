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
