#Supervised ML 

#  REGRESSION ANALYSIS WITH ONE QUALITATIVE VARIABLE - region 

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

load(file="corruption.RData")
corruption <-corrupcao
rm(corrupcao)

colnames(corruption)[1] <- "Country"
colnames(corruption)[2] <- "CPI" #- corruption index 
colnames(corruption)[3] <- "Region" #

#model cpihat = beta_region region 

glimpse(corruption)
levels(glimpse(corruption$Region))
table(corruption$Region)
summary(corruption)

corruption %>%
  group_by(Region) %>%
  mutate(rotulo = paste(Country, CPI)) %>%
  ggplot(aes(x = as.numeric(Region), y = CPI, label = rotulo)) +
  geom_point(aes(x = Region, y = CPI), color = "#FDE725FF", alpha = 0.5, size = 5) +
  scale_color_manual("Legend:",
                     values = "#440154FF") +
  labs(x = "Region",
       y = "Corruption Perception Index") +
  geom_text_repel() +
  theme_bw()

