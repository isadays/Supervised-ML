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


if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  install_packages <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}
#Brazilian database National Basic Education Assessment System (SAEB) which is
#important to improve the educational performance.
load(file = "saeb_rend.rdata")

summary(saeb_rend)

#Plot saeb
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb)) +
    geom_point(size = 1, color = "lightblue3") +
    geom_smooth(method = "lm", formula = y ~ x,
                color = "grey40", se = F) +
    xlab("Rendimento") +
    ylab("Nota saeb") +
    theme_classic()
)

ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    xlab("Rendimento") +
    ylab("Nota saeb") +
    scale_colour_viridis_b() +
    theme_classic()
)
# Model and Diagnostics of Heteroscedasticity
model_saeb <- lm(formula=saeb ~rendimento,
                 data = saeb_rend)
summary(model_saeb)
# Breusch-Pagan test
ols_test_breusch_pagan(model_saeb)
#omission of variables from the model

# a possible solution is to use dummy variables
saeb_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",
                                      remove_selected_columns = T,
                                      remove_most_frequent_dummy = T)

#Now, our model will consider the UF
model_saeb_dummies_uf <- lm(formula =  saeb ~.-municipio - codigo -escola -rede,
                            data = saeb_dummies_uf)
summary(model_saeb_dummies_uf)

ols_test_breusch_pagan(model_saeb_dummies_uf)

ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = uf, shape = uf)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)
