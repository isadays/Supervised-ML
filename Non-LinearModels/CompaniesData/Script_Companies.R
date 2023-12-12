#MULTIPLE REGRESSION ANALYSIS

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
