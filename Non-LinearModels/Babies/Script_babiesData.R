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

#Shapiro–Francia test -> test of adherence  
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

