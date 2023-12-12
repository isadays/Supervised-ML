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


corruption %>%
  group_by(Region) %>%
  mutate(average_cpi = mean(CPI, na.rm = TRUE)) %>%
  mutate(rotulo = paste(Country, CPI)) %>%
  ggplot(aes(x = as.numeric(Region), y = CPI, label = rotulo)) +
  geom_point(aes(x = Region, y = CPI), color = "#FDE725FF", alpha = 0.5, size = 5) +
  geom_line(aes(x = Region, y = average_cpi, 
                group = 1, color = "Average CPI "), linewidth = 1.5) +
  scale_color_manual("Legend:",
                     values = "#440154FF") +
  labs(x = "Region",
       y = "Corruption Perception Index") +
  geom_text_repel() +
  theme_bw() +
  theme(legend.position = "bottom")

#Dummy variables  -> one-hot encoding  (n-1) dummies
#Reference category -> Europe
corruption_dummies <-dummy_columns(.data=corruption,
                                   select_columns = "Region",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)

corruption_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

corruption_dummies_model <- lm(CPI ~. -Country, corruption_dummies)
summary(corruption_dummies_model)
#we removed Europe Region, and its mean appears on the intercept 6.2583
myplot3 <- corruption %>%
  mutate(rotulo = paste(Country,CPI)) %>%
  ggplot(aes(x = as.numeric(Region), y = CPI, label = rotulo)) +
  geom_point(color = "#FDE725FF", alpha = 0.5, size = 4) +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ bs(x, df = 4),
              se = T) +
  labs(x = "Region",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "South America", 
                              "2" = "Oceania", 
                              "3" = "Europe", 
                              "4" = "USA and Canada", 
                              "5" = "Asia")) +
  scale_color_manual("Legend:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw() +
  theme(legend.position = "bottom")
myplot3

# GIF

ggsave("my_plot3.png")
my_plot3 <- image_read("my_plot3.png") #function of 'magick' package

# If we want to change the reference 


corruption_dummies2 <-dummy_columns(.data=corruption,
                                   select_columns = "Region",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = F)

#Reference Category for dummy -> South America
corruption_dummies2 <- corruption_dummies2[,-3]

corruption_dummies2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

corruption_dummies2_model <-lm(CPI ~. -Country, corruption_dummies2)

summary(corruption_dummies2_model)

