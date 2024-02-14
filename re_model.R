# Read in data:

# Random Effects:
data <- read.csv('model_data_file.csv')
data1 <- read.csv('final_rating.csv')

# OLS Data:
ols18 <- read.csv('ols_2018.csv') 
ols19 <- read.csv('ols_2019.csv')
ols20 <- read.csv('ols_2020.csv')

# Google ratings dummy variable model:
rating <- read.csv('google_ratings_model.csv')

library(plm)
library(dotwhisker)
library(magrittr)
library(ggplot)
library(tidyverse)

data.pd <- pdata.frame(data, index = c("Zip.Codes","year"), drop.index = TRUE)
data1.pd <- pdata.frame(data1, index = c("Zip","year"), drop.index = TRUE) 

# Random Effects Model:
re <- plm((sales) ~ (Male.pop) + (Black.pop) + (Asian.pop) + (Japanese.pop) +
              (Median.income) + Google.Rating + Undergrad.student + store_type,
                            data = data1.pd, model = 'random')
summary(re)


# OLS Regression 2018
ols2018 <- lm((sales) ~ (Male.pop) + (Black.pop) + (Asian.pop) +
                    (Japanese.pop) + (Median.income) + 
                  Google.Rating + Undergrad.student + store_type,
               data = ols18)

summary(ols2018)

# OLS Regression 2019
ols2019 <- lm((sales) ~ (Male.pop) + (Black.pop) + (Asian.pop) +
                  (Japanese.pop) +
                  (Median.income) + Google.Rating + Undergrad.student + store_type,
              data = ols19)

summary(ols2019)

# OLS Regression 2020
ols2020 <- lm((sales) ~ (Male.pop) + (Black.pop) + (Asian.pop)
              + (Japanese.pop) + (Median.income) + Google.Rating + Undergrad.student,
              data = ols20)

summary(ols2020)


# Google Ratings Dummy variable model:
google_ratings <- lm(log_sales ~ above_average_rating,
                     data = rating)

summary(google_ratings)
#ggplot(data = data1, aes(x = log(Hispanic.pop))) + theme_classic() + geom_histogram(fill = "dodgerblue", color = 'black') + labs(x = "sales", y = "Density", title = "Is this normally distributed?")

# add log to sales
temp <- data1 %>% filter(!is.na(Male.pop) & !is.na(Female.pop) & !is.na(White.pop) & !is.na(Black.pop)
                         & !is.na(Asian.pop) & !is.na(Hispanic.pop) & !is.na(Japanese.pop) & !is.na(Korean.pop)
                         & !is.na(Chinese.pop) & !is.na(Median.income) & !is.na(sales))
# Correlation:
cor(temp$Japanese.pop, temp$Chinese.pop)

# PLOT THE DATA:
dwplot(list(re,ols2018,ols2019,ols2020))+
    scale_color_discrete(labels = c('Model 1' = 'Random Effects Model', 'Model 2'='OLS 2018','Model 3'='OLS 2019','Model 4'='OLS 2020'))+
    scale_y_discrete(labels = c('Male.pop'='Males', 'Black.pop'='African American','Asian.pop'='Asian', 'Japanese.pop'='Japanese descent', 'Median.income'='Median Income', 'Google.Rating' = 'Google Ratings','Undergrad.student'='Undergrad Students','store_type'='Retail & Food Services'))
