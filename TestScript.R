library(ggplot2)
library(ggrepel)
library(tidyverse)
data <- readxl::read_xlsx("./Data/KIB - Best in Show (public).xlsx", "Best in show full sheet")
data <- as.data.frame(data)

colnames(data) <- c(as.character(data[3,1]), as.character(data[2,-(1)]))
str(data)
data <- data[-c(1:3),]
isNaCol <- is.na(colnames(data))
isNACol <- c(colnames(data) == "NA")
data <- data[,!isNaCol]
data <- data[,!isNACol]

data <- data %>% select(c('Dog breed', 'category', `datadog score`, popularity, size, INTELLIGENCE.RANKING)) %>% 
  mutate(`Dog breed` = as.factor(`Dog breed`), 
         category = as.factor(category),
        `size` = factor(`size category`, levels = c('small', 'medium','large')),
        `datadog score` = as.numeric(`datadog score`),
        `Intelligence` = as.numeric(`INTELLIGENCE (TRAINABILITY) ranking`),
        Popularity = as.numeric(`POPULARITY IN US`)) %>%
  mutate(`INTELLIGENCE (TRAINABILITY) ranking` = as.factor(ifelse("INTELLIGENCE (TRAINABILITY) ranking" >= .35, 'clever', 'dumb')))
data['INTELLIGENCE (TRAINABILITY) ranking']





data <- data[c(1, 2, 3, 5, 8, 27)]
data <- data %>% mutate(Dog.breed = as.factor(`Dog breed`), category = as.factor(category), 
                        datadog.score = as.numeric(`datadog score`), popularity = as.numeric(`POPULARITY IN US`),
                        INTELLIGENCE.RANKING = as.numeric(`1 INTELLIGENCE (TRAINABILITY) ranking`),
                        size = factor(`size category`, levels = c("small", "medium", "large"))) %>%
  mutate(intelligence = as.factor(ifelse(INTELLIGENCE.RANKING >= 39, "clever", "dumb")))

data


