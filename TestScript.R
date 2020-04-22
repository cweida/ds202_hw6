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

data <- data %>% select(c('Dog breed', 'category', `datadog score`, `POPULARITY IN US`, `INTELLIGENCE (TRAINABILITY) ranking`, `size category`)) %>% 
  mutate(`Dog breed` = as.factor(`Dog breed`), 
         category = as.factor(category),
        `size` = factor(`size category`, levels = c('small', 'medium','large')),
        `datadog score` = as.numeric(`datadog score`),
        `Intelligence` = as.numeric(`INTELLIGENCE (TRAINABILITY) ranking`),
        Popularity = as.numeric(`POPULARITY IN US`)) %>%
  mutate(`Intelligence` = as.factor(ifelse(`Intelligence` >= .35, 'clever', 'dumb')))
data['Intelligence']
str(data)
data <- data %>% select(c("Dog breed", "category", "datadog score", "size", "Intelligence", "Popularity"))

data <- data %>% filter(!is.na("Dog breed"),!is.na("category"),!is.na("datadog score"),!is.na("size"),!is.na("Intelligence"),!is.na("Popularity"))

data <- drop_na(data)


plot <- ggplot(data, mapping = aes(x = `datadog score`,y = Popularity))
plot <- plot + geom_point(aes(size = `size`, color = category, shape = Intelligence)) + 
  geom_text_repel(mapping = aes(label = `Dog breed`, color = category), size = 3) 
plot <- plot + ylab('Popularity') + xlab('Dog Data Score')
plot

median(data$Popularity)


plot <- ggplot(data, mapping = aes(x = `datadog score`,y = Popularity))
plot <- plot + geom_point(aes(size = `size`, color = category, shape = Intelligence)) + 
  geom_text_repel(mapping = aes(label = `Dog breed`, color = category), size = 3) 
plot <- plot + ylab('Popularity') + xlab('Dog Data Score')
plot <- plot + ylim(max(data$Popularity), min(data$Popularity)) + geom_hline(yintercept = (median(data$Popularity))) + geom_vline(xintercept = median(data$`datadog score`))
plot

