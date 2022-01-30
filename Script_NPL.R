# Practica NLP

install.packages("readr")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tm")
install.packages("tmap")
install.packages("wordcloud")
install.packages("RColorBrewer") 
install.packages("dplyr")
install.packages("sentimentr") 
install.packages("tidytext")
install.packages("textdata")
install.packages("syuzhet")
install.packages("naivebayes")
install.packages("e1071")
install.packages("caret")
install.packages("janitor")
install.packages("tm")
install.packages("qdap")
install.packages("wordcloud")
install.packages("NLP")
install.packages("openNLP") 
install.packages("openNLPmodels.en")
install.packages("corpus")
install.packages("Rcpp")

library(readr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(tm)
library(tmap)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(sentimentr)
library(tidytext)
library(textdata)
library(tidyverse)
library(syuzhet)
library(naivebayes)
library(e1071)
library(caret)
library(janitor)
library(NLP)
library(openNLP) 
library(openNLPmodels.en)
library(corpus)
library(SnowballC)
library(DT)
library(Rcpp)


dataset_TA <- read_csv("C:/Users/yanir/Desktop/NPL/TA_restaurants_curated.csv")
summary(dataset_TA)

palabras <- dataset_TA %>%
  select(c("Name","Reviews", "Ranking","Rating")) %>%
  unnest_tokens(word, Reviews) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

#Análisis de sentimientos 

afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
afinn <- data.frame(afinn)
afinnValue <- palabras %>%
  inner_join(afinn, by = "word")

#Palabras más frecuentes/habituales

word_summary <- afinnValue %>%
  group_by(word) %>%
  summarise(mean_rating = mean(Rating), score = max(value), count_word = n()) %>%
  arrange(desc(count_word))
datatable(head(word_summary))

#Palabras más frecuentes/habituales - ggplot

ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + scale_color_gradient(low = "lightblue", high = "darkblue")+ guides(size = FALSE, color=FALSE)+ coord_cartesian(xlim=c(3.5,4.5)) 

#Nube de palabras más frecuentes/habituales

wordcloud(words = word_summary$word, freq = word_summary$count_word, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Set2"))

#Palabras positivas

positives <- afinnValue %>%
  group_by(word) %>%
  summarise(mean_rating = mean(Rating), score = max(value), count_word = n()) %>%
  filter(mean_rating > mean(mean_rating, na.rm=TRUE)) %>%
  arrange(desc(mean_rating))

wordcloud(words = positives$word, freq = positives$count_word, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Set2"))

#Palabras negativas

negatives <- afinnValue %>%
  group_by(word) %>%
  summarise(mean_rating = mean(Rating), score = max(value), count_word = n()) %>%
  filter(mean_rating<mean(mean_rating, na.rm=TRUE)) %>%
  arrange(mean_rating)
wordcloud(words = negatives$word, freq = negatives$count_word, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Set2"))

#/Palabras mas frecuentes-----------------------------------------------------------------------------------------------------


#Reviews por valoración a restaurante

review_summary <- afinnValue %>%
  group_by(Name) %>%
  summarise(mean_rating = mean(Rating), sentiment = mean(value))
datatable(head(review_summary))

#Sentimientos y valoraciones de los productos

y_mid = 0
x_mid = 3.5

review_summary %>% 
  mutate(quadrant = case_when(mean_rating > x_mid & sentiment > y_mid   ~ "Valoracion Positiva/Sentimiento positivo",
                              mean_rating <= x_mid & sentiment > y_mid  ~ "Valoracion Negativa/Sentimiento positivo",
                              mean_rating <= x_mid & sentiment <= y_mid ~ "Valoracion Negativa/Sentimiento Negativo",
                              TRUE                                      ~ "Valoracion Positiva/Sentimiento Negativo")) %>% 
  ggplot(aes(x = mean_rating, y = sentiment, color = quadrant)) + 
  geom_hline(yintercept=y_mid, color = "black", size=.5) + 
  geom_vline(xintercept=x_mid, color = "black", size=.5) +
  guides(color=FALSE) +
  scale_color_manual(values=c("lightgreen", "pink", "pink","lightgreen")) +
  ggtitle("Valoracion Restaurantes TripAdvisor vs Valoracion Sentimental") +
  ggplot2::annotate("text", x = 4.33, y=3.5,label="Valoracion Positiva/Sentimiento positivo") +
  ggplot2::annotate("text", x = 2, y=3.5,label="Valoracion Negativa/Sentimiento positivo") +
  ggplot2::annotate("text", x = 4.33, y=-2.5,label="Valoracion Negativa/Sentimiento Negativo") +
  ggplot2::annotate("text", x = 2, y=-2.5,label="Valoracion Positiva/Sentimiento Negativo") +
  geom_point()


