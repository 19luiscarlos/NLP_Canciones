library(genius) #bajar canciones
library(tidyverse) #manipulación de datos
library(tidytext) #text mining
library(tm) #text mining: stop words,
library(wordcloud) #nubes de palabras
library(textdata) #
library(widyr) #correlaciones entre textos

# Versos de las canciones seleccionadas 
textoA <- genius_lyrics("amanda miguel", "castillos")
textoB <- genius_lyrics("mi banda el mexicano", "ramito de violetas")
textoC <- genius_lyrics("Jeanette", "El Muchacho de los Ojos Tristes")[-1,]
textoD <- genius_lyrics("El Tri", "Las Piedras Rodantes")

# Todas las canciones juntas
textos <- rbind(textoA, textoB, textoC, textoD)

# Ahora se analizará palabra por palabra
textos_palabras <- textos %>%
                   unnest_tokens(word, lyric) 

# Se cuentan incluyendo cada palabra
textos_palabras %>%
  count(word, sort = TRUE) 

# Lista de stop words en inglés y español
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = stopwords("spanish"),
                                          lexicon = "custom"),
                               data_frame(word = c("tururu", "turututuru",
                                                   "turututururu", "quién",
                                                   "asi", "cada", "mas"),
                                          lexicon = "custom"))
# https://jvera.rbind.io/post/2017/10/16/spanish-stopwords-for-tidytext-package/

# Se eliminan las stop words
tidy_textos <- textos_palabras %>%
  anti_join(custom_stop_words)

# Cuenta sin stop words
tidy_count <- tidy_textos %>%
              count(track_title, word, sort = TRUE)

# Gráfica de las más frecuentes
most_frequent <- function(track, lim){
  tidy_textos %>%
    #filtering to get only the information we want on the plot
    filter(track_title == track)%>%
    count(word, sort = TRUE) %>%
    filter(n > 1)%>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col(color = "#0e7eb2", fill = "#0e7eb2", width = 0.8)+
    geom_text(aes(label = reorder(word, n)), 
              hjust = 1.01, vjust = 0.4, color = "white", 
              size = 4)+
    labs(y = "Número de veces que aparece", 
         x = NULL,
         title = track)+
    coord_flip() +
    ylim(c(0, lim))+ 
    theme_minimal()+
    #now making more visually appealing
    theme(plot.title = element_text(hjust = 0.5, size = 14, color = "grey40"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 8, color = "grey40"),
          axis.title.x = element_text(size = 10, color = "grey40"),
          plot.caption = element_text(size = 7.5, color = "grey40"))
}

most_frequent('Castillos', 4)
most_frequent('Las Piedras Rodantes', 8)
most_frequent('Ramito De Violetas', 8)
most_frequent('El Muchacho de los Ojos Tristes', 6)

# Correlaciones entre canciones
textos_cors <- tidy_textos %>%
  pairwise_cor(track_title, word, sort = TRUE) %>%
  filter(item1 == 'Castillos')

textos_cors %>%
ggplot(aes(x = reorder(item2, correlation), y = correlation, group = 1)) +
  geom_line() +
  geom_point() + 
  geom_text(aes(label = item2), size = 4,
            vjust = -0.5, hjust = 1, col = '#19613D') +
  geom_text(aes(label = round(correlation, 2)),
            vjust = 1.6, hjust = 0, size = 4.3) +
  ylim(c(-0.39, -0.20)) +
  labs(title = 'Correlación con "Castillos"') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.4, size = 14, color = "grey40"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #plot.margin = unit(c(5, 1, 1, 1), "cm"),
        plot.caption = element_text(size = 7.5, color = "grey40"))

# Correlaciones entre canciones
tidy_textos %>%
  pairwise_count(track_title, word, sort = TRUE)

# Intento de cloud
tidy_tdm <- tidy_count %>%
            cast_tdm(word, track_title, n)
tidy_matrix <- as.matrix(tidy_tdm)
comparison.cloud(tidy_matrix)
comparison.cloud(tidy_matrix[,c(4,1)]) #4 es castillos, 1 es ojos tristes
comparison.cloud(tidy_matrix[,c(4,2)]) #2 es piedras
comparison.cloud(tidy_matrix[,c(4,3)]) #3 es ramitos

commonality.cloud(tidy_matrix[,c(4,1)], # muchacho ojos tristes
                  colors = c('#15ef32', '#10b526', '#0e821d', '#084210'), 
                  random.order = FALSE) 
commonality.cloud(tidy_matrix[,c(4,2)], # piedras rodantes
                  colors = c('#ff77ab', '#c64d7c', '#99345a','#5e1531'),
                  random.order = FALSE) 
commonality.cloud(tidy_matrix[,c(4,3)], # ramitos violetas
                  colors = c('#32c1ff', '#299cce', '#217da5', '#11475e'),
                  random.order = FALSE) 

# Sentiment analysis
# Se tradujeron las canciones para facilitar esta sección
textA <- read.csv('castillos_eng.csv')
textB <- read.csv('ramito_eng.csv')
textC <- read.csv('muchacho_eng.csv')
textD <- read.csv('piedras_eng.csv')

# Todas las canciones juntas
texts <- rbind(textA, textB, textC, textD)

# Ahora se analizará palabra por palabra
texts_words <- texts %>%
  #word is the new column, lyric the column to retrieve the information from
  unnest_tokens(word, lyric) 

# Se cuentan incluyendo cada palabra
texts_words %>%
  count(word, sort = TRUE) 

# Se eliminan las stop words
tidy_texts <- texts_words %>%
  anti_join(custom_stop_words)

tidy_texts %>%
  # filter(track_title == 'Ramito De Violetas') %>%
  count(track_title, word, sort = TRUE)

tidy_sentiment <- tidy_texts%>%
  inner_join(get_sentiments("bing"))%>% 
  count(track_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

tidy_sentiment

tidy_sentiment%>%
  ggplot(aes(reorder(track_title, sentiment), sentiment)) +
  geom_col(show.legend = FALSE, fill = c('#9F1A2E', '#19613D', '#9F1A2E', '#19613D')) +
  geom_text(aes(label = reorder(track_title, sentiment)), 
            hjust = c(-2.9, 1.04, -0.25, 1.1), vjust = 0.4, color = "white", 
            size = 4) + 
  labs(x = NULL,
       y = "Sentiment",
       title = "Análisis de sentimiento")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.05, size = 7, color = "grey40", angle = 0),
        axis.title.x =  element_text(size = 10, color = "grey40"),
        axis.text.x = element_text(size = 9, color = "grey40"),
        axis.text.y = element_blank(), 
        strip.text = element_text(size = 9, color = "grey40", face = "bold"),
        plot.caption = element_text(size = 7.5, color = "grey40"))+
  coord_flip()
save.image('BancoAzteca.RData')
