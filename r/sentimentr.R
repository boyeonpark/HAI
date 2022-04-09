install.packages("pacman")
install.packages("xlsx")
library("xlsx")

pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr")
pacman::p_load("tidyverse","rio")

# mytext <- c(
#   'Smith feels angry.',
#   'Johnson feels angry.',
#   'Begay feels angry.',
#   'Lee feels angry.'
# )
# 
# mytext <- get_sentences(mytext)
# sentiment(mytext)


df <- read.csv(file = 'template.csv')

head(df)

#mytext <- get_sentences(mytext)
sentiment(df$Sentence)

df<-df%>% mutate(df,'SentimentR'=sentiment(df$Sentence))

#write.xlsx(df, file = "df_sentimentr.xlsx")
write.csv(df,"df_sentimentr.csv", row.names = FALSE)
