pacman::p_load("tidyverse","formattable","dpylr")

df <- read.csv(file = 'df_result_tb_f.csv')

names(df)<- tolower(names(df))#lowercase column names

# for emotion analysis
df["emotion"] <- "neutral"

angry_words<-"angry|annoyed|enraged|furious|irritated|annoying|displeasing|irritating|outrageous|vexing"
fear_words<-"anxious|discouraged|fearful|scared|terrified|dreadful|horrible|shocking|terrifying|threatening"
joy_words <-"ecstatic|excited|glad|happy|relieved|amazing|funny|great|hilarious|wonderful"
sadness_words<-"depressed|devastated|disappointed|miserable|sad|depressing|gloomy|grim|heartbreaking|serious"


df<-df%>%
  mutate(emotion = case_when(str_detect(sentence,angry_words)~"angry",
                             str_detect(sentence,fear_words)~"fear",
                             str_detect(sentence,joy_words)~"joy",
                             str_detect(sentence,sadness_words)~"sadness",
                             TRUE ~ "neutral"))

df%>%filter(emotion=="neutral")
df%>%filter(template=="11")


df_group<-df%>%filter(template <= 11)

df_name<-df%>%filter(template>11)

# Mean Scores by algorithms, group

df_group%>% group_by(group)%>%
  summarise_at(vars(textblob, sentimentr, perspective ),mean,na.rm=TRUE)%>%
  formattable(align = c("c","c"),caption = "[Mean Sentiment Scores of group template sentences(T1~T11)]")

df_name%>% group_by(group)%>%
  summarise_at(vars(textblob, sentimentr, perspective ),mean,na.rm=TRUE)%>%
  formattable(align = c("c","c"),caption = "[Mean Sentiment Scores of group template sentences(T1~T11)]")

# group - Textblob
df_group%>% group_by(group)%>%
  summarise_at(vars(textblob, sentimentr, perspective ),mean,na.rm=TRUE)%>%
  ggplot(mapping=aes(x=group, y=textblob, fill=group))+
  geom_bar(stat='identity')+
  geom_text(aes(group,textblob, 
              label=round(textblob,2)),
          vjust = -0.5, size = 4) +
  scale_fill_hue(c=45, l=80)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=13,hjust = 0.5)
        ,plot.subtitle = element_text(size=10,hjust = 0.5)
        ,axis.text=element_text(size=11)
        ,axis.title=element_text(size=11,face="bold")) +
  ggtitle(label = "Textblob Mean Sentiment scores",
          subtitle = "- Ethnic group name template sentences[T1~T11]")


# group - SentimentR
df_group%>% group_by(group)%>%
  summarise_at(vars(textblob, sentimentr, perspective ),mean,na.rm=TRUE)%>%
  ggplot(mapping=aes(x=group, y=sentimentr, fill=group))+
  geom_bar(stat='identity')+
  geom_text(aes(group,sentimentr, 
                label=round(sentimentr,2)),
            vjust = -0.2, size = 4) +
  scale_fill_hue(c=45, l=80)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=13,hjust = 0.5)
        ,plot.subtitle = element_text(size=10,hjust = 0.5)
        ,axis.text=element_text(size=11)
        ,axis.title=element_text(size=11,face="bold")) +
  ggtitle(label = "SentimentR Mean Sentiment scores",
          subtitle = "- Ethnic group name template sentences[T1~T11]")

# Analysis by emotion
df_emotion<-df_group %>% group_by(emotion, group)%>%
  summarise_at(vars(textblob, sentimentr),mean,na.rm=TRUE)

#angry
df_emotion %>% filter(emotion=="angry")%>%
  group_by(group)%>%
  summarise_at(vars(textblob, sentimentr ),mean,na.rm=TRUE)%>%
  formattable(align = c("c","c"),caption = "[Mean Sentiment Scores of Emotion - 'Angry' (T1~T11)]")

df_emotion%>% filter(emotion=="angry")%>%group_by(group)%>%
  summarise_at(vars(textblob, sentimentr ),mean,na.rm=TRUE)%>%
  ggplot(mapping=aes(x=group, y=textblob, fill=group))+
  geom_bar(stat='identity')+
  geom_text(aes(group,textblob, 
                label=round(textblob,2)),
            vjust = -0.2, size = 4) +
  scale_fill_hue(c=45, l=80)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=13,hjust = 0.5)
        ,plot.subtitle = element_text(size=11,hjust = 0.5)
        ,axis.text=element_text(size=11)
        ,axis.title=element_text(size=11,face="bold")) +
  ggtitle(label = "Textblob Mean Sentiment scores by Emotion",
          subtitle = "- Angry")

#joy


df_emotion %>% filter(emotion=="joy")%>%
  group_by(group)%>%
  summarise_at(vars(textblob, sentimentr ),mean,na.rm=TRUE)%>%
  formattable(align = c("c","c"),caption = "[Mean Sentiment Scores of Emotion - 'Angry' (T1~T11)]")

df_emotion%>% filter(emotion=="joy")%>%group_by(group)%>%
  summarise_at(vars(textblob, sentimentr ),mean,na.rm=TRUE)%>%
  ggplot(mapping=aes(x=group, y=textblob, fill=group))+
  geom_bar(stat='identity')+
  geom_text(aes(group,textblob, 
                label=round(textblob,2)),
            vjust = -0.2, size = 4) +
  scale_fill_hue(c=45, l=80)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=13,hjust = 0.5)
        ,plot.subtitle = element_text(size=11,hjust = 0.5)
        ,axis.text=element_text(size=11)
        ,axis.title=element_text(size=11,face="bold")) +
  ggtitle(label = "Textblob Mean Sentiment scores by Emotion",
          subtitle = "- Joy")

#sadness


df_emotion %>% filter(emotion=="sadness")%>%
  group_by(group)%>%
  summarise_at(vars(textblob, sentimentr ),mean,na.rm=TRUE)%>%
  formattable(align = c("c","c"),caption = "[Mean Sentiment Scores of Emotion - 'Angry' (T1~T11)]")

df_emotion%>% filter(emotion=="sadness")%>%group_by(group)%>%
  summarise_at(vars(textblob, sentimentr ),mean,na.rm=TRUE)%>%
  ggplot(mapping=aes(x=group, y=textblob, fill=group))+
  geom_bar(stat='identity')+
  geom_text(aes(group,textblob, 
                label=round(textblob,2)),
            vjust = -0.2, size = 4) +
  scale_fill_hue(c=45, l=80)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=13,hjust = 0.5)
        ,plot.subtitle = element_text(size=11,hjust = 0.5)
        ,axis.text=element_text(size=11)
        ,axis.title=element_text(size=11,face="bold")) +
  ggtitle(label = "Textblob Mean Sentiment scores by Emotion",
          subtitle = "- sadness")

#fear


df_emotion %>% filter(emotion=="fear")%>%
  group_by(group)%>%
  summarise_at(vars(textblob, sentimentr ),mean,na.rm=TRUE)%>%
  formattable(align = c("c","c"),caption = "[Mean Sentiment Scores of Emotion - 'Angry' (T1~T11)]")

df_emotion%>% filter(emotion=="fear")%>%group_by(group)%>%
  summarise_at(vars(textblob, sentimentr ),mean,na.rm=TRUE)%>%
  ggplot(mapping=aes(x=group, y=textblob, fill=group))+
  geom_bar(stat='identity')+
  geom_text(aes(group,textblob, 
                label=round(textblob,2)),
            vjust = -0.2, size = 4) +
  scale_fill_hue(c=45, l=80)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size=13,hjust = 0.5)
        ,plot.subtitle = element_text(size=11,hjust = 0.5)
        ,axis.text=element_text(size=11)
        ,axis.title=element_text(size=11,face="bold")) +
  ggtitle(label = "Textblob Mean Sentiment scores by Emotion",
          subtitle = "- fear")

