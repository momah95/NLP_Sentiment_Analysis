#load library
library(tidytext)
library(stringr)
library(rtweet)
library(tidyverse)
library(httpuv)
library(tweetrmd)
library(emo)
library(wordcloud)

# install.packages("devtools")
devtools::install_github("gadenbuie/tweetrmd")
# install.packages("devtools")
devtools::install_github("hadley/emo")

#Accessing tweets from TwitterAPI (search for tweets with #ClimateEmergency)
tweets <- search_tweets(q = "#EndSars",
                        n = 1800,
                        include_rts = FALSE,
                        `-filter` = "replies",
                        langs = "eng")

#sample tweet
tweets %>% 
  sample_n(5) %>%
  select(created_at, screen_name, text, favorite_count, retweet_count)

#export tweets as csv
write_as_csv(tweets, "tweets.csv")


#explore tweets with ts_plot() function
ts_plot(tweets, "hours") +
  labs(x = NULL, y = NULL, title = "Frequency of tweets with a #EndSars hashtag", 
       subtitle = paste0(format(min(tweets$created_at), "%d %B %Y"), " to ", format(max(tweets$created_at),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") + theme_minimal()

#select tweets with particular word
specific_word <- tweets %>% 
  select(created_at, screen_name, text) %>% 
  filter(str_detect(text, "bless"))
View(specific_word)

#top tweet location
tweets %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(5)


#most frequently shared links
urls <- tweets %>% 
  filter(!is.na(urls_expanded_url)) %>% 
  count(urls_expanded_url, sort = TRUE) %>% 
  top_n(5)
View(urls)


#most retweeted tweets
mrt <- tweets %>% 
  arrange(-retweet_count) %>% 
  slice(1) %>% 
  select(created_at, screen_name,status_id, text, retweet_count)
View(mrt)


#screenshot of most recent tweet
screenshot <- tweet_screenshot(tweet_url("get_carbon", "1355088950067679235"))

#most liked tweets
tweets %>% 
  arrange(-favorite_count) %>% 
  top_n(5, favorite_count) %>% 
  select(created_at, screen_name, text, favorite_count)


#top tweeters
tweets %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))

#top emoji
emoji <- tweets %>% 
  mutate(emoji = ji_extract_all(text)) %>% 
  unnest(cols = c(emoji)) %>% 
  count(emoji, sort = TRUE) %>% 
  top_n(10)
View(emoji)


#top hashtags
hashtags <- tweets %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>% 
  filter(str_detect(hashtag, "^#"), hashtag != "#EndSars") %>% 
  count(hashtag, sort = TRUE) %>% 
  top_n(10)

#top mentions
tweets %>% 
  unnest_tokens(mentions, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE) %>%
  top_n(10)

#top words
top_words <- tweets %>%
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"),
         !str_detect(word, "^#"),         
         !str_detect(word, "@\\S+")) %>%
  count(word, sort = TRUE)

#Then we use the wordcloud package to create a visualisation of the word frequencies.
words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = "#F29545"))



#get sentiment score for each tweet
sentiment_bing = function(twt){
  #step 1: perform basic text cleaning (on the tweet), as seen earlier
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      #remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>% 
    unnest_tokens(word, stripped_text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    #create a column "score" that assigns a -1 to all negative words, and 1 to positive words
    mutate(
      score = case_when(
        sentiment == 'negative' ~ n*(-1),
        sentiment == 'positive' ~ n*(1)
      ))
  #calculate the total score
  sent.score = case_when(
    nrow(twt_tbl) == 0~0, #if there are no words, score is 0
    nrow(twt_tbl) >0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  #this is to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", #no words at all
    nrow(twt_tbl)>0~"Type 2" #Type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

endsars_tweets <- lapply(tweets$text, function(x){sentiment_bing(x)})




##################################################################################################################
#remove http elements manually
tweets$stripped_text <- gsub("http\\S+","",tweets$text)

#use unnest_tokens() to convert to lowercase, remove punctuation and add id for each tweet
tweets_stem <- tweets %>% 
  select(stripped_text) %>% 
  unnest_tokens(word, stripped_text)

#remove stop words from list of words
clean_tweets <- tweets_stem %>% 
  anti_join(stop_words)

#find top 10 words in #EndSars tweets
clean_tweets %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", y = "Count", title = "Unique word counts found in #EndSars tweets")

#sentiment analysis using Bing Lexicon for tweets
sars_tweets <- clean_tweets %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()


#Visually representing negative and positive sentiments
sars_tweets %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = 'free_y') +
  labs(title = "Tweets containing #EndSars", y = "Contribution to sentiment", x = NULL) +
  coord_flip() + theme_bw()


#visualizing the sentiments
#creating a tibble 
sars_sentiment <- tibble(
    score = unlist(map(endsars_tweets, 'score')),
    type = unlist(map(endsars_tweets, 'type'))
  )
  
ggplot(sars_sentiment, aes(score)) + geom_histogram(bins = 15, alpha = 0.6)  + labs(title = "sentiment score of #endsars") +theme_bw()



