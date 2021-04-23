#load libraries
library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(httpuv)
library(tweetrmd)
library(emo)

get_sentiments("bing")

#Accessing tweets from TwitterAPI 
country1 <- search_tweets(q = "#Nigeria",
                        n = 100,
                        include_rts = FALSE,
                        `-filter` = "replies",
                        langs = "eng")
country2 <- search_tweets(q = "Canada",
                          n = 100,
                          include_rts = FALSE,
                          `-filter` = "replies",
                          langs = "eng")

#selecting tweets with screen_name and text
tweets.country1 = country1 %>% select(screen_name, text)
tweets.country2 = country2 %>% select(screen_name, text)

#tidying tweets
head(tweets.country1$text)
#remove http elements manually
tweets.country1$stripped_text1 <- gsub("http\\S+","",tweets.country1$text)

#use unnest_tokens() to convert to lowercase, remove punctuation and add id for each tweet
tweets.country1_stem <- tweets.country1 %>% 
  select(stripped_text1) %>% 
  unnest_tokens(word, stripped_text1)

head(tweets.country1_stem)

#remove stop words from list of words
cleaned_tweets.country1 <- tweets.country1_stem %>% 
  anti_join(stop_words)


#remove http elements manually for country 2
tweets.country2$stripped_text2 <- gsub("http\\S+","",tweets.country2$text)

#use unnest_tokens() to convert to lowercase, remove punctuation and add id for each tweet
tweets.country2_stem <- tweets.country2 %>% 
  select(stripped_text2) %>% 
  unnest_tokens(word, stripped_text2)

#remove stop words from list of words
cleaned_tweets.country2 <- tweets.country2_stem %>% 
  anti_join(stop_words)

#find top 10 words in #Nigeria tweets
cleaned_tweets.country1 %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", y = "Count", title = "Unique word counts found in #Nigeria tweets")


#find top 10 words in #Canada tweets
cleaned_tweets.country2 %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", y = "Count", title = "Unique word counts found in #Canada tweets")


#sentiment analysis using Bing Lexicon for Nigerian tweets
bing_country1 <- cleaned_tweets.country1 %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()
bing_country1


#Visually representing negative and positive sentiments
bing_country1 %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = 'free_y') +
  labs(title = "Tweets containing #Nigeria", y = "Contribution to sentiment", x = NULL) +
  coord_flip() + theme_bw()

#sentiment analysis using Bing Lexicon for Canadian tweets
bing_country2 <- cleaned_tweets.country2 %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()
bing_country2


#Visually representing negative and positive sentiments
bing_country2 %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = 'free_y') +
  labs(title = "Tweets containing #Canada", y = "Contribution to sentiment", x = NULL) +
  coord_flip() + theme_bw()

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

#apply functions to tweets
country1_sent <- lapply(country1$text, function(x){sentiment_bing(x)})
country2_sent <- lapply(country2$text, function(x){sentiment_bing(x)})
country1_sent[[2]]
country2_sent[[5]]

#creating a tibble by country
country_sentiment <- bind_rows(
  tibble(
    country = '#Nigeria',
    score = unlist(map(country1_sent, 'score')),
    type = unlist(map(country1_sent, 'type'))
  ),
  tibble(
  country = '#Canada',
  score = unlist(map(country2_sent, 'score')),
  type = unlist(map(country2_sent, 'type'))
  )
)

#visualizing the sentiments
ggplot(country_sentiment, aes(score, fill = country)) + geom_histogram(bins = 15, alpha = 0.6) +
  facet_grid(~country) + theme_bw()
