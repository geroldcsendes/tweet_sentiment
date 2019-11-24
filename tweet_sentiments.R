# Library calls
library(tidyverse)
library(rtweet)
library(ggplot2)
library(glue)
library(data.table) 
library(scales)


# Setup twhitter App
appname <- "<your_appname>"
key <- "<your_key>"
secret_key <- "<secret_key>"
access_token <- "<acces_token>"
access_secret <- "<assess_sectet>"

# Create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret_key,
  access_token = access_token,
  access_secret = access_secret
)

# Get twitter data, pass include_rts = FALSE if you don't want to include retweets
trainer <- search_tweets(q = "Guardiola", n=18000, type='mixed', lang="en") 

# You could add further geographical data but this doesn't really help. Hardly any filled
trainer <- lat_lng(trainer)

# Assign relevant columns
keep_cols <- c("user_id", "screen_name", "created_at", "text", "source", "is_quote","is_retweet", "favorite_count", "retweet_count","quote_count","reply_count","hashtags", 
               "retweet_text", "followers_count", "verified")


# Filter to relevant columns
trainer <- trainer %>% 
  select(keep_cols)
View(trainer)


# Remove URLs ant emojis
# URL removal, source: https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/
trainer$stripped_text <- gsub("http.*","",  trainer$text)
trainer$stripped_text <- gsub("https.*","", trainer$stripped_text)

# Emoji removal
trainer$plain_tweet <- enc2native(trainer$stripped_text) # Covnert emojis to native encoding
trainer$plain_tweet <- gsub("<.*.>", "", trainer$plain_tweet)

# Remove leading whitespaces from the beginning
trainer$plain_tweet <- trimws(trainer$plain_tweet)

# Get rid of text an stripped text columns
trainer <- trainer %>% 
  select(-text, -stripped_text)

# We also need to get rid of empty tweets!
trainer <- read_csv("trainer.csv")
trainer <- trainer %>% 
  filter(nchar(plain_tweet)!=0)

# Bulitin tweet cleaning which doesn't work that great
builtin_clean <- plain_tweets(trainer)



# Calculate estimated sentiment cost
estimate_cost <- function(df) {
  
  df <- transform(df, chars = ifelse(nchar(plain_tweet) > 300, nchar(plain_tweet), 300))
  
  cost_estimate <- round(((sum(try_calc$chars))/(100)*0.0001),2)
  
  return_string <- glue("The estimated price for this sentiment analysis is {cost_estimate} dollars")
  print(return_string)
}

cost <- estimate_cost(trainer)

characters <- transform(trainer, chars = nchar(plain_tweet))

# Histogram with density plot
ggplot(characters, aes(x=chars)) + 
  geom_histogram(aes(y=..density..), colour="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(chars)),
             color="purple", linetype="dashed", size=1) +
  geom_rect( inherit.aes=FALSE, aes(xmin=300, xmax=max(characters$chars),ymin=0,
                                    ymax=0.01),  color="transparent", fill="lightgreen", alpha=0.03) +
  ggtitle("Tweet character distribution")

# Save plot
ggsave('character_distribution.png', width = 7, height = 5)


# Set up your R w/ AWS
keyTable <- read.csv("accessKeys.csv", header = T) # accessKeys.csv == the CSV downloaded from AWS containing your Acces & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

library("aws.comprehend")
#example: detect_sentiment("Hey, I'm feeling great today!")


# Get sentiments

sentiment <- function(row, df) {
  # Extraxt a specific row
  record <- df[row,]
  # Do the sentiment with Amazon's Comprehemd
  sentiment <- detect_sentiment(as.character(record$plain_tweet))
  # Merge the sentiment result to the original data
  merged <- merge(sentiment, record)
  #print(merged)
  print(row)
  return (merged)
  
}

row_seq <- seq(1,nrow(trainer)) # Define argument for function, nrow(dataframe)

sentiment_df <- lapply(row_seq, sentiment, df=trainer)


# Analyize tweet sentiments
# First plot of the analysis section
ggplot(data=sentiment_df, aes(x=Sentiment)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), colour="darkblue", fill="lightblue",
            position=position_dodge(), width=0.5) + 
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title = "Tweet sentiments for Guardiola", y='')

ggsave("tweet_sentiment.png", height = 5, width = 7)

# Convert to date
sentiment_df$date <- as.Date(sentiment_df$created_at)

# Prepare df for seciond chart in the analysis section
trends <- sentiment_df %>% 
  group_by(date, Sentiment) %>% 
  summarise(count = n())
trends_day <- sentiment_df %>% 
  group_by(date) %>% 
  summarise(sum = n())
trends_full <- trends %>% 
  left_join(trends_day, by='date')


trends_full$rel = (trends_full$count) / trends_full$sum
trends_full

# Plot second chart 
ggplot(data=trends_full, aes(x=date,y=rel, fill=Sentiment)) +
  geom_bar(stat="identity", position="stack")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + 
  labs(title= "Sentiment distribution per day" ,x = "Date", y = "") +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_y_continuous(labels=percent)

ggsave("sentiment_distribution.png", width = 7, height = 5)

# Save data
save_as_csv(sentiment_df, "sentiment.csv")




