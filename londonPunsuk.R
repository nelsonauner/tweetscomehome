# Steps will be: 
# 1) Load data
# 2) Parse data
# 3) Run regression


# Step 1)
# This is possibly the worse possible way to load the data
bigd <- read.csv('~/Home/LondonData/london2012-tweets/takeOut.csv',
                 stringsAsFactors=FALSE)


smalld <- bigd %>% head(50)
# Easy to play around with this. 

bigd$longitude <-bigd$tweet_longitude %>% as.character %>% as.numeric 
bigd$latitude <- bigd$tweet_latitude %>% as.character %>% as.numeric

ftn <- function(x) x %>% as.character %>% as.numeric

# Take subset of data that has 
reald <- bigd %>% filter(tweet_longitude %>% ftn %>% is.na ==FALSE,
                         tweet_latitude %>% ftn %>% is.na==FALSE)




# Load required libraries for text analysis
library(tm);library(textir);library(SnowballC)


# Map all postal codes, with code that can be rerun upon error 
reald$pdistrict <- NA
for (j in reald$pdistrict %>% is.na %>% which) {
  reald$pdistrict[i] <- findPostalCode(reald$tweet_longitude[i],reald$tweet_latitude[i],ct)
}

# And combine into a unique postalcode - date (year?)


tweetCorpus <- reald$tweet_text %>% as.character %>% VectorSource %>% Corpus

# tm_map runs in parallel, which just means a) it's fast and b) any errors are impossible to debug

tweetCorpusCleaned <- tweetCorpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) 

tweetCorpusStemmed <- tweetCorpusCleaned %>%
  tm_map(stemDocument)  %>%
  tm_map(stripWhitespace)



TDM <- tweetCorpusStemmed %>% DocumentTermMatrix
inspect(TDM[1:10,1:10])


library(Matrix)
class(TDM)
#so this is not necessary?
sm <- sparseMatrix(i = TDM$i,
                   j = TDM$j,
                   x = TDM$v)


#Load the required library and shoot away. 
require(textir)
cl <- makeCluster(detectCores(), type="FORK")
fitTweets <- mnlm(cl = cl,
                  covars = reald$tweet_retweet_count %>% ftn,
                  counts = TDM)
                  
                  
stopCluster(cl)


# For more info on this regression: http://cran.r-project.org/web/packages/textir/textir.pdf