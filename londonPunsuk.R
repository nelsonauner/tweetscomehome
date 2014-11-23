# Steps will be: 
# 1) Load data
# 2) Parse data
# 3) Run regression


# Step 1)
# This is possibly the worse possible way to load the data
bigd <- read.csv('~/Home/LondonData/london2012-tweets/takeOut.csv',
                 stringsAsFactors=FALSE)


reald <- bigd %>% head(50)
# Easy to play around with this. 

bigd$longitude <-bigd$tweet_longitude %>% as.character %>% as.numeric 
bigd$latitude <- bigd$tweet_latitude %>% as.character %>% as.numeric

ftn <- function(x) x %>% as.character %>% as.numeric

# Take subset of data that has 
reald <- bigd %>% filter(tweet_longitude %>% ftn %>% is.na ==FALSE,
                         tweet_latitude %>% ftn %>% is.na==FALSE)




# Load required libraries for text analysis
library(tm);library(textir);library(SnowballC)


# Map all postal codes, with code that can be rerun upon error.  
reald$pdistrict <- NA
for (i in 1:dim(reald)[1]) {
  print(i)
  reald$pdistrict[i] <- findPostalCode(reald$tweet_longitude[i],reald$tweet_latitude[i],ct)
}

# Use realds as smaller one. 
 realds$latlong <- paste(realds$tweet_latitude,realds$tweet_longitude,sep=":")


# sample geo graph: 


AndrewMap <- gvisMap(realds %>% head(100), "latlong" , "tweet_text", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))


l# And combine into a unique postalcode - date (year?)
tsum <- 
reald %>% group_by(pdistrict) %>% 
  summarise(ntweets = n(),
            users = length(unique(user_id)),
            friends = user_friends_count %>% as.numeric %>% sum,
            followers = user_followers_count %>% as.numeric %>% sum,
            fromiPhone = sum(tweet_source == "Twitter for iPhone"),
            fromAndroid = sum(tweet_source == "Twitter for Android"),
            fromiPad = sum(tweet_source == "Twitter for iPad"),
            fromBlackBerry = sum(tweet_source == "Twitter for BlackBerry®"),
            fromInsta = sum(tweet_source == "Instagram"),
            content = tweet_text %>% paste(collapse= "|")
            
# This works perfectly. Now add back latlong using pdistrict
tsum$latlong = sapply(tsum$pdistrict,FUN= function(pc) {
  ct[ct$pdisrict == pc %>% as.character,][c("latitude","longitude")] %>% paste(collapse=":")
})


#That worked well too. Now, let's wrap it into a graph: 
PostTweets <- gvisGeoChart(tsum,
                           locationvar="latlong",
                           colorvar = "users",
                           sizevar="ntweets",
                           options=list(
                             height=700,
                             #region="GB",
                             displayMode='Markers',
                             colorAxis="{colors:['purple', 'red', 'orange','gray']}",
                             backgroundColor = "lightblue"))
                             chartid = "EQ")
)
)
                             #displayMode="regions",
                             #resolution="provinces",
                             #width=600,
                             #height=400))

plot(PostTweets)
tsum <- cbind(tsum, do.call(rbind,
                            tsum$latlong %>% strsplit(":")))
names(tsum)[13:14] <- c("x","y")

map <-  get_googlemap('london',markers = tsum[c("x","y")]) 
ggmap(map) + geom_point(x=)

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