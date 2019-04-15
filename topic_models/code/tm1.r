## @knitr load.libs
library(tidyverse)
library(topicmodels)
library(tidytext)
library(SnowballC)
library(LDAvis)

## @knitr ExampleGenerativeModel 

nUsers <- 100    ## number of users
nDays <- 30      ## number of days observing users

set.seed(123)
usageRate <- runif(nUsers) ## simulate usage rates

userData <- data.frame(userID = rep(c(1:nUsers),each=nDays),
                       day = rep(c(1:nDays),nUsers),
                       y = rep(FALSE,nDays*nUsers))

## simulate user data 

for (theUser in c(1:nUsers)){
  userData$y[userData$userID==theUser] <- runif(nDays) < usageRate[theUser]
}


## @knitr BayesLearningExample


## - y is user data 
## - function returns posterior of usage rate

posteriorUser <- function(y){
  
  T <- length(y)
  N <- sum(y)
  
  seqx <- seq(0,1,0.01)
  return(data.frame( x= seqx, y = dbeta(seqx,N+1,T-N+1)))
  
} 

selUsers <- c(4,5,6)

theUsers <- bind_rows(
    data.frame(posteriorUser(userData$y[userData$userID==selUsers[1]]),user=selUsers[1]),
    data.frame(posteriorUser(userData$y[userData$userID==selUsers[2]]),user=selUsers[2]),
    data.frame(posteriorUser(userData$y[userData$userID==selUsers[3]]),user=selUsers[3])
      )

theUsers %>%
  mutate(user=factor(user)) %>%
  ggplot(aes(x=x,ymin=0,y=y,group=user,fill=user)) + 
  geom_area(alpha=0.5,position = position_dodge(width = 0.0)) + 
  theme_bw()+
    labs(x='Usage Rate',
         title='Posterior Distribution of Usage Rate for Three Users')



## @knitr ExampleGenerativeModel2 

nUsers <- 100    ## number of users

set.seed(123)
nDays <- sample.int(100,size=nUsers,replace=T)      ## number of days observing users
usageRate <- runif(nUsers) ## simulate usage rates

userData <- NULL

for (theUser in c(1:nUsers)){
  
  theUserData <- data.frame(userID = rep(theUser,nDays[theUser]),day=c(1:nDays[theUser]))
  theUserData$y <- runif(nDays[theUser]) < usageRate[theUser]
  
  userData <- bind_rows(userData,theUserData)
}

## @knitr BayesLearningExample2

selUsers <- c(10,11,98)

theUsers <- bind_rows(
  data.frame(posteriorUser(userData$y[userData$userID==selUsers[1]]),user=selUsers[1]),
  data.frame(posteriorUser(userData$y[userData$userID==selUsers[2]]),user=selUsers[2]),
  data.frame(posteriorUser(userData$y[userData$userID==selUsers[3]]),user=selUsers[3])
)

theUsers %>%
  mutate(user=factor(user)) %>%
  ggplot(aes(x=x,ymin=0,y=y,group=user,fill=user)) + 
  geom_area(alpha=0.5,position = position_dodge(width = 0.0)) + 
  theme_bw()+
  labs(x='Usage Rate',
       title='Posterior Distribution of Usage Rate for Three Users')



## @knitr TopicModelExample

vocab <- c("messi","ronaldo","neymar","offside","pitch","goal","kick","foul",
           "corner","forward","back","penalty","header","tackle","throwin",
           "basket","point","lebron","guard","jordan","block","paint","post","rebound",
           "screen","zone","airball","backcourt","buzzer","carry")


beta <- data.frame(word = vocab, 
                  topic1 = c(0.1,0.1,0.05,0.05,0.05,0.06,0.07,0.08,0.08,0.04,0.04,0.05,0.05,0.04,0.04,
                             0.0,0.05,0.0,0.0,0.0,0.0,0.0,0.05,0.0,0.0,0.0,0.0,0.0,0.0,0.0),
                  topic2 = c(0.0,0.0,0.0,0.0,0.0,0.05,0.0,0.05,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                             0.0,0.05,0.1,0.1,0.05,0.1,0.05,0.05,0.06,0.05,0.1,0.05,0.04,0.05,0.05))

topicWordDist <- beta %>% 
  mutate(word = factor(word,levels=rev(as.character(beta$word)))) %>%
  gather(topic,value,-word) %>%
  ggplot(aes(x=word,y=value)) + geom_bar(stat='identity') + coord_flip() + facet_wrap(~topic)

docLength <- 20
vocabLength <- length(vocab)

nDoc <- 3
nTopic <- 2
gamma <- array(0,c(nDoc,2))
gamma[1,] <- c(0.9,0.1)
gamma[2,] <- c(0.05,0.95)
gamma[3,] <- c(0.4,0.6)

topicDist <- data.frame(gamma) %>%
  rename(topic1 = X1, topic2 =  X2) %>%
  rownames_to_column(var="doc") %>%
  gather(topic,value,-doc) %>%
  mutate(doc = paste("doc",doc)) %>%
  ggplot(aes(x=topic,y=value)) + geom_bar(stat='identity') + facet_wrap(~doc) +
  labs(y="theta")

wordIndex <- rep(0,docLength)
doc <- rep("",nDoc)

for (theDoc in c(1:nDoc)){
  
  ## sample topics
  z <- sample( c(1:nTopic), docLength, replace=TRUE, prob=gamma[theDoc,])
  ## sample words
  for (theWord in c(1:docLength)){
    wordIndex[theWord] <- sample(c(1:vocabLength),1,prob = beta[,z[theWord]+1])
  }
  
  doc[theDoc] <- paste(vocab[wordIndex],collapse=" ") 
}


## @knitr SetUpTripReviews

## load review data
reviews <- read_rds('data/ariaTrip.rds')

## get word count
reviewsTidy <- reviews %>%
  select(reviewID,reviewText) %>%
  unnest_tokens(word, reviewText)%>%
  anti_join(stop_words)

wordCount <- reviewsTidy %>%
  count(word,sort = TRUE)
                  
## remove words that are too common (they won't help us in identifying topics since they occur everywhere)
## and stem reviews 
commonWords <- c('hotel','aria','vegas')

reviewsTidy <- reviewsTidy %>%
  filter(!word %in% commonWords) %>%
  mutate(word = wordStem(word))

wordCount <- reviewsTidy %>%
  count(word,sort = TRUE)

## remove infrequent words - they are also not interesting
## here we just pick the top 3000 words

wordCut <- 3000

vocab <- wordCount %>%
  slice(1:wordCut)
  
reviewsTidy <- reviewsTidy %>%
  filter(word %in% vocab$word)

## count words per review 
reviewLength <- reviewsTidy %>%
  count(reviewID)

minLength <- 20

reviewLength <- reviewLength %>%
  filter(n >= minLength)

## create document term matrix for use in LDA 

dtm <- reviewsTidy %>%
  filter(reviewID %in% reviewLength$reviewID) %>%
  count(reviewID,word) %>%
  cast_dtm(reviewID, word, n)



## @knitr runLDATrip


##
## Warning: this takes a while to run 
##
# numTopics <- c(20,30,40)
#  
# for (theNum in c(1:length(numTopics))){
#   theLDA <- LDA(dtm, k = numTopics[theNum], method="Gibbs",
#                 control = list(alpha = 1/numTopics[theNum],iter=5000,burnin=1000,seed = 1234))
#   
#    saveRDS(theLDA,file=paste0('ldaTrip',numTopics[theNum],'.rds'))
# }


theNumTopics <- 30
theLDA <- read_rds(paste0('data/ldaTrip',theNumTopics,'.rds'))

## study estimated topics - look at top words in each topic
theTopicsBeta <- tidy(theLDA, matrix = "beta")

TopicsTop <- theTopicsBeta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ungroup() %>%
  mutate(x = n():1)  # for plotting

plTopicWeights <- TopicsTop %>%
  mutate(topic=factor(topic)) %>%
  ggplot(aes(x=x,y=beta,fill=topic)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~topic,scales='free') +
  scale_x_continuous(breaks = TopicsTop$x,
                     labels = TopicsTop$term,
                     expand = c(0,0)) + 
  labs(title='Top Words by Topic',
       subtitle = paste0(theNumTopics,' Topic LDA of ',
                         prettyNum(nrow(reviewLength),big.mark=",",scientific=FALSE), ' TripAdvisor Reviews'),
       x = 'word',
       y = 'beta')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6),
        axis.text.y = element_text(size = 5))

theTopicsGamma <- tidy(theLDA, matrix = "gamma")

theTopicsGammaMeta <- theTopicsGamma %>%
  inner_join(reviews,by=c("document"="reviewID"))




## @knitr ldaViz

theTopicsBetaW <- select(spread(tidy(theLDA, matrix = "beta"),term,beta),-topic)
theTopicsGammaW <- select(spread(tidy(theLDA, matrix = "gamma"),topic,gamma),-document)
theTerms <- colnames(theTopicsBetaW)

theVocab <- vocab %>%
  mutate(word = factor(word,levels=theTerms)) %>%
  arrange(word) %>%
  mutate(word=as.character(word))

json <- createJSON(
  phi = theTopicsBetaW, 
  theta = theTopicsGammaW, 
  doc.length = reviewLength$n, 
  vocab = theTerms, 
  R = theNumTopics,
  term.frequency = theVocab$n
)

serVis(json)


## @knitr TripLDAMeta

tmp <- theTopicsGamma %>%
  spread(topic,gamma) %>%
  inner_join(select(reviews,reviewID,reviewRating),by=c('document'='reviewID'))

meanRating <- mean(tmp$reviewRating)
  
gammaRating <- data.frame(cor = cor(select(tmp,-document,-reviewRating),tmp$reviewRating)) %>%
  rownames_to_column(var='topic')

corPL <- gammaRating %>%
  ggplot(aes(x=fct_reorder(topic,cor),y=cor)) + 
  geom_point(size=2)+ geom_hline(aes(yintercept=0)) + 
  labs(title='Correlation between Topic Weight and Review Rating',
       x = 'Topic Index',
       y = 'Correlation')


theTopics <- c(8,6,2,1)

tmp <- theTopicsGamma %>%
  filter(topic %in% theTopics) %>%
  inner_join(select(reviews,reviewID,reviewRating),by=c('document'='reviewID')) %>%
  mutate(topic=factor(paste0('Topic ',topic)))

tmpMeans <- tmp %>%
  group_by(topic,reviewRating) %>%
  summarize(mean=mean(gamma)) 

scatterPL <- tmp %>%
  ggplot(aes(x=reviewRating,y=gamma,color=topic)) + 
    geom_jitter(size=0.3,alpha=0.5,show.legend = F) +
    geom_point(data=tmpMeans,aes(y=mean),size=2,color='black',show.legend = F)+
    facet_wrap(~topic,scales='free')+
  labs(x='Review Rating',
       title='Topic Weights by Review Rating',
       caption='Black dots represent average gamma weight')



gammaCut <- 0.33

extremeReviews <- theTopicsGamma %>%
  inner_join(select(reviews,reviewID,reviewRating),by=c('document'='reviewID')) %>%
  group_by(topic) %>%
  filter(gamma > 0.33) %>%
  summarize(nExtreme = n(),
    meanDelta = mean(reviewRating) - meanRating) %>%
  mutate(topic=factor(topic),
         Delta = meanDelta > 0) %>%
  ggplot(aes(x=fct_reorder(topic,meanDelta),y=meanDelta,fill=Delta)) + 
  geom_col(show.legend = FALSE) + coord_flip() + 
  labs(title = 'Average Review Ratings for Reviews Focused on a Primary Topic',
    x = 'Topic',
    caption = 'Note: A Primary Topic is defined as a topic with a gamma weight bigger than 0.33',
       y = 'Mean Review Rating Compared to Overall Average')




## @knitr SetUpKindleReviews

## load review data
reviews <- read_rds('data/kindle.rds')

## get word count
reviewsTidy <- reviews %>%
  select(reviewerID,reviewText) %>%
  unnest_tokens(word, reviewText)%>%
  anti_join(stop_words)

wordCount <- reviewsTidy %>%
  count(word,sort = TRUE)

## remove words that are too common (they won't help us in identifying topics since they occur everywhere)
## and stem reviews 
commonWords <- c('kindle','fire')

reviewsTidy <- reviewsTidy %>%
  filter(!word %in% commonWords) %>%
  mutate(word = wordStem(word))

wordCount <- reviewsTidy %>%
  count(word,sort = TRUE)

## remove infrequent words - they are also not interesting
## here we just pick the top 3000 words

wordCut <- 2000

vocab <- wordCount %>%
  slice(1:wordCut)

reviewsTidy <- reviewsTidy %>%
  filter(word %in% vocab$word)

## count words per review 
reviewLength <- reviewsTidy %>%
  count(reviewerID)

minLength <- 10

reviewLength <- reviewLength %>%
  filter(n >= minLength)


## create document term matrix for use in LDA 

dtm <- reviewsTidy %>%
  filter(reviewerID %in% reviewLength$reviewerID) %>%
  count(reviewerID,word) %>%
  cast_dtm(reviewerID, word, n)

## @knitr runLDAKindle


numTopics <- 10
numTopics <- 15
tripLDA <- LDA(dtm, k = numTopics, method="Gibbs",
               control = list(alpha = 1/numTopics,iter=5000,burnin=1000,thin=10,seed = 1234)
)

saveRDS(tripLDA,file=paste0('ldaKindle',numTopics,'.rds'))


theLDA <- read_rds('ldaKindle15.rds')

theTopics <- tidy(theLDA, matrix = "beta")

TopicsTop <- theTopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ungroup() %>%
  mutate(x = n():1)  # for plotting

TopicsTop %>%
  mutate(topic=factor(topic)) %>%
  ggplot(aes(x=x,y=beta,fill=topic)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~topic,scales='free') +
  scale_x_continuous(breaks = TopicsTop$x,
                     labels = TopicsTop$term,
                     expand = c(0,0)) + 
  labs(title='Top Words by Topic',
       #subtitle = paste0(nReviews,' reviews from Tripadvisor'),
       #caption = 'Note: The nouns "hotel" and "room" has been removed.',
       x = 'word',
       y = 'beta')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





## @knitr SetUpMonAmiGabiReviews

## load review data
reviews <- read_rds('data/Mon_Ami_Gabi_Yelp_Reviews.rds') %>%
  mutate(text = gsub("[^\x20-\x7E]", "", text))


## get word count
reviewsTidy <- reviews %>%
  select(review_id,text) %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

wordCount <- reviewsTidy %>%
  count(word,sort = TRUE)

## remove words that are too common (they won't help us in identifying topics since they occur everywhere)
## and stem reviews 
commonWords <- c('food')

reviewsTidy <- reviewsTidy %>%
  filter(!word %in% commonWords) %>%
  mutate(word = wordStem(word))

wordCount <- reviewsTidy %>%
  count(word,sort = TRUE)

## remove infrequent words - they are also not interesting
## here we just pick the top 3000 words

wordCut <- 2500

vocab <- wordCount %>%
  slice(1:wordCut)

reviewsTidy <- reviewsTidy %>%
  filter(word %in% vocab$word)

## count words per review 
reviewLength <- reviewsTidy %>%
  count(review_id)

minLength <- 20

reviewLength <- reviewLength %>%
  filter(n >= minLength)


## create document term matrix for use in LDA 

dtm <- reviewsTidy %>%
  filter(review_id %in% reviewLength$review_id) %>%
  count(review_id,word) %>%
  cast_dtm(review_id, word, n)

termVector <- dtm$dimnames$Terms
docVector <- dtm$dimnames$Docs


## @knitr runLDAMonAmiGabi

numTopics <- c(10,15,20)
 
for (theNum in c(1:length(numTopics))){
   theLDA <- LDA(dtm, k = numTopics[theNum], method="Gibbs",
                  control = list(alpha = 1/numTopics[theNum],iter=5000,burnin=1000,seed = 1234))
   saveRDS(theLDA,file=paste0('ldaMonAmiGabi',numTopics[theNum],'.rds'))
 }


theLDA <- read_rds('ldaMonAmiGabi20.rds')

theTopics <- tidy(theLDA, matrix = "beta")

TopicsTop <- theTopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ungroup() %>%
  mutate(x = n():1)  # for plotting

TopicsTop %>%
  mutate(topic=factor(topic)) %>%
  ggplot(aes(x=x,y=beta,fill=topic)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~topic,scales='free') +
  scale_x_continuous(breaks = TopicsTop$x,
                     labels = TopicsTop$term,
                     expand = c(0,0)) + 
  labs(title='Top Words by Topic',
       #subtitle = paste0(nReviews,' reviews from Tripadvisor'),
       #caption = 'Note: The nouns "hotel" and "room" has been removed.',
       x = 'word',
       y = 'beta')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



theTopicsGamma <- tidy(theLDA, matrix = "gamma")

theTopicsGammaMeta <- theTopicsGamma %>%
  inner_join(reviews,by=c("document"="review_id"))


