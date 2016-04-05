library(tm)
library(SnowballC)

## PRE PROCESSING STAGE
cname<-file.path("~", "Data Science Specialization", "10 Data Science Capstone", "Dataset", "en_US")
docs <- Corpus(DirSource(cname)) 
#Remove punctuation from texts
docs <- tm_map(docs, removePunctuation)
#Remove numbers
docs <- tm_map(docs, removeNumbers)   
#Change everything to lowercase
docs <- tm_map(docs, tolower)  
#Remove stopwords for english language (a, and, also, the, etc)
docs <- tm_map(docs, removeWords, stopwords("english"))
#Removing commond word endings ("ing", "es", "s", etc. Stemming)
docs <- tm_map(docs, stemDocument)   
#Stripping unnecesary whitespace from your documents
docs <- tm_map(docs, stripWhitespace)  
#Treat pre-processed docs as text documents
docs <- tm_map(docs, PlainTextDocument)   

## STAGE THE DATA
#Create a document term matrix
dtm <- DocumentTermMatrix(docs)  
# Get a transpose of the matrix
tdm <- TermDocumentMatrix(docs)   

##Explore the data
#Organize terms by their frequency
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
#Export matrix to Excel
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv")  
#Remove sparse terms from a document-term or term-document matrix
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
#Check the most and least frequently occuring words
freq[head(ord)]   
# Check the frequency of frecuencies
head(table(freq), 20)   
tail(table(freq), 20) 
# For a less, fine-grained look at term freqency we can view a table of the terms we selected when we removed sparse terms
freq <- colSums(as.matrix(dtms))   
freq 

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)  
##This will identify all terms that appear frequently (in this case, 50 or more times) on the dtm.
findFreqTerms(dtm, lowfreq=50)
##Another way to do the same thing
wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 

#PLOT WORD FREQUENCIES
library(ggplot2)   
p <- ggplot(subset(wf, freq>2), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p  
#Relationship between terms
findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) # specifying a correlation limit of 0.98   
#same excercise with less correlation and a small matrix dtms
findAssocs(dtms, "mexico", corlimit=0.90)

library(wordcloud)  
#Plot words that occur at least 50000 times.
set.seed(10)   
wordcloud(names(freq), freq, min.freq=2)  
wordcloud(names(freq), freq, min.freq=2, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   
#Plot the 100 most frequently occuring words
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=30, rot.per=0.2, colors=dark2)   

##Clustering by Term Similarity
#First you should always first remove a lot of the uninteresting or infrequent words
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.  
#Hierarchal Clustering First calculate distance between words & then cluster them according to similarity.
library(cluster)   
#First calculate distance between words & then cluster them according to similarity
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")  
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   

#Apply k-means clustering
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  

