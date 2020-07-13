# Code for text analysis of CSR reports
# First part information about all companies in excel file
# Descriptive results in graphs

rm(list=ls())
library(tidyverse)
library(ggplot2)
setwd("~/Documents/dvwork")
# Excel files containing information of CSR of all industries submitted to GRI
datafile <- read.csv("~/Documents/dvwork/alldata1.csv",sep=";")

head(datafile)
str(datafile)

# Graph of reports by size of company
ggplot(datafile, aes(x = as.character.Date(Publication.Year) , fill= Size)) +
  geom_bar()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x="Publication year", y="Number of reports", title="Reports by size of company")+
  theme(plot.title = element_text(hjust = 0.5))
  
# Graph of reports by region wise
ggplot(datafile, aes(x = as.character.Date(Publication.Year) , fill= Region)) +
  geom_bar()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x="Publication year", y="Number of reports", title="Region wise reports ")+
  theme(plot.title = element_text(hjust = 0.5))  
  
# Graph of reports by countryStatus wise
ggplot(datafile, aes(x = as.character.Date(Publication.Year) , fill= Country.Status)) +
  geom_bar()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x="Publication year", y="Number of reports", title="Country status wise reports ")+
  theme(plot.title = element_text(hjust = 0.5))
  gc()
  
#####################################Q2###############################

# Text analysis by using text mining techniques
library(NLP)
library(tm)
pathname <- file.path("~","Documents","dvwork")
setwd("~/Documents/dvwork/")
dir(pathname)
docs <- VCorpus(DirSource(pathname), readerControl = list(language="eng"))
inspect(docs[1])
# Removing meta data
ids <- sapply(1:length(docs), function(x) meta(docs[[x]], "Id"))
metas <- as.numeric(sapply(ids, function(x) sub("[0-9]+_([0-9]+)\\.txt","\\1",x)))
for (j in seq(docs))
{
  docs[[j]] <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", docs[[j]])
}
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower)
length(stopwords("eng")) 
docs <- tm_map(docs, removeWords, stopwords("english"))
  
toremove1 <- c("also", "based", "board", "company", "costs", "customers", "december", "director","group","including", "local", "number","project", "reporting", "rate", "service", "well", "year")
docs <- tm_map(docs, removeWords, toremove1)
  
writeLines(as.character(docs[1]))
docs <- tm_map(docs, stripWhitespace)

gc() # for memory release
  
docs <- tm_map(docs, PlainTextDocument)
  
dtm <-  DocumentTermMatrix(docs) # Document term matrix or bag-of-words
  
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq) 
m <- as.matrix(dtm)   
dim(m)
dtms <- removeSparseTerms(dtm, 0.10) # for dimension reduction
dim(dtms)
freq[head(ord)] 
freq[tail(ord)]
# Word association, correlation of of perticular word with others
# Range of this correlation is between 0-1.
wordasso <- findAssocs(dtms, "corporate", corlimit = 0.5)
freq <- sort(colSums(as.matrix(dtms)), decreasing=TRUE)
head(freq, 14)
findFreqTerms(dtm, lowfreq=150)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)
# Split data set to train and test
library(caTools)
ind <- sample.split(wf, SplitRatio = 0.7)
train_data <- wf[ind,]
test_dat <- wf[!ind,]
library(ggplot2)
gc()
p <- ggplot(subset(test_dat, freq>8000), aes(word, freq))
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p
# wordcloud
library(RColorBrewer)
library(wordcloud)
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=200, rot.per=0.1, colors=dark2)
  
# dendogram
library(cluster)   
d <- dist(subset(train_data, freq>10000), method="euclidian")   
fit <- hclust(d=d, method="complete")
plot(fit, hang=0.7)
rect.hclust(fit, k=6, border = "red")
  
associations <- findAssocs(dtms, "corporate", corlimit = 0.3)
  
associations <- as.data.frame(associations)
  
associations_csv <- write.csv(associations, "~/Documents/dvwork/datacsv1.csv")
associations_csv <- read.csv("~/Documents/dvwork/datacsv1.csv")
head(associations_csv)
str(associations)
ggplot(associations_csv, aes(x = corporate, y =X))+
  geom_point()
  
