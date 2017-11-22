# The most important thing I've learned from this exercise to take the opposite approach of what you
# want to. Instead of trying to load all the data in and get a really polished model from the outset,
# I started by seeing how little data and how basic a model I could get to come up with some kind
# of prediction, even if it performed very poorly. Once the structure is in place the refinements
# come far more easily.

# Load libraries
library(tm)
library(quanteda)
library(readtext)
library(dplyr)

# Load in data

if(!dir.exists("./JHU_Capstone/Data")) {
  dir.create("./JHU_Capstone/Data")}

url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

if(!file.exists("./JHU_Capstone/Data/SwiftkeyData.zip")) {
  download.file(url,"./JHU_Capstone/Data/SwiftkeyData.zip",method="curl")}

# Subset data

if(!dir.exists("./JHU_Capstone/Data/Samples")) {
  dir.create("./JHU_Capstone/Data/Samples")}

set.seed(1966)

textSample <- function(filename,samplename,prop) {
  if(!file.exists(samplename)) {
    
    con <- file(filename, "r")
    linestext <- readLines(con)
    close(con)
    
    samptext <- linestext[sample(1:length(linestext),round(length(linestext)*prop,0),FALSE)]
    
    con <- file(samplename, "w")
    writeLines(samptext,con)
    close(con)
  }
  else {
    print("This file already exists")
  }
}

propn <- 0.01 # use 1% of the data for the sample

textSample("./Data/final/en_US/en_US.twitter.txt","./JHU_Capstone/Data/Samples/en_US.twitter_Sample1.txt",propn)
textSample("./Data/final/en_US/en_US.news.txt","./JHU_Capstone/Data/Samples/en_US.news_Sample1.txt",propn)
textSample("./Data/final/en_US/en_US.blogs.txt","./JHU_Capstone/Data/Samples/en_US.blogs_Sample1.txt",propn)


# Preprocesing Data

corp <- VCorpus(DirSource("./JHU_Capstone/Data/Samples"),readerControl = list(language="en_US"))

corp <- tm_map(corp, content_transformer(tolower)) #converts to lower case
corp <- tm_map(corp, stripWhitespace) # removes superfluous white space
corp <- tm_map(corp, stemDocument) # perform stemming so misspelt words are corrected - what happens if I don't do this??
corp <- tm_map(corp, removePunctuation) #removes punctuation, how about words like "New-York", "I'm", "m.p.h."?
corp <- tm_map(corp, removeNumbers) #removes numbers
# corp <- tm_map(corp, removeWords, stopwords("english")) we lose some of the analysis here
corp <- tm_map(corp, PlainTextDocument)


mytf <- readtext("./JHU_Capstone/Data/Samples/*.txt")
qcorp <- corpus(mytf) # in one step?

qcorp <- corpus(readtext("./JHU_Capstone/Data/Samples/"))

ngram1 <- tokenize(qcorp, what = "word",remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE, simplify = TRUE, ngrams = 1 ,verbose=TRUE)

ngram2 <- tokenize(qcorp, what = "word",remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE, simplify = TRUE, ngrams = 2 ,verbose=TRUE)

ngram3 <- tokenize(qcorp, what = "word",remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE, simplify = TRUE, ngrams = 3 ,verbose=TRUE)

# https://www.rdocumentation.org/packages/quanteda/versions/0.99.12/topics/tokenize

dfm1 <- dfm(ngram1, tolower = TRUE, stem = FALSE, select = NULL, remove = NULL,
            dictionary = NULL)

dfm2 <- dfm(ngram2, tolower = TRUE, stem = FALSE, select = NULL, remove = NULL,
            dictionary = NULL)

dfm3 <- dfm(ngram3, tolower = TRUE, stem = FALSE, select = NULL, remove = NULL,
            dictionary = NULL)

df1 <- data.frame(Content = features(dfm1), Frequency = colSums(dfm1), 
                 row.names = NULL, stringsAsFactors = FALSE)

df2 <- data.frame(Content = features(dfm2), Frequency = colSums(dfm2), 
                  row.names = NULL, stringsAsFactors = FALSE)

df3 <- data.frame(Content = features(dfm3), Frequency = colSums(dfm3), 
                  row.names = NULL, stringsAsFactors = FALSE)

strsplit("it's_been_a", "_(?=[^_]+$)", perl=TRUE)
df2a <- data.frame(do.call('rbind', strsplit(as.character(df2$Content), "_(?=[^_]+$)", perl=TRUE)),df2$Frequency)
df3a <- data.frame(do.call('rbind', strsplit(as.character(df3$Content), "_(?=[^_]+$)", perl=TRUE)),df3$Frequency)

prediciton <- as.character(df2a[df2a$X1=="it's",][which.max(df2a[df2a$X1=="it's",]$df2.Frequency),2])

predict_word <- function(txt) {
  prediciton <- as.character(df2a[df2a$X1==txt,][which.max(df2a[df2a$X1==txt,]$df2.Frequency),2])
  return(prediciton)
}

