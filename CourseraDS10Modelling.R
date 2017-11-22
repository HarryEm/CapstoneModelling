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
library(parallel)

# Load in data

no_cores <- detectCores() - 1

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

qcorp <- corpus(readtext("./JHU_Capstone/Data/Samples/"))

ngram1 <- tokenize(qcorp, what = "word",remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE, simplify = TRUE, skip = 1, ngrams = 1 ,verbose=TRUE)

ngram2 <- tokenize(qcorp, what = "word",remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE, simplify = TRUE, skip = 1,  ngrams = 2 ,verbose=TRUE)

ngram3 <- tokenize(qcorp, what = "word",remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE, simplify = TRUE, skip = 1,  ngrams = 3 ,verbose=TRUE)

ngram4 <- tokenize(qcorp, what = "word",remove_numbers = TRUE, remove_punct = TRUE, 
                   remove_symbols = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, 
                   remove_url = TRUE, simplify = TRUE, skip = 1,  ngrams = 4 ,verbose=TRUE)

# https://www.rdocumentation.org/packages/quanteda/versions/0.99.12/topics/tokenize

dfm1 <- dfm(ngram1, tolower = TRUE, stem = TRUE, select = NULL, remove = NULL,
            dictionary = NULL)

dfm2 <- dfm(ngram2, tolower = TRUE, stem = TRUE, select = NULL, remove = NULL,
            dictionary = NULL)

dfm3 <- dfm(ngram3, tolower = TRUE, stem = TRUE, select = NULL, remove = NULL,
            dictionary = NULL)

dfm4 <- dfm(ngram4, tolower = TRUE, stem = TRUE, select = NULL, remove = NULL,
            dictionary = NULL)

df1 <- data.frame(Content = features(dfm1), Frequency = colSums(dfm1), 
                 row.names = NULL, stringsAsFactors = FALSE)

df2 <- data.frame(Content = features(dfm2), Frequency = colSums(dfm2), 
                  row.names = NULL, stringsAsFactors = FALSE)

df3 <- data.frame(Content = features(dfm3), Frequency = colSums(dfm3), 
                  row.names = NULL, stringsAsFactors = FALSE)

df4 <- data.frame(Content = features(dfm4), Frequency = colSums(dfm4), 
                  row.names = NULL, stringsAsFactors = FALSE)

df2 <- data.frame(do.call('rbind', strsplit(as.character(df2$Content), "_(?=[^_]+$)", perl=TRUE)),df2$Frequency)
df3 <- data.frame(do.call('rbind', strsplit(as.character(df3$Content), "_(?=[^_]+$)", perl=TRUE)),df3$Frequency)
df4 <- data.frame(do.call('rbind', strsplit(as.character(df4$Content), "_(?=[^_]+$)", perl=TRUE)),df4$Frequency)

prediciton <- as.character(df2[df2$X1=="it's",][which.max(df2[df2$X1=="it's",]$df2.Frequency),2])

predict_word3 <- function(txt) {
  prediciton <- as.character(df3[df3$X1==txt,][which.max(df3[df3$X1==txt,]$df3.Frequency),2])
  return(prediciton)
}

predict_word4 <- function(txt) {
  prediciton <- as.character(df4[df4$X1==txt,][which.max(df4[df4$X1==txt,]$df4.Frequency),2])
  return(prediciton)
}

x <- "faith during the"

linestiwtter[grep(x,linestiwtter)]
linesnews[grep(x,linesnews)]
linesblogs[grep(x,linesblogs)]



