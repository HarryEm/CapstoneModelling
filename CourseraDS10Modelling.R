# Load libraries
library(tm)
library(quanteda)
library(readtext)

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
