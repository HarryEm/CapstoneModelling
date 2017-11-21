# Load in data

if(!dir.exists("./JHU_Capstone/Data")) {
  dir.create("./JHU_Capstone/Data")}

url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

if(!file.exists("./JHU_Capstone/Data/SwiftkeyData.zip")) {
  download.file(url,"./JHU_Capstone/Data/SwiftkeyData.zip",method="curl")}

# Subset data

