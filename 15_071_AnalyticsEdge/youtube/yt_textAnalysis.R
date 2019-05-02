# 整理前

# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# # https://exploratory.io/note/2ac8ae888097/Mecab-RMeCab-0944283373151109
# install.packages("RMeCab", repos = "http://rmecab.jp/R")

library(ggplot2)
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library("wordcloud")
library("RColorBrewer")
library(RMeCab)


options(scipen=999)
setwd("/Users/yukako/WorkSpace/ML/15_071_AnalyticsEdge/Youtube/data")

USvideo <- read.csv("USvideo_pd.csv")
JPvideo <- read.csv("JPvideo_pd.csv")
INvideo <- read.csv("INvideo_pd.csv")
# video <- USvideo
video <- JPvideo
# video <- INvideo
# head(video)


# month <- 6
# text <- subset(as.character(video$description), video$publish_month==month)
text <- as.character(video$description)

RMeCabFreq(text)

makeDocumentTerms <- function(analyzedText, atleaset_percentage){
  # なぜかremoveWordsで"video"がremoveできないので無理やり削除
  analyzedText <- gsub("video", "", analyzedText)
  
  corpus = Corpus(VectorSource(analyzedText))
  corpus = tm_map(corpus, tolower)
  if (!("PlainTextDocument" %in% class(corpus[[1]]))) {
    corpus = tm_map(corpus, PlainTextDocument)
  }
  corpus = tm_map(corpus, removeWords,
                  c(stopwords("english"),
                    "youtube", "video", "channel", "that",
                    "show", "watch", "can", "make", "subscribe")) # ほかあれば
  corpus = tm_map(corpus, stemDocument)
  
  frequencies = DocumentTermMatrix(corpus)
  findFreqTerms(frequencies, lowfreq=1000)
  findFreqTerms(frequencies, lowfreq=500)
  sparse = removeSparseTerms(frequencies, atleaset_percentage); dim(sparse)
  document_terms = as.data.frame(as.matrix(sparse))
  str(document_terms); dim(document_terms)
  # head(document_terms)
  
  return (document_terms)
}

document_terms <- makeDocumentTerms(text, 0.90) # 0.99がrecitationのデフォ

# 目的変数はpop(めちゃ人気かどうか)
document_terms$pop = video$pop
# document_terms$pop = subset(video$pop, video$publish_month == month)

# Reciataionでは日付を元にsplitしてたけど、今回は単にランダムに6:4でtrain:test
split1 = sample(row.names(document_terms), 0.6*nrow(document_terms))
split2 = setdiff(row.names(document_terms), split1)
train = document_terms[split1,]
test = document_terms[split2,]
cart = rpart(pop ~ ., data=train, method="class", cp = .003)
prp(cart, cex=0.8)

