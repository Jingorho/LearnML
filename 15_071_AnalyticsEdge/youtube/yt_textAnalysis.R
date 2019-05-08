# 上位200ワードを取り除いたtextデータで、cart
# テキストのクラスタ分析
# 分布から「ここの部分取り出してみたら」
# テキストの意味を鑑みる分析(?)
# logistic, linear, 


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
INvideo <- read.csv("INvideo_pd.csv")
video <- USvideo
video <- INvideo
head(video)


###############################
# テキストの前処理
###############################
# textall <- video$description
# text90 <- video$description[which(video$pop90==TRUE)]
# textall <- video$tags
# text90 <- video$tags[which(video$pop90==TRUE)]
textall <- video$title
text90 <- video$title[which(video$pop90==FALSE)]
text10 <- video$title[which(video$pop90==TRUE)]
length(textall); length(text90); length(text10)

removeTerms <- c(stopwords("english"), "youtube", "video", "channel", "that", 
                 "show", "watch", "can", "make", "us", "use", "subscribe",
                 "facebook", "twitter", "instagram", "•", "youtub", "–",
                 "2017", "2018", "will")
onlywords10 <- data.frame()
formerwords <- ""
formerwords200 <- ""
latterwords <- ""
latterwords200 <- ""

# ------------
# INとUS用の関数 : コーパスを作る
# ------------
makeCorpus <- function(t){
  # なぜか削除できない単語を無理やり削除
  t <- gsub("–", "", t)
  t <- gsub("video", "", t)
  t <- gsub("subscribe", "", t)
  
  corpus = Corpus(VectorSource(t)) # Warning出るけどVCorpusにするとrpartできない
  corpus = tm_map(corpus, tolower)
  if (!("PlainTextDocument" %in% class(corpus[[1]]))) {
    corpus = tm_map(corpus, PlainTextDocument)
  }
  
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, removeTerms) # ほかあれば追加
  corpus = tm_map(corpus, stemDocument)
  return (corpus)
}







###############################
# ワードクラウド
# 英語と日本語の違い上、JPと、USとIN、で処理が分かれる
###############################

# ------------
# USとIN : ワードクラウド
# ------------
makeWordFreq <- function(t){
  corpus <- makeCorpus(t)
  dtm <- TermDocumentMatrix(corpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  # d <- subset(d, d$word != "subscrib") # なんかINの"subscrib"という単語だけなぜか削除できないので力技で消す
  # head(d, 10)
  d <- d[order(d$freq, decreasing = T),]
  rownames(d) <- as.character(c(1:length(d$freq)))
  return(d)
}


makeWordFreq_wc <- function(){
  wordsall <- makeWordFreq(textall)
  words90 <- makeWordFreq(text90)
  words10 <- makeWordFreq(text10)
  length(tall); length(t90); length(t10);
  length(wordsall$word); length(words90$word); length(words10$word);
  
  onlypop10_words <- setdiff(words10$word, words90$word) # pop10%にあってallにないやつ
  onlywords10 <- data.frame()
  for(i in c(1:length(onlypop10_words))){
    temp <- words10[(words10$word == onlypop10_words[i]),]
    onlywords10 <- rbind(onlywords10, temp)
  }
  id <- round(length(words10$word)*0.5)
  formerwords <- wordsall[1:id,]
  formerwords200 <- wordsall[1:200,]
  latterwords <- wordsall[id:length(wordsall$freq),]
  latterwords200 <- wordsall[200:length(wordsall$freq),]
  head(formerwords200); dim(formerwords200)
  write.csv(formerwords200, "IN_formerwords200.csv")
}



# 実行
makeWordFreq_wc()
# write.csv(formerwords, "IN_former_words.csv")
# write.csv(formerwords200, "IN_former200_words.csv")
# write.csv(onlywords10, "IN_onlyWords10.csv")

# ワードクラウド描画!
wordcloud(onlywords10$word, onlywords10$freq, 
          color=rainbow(5), random.order=F, random.color=F)
# latter_words10 <- words10[former_id:length(words10$freq),]
# wordcloud(latter_words10$word, latter_words10$freq, 
#           color=rainbow(5), random.order=F, random.color=F)
wordcloud(latterwords$word, latterwords$freq, 
          color=rainbow(5), random.order=F, random.color=F)
wordcloud(latterwords200$word, latterwords200$freq, 
          color=rainbow(5), random.order=F, random.color=F)
# 時間かかるというか終わらないので、キリのいいところで強制終了して止めるしかない...?





###############################
# tree
# 英語と日本語の違い上、JPと、USとIN、で処理が分かれる
# JPの処理はまだとちゅう
###############################

# ------------
# USとIN
# ------------
# length(textall); length(text10); length(text90);
length(onlywords10$word)
makeDocumentTerms <- function(t, p){
  corpus <- makeCorpus(t)
  dtm <- DocumentTermMatrix(corpus)
  sparse = removeSparseTerms(dtm, p) # 0.9を残す(sarseな0.1を除く)
  # str(sparse)
  dt = as.data.frame(as.matrix(sparse))
  colnames(dt) <- make.names(colnames(dt))
  dt <- dt[,!duplicated(colnames(dt))]
  # str(dt)
  # dim(dt)
  return(dt)
}

# t <- textall
# p <- 0.99
docterm <- makeDocumentTerms(textall, 0.995)
head(docterm); dim(docterm)
write.csv(docterm, "IN_textall.csv", col.names=T, row.names=F)
docterm <- docterm[, !(colnames(docterm) %in% former_words$word)]
docterm <- docterm[, !(colnames(docterm) %in% former200_words$word)]
dim(docterm)
str(docterm)
head(docterm)


sum(video$pop90)
v <- video[video$pop90,]
v <- v[-removedFile,]
# 目的変数はpopXX
colnames(v)
summary(v$descLen)
docterm$pop90 = (v$descLen < 100)
# docterm$pop = video$pop_com_lR
# sum(docterm$pop90)
# str(docterm)

# Reciataionでは日付を元にsplitしてたけど、今回は単にランダムに6:4でtrain:test
split1 = sample(row.names(docterm), 0.6*nrow(docterm))
split2 = setdiff(row.names(docterm), split1)
train = docterm[split1,]
test = docterm[split2,]
dim(train)
cart = rpart(pop90 ~ ., data = docterm, method = "class", cp = .00000003)
prp(cart, cex=0.8)
sum(train$pop90)

