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
video <- USvideo
video <- JPvideo
video <- INvideo
head(video)




###############################
# よく使う処理をまとめて関数にした
# 一回読み込んだら、中身を編集しない限り、繰り返し使えます。
# 使用例:
# 分析結果のドキュメント <- make_nounverb_JP(分析するテキスト, 抽出する単語の頻出度(0.90なら上位90%))
###############################

# ------------
# INとUS用の関数 : コーパスを作る
# ------------
makeCorpus <- function(analyzedText){
  corpus = Corpus(VectorSource(analyzedText))
  corpus = tm_map(corpus, tolower)
  if (!("PlainTextDocument" %in% class(corpus[[1]]))) {
    corpus = tm_map(corpus, PlainTextDocument)
  }
  corpus = tm_map(corpus, removeWords, removeTerms) # ほかあれば
  corpus = tm_map(corpus, stemDocument)
  return (corpus)
}

# ------------
# JP用の関数 : 名詞と動詞だけのリスト作成
# ------------
make_nounverb_JP <- function(filename){
  JPtextFreq <- RMeCabFreq(filename)
  JPtext_nv <- JPtextFreq[JPtextFreq$Info1 == "名詞" | JPtextFreq$Info1 == "動詞",]
  # いらなそうなやつを調べる
  table(JPtext_nv$Info1, JPtext_nv$Info2)
  subset(JPtext_nv, JPtext_nv$Info1 == "動詞" & JPtext_nv$Info2 == "接尾")
  # "がる、させる、す、せる、られる、れる" を削除
  JPtext_nv <- JPtext_nv[-which(JPtext_nv$Info1 == "動詞" & JPtext_nv$Info2 == "接尾"),]
  # "こちら、あっち、あいつ、彼、みなさん"などを削除
  JPtext_nv <- JPtext_nv[-which(JPtext_nv$Info1 == "名詞" & JPtext_nv$Info2 == "代名詞"),]
  # 数字を削除
  JPtext_nv <- JPtext_nv[-which(JPtext_nv$Info1 == "名詞" & JPtext_nv$Info2 == "数"),]
  
  # 頻出単語をチラ見
  JPtext_nv[order(JPtext_nv$Freq, decreasing=T)[1:20],]
  # 頻出単語にゴミが混ざってるので手動で削除
  removeTerms <- c(removeTerms, "動画", "チャンネル", "登録", "\"", "nn",
                   "する", "いる", "ある", "なる", "こと", "videos", "さん", "の",
                   "\"\"", "再生", "リスト", "nnn", "お願い",
                   "フェイスブック", "ツイッター", "インスタグラム")
  JPtext_nv <- JPtext_nv[-which(JPtext_nv$Term %in% removeTerms),]
  # "aa"とか"ld"とか"`"とか無意味な単語とか記号を削除
  JPtext_nv <- JPtext_nv[-which(nchar(JPtext_nv$Term, type = "bytes") <= 2),]
  
  return(JPtext_nv)
}




###############################
# テキストの前処理
###############################
removeTerms <- c(stopwords("english"), "youtube", "video", "channel", "that", 
                 "show", "watch", "can", "make", "subscrib", "us", "use",
                 "facebook", "twitter", "instagram", "subscribe")
video$description <- gsub("video", "", video$description) # なぜかremoveWordsで"video"がremoveできないので無理やり削除
textall <- tolower(video$description)
text85 <- tolower(video$description[which(video$pop85==TRUE)])
text90 <- tolower(video$description[which(video$pop90==TRUE)])
text95 <- tolower(video$description[which(video$pop95==TRUE)])




###############################
# ワードクラウド
# 英語と日本語の違い上、JPと、USとIN、で処理が分かれる
###############################

# ------------
# USとIN : ワードクラウド
# ------------
text <- textall
text <- text85
text <- text90
text <- text95
# text <- as.character(text)
corpus <- makeCorpus(text)
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
# なんかINの"subscrib"という単語だけなぜか削除できないので力技で消す
d <- subset(d, d$word != "subscrib")
head(d, 10)
# ワードクラウド描画!
wordcloud(d$word, d$freq, 
          color=rainbow(5), random.order=F, random.color=F)
# 時間かかるというか終わらないので、キリのいいところで強制終了して止めるしかない...?


# ------------
# JP
# ------------
# http://www.ic.daito.ac.jp/~mizutani/mining/rmecab_func.html
write.table(textall, file="JPtextall.txt", row.names=F, col.names=F)
write.table(text85, file="JPtext85.txt", row.names=F, col.names=F)
write.table(text90, file="JPtext90.txt", row.names=F, col.names=F)
write.table(text95, file="JPtext95.txt", row.names=F, col.names=F)

JPtext_nounverb <- make_nounverb_JP("JPtextall.txt")
JPtext_nounverb <- make_nounverb_JP("JPtext85.txt")
JPtext_nounverb <- make_nounverb_JP("JPtext90.txt")
JPtext_nounverb <- make_nounverb_JP("JPtext95.txt")

par(family = "Osaka") # 日本語フォント使えるようにする設定。WindowsとMacで違うかも
# ワードクラウド描画!
wordcloud(JPtext_nounverb$Term, JPtext_nounverb$Freq, 
          color=rainbow(5), random.order=F, random.color=F)








###############################
# tree
# 英語と日本語の違い上、JPと、USとIN、で処理が分かれる
# JPの処理はまだとちゅう
###############################

# ------------
# USとIN
# ------------
text <- textall
# text <- as.character(text)
corpus <- makeCorpus(text)
frequencies = DocumentTermMatrix(corpus)
findFreqTerms(frequencies, lowfreq=1000)
findFreqTerms(frequencies, lowfreq=500)
sparse = removeSparseTerms(frequencies, 0.90); dim(sparse)
document_terms = as.data.frame(as.matrix(sparse))
str(document_terms); dim(document_terms)
# head(document_terms)


# ------------
# JP
# ------------
head(JPtext_nounverb)
head(document_terms)

# 目的変数はpopXX
document_terms$pop85 = video$pop85
document_terms$pop90 = video$pop90
document_terms$pop95 = video$pop95

# Reciataionでは日付を元にsplitしてたけど、今回は単にランダムに6:4でtrain:test
split1 = sample(row.names(document_terms), 0.6*nrow(document_terms))
split2 = setdiff(row.names(document_terms), split1)
train = document_terms[split1,]
test = document_terms[split2,]

# 結構実行ごとに結果変わる...

train <- train[, !(colnames(train) %in% c("pop90", "pop95"))] # 85%だけ抽出
test <- test[, !(colnames(test) %in% c("pop90", "pop95"))] # 85%だけ抽出
cart = rpart(pop85 ~ ., data = train, method="class", cp = .003)
prp(cart, cex=0.8)

train <- train[, !(colnames(train) %in% c("pop85", "pop95"))] # 90%だけ抽出
test <- test[, !(colnames(test) %in% c("pop85", "pop95"))] # 90%だけ抽出
cart = rpart(pop90 ~ ., data = train, method="class", cp = .003)
prp(cart, cex=0.8)

train <- train[, !(colnames(train) %in% c("pop85", "pop90"))] # 95%だけ抽出
test <- test[, !(colnames(test) %in% c("pop85", "pop90"))] # 95%だけ抽出
cart = rpart(pop95 ~ ., data = train, method="class", cp = .003)
prp(cart, cex=0.8)














# ###############################
# # ボツ
# ###############################
# 
# # ボツ: RMecabFreqではなくRMecabCとかいうやつを使おうとした残骸
# analyzedText[which(analyzedText == "")] <- "NO TEXT"
# wordList <- lapply(analyzedText, RMeCabC)
# wordList <- lapply(wordList, unlist)
# a <- wordList[[10]]
# a[names(unlist(wordList[10])) == "名詞"]
# 
# 
# # ボツ: 一つ一つの動画に対して行おうとした残骸
# JPtextFreq <- list()
# if(!file.exists("JPtext/*.txt")){
#   if(file.exists("JPtext")){
#     print("JPtext directory already exists")
#   }else{
#     print("JPtext directory was not found and created")
#     dir.create("JPtext", showWarnings=F, recursive=T)
#   }
#   for(i in c(1:length(analyzedText))){
#     fileName <- paste("JPtext/", i, ".txt", sep="")
#     if(!file.exists(fileName)){
#       write.table(analyzedText[i], file=fileName, row.names=F, col.names=F)
#     }
#     newJPtextFreq <- RMeCabFreq(fileName)
#     JPtextFreq <- c(JPtextFreq, list(newJPtextFreq))
#     print(i)
#   }# if JPtext is not exist
# }#