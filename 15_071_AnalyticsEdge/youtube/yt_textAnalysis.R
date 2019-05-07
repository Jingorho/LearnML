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
JPvideo <- read.csv("JPvideo_pd.csv")
INvideo <- read.csv("INvideo_pd.csv")
video <- USvideo
video <- JPvideo
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

# ------------
# JP用の関数 : 単語の頻度表リスト作成
# ------------
make_wordsFreq_JP <- function(t){
  write.table(t, file="JPtext.txt", row.names=F, col.names=F)
  JPtextFreq <- RMeCabFreq("JPtext.txt")
  JPtext <- JPtextFreq[JPtextFreq$Info1 == "名詞" | JPtextFreq$Info1 == "動詞" | JPtextFreq$Info1 == "副詞" | JPtextFreq$Info1 == "形容詞",]
  # いらなそうなやつを調べる
  # table(JPtext$Info1, JPtext$Info2)
  # subset(JPtext, JPtext$Info1 == "動詞" & JPtext$Info2 == "接尾")
  # "がる、させる、す、せる、られる、れる" を削除
  JPtext <- JPtext[-which(JPtext$Info1 == "動詞" & JPtext$Info2 == "接尾"),]
  # "こちら、あっち、あいつ、彼、みなさん"などを削除
  JPtext <- JPtext[-which(JPtext$Info1 == "名詞" & JPtext$Info2 == "代名詞"),]
  # 数字を削除
  JPtext <- JPtext[-which(JPtext$Info1 == "名詞" & JPtext$Info2 == "数"),]
  
  # 頻出単語をチラ見
  # JPtext_nv[order(JPtext_nv$Freq, decreasing=T)[1:20],]
  # 頻出単語にゴミが混ざってるので手動で削除(日本語よう)
  removeTerms <- c(removeTerms, "動画", "チャンネル", "登録", "\"", "nn",
                   "する", "いる", "ある", "なる", "こと", "てる", "くる", "やる",
                   "videos", "さん", "の", "ん", "Youtuber", 
                   "\"\"", "再生", "リスト", "nnn", "お願い", "！", "゙",
                   "みる", "〜", "ー", "-", "／", "「", "」", "2018", "2017", "2018年", "2017年",
                   "フェイスブック", "ツイッター", "インスタグラム")
  JPtext <- JPtext[-which(JPtext$Term %in% removeTerms),]
  # "aa"とか"ld"とか"`"とか無意味な単語とか記号を削除
  JPtext <- JPtext[-which(nchar(JPtext$Term, type = "bytes") <= 2),]
  
  # 頻度で降順に並べる
  JPtext <- JPtext[order(JPtext$Freq, decreasing = T),]
  
  return(JPtext)
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
}

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



# ------------
# JP
# ------------
# http://www.ic.daito.ac.jp/~mizutani/mining/rmecab_func.html
makeWordFreq_wc_JP <- function(){
  wordsall <- make_wordsFreq_JP(textall)
  words90 <- make_wordsFreq_JP(text90)
  words10 <- make_wordsFreq_JP(text10)
  
  onlypop10_words <- setdiff(words10$Term, words90$Term) # pop10%にあってallにないやつ
  onlywords10 <- data.frame()
  for(i in c(1:length(onlypop10_words))){
    temp <- words10[(words10$Term == onlypop10_words[i]),]
    onlywords10 <- rbind(onlywords10, temp)
  }
  id <- round(length(words10$Term)*0.5)
  formerwords <- wordsall[1:id,]
  formerwords200 <- wordsall[1:200,]
  latterwords <- wordsall[id:length(wordsall$Freq),]
  latterwords200 <- wordsall[200:length(wordsall$Freq),]
}

makeWordFreq_wc_JP()
# write.csv(former_words, "JP_former_words.csv")
# write.csv(former_words, "JP_former200_words.csv")
# write.csv(onlywords10, "JP_onlyWords10.csv")

# par(family = "Osaka") # 日本語フォント使えるようにする設定。WindowsとMacで違うかも
par(family = "HiraKakuProN-W3")

# ワードクラウド描画!
wordcloud(onlywords10$Term, onlywords10$Freq, 
          color=rainbow(5), random.order=F, random.color=F)
wordcloud(latterwords$Term, latterwords$Freq, 
          color=rainbow(5), random.order=F, random.color=F)
wordcloud(latterwords200$Term, latterwords200$Freq, 
          color=rainbow(5), random.order=F, random.color=F)
head(wordsall,50)






# ワードクラウド描画!
wordcloud(wordsall$word, wordsall$freq, 
          color=rainbow(5), random.order=F, random.color=F)
# head(wordsall$word, 50)

t <- wordsall[id[200:length(wordsall$word)],]
wordcloud(t$Term, t$Freq,
          color=rainbow(5), random.order=F, random.color=F)

onlypop10_words <- setdiff(words10$word, words90$word) # pop10%にあってallにないやつ
onlywords10 <- data.frame()
for(i in c(1:length(onlypop10_words))){
  temp <- words10[(words10$word == onlypop10_words[i]),]
  onlywords10 <- rbind(onlywords10, temp)
}

# ワードクラウド描画!





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
docterm <- makeDocumentTerms(text10, 0.99)
write.csv(docterm, "IN_text10.csv", col.names=T, row.names=F)
docterm <- makeDocumentTerms(text10, 0.99)
dim(docterm)
docterm <- docterm[, !(colnames(docterm) %in% former_words$word)]
docterm <- docterm[, !(colnames(docterm) %in% former200_words$word)]
dim(docterm)
str(docterm)
head(docterm)



# ------------
# JP
# ------------
head(wordsall)

# head(document_terms)
# rownames(docterm) <- c(1:dim(docterm)[1])
# head(docterm[1:10,1:10])
t <- as.character(text90)
makeDocumentTerms_JP <- function(dontMakeFile, t){
  if(!dontMakeFile){
    dir.create("JPtexts")
    for(i in c(1:length(t))){
      fname <- paste("JPtexts/", i, ".txt", sep="")
      write.table(t[i], file=fname, row.names=F, col.names=F)
    }
  }
  dt <- docMatrix("JPtexts", 
                  pos=c("名詞", "動詞", "形容詞")) # MeCabの関数
  dt <- dt[rowSums(dt) >= 2,] # 全文書を通しての総頻度が 2 以上のターム
  
  dt_transpose_df <- data.frame(t(matrix(as.matrix(dt), nrow(dt), ncol(dt))))
  rownames(dt_transpose_df) <- colnames(dt)
  colnames(dt_transpose_df) <- rownames(dt)
  
  dt <- as.data.frame(dt_transpose_df)
  dt <- dt[,-c(1,2)] # 最初の2列は集計なので削除
  str(dt)
  dim(dt)
  removedFile <- c()
  for(i in c(1:length(t))){
    if(sum(rownames(dt) %in% paste(i, ".txt", sep="")) == 0){
      removedFile <- c(removedFile, i)
    }
  }
  
  return(dt)
}
t[604]
docterm <- dt
docterm <- makeDocumentTerms_JP(T, textall)
dim(docterm)
# docterm$pop90 <- c(T,F,T,T,F,T,T,F,F,F,T,F,T,T,F,T,T,F,F,F,F,T,T,F)

# ------------
sum(video$pop90)
v <- video[video$pop90,]
v <- v[-removedFile,]
# 目的変数はpopXX
docterm$pop90 = (v$tagCounts < 20)
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

