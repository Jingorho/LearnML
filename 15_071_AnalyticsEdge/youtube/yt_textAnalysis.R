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
# video <- JPvideo
video <- INvideo
# head(video)


###############################
# 処理をまとめて関数にした
# 一回読み込んだら、中身を編集しない限り、繰り返し使えます。
# 使用例:
# 分析結果のドキュメント <- makeDocumentTerms(分析するテキスト, 抽出する単語の頻出度(0.90なら上位90%))
###############################
removeTerms <- c(stopwords("english"), "youtube", "video", "channel", "that", 
                 "show", "watch", "can", "make", "subscrib", "us", "use")



###############################
# JPを分析するとき
###############################
# http://www.ic.daito.ac.jp/~mizutani/mining/rmecab_func.html

selectedVideo <- video$description[which(video$pop==TRUE)]
# selectedVideo <- video$description
text <- tolower(selectedVideo)

write.table(text, file="JPtext.txt", row.names=F, col.names=F)
JPtextFreq <- RMeCabFreq("JPtext.txt")
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
                 "\"\"", "再生", "リスト", "nnn")
JPtext_nv <- JPtext_nv[-which(JPtext_nv$Term %in% removeTerms),]
# "aa"とか"ld"とか"`"とか無意味な単語とか記号を削除
JPtext_nv <- JPtext_nv[-which(nchar(JPtext_nv$Term, type = "bytes") <= 2),]

par(family = "Osaka") # 日本語フォント使えるようにする設定。WindowsとMacで違うかも
# ワードクラウド描画!
wordcloud(JPtext_nv$Term, JPtext_nv$Freq, 
          color=rainbow(5), random.order=F, random.color=F)
# 時間かかるというか終わらないので、キリのいいところで強制終了して止めるしかない...?




###############################
# INとUSはこっちを使う
###############################

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






###############################
# 実際にテキストデータに対して分析処理を行う
###############################
# month <- 6
# text <- subset(as.character(video$description), video$publish_month==month)
video$description <- gsub("video", "", video$description) # なぜかremoveWordsで"video"がremoveできないので無理やり削除
selectedVideo <- video$description[which(video$pop==TRUE)]
selectedVideo <- video$description
text <- as.character(selectedVideo)

atleaset_percentage <- 0.90
corpus <- makeCorpus(text) # 0.99がrecitationのデフォ
frequencies = DocumentTermMatrix(corpus)
findFreqTerms(frequencies, lowfreq=1000)
findFreqTerms(frequencies, lowfreq=500)
sparse = removeSparseTerms(frequencies, atleaset_percentage); dim(sparse)
document_terms = as.data.frame(as.matrix(sparse))
str(document_terms); dim(document_terms)
# head(document_terms)




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


dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 10)
# ワードクラウド描画!
wordcloud(d$word, d$freq, 
          color=rainbow(5), random.order=F, random.color=F)
















###############################
# ボツ
###############################

# ボツ: RMecabFreqではなくRMecabCとかいうやつを使おうとした残骸
analyzedText[which(analyzedText == "")] <- "NO TEXT"
wordList <- lapply(analyzedText, RMeCabC)
wordList <- lapply(wordList, unlist)
a <- wordList[[10]]
a[names(unlist(wordList[10])) == "名詞"]


# ボツ: 一つ一つの動画に対して行おうとした残骸
JPtextFreq <- list()
if(!file.exists("JPtext/*.txt")){
  if(file.exists("JPtext")){
    print("JPtext directory already exists")
  }else{
    print("JPtext directory was not found and created")
    dir.create("JPtext", showWarnings=F, recursive=T)
  }
  for(i in c(1:length(analyzedText))){
    fileName <- paste("JPtext/", i, ".txt", sep="")
    if(!file.exists(fileName)){
      write.table(analyzedText[i], file=fileName, row.names=F, col.names=F)
    }
    newJPtextFreq <- RMeCabFreq(fileName)
    JPtextFreq <- c(JPtextFreq, list(newJPtextFreq))
    print(i)
  }# if JPtext is not exist
}#

