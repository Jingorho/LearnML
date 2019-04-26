# library(ggplot2)

###############################
# publish_time時刻との閲覧数の関係 -> 結論: Youtuberは1-2時間前倒しでpublishするべき?
###############################
# 日付を削って時刻だけ取り出し
video$publish_hour <- format(as.POSIXct(video$publish_time), "%H")
video$publish_hour <- as.numeric(video$publish_hour) # あとはもう0-23の数値としての方が扱いやすい
hist(video$publish_hour, breaks=24)
# 時刻と閲覧数の関係を見たい
hours_views <- c(0:23)
for(i in c(0:23)){
  # 0-23時に公開された動画を抽出して、そのviewをsum()
  hours_views[i-1] <- sum(as.numeric(subset(video, video$publish_hour==i)$views))
}
# データフレーム にまとめる
hour_views <- data.frame(
  table(video$publish_hour), # 0-23時に公開された動画の本数
  hours_views) # その時間に公開された動画の閲覧数(その時刻での閲覧数じゃない)
colnames(hour_views) <- c("hours", "counts", "views")
# 色の濃さは、その時間に公開された動画の閲覧数
g <- ggplot(hour_views, aes(x = hour_views$hours, y = hour_views$counts, fill = hour_views$views))
g <- g + geom_bar(stat = "identity")
g <- g + ggtitle("Publish hour and views") + xlab("Publish hour") + ylab("Frequency")
plot(g) # なんか2時に公開された動画がやたら見られてる。。?
# 15-17時公開されてる動画が多い(棒が長い)けど、実際によく見られてる(色が薄い)のは
# 12-14時に公開されたやつ？



###############################
# publish_time日付と人気の関係
###############################
# 2017-10以降にデータが集中してる...なんで？データに変に偏りないかな。不安
table(format(as.POSIXct(video$publish_time), "%Y-%m"))

video$publish_month <- format(as.POSIXct(video$publish_time), "%m")
video$publish_month <- as.numeric(video$publish_month)
hist(video$publish_month) # 11~5月に集中

# library(tm)
# library(SnowballC)
# library(rpart)
# library(rpart.plot)

month <- 12
text <- subset(video, video$publish_month==month)$description
# text <- subset(video, video$publish_month==month)$description
# text <- subset(video, video$publish_month==month)$description

makeDocumentTerms <- function(analyzedText, atleaset_percentage){
  corpus = Corpus(VectorSource(analyzedText))
  corpus = tm_map(corpus, tolower)
  if (!("PlainTextDocument" %in% class(corpus[[1]]))) {
    corpus = tm_map(corpus, PlainTextDocument)
  }
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, removeWords, c("youtube", "video", "channel", "that")) # ほかあれば
  corpus = tm_map(corpus, stemDocument)
  strwrap(corpus[[1]])
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

# 目的変数はsuperPop(めちゃ人気かどうか)
document_terms$superPop = subset(video, video$publish_month == month)$superPop

# Reciataionでは日付を元にsplitしてたけど、今回は単にランダムに6:4でtrain:test
split1 = sample(row.names(document_terms), 0.6*nrow(document_terms))
split2 = setdiff(row.names(document_terms), split1)
train = document_terms[split1,]
test = document_terms[split2,]
cart = rpart(superPop ~ ., data=train, method="class", cp = .003)
prp(cart, cex=0.6)






