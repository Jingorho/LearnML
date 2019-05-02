# 時間、日時関係のいろいろをやってみたスクリプト。
# update.packages(ask = FALSE)
library(ggplot2)
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)

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
# publish_time時刻との閲覧数の関係 -> 結論: Youtuberは1-2時間前倒しでpublishするべき?
###############################
# 日付を削って時刻だけ取り出して分布をみる
# hist(video$publish_hour, breaks=24)
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
hist(video$publish_month, breaks=12) # 11~5月に集中





