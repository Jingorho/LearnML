library(ggplot2)
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library("wordcloud")
library("RColorBrewer")
library(RMeCab)
library(GGally)
library(ggplot2)
library(ggsci)
library(dplyr)

options(scipen=999)
setwd("/Users/yukako/WorkSpace/ML/15_071_AnalyticsEdge/Youtube/data")

USvideo <- read.csv("USvideo_pd.csv")
JPvideo <- read.csv("JPvideo_pd.csv")
INvideo <- read.csv("INvideo_pd.csv")
video <- USvideo
video <- INvideo
video <- JPvideo
head(video)
dim(video)

# "trending_date", "category_id", "publish_time"
# "views", "likes", "dislikes", "comment_count"
# "trend_dur", "publish_hour", "publish_month", 
# "lviews", "llikes", "ldislikes", "lcomments"
# "likeRate", "pop90", "tagCounts", "descLen", "titleLen"  

# IN
colnames(video)
# video$category_id <- as.factor(video$category_id)
# select_col <- c("category_id", "tagCounts", "descLen", "titleLen")
# vi <- video[video$pop90, colnames(video) %in% select_col]
vi <- video
head(vi); dim(vi)

docterm <- read.csv("US_textall.csv")
docterm <- read.csv("US_text90.csv")
docterm <- read.csv("US_text10.csv")
docterm <- read.csv("IN_textall.csv")
docterm <- read.csv("IN_text90.csv")
docterm <- read.csv("IN_text10.csv")
docterm <- read.csv("JP_text10.csv")

# dt <- cbind(docterm, vi)
dt <- docterm
head(dt); dim(dt)

# 最頻200wordsを取り除く
formerwords200 <- read.csv("US_formerwords200.csv")
formerwords200 <- read.csv("IN_formerwords200.csv")
fw200 <- formerwords200$word
formerwords200 <- read.csv("JP_formerwords200.csv")
fw200 <- formerwords200$Term

dt <- dt[,!(colnames(dt) %in% fw200)]
head(dt); dim(dt)


vi <- video
vi <- video[video$pop90==FALSE,]
vi <- video[video$pop90==TRUE,]
# JP
vi <- vi[-removedFile,]

hc <- hclust(dist(dt))
# plot(hc, cex=0.6)

result <- cutree(hc, k=5)
length(result)
length(vi$category_id)



cordf <- data.frame(
  logviews = log(vi$views),
  loglikes = log(vi$likes),
  logdislikes = log(vi$dislikes),
  logcomments = log(vi$comment_count),
  trend_dur = vi$trend_dur,
  likeRate = vi$likeRate,
  tagcounts = vi$tagCounts,
  desclength = vi$descLen,
  titlelength = vi$titleLen,
  gr = as.factor(result)
)
# head(cordf)
# plot(video$llikes, video$lviews, col=result, cex=0.3)
# pairs(cordf, col=result, cex=0.1)

g <- ggpairs(cordf, 
             aes_string(colour="gr", alpha=0.3), 
             upper = list(continuous = wrap("cor", size = 2.3)), 
             lower = list(continuous=wrap("points", size=0.05)))
g <- g + theme(axis.text= element_text(size=5),
               legend.title = element_text(size=7),
               legend.text = element_text(size=7),
               axis.title = element_text(size=7),
               plot.title = element_text(size=7),
               strip.text = element_text(size=7))
g # plot(g)じゃないので注意

g <- ggplot(cordf, aes(x = cordf$logviews, y = cordf$desclength, 
                   color = gr))
g <- g + geom_point()
g <- g + scale_color_nejm()
plot(g)

