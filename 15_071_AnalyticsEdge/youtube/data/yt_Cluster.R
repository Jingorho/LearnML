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
video <- JPvideo
video <- INvideo
head(video)

# IN
colnames(video)
video$category_id <- as.factor(video$category_id)
select_col <- c("category_id", "tagCounts", "descLen", "titleLen")
vi <- video[video$pop90, colnames(video) %in% select_col]
head(vi); dim(vi)
docterm <- read.csv("IN_text10.csv")
dt <- cbind(docterm, vi)
head(dt); dim(dt)

hc <- hclust(dist(dt))
plot(hc, cex=0.6)

result <- cutree(hc, k=3)
length(result)
length(vi$category_id)


cordf <- data.frame(
  logviews = log(video$views[video$pop90]),
  loglikes = log(video$likes[video$pop90]),
  logdislikes = log(video$dislikes[video$pop90]),
  logcomments = log(video$comment_count[video$pop90]),
  trend_dur = video$trend_dur[video$pop90],
  likeRate = video$likeRate[video$pop90],
  tagcounts = video$tagCounts[video$pop90],
  desclength = video$descLen[video$pop90],
  titlelength = video$titleLen[video$pop90],
  gr = as.factor(result)
)
head(cordf)
plot(video$llikes, video$lviews, col=result, cex=0.3)

pairs(cordf, col=result, cex=0.1)

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

