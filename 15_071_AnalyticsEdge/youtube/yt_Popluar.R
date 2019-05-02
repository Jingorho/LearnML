# 整理前

options(scipen=999)
setwd("/Users/yukako/WorkSpace/ML/15_071_AnalyticsEdge/Youtube/data")

USvideo <- read.csv("USvideo_pd.csv")
JPvideo <- read.csv("JPvideo_pd.csv")
INvideo <- read.csv("INvideo_pd.csv")
# video <- USvideo
video <- JPvideo
# video <- INvideo
# head(video)


video$likeRate <- video$dislikes /  (video$likes +  video$dislikes)
head(video$likeRate)
plot(video$likeRate, video$views)

video$llikeRate <- video$ldislikes / (video$llikes +  video$ldislikes)
plot(video$llikeRate, video$lviews)


summary(video$llikeRate)


library(dplyr)
filter(video$views, cume_dist(desc(G)) < 0.05)

p5 <- subset(video$channel_title, cume_dist(video$views) > 0.95)
head(p5)
summary(p5)
table(p5)

plot(cume_dist(video$views))

a <- subset(video$video_id, video$channel_title == "The Late Show with Stephen Colbert")


