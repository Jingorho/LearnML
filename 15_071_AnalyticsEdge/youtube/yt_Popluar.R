# θのとりうる値
theta <- seq(0, 1, 0.1)
# 事前分布
prior <- c(0.01, 0.07, 0.1, 0.12, 0.13, 0.14, 0.13, 0.12, 0.1, 0.07, 0.01)
# データ（5回中4回表が出た）
data <- list(n = 5, s = 4)
# 尤度
likelihood <- dbinom(data$s, size = data$n, prob = theta)
likelihood
plot(likelihood)
# 事後分布
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # 合計が1になるようにsum(posterior)で割る

print(sprintf("%.2f", posterior))










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


