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
video$category_id <- as.factor(video$category_id)
video$publish_hour <- as.factor(video$publish_hour)
head(video)


###############################
# 人気度の指標(again)
###############################

# 閾値 = mean or median
# threshold_com <- median(video$comment_count)
# threshold_lR <- median(video$likeRate)
threshold_com <- mean(video$comment_count)
threshold_lR <- mean(video$likeRate)
video$pop_com_lR <- (video$comment_count > threshold_com & video$likeRate > threshold_lR)
length(video$pop_com_lR); sum(video$pop_com_lR)

# 閾値 = 上位x%
video$pop_com_lR <- (cume_dist(video$comment_count) > 0.95) & (cume_dist(video$likeRate) > 0.95)
# USは総数が少ないので90%にしないと抽出後が少なくなりすぎる(9個とか)
video$pop_com_lR <- (cume_dist(video$comment_count) > 0.90) & (cume_dist(video$likeRate) > 0.90)
length(video$pop_com_lR); sum(video$pop_com_lR)
# cf. 99%にすると、該当データがしか無くなる

# 閲覧数上位数パーセント
video$pop85 <- (video$pop_com_lR & cume_dist(video$views) > 0.85)
video$pop90 <- (video$pop_com_lR & cume_dist(video$views) > 0.90)
video$pop95 <- (video$pop_com_lR & cume_dist(video$views) > 0.95)
sum(video$pop90)





###############################
# tree
###############################
# colnames(video)
idvars <- c("category_id", "tagCounts", "descLen", "titleLen",
            # "trend_dur", "publish_hour", "publish_month",
            "pop90")
idvars <- c("category_id", "tagCounts", "descLen", "titleLen",
            # "trend_dur", "publish_hour", "publish_month",
            "pop_com_lR")
vi <- video[, (colnames(video) %in% idvars)]
# head(vi)

split1 = sample(row.names(vi), 0.6*nrow(vi))
split2 = setdiff(row.names(vi), split1)
train = vi[split1,]
test = vi[split2,]
dim(train); sum(train$pop90); sum(vi$pop90)

sum(train$pop_com_lR); sum(vi$pop_com_lR)
cart = rpart(pop90 ~ ., data = vi, method = "class", cp = .003)
cart = rpart(pop_com_lR ~ ., data = vi, method = "class", cp = .003)
prp(cart, cex=0.8)


mo <- glm(views~., data = video)
summary(mo)



plot(video$trend_dur, video$views, cex=0.4)
