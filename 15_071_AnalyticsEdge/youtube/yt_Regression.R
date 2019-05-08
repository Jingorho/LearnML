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
dim(video)

# 整理前

head(video)
colnames(video)

video$category_id <- as.factor(video$category_id)
select_col <- c("category_id", "tagCounts", "descLen", "titleLen",
                "pop90", "views", "lviews", "likeRate")
vi <- video[, colnames(video) %in% select_col]
head(vi)

# JP
# (20<tag<40) &&/|| (80<title) => 人気?
vi1 <- (20 < vi$tagCounts & vi$tagCounts < 40)
vi2 <- (80 < vi$titleLen)
# IN
# tag>20 &&/|| title<50 => 人気?
vi1 <- (20 < vi$tagCounts)
vi2 <- (vi$titleLen < 50)

vi_and <- vi[(vi1 & vi2),]
vi_or <- vi[vi1 | vi2,]
vi <- vi_and
vi <- vi_or
head(vi); dim(vi)

train = vi[sample(row.names(vi), 0.6*nrow(vi)),]
test = vi[setdiff(row.names(vi), split1),]
head(train); dim(train)

# model <- glm(superPop~., family=binomial, data = train) # ロジスティック回帰
model <- glm(pop90 ~ category_id + tagCounts + descLen + titleLen, 
             family=binomial, data = vi) # 普通の重回帰
summary(model)
model <- glm(pop90 ~ tagCounts + descLen + titleLen, 
             family=binomial, data = vi) # 普通の重回帰
summary(model)


