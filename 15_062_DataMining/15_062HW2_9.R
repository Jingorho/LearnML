# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 2 : 9.1 Decision Trees
# -----------------------------------------------------------

###############################
# ライブラリとか
###############################
#install.packages("rpart.plot") # tree用
library(rpart)
library(rpart.plot)
library(caret)

# --- 実行不要 --- #
#options(scipen=999, digits = 4) # 表示桁を揃える
# ---------------- #


###############################
# データの読み込みと前処理
###############################
ebay <- read.csv("data/eBayAuctions.csv")
#set.seed(1) #確率を固定する

# 変数Durationをカテゴリカルデータに変換
ebay$Duration <- as.factor(ebay$Duration)

# trainデータとvalidationデータに分割
train.index <- sample(row.names(ebay), 0.6*dim(ebay)[1])
valid.index <- setdiff(row.names(ebay), train.index)
train.df <- ebay[train.index, ]
valid.df <- ebay[valid.index, ]

# --- ここは任意 --- #
head(ebay); 
head(train.df); head(valid.df) # 最初の5行だけ確認
sum(is.na(ebay)) # 欠損値の確認、ないのでおk
# ------------------ #



###############################
# a
###############################

# 木を作る Note:与えられたデータが「Competitive」ではなくなぜか「Competitive.」となってるので注意。ミスタイプ？
# 問題文の指定の通り、minbucketとmaxdepthを指定する
# 教科書の通り、決定木のモデル構築にはrpart()関数を使うらしい。
# 引数は問題文で指定されたもの以外はRecitationのものそのまま
class.tree_a <- rpart(Competitive. ~ ., data = train.df, minbucket = 50, 
                    control = rpart.control(maxdepth = 7), method = "class")
# 木を表示。引数はRecitationそのまま
png("plot1.png", width = 1000, height = 1000) 
prp(class.tree_a, type = 1, extra = 1, split.font = 1, varlen = -10) # 結果をプロット
dev.off()
# 木を刈り込む（誤差が最も小さくなるっぽいところで木をぶった切る、最適化する）
# 本当はここで刈り込めるんだろうが、誤差が一番最後に最小になってるので、刈り込める所がない...?
printcp(class.tree_a) # 誤差表示
# 刈り込み
pruned.ct <- prune(class.tree_a,
                   cp = class.tree_a$cptable[which.min(class.tree_a$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"]) # 刈り込んだ部分の枝?の数
# 刈り込んだ結果をプロット
png("plot0.png", width = 1000, height = 1000) 
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10) # 今回は変わらない...?
dev.off()


# [???] 中島さんの結果と宮西の環境での結果が違うのはなんで、、?
# [???] 借り込む前後で結果が変わらない。なぜだ〜


# うまいdescribeの仕方はSection9.5が参考になります

# かけそうなこと ******************************
# 例えば
# IF (5 < OpenPrice) AND (OpenPrice < 10)
# THEN Class = 1.
# かけそうなこと ******************************



###############################
# b
###############################

# modelがpredictに対してpracticalかはよく分からない
# [???] 何を持ってpracticalというのか

# type="class"で分類学習のpredictをすることを明示的に書く
pred.train <- predict(class.tree_a, type = "class")
cm_tree <- confusionMatrix(as.factor(pred.train), as.factor(train.df$Competitive.))
# Accuracy 0.88くらいだしいい感じではないでしょうか...

# かけそうなこと ******************************
# aで作ったモデルをpredict()で推定して、そのモデル推定の精度の評価をconfusionMatrix()で行った。
# cm_treeのうち、Accuracyが0.874なので、高い精度で分類できているっぽい。
# かけそうなこと ******************************



###############################
# c
###############################

# 記述なので省略、png参照


###############################
# d
###############################

# 問題文より、「this time only with predictors 
# that can be used for predictors the outcome of a new auction」
# -> aの結果で使われたpredictorを使うってこと？ aで出力された変数
# (結果の「Variables actually used in tree construction:」欄)を手動で入力した...
class.tree_d <- rpart(Competitive. ~ OpenPrice + ClosePrice + currency + sellerRating,
                      data = train.df, minbucket = 50, 
                      control = rpart.control(maxdepth = 7), method = "class")
prp(class.tree_d, type = 1, extra = 1, split.font = 1, varlen = -10) # 結果をプロット
# 木を刈り込む（最適化する）
printcp(class.tree_d)
pruned.ct_d <- prune(class.tree_d,
                     cp = class.tree_d$cptable[which.min(class.tree_d$cptable[,"xerror"]),"CP"])
length(pruned.ct_d$frame$var[pruned.ct_d$frame$var == "<leaf>"])

png("plot2.png", width = 1000, height = 1000) 
prp(pruned.ct_d, type = 1, extra = 1, split.font = 1, varlen = -10) # 刈り込んだ結果をプロット
dev.off()


# かけそうなこと ******************************
# 問題文"this time only with predictors 
# that can be used for predictors the outcome of a new auction"とのことなので、
# aの出力から分かるaで使われたpredictors(結果の「Variables actually used in tree construction:」欄)を
# 手動で入力してモデルを構築した。
# (smallest set of rulesは、一つを取り出して例示すればいいのか全て記述する必要があるのか分からないが、
# 一つの例としては以下があるのでは...という感じ)
# IF (OpenPrice >= 11) AND (CloseProce < 10)
# THEN Class = 1.
# かけそうなこと ******************************



###############################
# e
###############################

# 本当はpredictorごとの寄与率的なものを算出する何かがあるのかもしれない[???]が、
# 感覚的にnumericのpredictorの中ではClosePriceとOpenPriceかと思うので、、、

# 本当はTreeの結果をplotするのでこれは違う?
plot(x = ebay$OpenPrice, y = ebay$ClosePrice, 
     xlim = c(1,100), ylim = c(1,100),
     xlab = "Open Price [$]", ylab = "Close Price [$]", 
     pch = c(1,3), col = c("blue", "red"), cex = 0.5)
legend("bottomright", legend = c("0: Competitive", "1: Noncompetitive"), pch = c(1,3), col = c("blue", "red"))
title("OpenPrise v.s. Close Price")
summary(ebay)

# [???] treeの結果をplotする方法とは？
# [???] 上のplotででできた結果で、境界線なんか引けない。どうすれば



###############################
# f
###############################

# lift chart
# [???] lift chart出し方分からない

# cofusion matrix : bで既に出してしまった...ので
cm_tree




###############################
# g
###############################


# EOF(-ω-)