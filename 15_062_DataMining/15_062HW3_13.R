# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 3 : 13.2 Ensamble
# -----------------------------------------------------------

###############################
# ライブラリとか
###############################
library(rpart)
library(rpart.plot)
library(caret)
library(gains)
#install.packages("adabag")
library(adabag)
#install.packages("randomForest")
library(randomForest)
#install.packages("ROCR") #prediction()用
library(ROCR)
#options(scipen=999, digits = 4) # 表示桁を揃える


###############################
# データの読み込みと前処理
###############################
ebay <- read.csv("/Users/yukako/WorkSpace/DataMiningHW/data/eBayAuctions.csv")
head(ebay); str(ebay); summary(ebay);
# set.seed(1) #確率を固定する
# sum(is.na(ebay)) # 欠損値NAの確認
# ebay <- na.omit(ebay) # 欠損値NAが含まれる行を削除
# 列名"Competitive."となぜか最後にドットが入っていて式で扱いにくいので修正
colnames(ebay) <- c("Category", "currency", "sellerRating", "Duration",
                    "endDay", "ClosePrice", "OpenPrice", "Competitive")

# trainデータとvalidationデータに分割
train.index <- sample(row.names(ebay), 0.6*dim(ebay)[1])
valid.index <- setdiff(row.names(ebay), train.index)
ebay_train <- ebay[train.index, ]
ebay_valid <- ebay[valid.index, ]
dim(ebay_train); dim(ebay_valid) # 行数と列数確認


###############################
# a
###############################

# [決定木を実行]
# controlオブションは、rpart.control()を用いて細かい決定木アルゴリズムのパラメータを設定する。
# maxdepthは、根を0としたときの最後のノードの木の深さ。
class.tree <- rpart(Competitive ~ ., data = ebay_train, method = "class")
# typeはプロットの書式。extraは、extra informationの表示有無。varlenは、split.fontするときに文字を区切る変数の長さ。
prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)


# [Overall accuracy]
# rpart()で求めた決定木のモデルclass.treeを元に、validデータを使ってデータを推定
pred_singleTree <- predict(class.tree, ebay_valid, type = "class")
# 推定データと実際のデータを比較して誤差を算出
cm_valid <- confusionMatrix(as.factor(pred_singleTree), as.factor(ebay_valid$Competitive))
cm_valid$overall[1] # Accuracyはoverallという要素の1番目に入っている



# [Lift chart]
# rpart()で求めた決定木のモデルclass.treeを元に、validデータを使ってデータを推定
# ただし今回は、クラス(1/0)ではなく、確率で出力
pred_singleTree_prob <- predict(class.tree, ebay_valid, type = "prob")
# 下で確認できるように、[0に分類される確率, 1に分類される確率]のnrow(ebay_valid)行2列で帰ってくる
# data.frame(pred_singleTree[100:110], pred_singleTree_prob[100:110,])
# table(pred_singleTree_prob[,2])

# 推定データと実際のデータとを比較したなんかの(?)値を算出(confusionMatrixとかと順番が逆なので注意)
gain <- gains(as.numeric(ebay_valid$Competitive), pred_singleTree_prob[,2]) #groups=dim(pred_singleTree_prob)[2]
# cume.pct.of.total(全ての従属変数(この場合1と0)の確率の累積和) * (Competitiveの中で1になってるデータの総数)

# メモ: plot(c(y1, y2) ~ c(x1, x2))またはplot(c(x1, x2), c(y1, y2))で、
#       (x1,y1)と(x2,y2)をプロット(なので~の前後は個数を合わせる必要あり、~有無でxとyの順番注意)
plot(c(0, gain$cume.pct.of.total * sum(ebay_valid$Competitive)) ~ c(0, gain$cume.obs),
     xlab="# cases", ylab="Cumulative Competitive", main="Lift Chart", type="l")

# Baseline
lines(c(0, sum(ebay_valid$Competitive)) ~ c(0, length(ebay_valid$Competitive)), col="gray", lty=2)



# [Decile-wise lift chart]
deciles <- gain$mean.resp / mean(ebay_valid$Competitive)
barplot(deciles, names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# lift on the first decile
deciles[1]


###############################
# b
###############################

# Boosting
ebay$Competitive <- as.factor(ebay$Competitive)
boost <- boosting(Competitive ~ ., data = ebay)
# Note: "ebay$Competitive"やas.factor()つけたりすると
# "undefined columns selected"のエラー発生. ただの"Competitive"しか通らなかった

pred_boost <- predict(boost, ebay_valid, type = "class")
cm_boost <- confusionMatrix(as.factor(pred_boost$class), as.factor(ebay_valid$Competitive))
cm_boost$overall[1]

# 自分用メモ: 
# rpartの結果class.treeをclass(class.tree)するとrpart型、
# boostingの結果pred_boostをclass(pred_boost)するとboosting型。アルゴリズムの結果は、
# それぞれの関数名のクラス(多分関数が実装されてるのがそのままクラス名になってる)で
# 帰ってくることが多い。predict()は、関数名の名前のクラスで帰ってきたオブジェクトを引数に
# してpredictした結果を返してくれるやつっぽい。
# predictしたclassificationの分類結果の型もまちまちで、class.treeを使ってpredict()した結果の
# predオブジェクトがfactor型で1/0を格納してるのに対して、
# boostを使ってpredict()した結果のpred_boostオブジェクトはlist型(Boostingの経緯全部が
# 格納されてるっぽい)。その中の要素$class(attribute()で確認できる)はcharacter型。
# -> confusionMatrixでのas.factor()とか、plot()でのas.numeric()とかが使用まちまちなのは、
# 決まりがあるわけじゃなくて各アルゴリズムで返してくる型が違うから仕方なさそう。

# Lift chart
pred_boost_prob <- predict(boost, ebay_valid, type = "prob")
pred_boost_prob <- pred_boost_prob$prob # prob要素だけ取り出し
gain_boost <- gains(as.numeric(ebay_valid$Competitive), as.numeric(pred_boost_prob[,2]))
plot(c(0, gain_boost$cume.pct.of.total * sum(ebay_valid$Competitive)) ~ c(0, gain_boost$cume.obs),
     xlab="# cases", ylab="Cumulative Competitive", main="Lift Chart", type="l")
# Baseline
lines(c(0, sum(ebay_valid$Competitive)) ~ c(0, length(ebay_valid$Competitive)), col="gray", lty=2)

# Decile-wise lift chart
deciles <- gain_boost$mean.resp / mean(ebay_valid$Competitive)
barplot(deciles, names.arg = gain_boost$depth,
                      xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
deciles[1] # lift on the first decile



###############################
# c
###############################

# Bagging
bag <- bagging(Competitive ~ ., data = ebay) # ebay_trainとebay_validだと"argument is of length zero"エラー...
pred_bag <- predict(bag, ebay_valid, type = "class")
cm_bag <- confusionMatrix(as.factor(pred_bag$class), as.factor(ebay_valid$Competitive))
cm_bag$overall[1]

# Lift chart
pred_bag <- pred_bag$prob
gain_bag <- gains(as.numeric(ebay_valid$Competitive), as.numeric(pred_bag[,2]))
plot(c(0, gain_bag$cume.pct.of.total * sum(ebay_valid$Competitive)) ~ c(0, gain_bag$cume.obs),
     xlab="# cases", ylab="Cumulative Competitive", main="Lift Chart", type="l")
# Baseline
lines(c(0, sum(ebay_valid$Competitive)) ~ c(0, length(ebay_valid$Competitive)), col="gray", lty=2)

# Decile-wise lift chart
decileLift_bag <- barplot(gain_bag$mean.resp / mean(ebay_valid$Competitive), names.arg = gain_bag$depth,
                          xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
decileLift_bag[2] # lift on the first decile



###############################
# d
###############################

# Random Forest
# 日本語でのランダムフォレストの詳しい実行例
# https://qiita.com/nkjm/items/e751e49c7d2c619cbeab
rf <- randomForest(as.factor(Competitive) ~ ., data = ebay_train, ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)
# 分析したデータのうち、各列の影響度
varImpPlot(rf, type = 1)
pred_rf <- predict(rf, ebay_valid)
cm_rf <- confusionMatrix(as.factor(pred_rf), as.factor(ebay_valid$Competitive))
cm_rf$overall[1]

# Lift chart
pred_rf <- predict(rf, ebay_valid, type="prob")
gain_rf <- gains(as.numeric(ebay_valid$Competitive), as.numeric(pred_rf[,2]))
plot(c(0, gain_rf$cume.pct.of.total * sum(ebay_valid$Competitive)) ~ c(0, gain_rf$cume.obs),
     xlab="# cases", ylab="Cumulative Competitive", main="Lift Chart", type="l")
# Baseline
lines(c(0, sum(ebay_valid$Competitive)) ~ c(0, length(ebay_valid$Competitive)), col="gray", lty=2)

# Decile-wise lift chart
deciles <- gain_rf$mean.resp / mean(ebay_valid$Competitive)
decileLift_rf <- barplot(deciles, names.arg = gain_rf$depth,
                          xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
deciles[1] # lift on the first decile




# EOF