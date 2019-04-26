# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 2 : 7.3 KNN
# -----------------------------------------------------------

###############################
# ライブラリとか
###############################
#install.packages("caret")
library(caret)
#install.packages("class")
library(class)



###############################
# データの読み込みと前処理
###############################
mower.df <- read.csv("data/BostonHousing.csv")
set.seed(1) # [???]結局set.seed()の引数の数は何を表してるんだ

train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])
valid.index <- setdiff(row.names(mower.df), train.index)
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]


new.df = data.frame(
  CRIM = 0.2,
  ZN = 0,
  INDUS = 7,
  CHAS = 0,
  NOX = 0.538,
  RM = 6,
  AGE = 62,
  DIS = 4.7,
  RAD = 4,
  TAX = 307,
  PTRATIO = 21,
  LSTAT = 10
)




###############################
# a
###############################

# [???]どのアルゴリズムでどの前処理すればいいのか問題
# [???]標準化、正規化、スケール、リスケール、...違い -> それぞれの変換したあとのデータプロットして比べたい

# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df
# use preProcess() from the caret package to normalize Income and Lot_Size.
#norm.values <- preProcess(train.df[, 1:12], method=c("center", "scale"))
norm.values <- preProcess(train.norm.df[, 1:12], method=c("center", "scale"))
head(train.norm.df)
train.norm.df[, 1:12] <- predict(norm.values, train.df[, 1:12])
valid.norm.df[, 1:12] <- predict(norm.values, valid.df[, 1:12])
mower.norm.df[, 1:12] <- predict(norm.values, mower.df[, 1:12])
new.norm.df <- predict(norm.values, new.df)

# [???] predict()の中で何が起こってるのか、引数はなんなのか、何に使えて何に使えないのか



# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 5, 1), accuracy = rep(0, 5))
# k=1からk=5まで順番に実行していく
for(i in 1:5) {
  knn.pred <- knn(train.norm.df[, 1:12], valid.norm.df[, 1:12],
                  cl = train.norm.df[, 13], k = i)
  # confusionmatrixはカテゴリカルデータの分類の精度比較でした;今回は数値データの予測なので使えない!
  # 教科書によると、predictionの場合の精度比較にはRMSEとか他の指標が使われるよとのこと -> Section 7.2
  accuracy.df[i, 2] <- RMSE(as.numeric(knn.pred), as.numeric(valid.norm.df[, 13]))
  # Root mean squared errorの公式をそのまま使っても良い by 龍一さん
  #accuracy.df[i, 2] <- sqrt(mean((as.numeric(knn.pred) - as.numeric(valid.norm.df[, 13]))^2))
  
  # [???] RMSE計算するとaccuracy(確率)のはずなのに74とかになる、、、なぜ
  
}
accuracy.df


#plot(train.norm.df[, 13])
#plot(as.numeric(knn.pred))
  


# 
# 
# for(i in 1:5) {
#   knn.classpackage <- class::knn(train = train.df[, 1:12], 
#                                  test = valid.df[, 1:12], 
#                                  cl = train.df[,13], 
#                                  k = 5,
#                                  prob=TRUE)
#   accuracy.df[i, 2] <- RMSE(as.numeric(knn.classpackage), as.numeric(valid.norm.df[, 13]))
#   k5 - valid.norm.df[, 13]
#   length(k5)
#   length(valid.norm.df[, 13])
# #  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2],
# #                  cl = train.norm.df[, 3], k = i)
# #  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 3])$overall[1]
# }
# mode(valid.norm.df[, 13])
# ?RMSE
# ?confusionMatrix
# 
# 
# h_scale <- scale(housing.df[,13]) #標準化
# h_norm <- norm(housing.df[,13])
# h_stand <- standardize(housing.df[,13], centerFun = mean, scaleFun = sd)
# normalize(housing.df[,13], method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
# 
# nn <- class::knn(train = train.norm.df[, 1:12], 
#                  test = new.norm.df,
#                  cl = train.norm.df[, 13],
#                  k =  5)
# nn
# 
# 


