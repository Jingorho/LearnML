# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 3 : 11.4 Neural Network
# -----------------------------------------------------------

###############################
# ライブラリとか
###############################
# install.packages("neuralnet")
# install.packages("gains")
library(neuralnet)
library(caret)
library(gains)
#options(scipen=999, digits = 4) # 表示桁を揃える


###############################
# データの読み込みと前処理
###############################
airlines_raw <- read.csv("/Users/yukako/WorkSpace/DataMiningHW/data/EastWestAirlinesNN.csv")
#set.seed(1) #確率を固定する
head(airlines_raw); str(airlines_raw); summary(airlines_raw);
sum(is.na(airlines_raw)) # 欠損値の確認


# カテゴリカルデータをダミー変数の数値データに変換
# もとからint型なのでたぶん不要

# [0,1]にリスケーリング
airlines <- predict(preProcess(airlines_raw, method="range", rangeBounds=c(0, 1)), airlines_raw)
head(airlines); str(airlines); summary(airlines);

# trainデータとvalidationデータに分割
airlines <- na.omit(airlines)
airlines <- airlines[,-1]
train.index <- sample(row.names(airlines), 0.6*dim(airlines)[1])
valid.index <- setdiff(row.names(airlines), train.index)
airlines_train <- airlines[train.index, ]
airlines_valid <- airlines[valid.index, ]




###############################
# a
###############################

# NN

# neuralnet()のバグらしく、"."が使えないので、長い式を手動で作る
# https://stackoverflow.com/questions/17794575/error-in-terms-formulaformula-in-formula-and-no-data-argument
airlines_train_temp <- airlines_train[,-ncol(airlines_train)]
name_airlines <- names(airlines_train_temp)
f <- as.formula(paste("Phone_sale ~", paste(name_airlines[!name_airlines %in% "y"], collapse = " + ")))
nn <- neuralnet(formula=f, data=airlines_train, hidden=5)


# TODO
# confusionMatrix()で精度確認、avoid overfittingの話
training.prediction = compute(nn, airlines_train[,-ncol(airlines_train)])
# training.class = apply(training.prediction$net.result, 1, which.max) - 1
training.class = round(training.prediction$net.result, digits=0) #四捨五入じゃないのかな...
confusionMatrix(as.factor(training.class), as.factor(airlines[train.index,]$Phone_sale))

validation.prediction = compute(nn, airlines_valid[,-ncol(airlines_valid)])
# validation.class = apply(validation.prediction$net.result, 1, which.max) - 1
validation.class = round(validation.prediction$net.result, digits=0) #四捨五入じゃないのかな...
confusionMatrix(as.factor(validation.class), as.factor(airlines_valid[valid.index,]$Phone_sale))



# decile-wise lift chart
gain_train <- gains(training.class, airlines_train$Phone_sale)
barplot(gain_train$mean.resp / mean(airlines_train$Phone_sale), names.arg = gain_train$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
gain_valid <- gains(validation.class, airlines_valid$Phone_sale)
barplot(gain_valid$mean.resp / mean(airlines_valid$Phone_sale), names.arg = gain_valid$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

gain_valid <- gains( airlines_valid$Phone_sale, validation.prediction$net.result[,1])
barplot(gain_valid$mean.resp / mean(airlines_valid$Phone_sale), names.arg = gain_valid$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")


gain <- gains(valid.df$Phone_sale_1, validation.prediction$net.result[,1])
gain
barplot(gain$mean.resp / mean(valid.df$Phone_sale_1,na.rm = TRUE), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart with validation data",ylim = c(0,2), col="gold")


# ビジネスの観点から、validationの最も左のバーを解釈





###############################
# b
###############################



###############################
# c
###############################



###############################
# d
###############################



###############################
# e
###############################




# EOF