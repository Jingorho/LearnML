# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 2 : 10.2 Logistic Regression
# -----------------------------------------------------------

###############################
# ライブラリとか
###############################
library(caret)

# --- 実行不要 --- #
#options(scipen=999, digits = 4) # 表示桁を揃える
# ---------------- #


###############################
# データの読み込みと前処理
###############################

sysAd <- read.csv("data/SystemAdministrators.csv")
#set.seed(1) #確率を固定する

# ロジスティック回帰で分類するときにYES/NOという文字列じゃない方が
# 良さげなので1/0に変換 <- 要確認
sysAd$Completed.task <- as.factor(as.numeric(sysAd$Completed.task)-1) #YES/NOをそのままnumericにすると2/1になるので便宜上-1してる


# --- ここは任意 --- #
head(sysAd); 
sum(is.na(sysAd)) # 欠損値の確認、ないのでおk
# ------------------ #



###############################
# a
###############################

# colは色を第一引数のxと第二引数のyそれぞれで指定、cexは点の大きさを指定、
# シンボルを変えたい場合はpchを使う
plot(x = sysAd$Training, y = sysAd$Experience, 
     xlab = "Number of relevant training credits", 
     ylab = "Months of full-time system administrator experience",
     col = c("blue", "red"), cex = 0.8)
legend("bottomright", legend = c("Completed", "Not Completed"), col = c("blue", "red"), pch = 1)
title("Experience v.s. Training") #いいタイトル思いつかない


# 参考: TrainingとExperienceだけで見るとこんな感じ
# plot(sysAd$Training,
#      xlab = "index", 
#      ylab = "Number of relevant training credits",
#      col = c("blue", "red"), cex = 0.8)
# plot(sysAd$Experience,
#      xlab = "index", 
#      ylab = "Months of full-time system administrator experience",
#      col = c("blue", "red"), cex = 0.8)


# かけそうなこと *****************************
# table(sysAd$Training, dnn = "Training") # 問題文の指定にはないけど、examineのために追加してみた
# table()でTrainingの中身を見ると、Trainingは4,6,8のみ。plot()を見ると、
# 4の時に赤（Not Completed）、6以上で青（Completed）が多いので、Trainingが判断に使えそう。
# かけそうなこと *****************************




###############################
# b
###############################

# glm()でfamily="binomial"("2項の"という意味)を指定するとロジスティック回帰を実行してくれる
logitReg <-glm(sysAd$Completed.task ~ sysAd$Experience + sysAd$Training,
               data = sysAd, family = "binomial")
summary(logitReg)

# できたモデルの精度を確認
pred_logitReg <- predict(logitReg, sysAd) # できたモデルを使って推定値を作ってみる
# ifelse(pred_logitReg > cutoff, 1, 0)は、
# predがcutoff値より大きい確率だったら1に分類、それ以外は0に分類としたデータ列
cutoff <- 0.5
cm_logitReg <- confusionMatrix(as.factor(ifelse(pred_logitReg > cutoff, 1, 0)), sysAd$Completed.task)
cm_logitReg
#incorrectlyClassified <- 1 - cm_logitReg$overall[1] #confusionMatrixの出力にアクセスするoverallという要素の1つめ = Accuracy
# confusionMatrixで出力されるtableのうち、Referenceは1なのにPredictionは0になってる割合
incorrectlyClassified <- cm_logitReg$table[1,2] / sum(cm_logitReg$table) 
incorrectlyClassified

# # かけそうなこと *****************************
# "using entire dataset as training data"に注意して、glm(data = 全体データ(sysAd))としてロジスティック回帰を実行。
# (plot()でみたときはTrainingが関係してそうだが、ロジスティック回帰はExperienceが関係してそうと言っている...)
# "what is the percentage of programmers incorrectly classified"とのことなので、confusionmatrixでconfusionMatrixで
# 誤って分類されてる数を確認。confusionmatrixで出力されたReference(正解)とPrediction(推定したやつ)の表を見ると、
# 正しく分類されてるやつ、つまり 正解が0で推定も0のやつが59個、正解が1で推定も1のやつが9個。
# 誤って分類されてるやつ、つまり正解が1なのに推定が0のやつが6個、正解が0なのに推定が1のやつが1個。
# 問題では、"incorrectly classified as failing to complete the task"とのことなので、
# 正解が1なのに誤って0と分類されてる6個の確率（全体個数に対する割合）を求める。
# (cm_logitReg$table[1,2]で表の1行目2列目) / sum(cm_logitReg$table)で表の全ての個数
# かけそうなこと *****************************




###############################
# c
###############################

# よく分からないのでベタがき...
# 推定値と実測値を並べて観察してみる
data.frame(Pred = pred_logitReg, Classified = ifelse(pred_logitReg > cutoff, 1, 0),
           Actual = sysAd$Completed.task)
# Classifiedが0なのにActualが1になってる所の推定値を見ると、値がcutoffより小さい
# ので、cutoffを小さくすれば、許容してくれる...かも
# でもPredって確率?だと思うんだが、何でマイナスとか絶対値1以上のものがあるのか謎...w [???]


# かけそうなこと *****************************
# bで求めた推定のクラス分類(Classifiedと表記、pred_logitRegがcutoffより大きかったら1、それ以外は0を並べたデータ列)と
# 正解値(Actualと表記、sysAd$Completed.task)を並べて表示してみた。
# Actualが1なのにClassifiedが0のPred（1に分類される確率）を見ると、
# いずれもcutoffより小さいので、cutoffを小さくすればこれらのデータも正しく1と分類されそう
# （でも確率なのになぜ1以上や負値があるのか謎...）。
# 逆に言うと、cutoffが小さすぎてActual0なのに間違えて1と分類してしまってるデータはない。
# かけそうなこと *****************************




###############################
# d
###############################
i <- 1
newdata <- sysAd
# これもよく分からないのでベタがき...泣
# (経験年数4年 = 48ヶ月, 訓練数 = i回)のデータを入れてpredictして、0.5を超えた時のi
# How much experience must be accumulated by a programmer with 4 years of training 
# before his or her estimated probability of completing the task exceeds 0.5?


# ※ロジスティック回帰のアルゴリズムをちゃんと理解してないので、
# 本当に頓珍漢なことをしてます...とりあえず何か書きました。

# ロジスティック回帰実行
logitReg2 <-glm(Completed.task ~ Experience + Training,
               data = sysAd, family = "binomial")
summary(logitReg2)

iterate <- 10 # 10回predictを繰り返す
pred.df <- data.frame(k = seq(1, iterate, 1), Pred = rep(0, iterate)) # predictを入れるデータフレームを予め作成
for (i in c(1:iterate)){
  # 経験年数4年 = 48ヶ月, 訓練数 = i回の新しいデータを作成
  newdata <- data.frame(Experience = 48, Training = i)
  # 新しいデータを作ったモデルで学習させる、type="response"は確率を求めるやつ
  pred.df[i, 2] <- predict(logitReg2, newdata, type="response")
}
pred.df # 48ヶ月も訓練したらCompleteの確率1になるに決まってるじゃん！
# 試しにExperience = 5とかにすると、訓練回数が増えるごとに
# いい感じに確率が増えていく感じになる


# かけそうなこと *****************************
# 問題文は"経験年数4年 = 48ヶ月のとき、Completeする確率が0.5を超えるのは
# 訓練回数が何回のとき？"と聞いてる？と思ったので、訓練回数を1から10まで増やして
# 推定を繰り返し実行した。pred.dfで、訓練回数が何回の時に0.5を超えてるか観察しようと
# した。だが、48ヶ月にもなると全部確率1になってしまった
# かけそうなこと *****************************

# [???] 全体的に問題が何を聞いてるのか分からない



# EOF(-ω-)