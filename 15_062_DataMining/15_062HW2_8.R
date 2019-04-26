# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 2 : 8.2 Naive Bayse
# -----------------------------------------------------------

###############################
# ライブラリとか
###############################
#install.packages("e1071") #ナイーブベイズ用
library(e1071)
library(caret)
library(forecast)

# --- 実行不要 --- #
#options(scipen=999, digits = 4) # 表示桁を揃える
# ---------------- #


###############################
# データの読み込みと前処理
###############################
accidents_raw <- read.csv("data/AccidentsFull.csv")
#set.seed(1) #確率を固定する

# 問題8.2用に、"INJURY"というダミー変数を作成
# ifelse(条件式, TRUEの時の処理, FALSEの時の処理)
injury <- ifelse(accidents_raw$MAX_SEV == 0, 0, 1)  # no-injury(0)のときだけ0, fatal, non-fatalは1
accidents <- data.frame(accidents_raw, INJURY = injury)

# --- ここは任意 --- #
head(accidents)       # 最初の5行だけ確認
sum(is.na(accidents)) # 欠損値の確認、ないのでおk

# [???] データ構造を最初に確認するときに把握しておくべきこと、get a senseするアイデア

# データ構造を予め把握しておく(自分用メモ)
summary(accidents) # 基本の記述統計量(四分位数(最小最大値と中央値含む)と平均値)をまとめて算出
str(accidents) # オブジェクトの構造と水準をまとめて調べる
sapply(accidents, mode) # オブジェクトの型(integer(整数), numeric(実数), character, list, ...)を調べる
sapply(accidents, class) # オブジェクトの構造(vector, matrix, factor, list, ...)を調べる
summary(as.factor(accidents$WEATHER_R))
summary(as.factor(accidents$TRAF_CON_R))
# ------------------ #



###############################
# a
###############################

# [???]問題文が何を聞きたいのか謎
table(accidents$INJURY, dnn = c("負傷者"))
# でINJURY=1の方が多いから負傷者がいるのでは、とか...？？？



###############################
# b
###############################

### i

# [???] 12個しか抽出しないのはなんで？しかもなぜランダムでなく最初の？

# 12個のrecord(サンプル)を抽出し、INJURYをresponse(予測する対象)として, 
# WEATHERとTRAFをpredictor(予測するために使う変数)とする
# sample(無作為抽出する数値列, 個数)はデータフレームからデータを直接抽出するのではなく、
# 行番号となる数値を抽出する感じで使う。accidents[抽出した行番号, ]でデータの中身を抽出
#pickedAcc <- accidents[ sample(nrow(accidents), 12),  ]
pickedAcc <- accidents[1:12, ]
# 問題で指定された列だけ抽出
#pickedAcc <- pickedAcc[, c("WEATHER", "TRAF", "INJURY")]
  
# table()でクロス集計表を作ってデータの分布を確認
# table(表示したい要素, 表示したい要素2, ..., dnn=c(表示したい要素1のラベル, 表示したい要素2のラベル, ...))
# 4要素以上になると表示が複雑になるので3要素くらいが良さげ
t <- table(pickedAcc$INJURY, pickedAcc$WEATHER_R, pickedAcc$TRAF_CON_R,
           dnn = c("負傷者", "悪天候", "2車線道路"))
t
# prop.table()で、頻度ではなく確率でのクロス集計表を表示
propt <- prop.table(t)
propt



### ii

# [???] 計算式が合ってるか全体的に不安(12個しかないので該当データがない=確率0になりがち,,,どうすんの)

# INJURY=1のときのexactな条件付き確率を求める
# 同時確率ではない(値が全然違う <- Recitation3Part2 68行目)ので注意
# ナイーブベイズの確率とも違うので注意
# table()で求めた3変数のクロス集計表は、3次元行列になってるので[x,y,z]的な感じでアクセスできる
# [INJURY, WEATHER, TRAF]の順
# ex.) Pr(INJURY=1 | WEATHER=0 && TRAF=0)
# = Pr(I=1 && W=0 && T=0) / Pr(W=0 && T=0)
# = {(I=1かつW=0かつT=0の個数)/(合計個数)}  /  {(W=0かつT=0の個数)/(合計個数)}
i1w1t0 <- t[2,1,1] / (t[1,1,1] + t[2,1,1]) # INJURY=1, WEATHER=1, TRAF=0
i1w1t1 <- t[2,1,2] / (t[1,1,2] + t[2,1,2]) # INJURY=1, WEATHER=1, TRAF=1
i1w1t2 <- t[2,1,3] / (t[1,1,3] + t[2,1,3]) # INJURY=1, WEATHER=1, TRAF=2
i1w2t0 <- t[2,2,1] / (t[1,2,1] + t[2,2,1]) # INJURY=1, WEATHER=2, TRAF=0
i1w2t1 <- t[2,2,2] / (t[1,2,2] + t[2,2,2]) # INJURY=1, WEATHER=2, TRAF=1
i1w2t2 <- t[2,2,3] / (t[1,2,3] + t[2,2,3]) # INJURY=1, WEATHER=2, TRAF=2
i1w1t0; i1w1t1; i1w1t2; i1w2t0; i1w2t1; i1w2t3; # 表示
# prop.table()で出した行列でやってもtable()で出した行列でやっても結果同じ。どっち使ってもいい



### iii

# [???] 計算式が合ってるか全体的に不安(12個しかないので該当データがない=確率0になりがち,,,どうすんの)

propt_nb <- prop.table(t, margin = 1)
propt_nb
propt_i <- prop.table(table(pickedAcc$INJURY))
P_i0 <- propt_i[1]
P_i1 <- propt_i[2]

P_i1w0 <- propt_nb[2,1,1] + propt_nb[2,1,2] + propt_nb[2,1,3] # Pr(INJURY=1 | WEATHER=1)
P_i1w1 <- propt_nb[2,2,1] + propt_nb[2,2,2] + propt_nb[2,2,3] # Pr(INJURY=1 | WEATHER=2)
P_i1t0 <- propt_nb[2,1,1] + propt_nb[2,2,1] # Pr(INJURY=1 | TRAF=0)
P_i1t1 <- propt_nb[2,1,2] + propt_nb[2,2,2] # Pr(INJURY=1 | TRAF=1)
P_i1t2 <- propt_nb[2,1,3] + propt_nb[2,2,3] # Pr(INJURY=1 | TRAF=2)
P_i0w0 <- propt_nb[1,1,1] + propt_nb[1,1,2] + propt_nb[1,1,3] # Pr(INJURY=0 | WEATHER=1)
P_i0w1 <- propt_nb[1,2,1] + propt_nb[1,2,2] + propt_nb[1,2,3] # Pr(INJURY=0 | WEATHER=2)
P_i0t0 <- propt_nb[1,1,1] + propt_nb[1,2,1] # Pr(INJURY=0 | TRAF=0)
P_i0t1 <- propt_nb[1,1,2] + propt_nb[1,2,2] # Pr(INJURY=0 | TRAF=1)
P_i0t2 <- propt_nb[1,1,3] + propt_nb[1,2,3] # Pr(INJURY=0 | TRAF=2)

Pnb_i1w0t0 <- (P_i1w0*P_i1t0 * P_i1) / ((P_i1w0*P_i1t0 * P_i1) + (P_i0w0*P_i0t0 * P_i0)) # INJURY=1, WEATHER=0, TRAF=0
Pnb_i1w1t0 <- (P_i1w1*P_i1t0 * P_i1) / ((P_i1w1*P_i1t0 * P_i1) + (P_i0w1*P_i0t0 * P_i0)) # INJURY=1, WEATHER=1, TRAF=0
Pnb_i1w0t1 <- (P_i1w0*P_i1t1 * P_i1) / ((P_i1w0*P_i1t1 * P_i1) + (P_i0w0*P_i0t1 * P_i0)) # INJURY=1, WEATHER=0, TRAF=1
Pnb_i1w1t1 <- (P_i1w1*P_i1t1 * P_i1) / ((P_i1w1*P_i1t1 * P_i1) + (P_i0w1*P_i0t1 * P_i0)) # INJURY=1, WEATHER=1, TRAF=1
Pnb_i1w0t2 <- (P_i1w0*P_i1t2 * P_i1) / ((P_i1w0*P_i1t2 * P_i1) + (P_i0w0*P_i0t2 * P_i0)) # INJURY=1, WEATHER=0, TRAF=2
Pnb_i1w1t2 <- (P_i1w1*P_i1t2 * P_i1) / ((P_i1w1*P_i1t2 * P_i1) + (P_i0w1*P_i0t2 * P_i0)) # INJURY=1, WEATHER=1, TRAF=2
Pnb_i1w0t0; Pnb_i1w1t0; Pnb_i1w1t1; Pnb_i1w0t1; Pnb_i1w0t2; Pnb_i1w1t2; 
# Pnb_i0w0t0 <- (P_i0w0*P_i0t0 * P_i0) / ((P_i0w0*P_i0t0 * P_i0) + (P_i0w0*P_i0t0 * P_i0)) # INJURY=0, WEATHER=0, TRAF=0
# Pnb_i0w1t0 <- (P_i0w1*P_i0t0 * P_i0) / ((P_i0w1*P_i0t0 * P_i0) + (P_i0w1*P_i0t0 * P_i0)) # INJURY=0, WEATHER=1, TRAF=0
# Pnb_i0w0t1 <- (P_i0w0*P_i0t1 * P_i0) / ((P_i0w0*P_i0t1 * P_i0) + (P_i0w0*P_i0t1 * P_i0)) # INJURY=0, WEATHER=0, TRAF=1
# Pnb_i0w1t1 <- (P_i0w1*P_i0t1 * P_i0) / ((P_i0w1*P_i0t1 * P_i0) + (P_i0w1*P_i0t1 * P_i0)) # INJURY=0, WEATHER=1, TRAF=1
# Pnb_i0w0t2 <- (P_i0w0*P_i0t2 * P_i0) / ((P_i0w0*P_i0t2 * P_i0) + (P_i0w0*P_i0t2 * P_i0)) # INJURY=0, WEATHER=0, TRAF=2
# Pnb_i0w1t2 <- (P_i0w1*P_i0t2 * P_i0) / ((P_i0w1*P_i0t2 * P_i0) + (P_i0w1*P_i0t2 * P_i0)) # INJURY=0, WEATHER=1, TRAF=2


# [???] naiveBayes()を使わず確率を手動で計算して、そしてClassifyする場合、Classifyの部分はどうするのか、、、

# ボツ案 -------
# 手計算でexact condentioned probabilityを求めた時のClassifyの仕方が分からなかったので、
# テキストの文章を元にベタがきしています...。もっとスマートなやり方があると思います。
pr <- rep(1, 12) # 確率を入れる変数列を用意
estimatedINJURY <- rep(0, 12)
cutoff <- 0.5 #問題文より
for (i in c(1:12)){
  d <- pickedAcc[i,] #pickAccデータを順番にみていく
  #既存データdのWEATHERとTRAFから、iiで計算したINJURY=1に所属する確率を当てはめていく
  #ifelse(条件式, TRUEの時の処理, FALSEの時の処理)で、FALSE時に次のifelse()を挿入するという入れ子構造にしてる
  pr[i] <- ifelse(d[2] == 0 && d[3] == 0, Pnb_i1w0t0,
           ifelse(d[2] == 0 && d[3] == 1, Pnb_i1w0t1,
           ifelse(d[2] == 0 && d[3] == 2, Pnb_i1w0t2,
           ifelse(d[2] == 1 && d[3] == 0, Pnb_i1w1t0,
           ifelse(d[2] == 1 && d[3] == 1, Pnb_i1w1t1,
           ifelse(d[2] == 1 && d[3] == 2, Pnb_i1w1t1, Pnb_i1w1t2 ))))))
  print(data.frame(i, pr[i]))
  if(pr[i] >= cutoff){
    estimatedINJURY[i] <- 1 #当てはめられたINJURY=1に所属する確率がcutoff値以上なら1
  }else{
    estimatedINJURY[i] <- 0 #cutoff値以下なら0
  }
}
data.frame(pickedAcc, pr, estimatedINJURY) #結果表示
# ボツ案 ------


### iv

# INJURYの"ナイーブベイズにおける"条件付き確率を求める
#propt[2,2,2] / (propt[2,2,2] + propt[2,1,2]) # INJURY=1, WEATHER=1, TRAF=1

# WED=1かつTRA=1 のうえで INJ=1 => Pr(WED | INJ) と Pr(TRA | INJ)を求める
pWI <- prop.table(table(pickedAcc$INJURY, pickedAcc$WEATHER_R, dnn = c("負傷者","悪天候")), margin = 1)
pTI <- prop.table(table(pickedAcc$INJURY, pickedAcc$TRAF_CON_R, dnn = c("負傷者","2車線道路")), margin = 1)
# INJURY
pI <- prop.table(table(pickedAcc$INJURY, dnn = "負傷者"))
pWI; pTI; pI

# Recitationはこんな感じで計算してる
# nbPr(Has CD | Has CC, !Sec. Acct)
# = Pr(Has CC | Has CD) * Pr(!Sec. Acct | Has CD) * Pr(Has CD) / [(numerator) + (almost same numerator except with (!Has CD))]
# nb.numerator <- p.t5[2,2]*p.t6[2,1]*p.t7[2]
# nb.denominator <- nb.numerator + p.t5[1,2]*p.t6[1,1]*p.t7[1]

# Recitationの真似をするとこんな感じ?
# nbPr(INJURY=1 | WED=1, TRA=1) 
# = Pr(WED=1 | INJURY=1) * Pr(TRA=1 | INJURY=1) * Pr(INJURY=1) / [(numerator) + (almost same numerator except with (!INJURY=1))]
# 日本語で解釈するならこんな感じ?
# nbPr(悪天候かつ2車線のときに負傷者がいる) 
# = Pr(負傷者がいるとき悪天候) * Pr(負傷者がいるとき2車線) * Pr(負傷者がいる) / [(numerator) + (almost same numerator except with (負傷者がいない))]
nb.numerator <- pWI[2,2] * pTI[2,2] * pI[2]
nb.denominator <- nb.numerator + pWI[1,2] * pTI[1,2] * pI[1]
nbPr <- nb.numerator/nb.denominator
nbPr

# [???]全体的に0になるのは12個しかないからしょうがないの？？？？



### v

# ナイーブベイズはカテゴリカルデータにしか使えないので、
# 数値データ(0/1とかも)をカテゴリカルデータに変換 (<-Recitation3Part2)
pickedAcc$INJURY <- factor(pickedAcc$INJURY)
pickedAcc$WEATHER_R <- factor(pickedAcc$WEATHER_R)
pickedAcc$TRAF_CON_R <- factor(pickedAcc$TRAF_CON_R)
# ナイーブベイズ実行
pickedAcc.nb <- naiveBayes(INJURY ~ WEATHER_R + TRAF_CON_R, data = pickedAcc)
pickedAcc.nb #pWIとpTIで出力した行列と同じ、条件付き確率が返される
# 注意: 以下のようにデータフレーム名$変数名と指定すると、predict()のところでエラーが出る
#nb <- naiveBayes(pickedAcc$INJURY ~ pickedAcc$WEATHER + pickedAcc$TRAF, data = pickedAcc)

# naiveBayes()で出力したnbが正しく予測に使えるか確認
test.row <- accidents[1,] #accidentsデータを見てみて27行目のデータが今回の条件「悪天候かつ2車線のときに負傷者がいる」を満たしてるので
nbPr2 <- predict(pickedAcc.nb, newdata = test.row, type = "class") 
nbPr2 #INJURY=1となるデータを入れて1と推測してるので良さげ

# [???] 結果の見方あってる？



###############################
# c
###############################

# trainデータとvalidationデータに分割
train.index <- sample(row.names(accidents), 0.6*dim(accidents)[1])
valid.index <- setdiff(row.names(accidents), train.index)
train.df <- accidents[train.index, ]
valid.df <- accidents[valid.index, ]
head(train.df); head(valid.df)


### i

# [???] 問題文が何を聞きたいか謎

vars <- c("INJURY", "HOUR_I_R", "ALIGN_I", "WRK_ZONE", "WKDY_I_R",
          "INT_HWY", "LGTCON_I_R", "PROFIL_I_R", "SPD_LIM", "SUR_COND",
          "TRAF_CON_R", "TRAF_WAY", "WEATHER_R")

accidents.nb <- naiveBayes(as.factor(INJURY) ~ HOUR_I_R + ALIGN_I + WRK_ZONE + WKDY_I_R
                           + INT_HWY + LGTCON_I_R + PROFIL_I_R + SPD_LIM + SUR_COND
                           + TRAF_CON_R + TRAF_WAY + WEATHER_R, data = train.df)


### ii

# ナイーブベイズはカテゴリカルデータにしか使えないので、
# 数値データ(0/1とかも)をカテゴリカルデータに変換 (<-Recitation3Part2)
for (i in c(1:ncol(accidents))){
  accidents[, i] <- as.factor(accidents[, i])
}
#str(accidents)

# [???] 全列（データフレーむ全体）を一気にカテゴリカルにするのにもっとスマートな方法はないのか


# as.factor()で明示的にカテゴリカルデータに変換する必要があるらしい
# https://stackoverflow.com/questions/19961441/naivebayes-in-r-cannot-predict-factor0-levels
# 上記for文の後にstr(accidents)で確認すると全てFactor型になってるようだがなぜ...
# あとaccidents$INJURYではなくINJURYと表記する必要があるらしい
accidents.nb <- naiveBayes(as.factor(INJURY) ~ ., data = train.df)
accidents.nb

# trainingの精度確認
# pred.class <- predict(accidents.nb, newdata = train.df)
# cm_train <- confusionMatrix(as.factor(pred.class), as.factor(train.df$INJURY)) #ここもas.factor()と明記しないとエラー

# validationの精度確認
pred.class <- predict(accidents.nb, newdata = valid.df)
cm_valid <- confusionMatrix(as.factor(pred.class), as.factor(valid.df$INJURY))

# [???] trainはモデル推定に使って、validは精度確認に使うので、
# この場合の「trainの精度確認」はいらないってことでおk？validの使い方合ってる？



# iii

# Overall error rate : a main accuracy measure, estimated misclassification rateとも呼ばれる(教科書TABLE5.3辺り)
# err = (n_{1,2} + n_{2,1}) / n     -> おさらい 教科書 TABLE5.2, 5.3
# n       : total number of record in validation dataset
# n_{1,2} : number of C_1(例えばno-injuryクラス) records classified incorrectly C_2(例えばfatalクラス)
# n_{2,1} : number of C_2(例えばfatalクラス) records classified incorrectly C_1(例えばno-injuryクラス)
# confusionMatrixで出力されるテーブルの中身にアクセスしてる
overallError <- (cm_valid$table[1,2] + cm_valid$table[2,1]) / sum(cm_valid$table)
# たまーに何かしら値出るけど、ほぼerror0でaccuracy1...

# [???] overall error の出し方あってるか不安



# iv

# Naive rule : predictorに関係なく一番メジャーなクラスに分類する -> おさらい TABLE5.2辺り
# 一番メジャーなクラスはINJURY=1なので、全てのrecordをpredictor関係なしにINJURY=1に分類するってこと？

prevalentClass <- names(which.max(table(valid.df$INJURY))) # 最頻クラスを特定
pred.class_naiveRule <- rep(prevalentClass, length(valid.df$INJURY)) # 全てが最頻クラスに分類されたとするデータ列作る

# 全てが最貧クラスに分類されたとするデータ列と、実際のデータ列との比較
cm_valid_naiveRule <- confusionMatrix(as.factor(pred.class_naiveRule), as.factor(valid.df$INJURY))
overallError_naiveRule <- (cm_valid_naiveRule$table[1,2] + cm_valid_naiveRule$table[2,1]) / sum(cm_valid_naiveRule$table)

# [???] Naive ruleでの分類学習はこれであってるの？


# Naive ruleで精度比較するときはR^2なるものを計算するらしい(-> TABLE5.2辺り)
# One example is multiple R^2, 
# which measures the distance between the fit of the classifier to the data 
# and the fit of the naive rule to the data.

# R^2とはなんぞやというと、Residual devianceと呼ばれるDらしい(-> Section10.6)
# The deviance D is a statistic that measures overall goodness of fit. 
# It is similar to the concept of sum of squared errors (SSE) 
# in the case of least squares estimation (used in linear regression). 
# We compare the deviance of our model, D (called Residual deviance in R), 
# to the deviance of the naive (Null) model, D0.

# Residual devianceの求め方は、glmの文脈だとよく出てくるけど、Bayesの文脈ではよくわからん [???]
# なので、ひとまずoverallErrorとoverallError_naiveRuleの比較をしてみる...
percentageImprovement <- overallError_naiveRule - overallError
percentageImprovement



# v

# Pr(INJURY = 0 | SPD_LIM = 5)を計算してみる
tSpeed <- table(accidents$INJURY, accidents$SPD_LIM, dnn = c("負傷者", "速度制限"))
table(accidents$SPD_LIM) # クロス集計表

pSI <- prop.table(table(accidents$INJURY, accidents$SPD_LIM, dnn = c("負傷者","速度制限")), margin = 1)
# INJURY
pI <- prop.table(table(accidents$INJURY, dnn = "負傷者"))
pSI; pI

# Recitationはこんな感じで計算してる
# nbPr(Has CD | Has CC)
# = Pr(Has CC | Has CD) * Pr(Has CD) / [(numerator) + (almost same numerator except with (!Has CD))]
# nb.numerator <- p.t5[2,2]*p.t6[2,1]*p.t7[2]
# nb.denominator <- nb.numerator + p.t5[1,2]*p.t6[1,1]*p.t7[1]

# Recitationの真似をするとこんな感じ?
# nbPr(INJURY=1 | SPD=5) 
# = Pr(SPD=5 | INJURY=1) * Pr(INJURY=1) / [(numerator) + (almost same numerator except with (!INJURY=1))]
# 日本語で解釈するならこんな感じ?
# nbPr(速度遅い時に負傷者がいる) 
# = Pr(負傷者がいるとき速度遅い) * Pr(負傷者がいる) / [(numerator) + (almost same numerator except with (負傷者がいない))]
nb.numerator <- pSI[1,1] * pI[1]
nb.denominator <- nb.numerator + pWI[1,2] * pI[2]
nbPr <- nb.numerator / nb.denominator
nbPr

# [???] nbPrが0になるらしいけど、なりませんけど！！！！！！怒


table(accidents$INJURY, accidents$SPD_LIM, dnn = c("負傷者","速度制限"))

# EOF(-ω-)