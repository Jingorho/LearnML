# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 2 : 6.1 Multiple Linear regression
# -----------------------------------------------------------

###############################
# ライブラリとか
###############################
# 初回実行時のみinstall.packages()を使って
# 2回目以降はコメントアウトして実行しないようにするとよいです
#install.packages("forecast")
library(forecast) # accuracy()用
#install.packages("car")
library(car) # vif()用
require(car) # vif()用
#install.packages("leaps")
library(leaps) # regsubsets()用

# ------ 任意 ------ #
#options(scipen=999, digits = 4) # 表示桁を揃える
# ------------------ #



###############################
# データの読み込みと前処理
###############################
# ""の中身はご自身の環境に合わせて書き換えてください
housing.df <- read.csv("data/BostonHousing.csv")
housing.df <- housing.df[,-14] # 問題文より13 predictorを使うらしいので14列目を削除
#set.seed(1) #確率を固定する

# trainデータとvalidationデータに分割
# sample(データ, 無作為抽出する個数)なので
# trainのインデックス <- sample(1から506までの全てのインデックス, 抽出する個数=506の60%)
# housing.dfの中からtrain.indexという数値列のインデックスに対応するデータを抽出する
train.index <- sample(as.numeric(row.names(housing.df)), 0.6*dim(housing.df)[1])
valid.index <- setdiff(row.names(housing.df), train.index)
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]
# または
#valid.df <- housing.df[-train.index] #インデックスをマイナスにすると「それ以外」

# ------ 任意 ------ #
head(housing.df)
summary(housing.df); str(housing.df); nrow(housing.df); ncol(housing.df)
head(train.df); head(valid.df) # 最初の5行だけ確認
sum(is.na(housing.df)) # 欠損値の確認、ないのでおk
# ------------------ #



###############################
# a
###############################

# 記述なので省略



###############################
# b
###############################

# !!!!!! 重要 !!!!!! (特にネットに載ってないポイント;;)
# predict()では、モデルに使った変数名と同じ変数名を持つデータを学習させる必要がある。
# ので、$を使わず変数(列)名で直接モデルを書くか、列をそれぞれ何か変数に入れて
# その変数名をnewdataにも使うか
# lm(予測する対象 ~ 予測するのに使う変数1 + 予測するのに使う変数2 + ..., data = データ)
model <- lm(MEDV ~ CRIM + CHAS + RM, data = train.df)
summary(model)

#model <- lm(housing.df$MEDV ~ housing.df$CRIM + housing.df$CHAS + housing.df$RM,
#            data = train.df)
# [???] subsetで条件式を指定してるのになぜ上下で結果が変わるの？
#model <- lm(housing.df$MEDV ~ housing.df$CRIM + housing.df$CHAS + housing.df$RM,
#            data = housing.df, subset = train.index)



###############################
# c
###############################

# モデル構築時に使った変数名と揃えて新しい学習データ作る
newdf <- data.frame(CRIM = 0.1, CHAS = 0, RM = 6) # 問題文で0.1, 0, 6指定あり
# 構築したモデルを使って新しいデータを学習させる
# MEDVを推定するモデルを作ったけど、このCRIM, CHAS, RMだとどんなMEDV?
pred <- predict(model, newdata = newdf)
# [???] モデルで式を作ったんだから、そのまま係数に代入する式を作っちゃダメなの?
#model$coefficients[2]*newdf[1] + model$coefficients[3]*newdf[2] + model$coefficients[4]*newdf[3] + model$coefficients[1]
# あれ...predict()の結果と一致した.........
# -> ちゃんとpredict()を正しく使えば式に代入した値と同じ奴が出てくるらしい。



###############################
# d
###############################

### i

model_all <- lm(MEDV ~ ., data = train.df)
summary(model_all)
# [???] 問題6.1.d.i measuring the same thing among the 13 predictors?
# same "thing"とは？係数が同じってこと？推定値が同じってこと？
# 同問題のINDUS, NOX, TAXの関係は、INDUS(*なし:p>.10なので有意でない), 
# NOX(***:p<.01なのでめっちゃ有意), TAX(*:p<.05なので有意)って感じ?



### ii

# 相関行列
cor(housing.df)
# 相関が強いやつは回帰分析に使えないので(多重共線性)、取り除く
# [???]「取り除く」の具体的な操作とは？単純にモデル推定の時に変数を使わない（orデータセットから削除する）ということ？




### iii

# ステップワイズ法にはstep()またはMASSパッケージのstepAIC()を使う
# stepAIC()の方が細かい解釈ができるらしいが、大まか同じ
model_no <- lm(MEDV ~ 1, data = train.df)

# backward(変数減少法)のモデルは、変数全部(全部を表す「.」)でスタート
stepBa <- step(model_all, direction="backward")
# forward(変数増加法)のモデルは、変数なし(定数項を表す1のみ)でスタート
stepFo <- step(model_no, direction="forward")
# both(変数増減法)のモデルは、変数なし(定数項1)のみスタートでも全部スタートでもいいらしい...
stepBo <- step(model_no, direction="both")

summary(stepBa); summary(stepFo); summary(stepBo);

# [???] forwardとbothで全然変数の変化なしにすぐモデルが終わっちゃう。なんで????www


# 構築したモデルでvalidationから推定値を作る
pred_Ba <- predict(stepBa, valid.df)
pred_Fo <- predict(stepFo, valid.df)
pred_Bo <- predict(stepBo, valid.df)

# 答え合わせ
accuracy(pred_Ba, valid$MEDV)
accuracy(pred_Fo, valid$MEDV)
accuracy(pred_Bo, valid$MEDV)



#EOF