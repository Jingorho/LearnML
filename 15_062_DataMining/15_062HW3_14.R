# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 3 : 14.2 Association Analysis
# -----------------------------------------------------------

###############################
# ライブラリとか
###############################
# install.packages("arules")
library(arules) 
#options(scipen=999, digits = 4) # 表示桁を揃える


###############################
# データの読み込みと前処理
###############################
course_raw <- read.csv("/Users/yukako/WorkSpace/DataMiningHW/data/Coursetopics.csv")
head(course_raw); str(course_raw); summary(course_raw);
# set.seed(1) #確率を固定する
# sum(is.na(course_raw)) # 欠損値NAの確認
# course <- na.omit(course) # 欠損値NAが含まれる行を削除
# table(course_raw) # なんとなくデータがどんなんか確認したかったので
dim(course_raw) # なんとなくデータがどんなんか確認したかったので



###############################
# アソシエーション分析
###############################

# [分析に使うデータ型の設定]

# convert to matrix
course_raw <- as.matrix(course_raw)

# convert the binary incidence matrix into a transactions database
# as.factor()とかと同じような感じで、データの型を変換する。このとき、
# アソシエーション分析で使われる特別なtransactions型というやつに変換する必要があるので
# (inspect()がtransaction型しか受け付けてないから)、
# いつも(as.factor()とかas.numeric()とか)のようにas.transaction()とは書けず、
# as(データ, "transactions")と書く必要がある。
# ちなみにas()でtransactions型に変換できるのはmatrix型のみなのでas.matrix()を使ってる、と予測、多分
course_tran <- as(course_raw, "transactions") 

# transaction型のデータをコンソールに表示するための関数inspect()
inspect(course_tran)

# (任意) transaction型のデータをグラフで表示するための関数itemFrequencyPlot()
itemFrequencyPlot(course_tran)
# (任意) データの概要を確認
summary(course_tran)




# [分析を実行]

# 使わない 自分用メモ -----
# itemFrequencyPlot(course_tran)で、"Intro"が多いっぽかったので、
# support値の最大値は"Intro"数/全数かなと予想
# minSup <- sum(course_raw$Intro) / dim(course_raw)[1]
# 使わない 自分用メモ -----


## get rules
# when running apriori(), include the minimum support, minimum confidence, and target as arguments.
# アソシエーション分析に使われるapriori()関数を使って、transactionデータの「ルール」を生成
# support値とconfidence値(->教科書参照)の最小値を指定。target="rules"はapriori()の場合は固定。
rules <- apriori(course_tran, 
                 parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
# suppとconfは一旦テキトーに決めてる [???]どうやって決めるの？


# inspect the first six rules, sorted by their lift
# 生成したルールを表示、それもlift値で降順で
inspect(head(sort(rules, by = "lift")))
# inspect(head(sort(rules, by = "confidence"))) # confidence値で降順で
# inspect(head(sort(rules, by = "support"))) # support値で降順で

# EOF