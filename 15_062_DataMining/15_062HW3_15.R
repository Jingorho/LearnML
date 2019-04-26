# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Homework 3 : 15.3 Cluster Analysis
# -----------------------------------------------------------

# 同じ処理がたくさん出てくるので、自分で関数「myClusterAnalysis」を
# 作りました(関数部分(196?279行目あたり)を最初に実行してから、25行目以降を実行してください)
# 
# myClusterAnalysis(変数1の列数, 変数2の列数, 元にするデータセット, クラスタ数, 
#                   "各点どうしの距離算出のmethodの種類", "クラスタどうしの距離算出のmethodの種類")
# 
# > ()の"各点どうしの距離算出のmethodの種類"の指定の仕方
# "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
# > ()の"クラスタどうしの距離算出のmethodの種類"の指定の仕方
# "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA),
# "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
# 
# 使用例)
# myClusterAnalysis(5, 12, cereals_raw, 6, "euclidean", "single")


###############################
# ライブラリとか
###############################
library(caret)
library(maptools)
#options(scipen=999, digits = 4) # 表示桁を揃える


###############################
# データの読み込みと前処理
###############################
cereals_raw <- read.csv("/Users/yukako/WorkSpace/DataMiningHW/data/Cereals.csv")
head(cereals_raw); str(cereals_raw); summary(cereals_raw);
# set.seed(1) #確率を固定する
sum(is.na(cereals_raw)) # 欠損値NAの確認
cereals_raw <- na.omit(cereals_raw) # 欠損値NAが含まれる行を削除

# 行名が長いので一旦取り出して削除
cerealNames <- cereals_raw[,1]
cereals_raw <- cereals_raw[,-1]

# カテゴリカルと数値データが混ざってたときの処理 1:
# カテゴリカルデータをダミー変数に変換
cereals_temp <- dummyVars(~., data = cereals_raw)
cereals_raw <- as.data.frame(predict(cereals_temp, cereals_raw))

row.names(cereals_raw) <- cerealNames

# カテゴリカルと数値データが混ざってたときの処理 2:
# 数値データを[0,1]に変換
cereals <- predict(preProcess(cereals_raw, method="range", rangeBounds=c(0, 1)), cereals_raw)
head(cereals);


# おまじまい(空のグローバル変数の定義)
hc <- NULL



###############################
# a
###############################

# [クラスタ分析の実行]
# - [最近隣法]
result_single <- myClusterAnalysis(10, 12, cereals, 6, "euclidean", "single")
# - [最遠隣法]
result_cmp <- myClusterAnalysis(10, 12, cereals, 6, "euclidean", "complete")

# FYI これでクラスタ形成の過程を観察できる
# hc_single$merge


# [structureとstabilityについてコメントする]
# - [structureを確認する]

# 何すればいいかよくわからん、要確認


# - [stabilityを確認する]
# データをAとBに分け、Aで生成したクラスタの重心をBにも使う
# 具体的には、Bの各データを、最も近い重心(Aで生成したクラスタの重心)に割り振る
a.index <- sample(row.names(cereals), 0.6*dim(cereals)[1])
b.index <- setdiff(row.names(cereals), a.index)
cereals_a <- cereals[a.index, ]
cereals_b <- cereals[b.index, ]

# "Bの各データを、最も近い重心(Aで生成したクラスタの重心)に割り振る"の具体的なやり方が不明、要確認




###############################
# b
###############################

colnames(cereals) # どんな変数があるか確認のため表示

# [クラスタ分析を実行]
# myClusterAnalysis()の()の中を分析者が自分で決めて
# いろんなパターンのクラスタ分析を実行できるようにしてみています
result_b <- myClusterAnalysis(10, 12, cereals, 6, "euclidean", "single")

# [meaningfulのために確認]
# 1. Cluster interpretability
#  a. Summary statistics
clSummary <- data.frame(
  sum = aggregate(result_b, by=list(result_b$clusters), sum),
  mean = aggregate(result_b, by=list(result_b$clusters), mean),
  sd = aggregate(result_b, by=list(result_b$clusters), sd),
  min = aggregate(result_b, by=list(result_b$clusters), min),
  median = aggregate(result_b, by=list(result_b$clusters), median),
  max = aggregate(result_b, by=list(result_b$clusters), max)
  # quantile = aggregate(cereals_cl, by=list(cereals_cl$cluster), quantile)
)

plotedVar <- cereals$calories # 見る変数を帰るときここを変更
clusterLists <- NULL
for (i in c(1:length(unique(result_b$clusters)))){
  clusterLists <- append(clusterLists, list(subset(plotedVar, result_b$clusters == i)))
}

boxplot(clusterLists)
# barplot(unlist(clusterLists))


#  b. Common feature that was not used in the cluster analysis
# 情報がないので不可能？

#  c. Labeling the clusters
# 結果を見てから

# 2. Cluster stability
# aでやることと重複

# 3. Cluster separation 有用性に疑問が残るとのことなので省略
# 4. Number of clusters 該当しなさそう




###############################
# c
###############################

# [クラスタ分析を実行] (bまでと同じ結果を用いる場合は実行不要)
# myClusterAnalysis()の()の中を分析者が自分で決めて
# いろんなパターンのクラスタ分析を実行できるようにしました
result_c <- myClusterAnalysis(10, 12, cereals, 6, "euclidean", "single")

# [cutoff値]
# 樹形図を見て、(私が実行した場合の樹形図では)0.2かなーーーとおもいます



###############################
# d
###############################

# "healty cereals"の定義:
# 1) Health diet : {calories, suger, fat, が少ない}かつ{fiber, vitaminsが多い} 
# [???]そのほかの変数はなに?
# 2) Every day a different cerelal : 少なくとも7種類(1週間分)を含むクラスタである?

# [クラスタ分析を実行] (cまでと同じ結果を用いる場合は実行不要)
# myClusterAnalysis()の()の中を分析者が自分で決めて
# いろんなパターンのクラスタ分析を実行できるようにしました
# result_d <- myClusterAnalysis(10, 12, cereals, 6, "euclidean", "single")
result_d <- myClusterAnalysis(1, 2, cereals_nut, 5, "euclidean", "average")
table(result_d$clusters)


# [ヒートマップを表示]
# rev() reverses the color mapping to large = dark
# hm <- heatmap(as.matrix(cereals_nut), Colv = NA, hclustfun = hclust,
#         col = rev(paste("gray", 1:99, sep="")))
hm <- heatmap(as.matrix(result_d[,-ncol(result_d)]), Colv = NA, hclustfun = hclust, 
              # ここで、行の順番を、関数myClusterAnalysis内で計算したhcオブジェクトと
              # 同じ樹形図の順番にすることを指定
              Rowv = as.dendrogram(hc),
              col = rev(paste("gray", 1:99, sep="")))
# heatmapの順番とクラスタの順番が合ってるぽいことを確認
# hm$rowInd
# data.frame(orig = rownames(cereals_nut),
#            ordered = rownames(cereals_nut[order(result_d$clusters),]))


# ヒートマップを見てみて、mftとtypeはほとんどクラスタに影響してなさそうなのと、
# shelf, weight, cupsは変数の定義が分からないので説明できないのと、
# ratingはクラスタしたい定義から外れるので、
# 栄養に関する変数だけ取り出して分析し直した方がいいかも、と思いました。
# 栄養に関する変数だけ取り出す
cereals_nut <- cereals[, c(10:18)]
colnames(cereals_nut)
# FYI クラスタごとの昇順に並べ替えたいとき
# cereals[order(clNumCol), ]



# [Normalizeする?しない?]




###############################
# 自作関数
###############################
myClusterAnalysis <- function(varNum1, varNum2, data, clNum, distanceMethod, clusterDistMethod) {
  
  # 2次元で描画するときのx軸とy軸にする変数の列番号を指定
  colNums <- c(varNum1, varNum2)
  # 列番号を元にデータから列名を抽出
  selectedColNames <- c(colnames(cereals)[colNums[1]], colnames(cereals)[colNums[2]])
  # 列番号を元にデータから列を抽出
  selectedVar <- data.frame(data[, colNums[1]], data[, colNums[2]]) 
  selectedVar <- data.frame(data$vitamins, data$potass, data$fiber, data$protein) 
  
  # [各点どうしの距離を算出]
  # 各点どうしの距離として下のうちどれかのmethodを指定
  # 今回は特別に自作関数にしているので、関数の外から引数として指定
  # "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski
  d <- dist(selectedVar, method = distanceMethod)
  # d <- dist(data, method = distanceMethod)
  
  # [クラスタ分析を実行]
  # クラスタ間どうしの距離としてmethodを下のどれかから指定
  # 今回は特別に自作関数にしているので、関数の外から引数として指定
  # "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
  
  ######## 20181208修正
  # hcオブジェクトは関数外で使う(heatmapの引数として)のでグローバル変数として定義。
  # returnでlistを返すと、
  # 関数外でunlist展開するときにうまくhclustクラスに戻ってくれなかったのでreturnで返す方法は諦めた
  ######## 
  
  hc <<- hclust(d, method = clusterDistMethod)
  # hc <- hclust(d, method = clusterDistMethod)
  
  plot(hc, hang = -1, ann = FALSE) # 樹形図(デンドログラム)を作成
  rect.hclust(hc, k = clNum) # 樹形図にクラスタで分ける境界線を描画
  
  # <任意> [クラスタ数色々で試して、画像で保存]
  # クラスタ数いろいろで試した結果をまとめて格納するデータフレームresultを準備しておく
  result <- data.frame(id = rownames(data))
  # クラスタ数いろいろで試すときに、ループを回すときのクラスタ数を指定
  # 今回はクラスタ数2〜10までを試すことにしてみてます
  cls <- c(2:10)
  
  # 描画デバイスを開く
  # pdf("/Users/yukako/WorkSpace/DataMiningHW/img/plots_k_all.pdf")
  # # クラスタ数いろいろで試す
  # for (i in cls){
  #   # cutree()の結果を変数resultに格納
  #   result <- cbind(result, data.frame(cutree(hc, k = i)))
  #   # x軸とy軸にする変数として指定した変数をもとに、クラスタで色分けして(col=の部分)
  #   # 結果をプロット(画像で保存しているのでプロット画面には表示されません)
  #   # plot(selectedVar, xlab = selectedColNames[1], ylab = selectedColNames[2],
  #   #      col = result[, i], cex=0.8, pch=16)
  #   # pointLabel(selectedVar, labels = rownames(cereals), col="gray") # ラベル表示してもいいけど重いので省略
  # }
  # dev.off() # 描画デバイスを閉じる
  
  result <- cbind(result, data.frame(cutree(hc, k = clNum)))
  
  # clNumをたくさんのパタンで試したそれぞれの結果をresultという変数に格納してて、
  # 列名を整理(1列目はidとしているので2列目から)
  # names(result)[2:ncol(result)] <- paste("k=", cls, sep="")
  
  # まあforで色々試したけど、
  # クラスタ数として指定したclNumの場合の結果のみ取り出して元データにくっつける
  # clusters <- result[, clNum]
  clusters <- result[, 2]
  data <- data.frame(data, clusters)
  
  
  # [各クラスタのcentrode(重心)を算出]
  # 各クラスタの算出方法は色々あるらしいが、問題文より平均ベクトル(クラスタAに所属する各列の平均値)を採用
  # aggregate()は指定した列のグループ(今回はクラスタ)ごとに指定した関数を実行してくれる
  # byで指定した列(今回はdata$clusters)を元にmean関数でデータを集計
  centroides <- aggregate(data[, -ncol(data)], by = list(data$clusters), mean)
  centroides <- centroides[, -1] # 1列目にGroupという変数が勝手にくっつくので削除
  # print("重心は以下のとおりです???")
  # print(centroides) # 簡単のために特に変数には格納せず結果をコンソールに表示するだけにしてます
  
  
  # [分析の結果をプロット(引数で指定したクラスタ数の図だけ)]
  # 描画デバイスを開く
  # pdf(paste("/Users/yukako/WorkSpace/DataMiningHW/img/plots_k_", clNum, ".pdf", sep=""))
  # plot(selectedVar, xlab = selectedColNames[1], ylab = selectedColNames[2], 
  #      col = data$clusters, cex = 0.8, pch = 16)
  # pointLabel(selectedVar, labels = rownames(cereals), col = "gray") # データのラベル
  # points(centroides[, colNums[1]], centroides[, colNums[2]], 
  #        col = "black", pch = 3, cex = 1) # 重心をプロット
  # # 描画デバイスを閉じる
  # dev.off()
  
  # 行名を戻す
  row.names(data) <- paste(data$clusters, ": ", cerealNames, sep="")
  
  return (data) # 結果をくっつけた元データを返す
  # return (list(hc, data, result)) # +αのデータも返すならこう
  
} # myClusterAnalysis()


# EOF
