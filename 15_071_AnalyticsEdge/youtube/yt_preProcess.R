# - 「人気」の指標を考えた: views + likes + p*dislikes. pはパラメータ. -2.0にすると、超人気の奴が浮かび上がる?
# - 勝手に設定した人気指標を目的変数に、descriptionをtree. 人気なのはクリスマスのポイントセールっぽい?

options(scipen=100)
setwd("/Users/yukako/WorkSpace/LearnML/15_071_AnalyticsEdge/youtube/data")

# INvideo_raw <- read.csv("data/INvideos.csv", encoding='utf-8')
# INvideo <- INvideo_raw
# head(INvideo); str(INvideo); dim(INvideo)
# JPvideo_raw <- read.csv("data/JPvideos.csv", encoding='utf-8')
# JPvideo <- JPvideo_raw
# head(JPvideo); str(JPvideo); dim(JPvideo)
# JPvideo[236,16] # emojiは \U0001f3c0 みたいに



# インドや日本にするときはここのcsvファイルの名前を変える
video_raw <- read.csv("USvideos.csv", encoding='utf-8')
video <- video_raw
dim(video)
summary(video); head(video); str(video);
# dim(na.omit(video)) # NAないので不必要



###############################
# Data Cleaning
###############################
colnames(video)

# like/dislikeやcommentをdisableにしてたり、
# エラーが起きてるデータを削除(それ以外のデータを抽出)
video <- subset(video, 
                  !(video$comments_disabled=="True" | video$ratings_disabled=="True" | video$video_error_or_removed=="True"))
dim(video) # 元の行数より少し減ってる

# trending_date(2列目)とpublish_time(6列目)の時間フォーマットを整える
video$trending_date <- as.POSIXct(as.character(video$trending_date), format="%y.%d.%m")# as.DateかPOSIXctか
video$publish_time <- as.POSIXct(video$publish_time, format="%Y-%m-%dT%H:%M:%S.000Z")
# class(video$trending_date)
# class(video$publish_time)

# descriptionの中身を綺麗に整理
# 参考: https://www.kaggle.com/dmanmeds/eda-supervised-learning-text-analysis

# 絵文字削除はまだとちゅう

# "\\n"(=改行コード)などを削除
video$description <- gsub("\\\\n", " ", video, 16)
video$description <- gsub("http[^[:blank:]]+", "", video$description)
video$description <- gsub("www[^[:blank:]]+", "", video$description)
video$description <- gsub('[[:digit:]]+', "", video$description)
video$description <- gsub("[[:punct:]]+", "", video$description)
video$description <- gsub("\\s+"," ", video$description)




# ここまでの加工をいちいちやるのが面倒なので、
# csvに出力しちゃって、次回からそれを読み込めばいいってことにする(普段は上書きしないようにコメントアウト)
# write.csv(video, "USvideo_pd.csv", row.names=FALSE, col.names=TRUE)

# (csv書き込み後なら)ここから
video <- read.csv("USvideo_pd.csv", encoding='utf-8')


###############################
# 人気の指標...とは...
###############################
# 人気に関係してそうな各変数の分布とか見てみる
hist(video$views, breaks=100)
hist(video$likes, breaks=100)
hist(video$dislikes, breaks=100)
hist(video$views, breaks=100)
pairs( data.frame( video$views, video$likes, 
                   video$dislikes, video$comment_count ), cex=0.1)
# 0付近に集中してるっぽい。こういうのはlogとるって聞いた気がする
hist(log(video$views), breaks=100)
hist(log(video$likes), breaks=100)
hist(log(video$dislikes), breaks=100)
hist(log(video$views), breaks=100)
# 0を含んでるデータだとlogが-Infになっちゃって相関係数計算とかができなくなる
summary(log(video$views)); 
summary(log(video$likes)); sum(length(which(video$likes==0)))
summary(log(video$dislikes)); sum(length(which(video$dislikes==0)))
summary(log(video$comment_count)); sum(length(which(video$comment_count==0)))
# ので0を0.001にでもするか...
video$likes[which(video$likes == 0)] <- 0.001
video$dislikes[which(video$dislikes == 0)] <- 0.001
video$comment_count[which(video$comment_count == 0)] <- 0.001
# 相関行列みてみる
cor( data.frame(
  views=log(video$views), 
  likes=log(video$likes), 
  dislikes=log(video$dislikes), 
  comments=log(video$comment_count) ))
# 散布図行列プロットしてみる
pairs( data.frame( log(video$views), 
                   log(video$likes), 
                   log(video$dislikes), 
                   log(video$comment_count) ), cex=0.1)

# プロット見てみたらいい感じなのでlog採用するか。毎回log()めんどいので列に追加
video$lviews <- log(video$views)
video$llikes <- log(video$likes)
video$ldislikes <- log(video$dislikes)
video$lcomments <- log(video$comment_count)

# 人気度の指標(仮)
p <- -2.0 
# pは自分でテキトーに設定する。
# dislike1はlike1の2倍ネガティブな評価かなと思ってテキトーに-2.0に
video$pop <- video$lviews + video$llikes + p*video$ldislikes
# 分布とかみてみる
hist(video$pop, breaks=100)
plot(video$pop)
# plot(video$ldislikes)
# plot(video$dislikes)
# plot(video$ldislikes, video$llikes)
# popのプロット見ると、飛び抜けてpopが高いグループがいる。そこを抽出
thresholdIsSuperPop <- 18 # plot(video$pop)から目視で設定した
# 閾値(18とテキトーに設定)より高ければsuperPop=1(めちゃ人気)、低ければ0(平凡)
video$superPop <- ifelse(video$pop > thresholdIsSuperPop, 1, 0)









# library(dplyr)
# head(video$views)
# summary(video$views)
# # plot(cume_dist(video$views))
# # popvideo <- subset(video, cume_dist(video$views) > 0.9)
# video$isPop <- cume_dist(video$views) > 0.9




# REMOVE

# 1列目(video_id)、12列目(thumbnail_link)、13~15列目を削除
# video <- video[, -c(1,12,13,14,15)]
# dim(video)
# head(video)






