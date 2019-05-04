# rawデータから処理済みのデータを作成するスクリプト。
# 毎回前処理するのは面倒なので、このスクリプトの結果をcsvに出力して、
# 別のスクリプトではその出力したcsvを読み込むようにすればいい。
# 逆に言えば、前処理を変えたかったらここを編集すると後ほど楽

library(dplyr)

options(scipen=999)
setwd("/Users/yukako/WorkSpace/ML/15_071_AnalyticsEdge/Youtube/data")

# インドや日本にするときはここのcsvファイルの名前を変える
video_raw <- read.csv("USvideos.csv", encoding='utf-8')
video_raw <- read.csv("JPvideos.csv", encoding='utf-8')
video_raw <- read.csv("INvideos.csv", encoding='utf-8')
video <- video_raw
# とりあえずいろいろ見る
# summary(video); head(video); str(video); dim(video);
# dim(na.omit(video)) # NAないので不要

video$video_id <- as.character(video$video_id) # 重複分を削除するときにcharacterとして扱う必要があるっぽい
# (factorのまま扱うと、subsetとかで条件つけてデータを削除してもvideo_idはfactorのlevelとして残る)



###############################
# データクリーニング
###############################
# colnames(video) # 列名を確認

# ------------
# like/dislikeやcommentをdisableにしてたり、
# エラーが起きてるデータを削除(それ以外のデータを抽出)
# ------------
video <- subset(video, 
                !(video$comments_disabled=="True" | video$ratings_disabled=="True" | video$video_error_or_removed=="True"))
# dim(video) # 元の行数より少し減ってる


# ------------
# trending_date(2列目)とpublish_time(6列目)の時間フォーマットを整える
# (as.POSIXctではなくas.Dateもあるので習った方に揃えてくださって大丈夫です)
# "年.日.月"と、"年-月-日時:分:秒.000Z"というフォーマットになってるっぽいので、
# そう教えてあげてそれぞれ月とか秒とかだけ取り出せる便利なフォーマットas.POSIXctに変換
# ------------
video$trending_date <- as.POSIXct(as.character(video$trending_date), format="%y.%d.%m")
video$publish_time <- as.POSIXct(video$publish_time, format="%Y-%m-%dT%H:%M:%S.000Z")
threshold_date <- format(as.POSIXct("2017-12-01"), "%Y-%m")

# ------------
# 2017-12以降を抽出
# ------------
video <- video[format(video$publish_time, "%Y-%m") >= threshold_date, ]
dim(video)


# ------------
# 重複したidから最新のトレンド日付の動画を抽出 + トレンド期間を算出
# ------------
# video_idの種類(実際の動画の種類の数)
length(table(video$video_id))
# 重複を除いた、実際の動画のvideo_idだけ抽出
all_videoid <- levels(as.factor(video$video_id))
# 重複分 = トレンドに入ってた日数(データサンプリングの間隔の日数によるけど)
trend_duration <- table(video$video_id)

latest_videos <- data.frame()
# video_idでforループ回す(重いので注意)
for(i in c(1:length(all_videoid))){
  # このvideo_idに一致する全てのvideo抽出
  videos_by_id <- subset(video, video$video_id == all_videoid[i])
  videos_by_id$trending_date <- as.POSIXct(videos_by_id$trending_date) # 日付処理できるよう変換
  # そのうち最新の日付を取得
  latest_trending_date <- max(videos_by_id$trending_date)
  # 最新の日付になってるvideoを抽出
  latest_video <- video[which(video$video_id == all_videoid[i] & video$trending_date == latest_trending_date), ]
  # トレンドに入ってた期間についての列を追加
  latest_video$trend_dur <- trend_duration[i]
  # JPはなぜか全く同じ動画(trending_dateやviewsやcommentsの数も同じ)が2つ現れることがある...
  if(dim(latest_video)[1] != 1){
    latest_video <- latest_video[1,]
  }
  
  # latest_videosという空のデータフレームに、最新日付の動画をforのたびにどんどん追加していく
  latest_videos <- rbind(latest_videos, latest_video)
}
video <- latest_videos
head(video); dim(video)
table(video$video_id) # 重複ない! yey


# ------------
# descriptionの中身を綺麗に整理
# 参考: https://www.kaggle.com/dmanmeds/eda-supervised-learning-text-analysis
# gsub()は、ある文字を別の文字に置き換える関数。
# "\\n"(=改行コード)などをスペースや""(何もなし)で置き換えることで便宜上削除
# 絵文字削除はまだとちゅう
# ------------
video$description <- gsub("\\\\n", " ", video, 16) # なんかエラーでるけど気にしない 問題あればあとで直す
video$description <- gsub("http[^[:blank:]]+", "", video$description)
video$description <- gsub("www[^[:blank:]]+", "", video$description)
video$description <- gsub('[[:digit:]]+', "", video$description)
video$description <- gsub("[[:punct:]]+", "", video$description)
video$description <- gsub("\\s+"," ", video$description)

# INとJPだと\|"みたいな文字が大量に入ってるので削除
video$tags <- gsub("\\\"", "", video$tags)

# JPだと文字コードがおかしい奴が入ってくるので削除
# for(i in c(1:length(video$description))){
#   nchar(video$description[i]) # 文字列の長さを調べるnchar()の確認
#   print(i)
# }
# 2827, 7429, 7544, 13555, 14617, 14858, 15138行目
video$description <- gsub("\xfc\xbe\x98\xa6\x94\xbc\xfc\xbe\x98\xb6\x84\xbc", "", video$description)
video$description <- gsub("\xfc\xbe\x98\xa6\x94\xbc\xfc\xbe\x98\xb3\xa0\xbc", "", video$description)
video$description <- gsub("\xfc\xbe\x8d\x96\x94\xbc", "", video$description)
video$description <- gsub("\xfc\xbe\x8c\xb6\x94\xbc", "", video$description)
video$description <- gsub("\xfc\xbe\x8d\xa6\x94\xbc", "", video$description)








###############################
# 他の指標の列を追加
###############################

# ------------
# publishした時刻だけ取り出した列作っておく
# ------------
video$publish_hour <- as.numeric(format(video$publish_time, "%H"))


# ------------
# publish_timeが特定の時期以降に偏ってる。それ以降に限定するか?
# table(format(video$publish_time, "%Y-%m"))
# publishした年月だけ取り出した列作っておく
# ------------
video$publish_month <- format(video$publish_time, "%Y-%m")


# # 人気に関係してそうな各変数の分布とか見てみる
# hist(video$views, breaks=100)
# hist(video$likes, breaks=100)
# hist(video$dislikes, breaks=100)
# hist(video$views, breaks=100)
# pairs( data.frame( video$views, video$likes, 
#                    video$dislikes, video$comment_count ), cex=0.1)
# # 0付近に集中してるっぽい。こういうのはlogとるって聞いた気がする
# hist(log(video$views), breaks=100)
# hist(log(video$likes), breaks=100)
# hist(log(video$dislikes), breaks=100)
# hist(log(video$views), breaks=100)
# # 0を含んでるデータだとlogが-Infになっちゃって相関係数計算とかができなくなる
# summary(log(video$views)); 
# summary(log(video$likes)); sum(length(which(video$likes==0)))
# summary(log(video$dislikes)); sum(length(which(video$dislikes==0)))
# summary(log(video$comment_count)); sum(length(which(video$comment_count==0)))
# ので0を0.001にでもするか...
video$likes[which(video$likes == 0)] <- 0.001
video$dislikes[which(video$dislikes == 0)] <- 0.001
video$comment_count[which(video$comment_count == 0)] <- 0.001
# 相関行列みてみる
# cor( data.frame(
#   views=log(video$views), 
#   likes=log(video$likes), 
#   dislikes=log(video$dislikes), 
#   comments=log(video$comment_count) ))
# # 散布図行列プロットしてみる(重い)
# pairs( data.frame( log(video$views), 
#                    log(video$likes), 
#                    log(video$dislikes), 
#                    log(video$comment_count) ), cex=0.1)

# プロット見てみたらいい感じなのでlog採用するか。毎回log()めんどいので列に追加
video$lviews <- log(video$views)
video$llikes <- log(video$likes)
video$ldislikes <- log(video$dislikes)
video$lcomments <- log(video$comment_count)

# likeのrateの列追加
video$likeRate <- video$likes / (video$likes + video$dislikes)



# ------------
# 人気度の指標(仮)
# ------------

# コメント数がthresholdより多く、かつlikeRateがthresholdより大きい
threshold_com <- mean(video$comment_count)
threshold_lR <- mean(video$likeRate)
video$pop_com_lR <- (video$comment_count > threshold_com & video$likeRate > threshold_lR)

# 閲覧数上位数パーセント
video$pop85 <- (video$pop_com_lR & cume_dist(video$views) > 0.85)
video$pop90 <- (video$pop_com_lR & cume_dist(video$views) > 0.90)
video$pop95 <- (video$pop_com_lR & cume_dist(video$views) > 0.95)
head(video)



###############################
# データ出力
###############################

# ここまでの加工をいちいちやるのが面倒なので、
# csvに出力しちゃって、次回からそれを読み込めばいいってことにする(普段は上書きしないようにコメントアウト)
write.csv(video, "INvideo_pd.csv", row.names=FALSE, col.names=TRUE)













# ボツ

# 案1 

# p <- -2.0 
# pは自分でテキトーに設定する。
# dislike1はlike1の2倍ネガティブな評価かなと思ってテキトーに-2.0に
# video$pop <- video$lviews + video$llikes + p*video$ldislikes
# 分布とかみてみる
# hist(video$pop, breaks=100)
# plot(video$pop)
# plot(video$ldislikes)
# plot(video$dislikes)
# plot(video$ldislikes, video$llikes)
# popのプロット見ると、飛び抜けてpopが高いグループがいる。そこを抽出
# thresholdIsSuperPop <- 18 # plot(video$pop)から目視で設定した
# 閾値(18とテキトーに設定)より高ければsuperPop=1(めちゃ人気)、低ければ0(平凡)
# video$superPop <- ifelse(video$pop > thresholdIsSuperPop, 1, 0)
