# いろんな変数の関係を見るために色々やってみた(Exploratory Data Analysis)スクリプト。

# install.packages("GGally")
library(GGally)
library(ggplot2)
library(ggsci)
library(dplyr)

setwd("/Users/yukako/WorkSpace/ML/15_071_AnalyticsEdge/Youtube/data")

USvideo <- read.csv("USvideo_pd.csv")
JPvideo <- read.csv("JPvideo_pd.csv")
INvideo <- read.csv("INvideo_pd.csv")
video <- USvideo
video <- JPvideo
video <- INvideo


###############################
# 人気度の指標(again)
###############################

# コメント数がthresholdより多く、かつlikeRateがthresholdより大きい
# -> thresholdについて検討する必要ありそう
hist(video$comment_count, breaks=100)
hist(video$likeRate, breaks=100)
# ↑なので、平均はdoesn't workってかんじ。medianか、それこそ上位10%とか?

# median
threshold_com <- median(video$comment_count)
threshold_lR <- median(video$likeRate)
video$pop_com_lR <- (video$comment_count > threshold_com & video$likeRate > threshold_lR)

# 上位x%
percentage <- 0.90
video$pop_com_lR <- (cume_dist(video$comment_count) > percentage) & (cume_dist(video$likeRate) > percentage)
# USは総数が少ないので90%にしないと抽出後が少なくなりすぎる(9個とか)
# video$pop_com_lR <- (cume_dist(video$comment_count) > 0.90) & (cume_dist(video$likeRate) > 0.90)
length(video$pop_com_lR)
sum(video$pop_com_lR)
# cf. 99%にすると、該当データがしか無くなる

# 閲覧数上位数パーセント
video$pop85 <- (video$pop_com_lR & cume_dist(video$views) > 0.85)
video$pop90 <- (video$pop_com_lR & cume_dist(video$views) > 0.90)
video$pop95 <- (video$pop_com_lR & cume_dist(video$views) > 0.95)
sum(video$pop90)


###############################
# 処理後のデータセットの基本情報
###############################
# dim(video)



###############################
# 国別の人気カテゴリ
###############################
ct <- setCategory()


# !!!!!!!!!!!!!! ↓ 本質ではないのでスルーしておk !!!!!!!!!!!!!!
setCategory <- function(){
  category_id <- c(1,2,10,15,17,19,20,22,23,24,25,26,27,28,29,30,43)
  category_titles <- c("Film & Animation", "Autos & Vehicles", "Music", "Pets & Animals",
                       "Sports", "Travel & Events", "Gaming", "People & Blogs", "Comedy",
                       "Entertainment", "News & Politics", "Howto & Style", "Education",
                       "Science & Technology", "Movies", "Nonprofits & Activism", "Shows")
  ct <- data.frame(category_id, category_titles)
  
  # (国ごとに異なるカテゴリidを持ってるので、それに自動で対応できるようにした)
  dif <- setdiff(ct$category_id, video$category_id)
  if(length(dif) != 0){
    for(i in c(1:length(dif)) ){
      ct <- ct[-which(ct$category_id == dif[i]),] # (カテゴリidが差分と一致するとき以外を抽出)
    }
  }else{
    # インドの場合、用意したカテゴリidがそのまま使えるので、特に何も処理しない
    print("difがないのでこの処理はスルー")
  }
  return(ct)
}
# !!!!!!!!!!!!!! ↑ 本質ではないのでスルーしておk !!!!!!!!!!!!!!


# ------------
# カテゴリごとの動画本数
# ------------
category_freq <- data.frame(category_titles = ct$category_titles, 
                            table(video$category_id),
                            pop85Freq = table(video$pop85, video$category_id)[2,],
                            pop90Freq = table(video$pop90, video$category_id)[2,],
                            pop95Freq = table(video$pop95, video$category_id)[2,])
category_freq <- category_freq[,-2] # 元々のid列はいらないので削除
# 動画の総数のうちそのカテゴリの本数の割合がどれくらいか
category_freq$Freq_rate <- category_freq$Freq / sum(category_freq$Freq) * 100
# pop85が"そのカテゴリの中で"何割を占めるか(全体の中で何割を占めるか、ではない、そりゃ85%だろうから)
# category_freq$pop85Freq_rate <- category_freq$pop85Freq / category_freq$Freq * 100
category_freq$pop90Freq_rate <- category_freq$pop90Freq / category_freq$Freq * 100
# category_freq$pop95Freq_rate <- category_freq$pop95Freq / category_freq$Freq * 100



# ------------
# カテゴリごとの視聴数
# ------------
category_freq$views <- ct$category_id
# category_freq$views85 <- ct$category_id
category_freq$views90 <- ct$category_id
# category_freq$views95 <- ct$category_id
# !!!!!!!!!!!!!! ↓本質ではないのでスルーしておk !!!!!!!!!!!!!!
for(i in c(1:length(ct$category_id))){
  # カテゴリごとにforループ回して、viewsのsumを数えてる
  v <- subset(video$views, video$category_id == ct$category_id[i])
  # v85 <- subset(video$views, (video$pop85 & video$category_id == ct$category_id[i]))
  v90 <- subset(video$views, (video$pop90 & video$category_id == ct$category_id[i]))
  # v95 <- subset(video$views, (video$pop95 & video$category_id == ct$category_id[i]))
  category_freq$views[i] <- sum(as.numeric(v)) # as.numeric入れないとwarningがうるさい
  # category_freq$views85[i] <- sum(as.numeric(v85))
  category_freq$views90[i] <- sum(as.numeric(v90))
  # category_freq$views95[i] <- sum(as.numeric(v95))
}
# !!!!!!!!!!!!!! ↑ 本質ではないのでスルーしておk !!!!!!!!!!!!!!
# カテゴリごとの視聴数を、全ての視聴数で割った
category_freq$views_rate <- category_freq$views / sum(category_freq$views) * 100
# category_freq$views85_rate <- category_freq$views85 / sum(category_freq$views85) * 100
category_freq$views90_rate <- category_freq$views90 / sum(category_freq$views90) * 100
# category_freq$views95_rate <- category_freq$views95 / sum(category_freq$views95) * 100
category_freq


# ------------
# 描画
# ------------
# colnames(category_freq)
# g <- ggplot(category_id_freq, aes(x=category_id_titles, y=Freq)) # , fill = category_id_titles入れるとカラフル
# g <- ggplot(category_freq, aes(x=category_titles, y=Freq, fill=views))
# g <- ggplot(category_freq, aes(x = category_titles, y = Freq, fill = views_rate))
# g <- g + geom_bar(stat = "identity", position = "dodge") # 棒グラフを指定
# g <- g + ggtitle("Number of Videos per Categories") + xlab("Categories") + ylab("Frequency") # タイトルとかラベル
# g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # ラベル縦書きの指定
# plot(g)

# 集合棒グラフのために特別な形式のデータセット作る
category_freq_forGroup <- data.frame(
  category_titles = category_freq$category_titles,
  Freq = category_freq$Freq,
  Freq_rate = category_freq$Freq_rate,
  views = category_freq$views,
  views_rate = category_freq$views_rate,
  gr = "all"
)
category_freq_forGroup <- rbind(
  category_freq_forGroup,
  # data.frame(category_titles = category_freq_forGroup$category_titles,
  #            Freq = category_freq$pop85Freq,
  #            Freq_rate = category_freq$pop85Freq_rate,
  #            views = category_freq$views85,
  #            views_rate = category_freq$views85_rate,
  #            gr = "pop85"),
  data.frame(category_titles = category_freq_forGroup$category_titles,
             Freq = category_freq$pop90Freq,
             Freq_rate = category_freq$pop90Freq_rate,
             views = category_freq$views90,
             views_rate = category_freq$views90_rate,
             gr = "pop90")
  # data.frame(category_titles = category_freq_forGroup$category_titles,
  #            Freq = category_freq$pop95Freq,
  #            Freq_rate = category_freq$pop95Freq_rate,
  #            views = category_freq$views95,
  #            views_rate = category_freq$views95_rate,
  #            gr = "pop95")
)
category_freq_forGroup
colnames(category_freq_forGroup)
# g <- ggplot(category_freq_forGroup, aes(x = category_titles, y = Freq, fill = gr))
# g <- ggplot(category_freq_forGroup, aes(x = category_titles, y = Freq_rate, fill = gr))
g <- ggplot(category_freq_forGroup, aes(x = category_titles, y = views_rate, fill = gr))
# g <- ggplot(category_freq_forGroup, aes(x = category_titles, y = views_rate, fill = gr))
g <- g + geom_bar(stat = "identity", position = position_dodge()) # 棒グラフを指定
g <- g + ggtitle("View Rate of Videos per Categories") + xlab("Categories") + ylab("Frequency") # タイトルとかラベル
# g <- g + geom_text(aes(x = category_titles, y = Freq, label = Freq, vjust = -0.5, group = gr), # 数値ラベルの位置をグループの水準ごとの位置に配置する
#                    position = position_dodge(width = 0.9)) # 指定しないとエラーが表示される
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # ラベル縦書きの指定
plot(g)
# ggsave("views_rate_category_US.png") # 画像保存したいなら(名前変えないと上書きされるので注意)
# 集合棒グラフもできるので必要であればいってください






###############################
# tag
###############################
# video$tags <- as.character(video$tags)
# splitedTags <- strsplit(video$tags, "[|]") # | という文字で分割
# # splitedTags[2] # 確認
# # lapply()は、第二引数の処理を第一引数に対して並列実行する便利な関数
# # unlist()は、lapplyの結果が特殊な形式なので、それをただの数値列に戻す関数
# video$tagCounts <- unlist(lapply(splitedTags, length))
tag_counts <- data.frame(
  tagCounts = video$tagCounts,
  gr = "all"
)
tag_counts <- rbind(
  tag_counts,
  data.frame(tagCounts = subset(video$tagCounts, video$pop85), gr = "pop85"),
  data.frame(tagCounts = subset(video$tagCounts, video$pop90), gr = "pop90"),
  data.frame(tagCounts = subset(video$tagCounts, video$pop95), gr = "pop95"))
# ヒストグラム
g <- ggplot(tag_counts, aes(x = tagCounts, fill = gr))
g <- g + geom_histogram(position = "identity", alpha = 0.8, binwidth=1)
g <- g + geom_density(aes(color = gr, alpha = 0.2), show.legend = F)
g <- g + ggtitle("Number of tags") + xlab("counts") + ylab("Frequency")
plot(g)
# hist(video$tagCounts, breaks=100)



###############################
# description
###############################
# video$description <- as.character(video$description)
# # nchar(video$description[1]) # 文字列の長さを調べるnchar()の確認
# video$descLen <- unlist(lapply(video$description, nchar))
desc_length <- data.frame(
  descLen = video$descLen,
  gr = "all"
)
desc_length <- rbind(
  desc_length,
  data.frame(descLen = subset(video$descLen, video$pop85), gr = "pop85"),
  data.frame(descLen = subset(video$descLen, video$pop90), gr = "pop90"),
  data.frame(descLen = subset(video$descLen, video$pop95), gr = "pop95"))
# ヒストグラム
g <- ggplot(desc_length, aes(x = descLen, fill = gr))
g <- g + geom_histogram(position = "identity", alpha = 0.8, binwidth=30)
g <- g + geom_density(aes(color = gr, alpha = 0.2), show.legend = F)
# g <- g + scale_y_continuous(limits = c(0, 80))
g <- g + ggtitle("description length") + xlab("counts") + ylab("Frequency")
plot(g)
# hist(video$descLen, breaks=100)


###############################
# title
# INはタイトルがめちゃくちゃ長い(日本は30字前後、
# USは40字前後に対して、100字以上が多い)
# (ヒンドゥー語はローマ字表記にすると単語数が多いとか?にしては
# descriptionには違いがみられない)
###############################
# video$title <- as.character(video$title)
# # nchar(video$title[1]) # 文字列の長さを調べるnchar()
# video$titleLen <- unlist(lapply(video$title, nchar))
title_length <- data.frame(
  titleLen = video$titleLen,
  gr = "all"
)
title_length <- rbind(
  title_length,
  data.frame(titleLen = subset(video$titleLen, video$pop85), gr = "pop85"),
  data.frame(titleLen = subset(video$titleLen, video$pop90), gr = "pop90"),
  data.frame(titleLen = subset(video$titleLen, video$pop95), gr = "pop95"))
# ヒストグラム
g <- ggplot(title_length, aes(x = titleLen, fill = gr))
g <- g + geom_histogram(position = "identity", alpha = 0.8, binwidth=1)
g <- g + geom_density(aes(color = gr, alpha = 0.2), show.legend = F)
g <- g + ggtitle("title length") + xlab("counts") + ylab("Frequency")
plot(g)
# hist(video$descLen, breaks=100)




###############################
# trending_duration
###############################
hist(video$trend_dur, breaks=1000)





###############################
# いろいろ表示してみる(処理重いので注意)
###############################
# colnames(video)
cordf <- data.frame(
  trend_dur = video$trend_dur,
  tagcounts = video$tagCounts,
  desclength = video$descLen,
  titlelength = video$titleLen,
  gr = "all")
# vi <- subset(video, video$pop85)
vi <- subset(video, video$pop90)
# vi <- subset(video, video$pop95)
cordf <- rbind(
  cordf,
  data.frame(
    trend_dur = vi$trend_dur,
    tagcounts = vi$tagCounts,
    desclength = vi$descLen,
    titlelength = vi$titleLen,
    gr = "pop90") # 85,90でもやる
)
# 散布図行列
g <- ggpairs(cordf, 
             aes_string(colour="gr", alpha=0.3), 
             upper = list(continuous = wrap("cor", size = 2.3)), 
             lower = list(continuous=wrap("points", size=0.05)))
g <- g + theme(axis.text= element_text(size=5),
               legend.title = element_text(size=7),
               legend.text = element_text(size=7),
               axis.title = element_text(size=7),
               plot.title = element_text(size=7),
               strip.text = element_text(size=7))
g # plot(g)じゃないので注意

# cordf <- data.frame(
#   logviews = log(video$views), 
#   loglikes = log(video$likes), 
#   logdislikes = log(video$dislikes), 
#   logcomments = log(video$comment_count),
#   trend_dur = video$trend_dur,
#   likeRate = video$likeRate,
#   tagcounts = video$tagCounts,
#   desclength = video$descLen,
#   titlelength = video$titleLen,
#   gr = "all")
# vi <- subset(video, video$pop85)
# vi <- subset(video, video$pop90)
# vi <- subset(video, video$pop95)
# cordf <- rbind(
#   cordf,
#   data.frame(
#     logviews = log(vi$views), 
#     loglikes = log(vi$likes), 
#     logdislikes = log(vi$dislikes), 
#     logcomments = log(vi$comment_count),
#     trend_dur = vi$trend_dur,
#     likeRate = vi$likeRate,
#     tagcounts = vi$tagCounts,
#     desclength = vi$descLen,
#     titlelength = vi$titleLen,
#     gr = "pop95") # 85,90でもやる
# )
# # 散布図行列(時間かかるので注意)
# g <- ggpairs(cordf, 
#         aes_string(colour="gr", alpha=0.3), 
#         upper = list(continuous = wrap("cor", size = 2.3)), 
#         lower = list(continuous=wrap("points", size=0.05)))
# g <- g + theme(axis.text= element_text(size=5),
#                legend.title = element_text(size=7),
#                legend.text = element_text(size=7),
#                axis.title = element_text(size=7),
#                plot.title = element_text(size=7),
#                strip.text = element_text(size=7))
# g # plot(g)じゃないので注意

# corMatrix <- cor(subset(cordf[-c()], cordf$gr == "all"))
# round(corMatrix, 3)
# pairs(corMatrix , cex=0.1)
