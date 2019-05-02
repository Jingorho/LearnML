# いろんな変数の関係を見るために色々やってみた(Exploratory Data Analysis)スクリプト。


library(ggplot2)
library(ggsci)

setwd("/Users/yukako/WorkSpace/ML/15_071_AnalyticsEdge/Youtube/data")

category_id <- c(1,2,10,15,17,19,20,22,23,24,25,26,27,28,29,30,43)
category_titles <- c("Film & Animation", "Autos & Vehicles", "Music", "Pets & Animals",
                     "Sports", "Travel & Events", "Gaming", "People & Blogs", "Comedy",
                     "Entertainment", "News & Politics", "Howto & Style", "Education",
                     "Science & Technology", "Movies", "Nonprofits & Activism", "Shows")


USvideo <- read.csv("USvideo_pd.csv")
JPvideo <- read.csv("JPvideo_pd.csv")
INvideo <- read.csv("INvideo_pd.csv")
video <- USvideo
# video <- JPvideo
# video <- INvideo



###############################
# 国別の人気カテゴリ
###############################
ct <- data.frame(category_id, category_titles)

# !!!!!!!!!!!!!! ↓ 本質ではないのでスルーしておk !!!!!!!!!!!!!!
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
# !!!!!!!!!!!!!! ↑ 本質ではないのでスルーしておk !!!!!!!!!!!!!!


# ------------
# カテゴリごとの動画本数
# ------------
category_freq <- data.frame(category_titles = ct$category_titles, 
                            table(video$category_id),
                            pop = table(video$pop, video$category_id)[2,])
category_freq$popRate <- category_freq$pop / category_freq$Freq * 100 # 人気動画の割合
category_freq <- category_freq[,-2] # 元々のid列はいらないので削除



# ------------
# カテゴリごとの視聴数
# ------------
category_freq$views <- ct$category_id
# !!!!!!!!!!!!!! ↓本質ではないのでスルーしておk !!!!!!!!!!!!!!
for(i in c(1:length(ct$category_id))){
  # カテゴリごとにforループ回して、viewsのsumを数えてる
  v <- subset(video$views, video$category_id == ct$category_id[i])
  category_freq$views[i] <- sum(as.numeric(v)) # as.numeric入れないとwarningがうるさい
}
# !!!!!!!!!!!!!! ↑ 本質ではないのでスルーしておk !!!!!!!!!!!!!!



# ------------
# 描画
# ------------
# g <- ggplot(category_id_freq, aes(x=category_id_titles, y=Freq)) # , fill = category_id_titles入れるとカラフル
# g <- ggplot(category_freq, aes(x=category_titles, y=Freq, fill=views))
g <- ggplot(category_freq, aes(x=category_titles, y=Freq, fill=popRate))
g <- g + geom_bar(stat = "identity") # 棒グラフを指定
g <- g + ggtitle("Number of Videos per Categories") + xlab("Categories") + ylab("Frequency") # タイトルとかラベル
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # ラベル縦書きの指定
plot(g)
ggsave("freq_category_JP.png") # 画像保存したいなら(名前変えないと上書きされるので注意)
# 集合棒グラフもできるので必要であればいってください






###############################
# タグの多さと人気度の関係
###############################
video$tags <- as.character(video$tags)
splitedTags <- strsplit(video$tags, "[|]") # | という文字で分割
splitedTags[2] # 確認
# lapply()は、第二引数の処理を第一引数に対して並列実行する便利な関数
# unlist()は、lapplyの結果が特殊な形式なので、それをただの数値列に戻す関数
video$tagCounts <- unlist(lapply(splitedTags, length))
# いろいろ表示してみる
hist(video$tagCounts, breaks=100)
corMatrix <- cor( data.frame(
  views = log(video$views), 
  likes = log(video$likes), 
  dislikes = log(video$dislikes), 
  comments = log(video$comment_count),
  tagCounts = video$tagCounts,
  pop = video$pop) )
# 四捨五入してスッキリ見る
round(corMatrix, 3) # よくわからんがtag長ければいいってわけじゃない



###############################
# descriptionの長さと人気度の関係
###############################
video$description <- as.character(video$description)
nchar(video$description[1]) # 文字列の長さを調べるnchar()の確認
video$descLen <- unlist(lapply(video$description, nchar))
# いろいろ表示してみる
hist(video$descLen, breaks=100) # 右肩下がり
corMatrix <- cor( data.frame(
  views = log(video$views), 
  likes = log(video$likes), 
  dislikes = log(video$dislikes), 
  comments = log(video$comment_count),
  tagCounts = video$tagCounts,
  descLen = video$descLen,
  pop = video$pop) )
round(corMatrix, 3)




###############################
# titleの長さと人気度の関係
# INはタイトルがめちゃくちゃ長い(日本は30字前後、
# USは40字前後に対して、100字以上が多い)
# (ヒンドゥー語はローマ字表記にしても単語数が多い?にしては
# descriptionには違いがみられない)
###############################
video$title <- as.character(video$title)
nchar(video$title[1]) # 文字列の長さを調べるnchar()
video$titleLen <- unlist(lapply(video$title, nchar))
# いろいろ表示してみる
hist(video$titleLen, breaks=100)
corMatrix <- cor( data.frame(
  views = log(video$views), 
  likes = log(video$likes), 
  dislikes = log(video$dislikes), 
  comments = log(video$comment_count),
  tagCounts = video$tagCounts,
  descLen = video$descLen,
  titleLen = video$titleLen,
  pop = video$pop) )
round(corMatrix, 3)
# たぶん、人気度popやviewsを目的変数にRegressionしたら、
# tagCountsやdescriptionLenやtitleLenは有意差なしになるんだろうなーと予測が立つ


###############################
# いろいろ表示してみる(処理重いので注意)
###############################
pairs( data.frame( log(video$views), 
                   log(video$likes), 
                   log(video$dislikes), 
                   log(video$comment_count),
                   video$tagCounts,
                   video$descLen,
                   video$titleLen,
                   video$pop), cex=0.1)
