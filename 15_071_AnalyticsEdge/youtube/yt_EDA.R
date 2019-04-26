library(ggplot2)
library(ggsci)
head(video)
colnames(video)

###############################
# 国別の人気カテゴリ
###############################
category_id_titles <- c("Film & Animation", "Autos & Vehicles", "Music", "Pets & Animals", 
                      "Sports", "Travel & Events", "Gaming", "People & Blogs", "Comedy", 
                      "Entertainment", "News & Politics", "Howto & Style", "Education",
                      "Science & Technology", "Nonprofits & Activism", "Shows")
category_id_freq <- data.frame(category_id_titles, 
                               table(video$category_id),
                               superPop = table(video$superPop, video$category_id)[2,])
category_id_freq$superPopRate <- category_id_freq$superPop / category_id_freq$Freq * 100 # 人気動画の割合
category_id_freq <- category_id_freq[,-2] # 元々のid列はいらないので削除

table(video$category_id, video$views)
category_id_freq

str(video$category_id)
id <- c(1,2,10,15,17,19,20,22,23,24,25,26,27,28,29,43) 
v <- id
for(i in c(1:16)){
  v[i] <- sum(as.numeric(subset(video, video$category_id==id[i])$views))
}

category_id_freq$views_count <- v


# g <- ggplot(category_id_freq, aes(x=category_id_titles, y=Freq)) # , fill = category_id_titles入れるとカラフル
g <- ggplot(category_id_freq, aes(x=category_id_titles, y=Freq, fill=views_count)) # fill=superPopRateにするとまた違う
g <- g + geom_bar(stat = "identity") # 棒グラフを指定
g <- g + ggtitle("Popular Categories") + xlab("Categories") + ylab("Frequency") # タイトルとかラベル
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # ラベル縦書きの指定
plot(g)
# ggsave("ggplotImg.png") # 画像保存したいなら

# India
# Japan



subset(video, video$superPop==1 & video$category_id==17)$video_id




###############################
# タグの多さと人気度の関係 -> 結論: 関係なさそうw
###############################
video$tags <- as.character(video$tags)
splitedTags <- strsplit(video$tags, "[|]")
splitedTags[2] # 確認
# lapply()は、第二引数の処理を第一引数に対して並列実行する便利な関数
# unlist()は、lapplyの結果が特殊な形式なので、それをただの数値列に戻す関数
video$tagCounts <- unlist(lapply(splitedTags, length))
# いろいろ表示してみる
hist(video$tagCounts, breaks=100) # 正規分布してないのね
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
# descriptionの長さと人気度の関係 -> 結論: 関係なさそうw
###############################
video$description <- as.character(video$description)
nchar(video$description[1]) # 文字列の長さを調べるnchar()
video$descriptionLen <- unlist(lapply(video$description, nchar))
# いろいろ表示してみる
hist(video$descriptionLength, breaks=100) # 右肩下がり
corMatrix <- cor( data.frame(
  views = log(video$views), 
  likes = log(video$likes), 
  dislikes = log(video$dislikes), 
  comments = log(video$comment_count),
  tagCounts = video$tagCounts,
  descLen = video$descriptionLen,
  pop = video$pop) )
round(corMatrix, 3)


###############################
# titleの長さと人気度の関係 -> 結論: 関係なさそうw
###############################
video$title <- as.character(video$title)
nchar(video$title[1]) # 文字列の長さを調べるnchar()
video$titleLen <- unlist(lapply(video$title, nchar))
# いろいろ表示してみる
hist(video$titleLen, breaks=100) # 正規分布っぽい！
corMatrix <- cor( data.frame(
  views = log(video$views), 
  likes = log(video$likes), 
  dislikes = log(video$dislikes), 
  comments = log(video$comment_count),
  tagCounts = video$tagCounts,
  descLen = video$descriptionLength,
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
                   video$descriptionLength,
                   video$titleLen,
                   video$pop), cex=0.1)
