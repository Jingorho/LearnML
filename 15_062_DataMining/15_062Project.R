# -----------------------------------------------------------
# Fall 2018 15.062 - Data Mining: Finding the Models and Predictions that Create Value
# Final Project
# -----------------------------------------------------------


###############################
# ライブラリとか
###############################
# install.packages("ggsci")
# install.packages("gridExtra")
# install.packages("dplyr")
library(ggplot2)
library(ggsci)
library(caret)
library(gridExtra)
library(reshape2)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
options(scipen=999, digits = 4) # 表示桁を揃える


###############################
# データの読み込みと前処理
###############################
data1 <- read.csv("/Users/yukako/Documents/_MIT/15_062/SampleProject/Project Example4(Fatal Police Shootings in the US)/data/PercentOver25CompletedHighSchool.csv")
data2 <- read.csv("/Users/yukako/Documents/_MIT/15_062/SampleProject/Project Example4(Fatal Police Shootings in the US)/data/MedianHouseholdIncome2015.csv")
data3 <- read.csv("/Users/yukako/Documents/_MIT/15_062/SampleProject/Project Example4(Fatal Police Shootings in the US)/data/ShareRaceByCity.csv")
data4 <- read.csv("/Users/yukako/Documents/_MIT/15_062/SampleProject/Project Example4(Fatal Police Shootings in the US)/data/PercentagePeopleBelowPovertyLevel.csv")
data5 <- read.csv("/Users/yukako/Documents/_MIT/15_062/SampleProject/Project Example4(Fatal Police Shootings in the US)/data/PoliceKillingsUS.csv")
data6 <- read.csv("/Users/yukako/Documents/_MIT/15_062/SampleProject/Project Example4(Fatal Police Shootings in the US)/data/fatal-police-shootings-data.csv")
#set.seed(1) #確率を固定する

# data3の列名"Geographic.Area"が"Geographic.area"(aが小文字)になってるので他と合わせる
colnames(data3) <- c("Geographic.Area", "City", "share_white", 
                     "share_black", "share_native_american", 
                     "share_asian", "share_hispanic")

# データの概要を色々な関数で確認
summary(data1); summary(data2); summary(data3); summary(data4); summary(data5); summary(data6); 
head(data1); head(data2); head(data3); head(data4); head(data5); head(data6); 
str(data1); str(data2); str(data3); str(data4); str(data5); str(data6);
dim(data1); dim(data2); dim(data3); dim(data4); dim(data5); dim(data6);


# for Debug
# グローバル変数定義
count <- 0



###############################
# データフレームの結合と欠損値の削除
###############################

# area(データ数多い方)の結合処理
# --------------------------------------------------

# merge(データ1, データ2)したやつをさらにmerge()してる
# all=Tは、共通部分以外も全て残すという意味
area <- merge(merge(data1, data2, all=T, sort=F), 
                     merge(data3, data4, all=T, sort=F), all=T, sort=F)
head(area); dim(area); class(area)


# 欠損値削除の処理前の次元数を確認 # 29477     10
dim(area); sum(is.na(area))

# それぞれの列の非数値を確認
str(area);

# 変数列percent_completed_hsが"-"の行の場合、どっかの列(今回は1列目)にNA代入(あとでまとめてNAが入ってる行削除する)
nrow(subset(area, area$percent_completed_hs == "-"))
area[rownames(subset(area, area$percent_completed_hs == "-")), 1] <- NA
nrow(subset(area, 
            area$Median.Income == "-" | area$Median.Income == "" | area$Median.Income == "(X)"))
area[rownames(subset(area, 
                          area$Median.Income == "-" | area$Median.Income == "" | area$Median.Income == "(X)")), 1] <- NA
nrow(subset(area, area$share_white == "(X)"))
area[rownames(subset(area, area$share_white == "(X)")), 1] <- NA

# 非数値が入ってる行にNA入れたのも含めて、NAが入ってる行を削除
area <- na.omit(area); dim(area) # 27243     10

# データの概要を色々な関数で確認
summary(area); head(area); str(area); dim(area)
colnames(area)

# もう一回as.factorしないと因子から"-"とかが完全に消えないっぽいので
area <- as.data.frame(sapply(area, as.factor))
str(area);


# Factor型をnumeric型に変換(factor->numericの変換は下のようにしないと値が謎に変わってしまうので注意)
# https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
# levels()[]とかいう特殊な形で変換しなきゃなのでsapplyが使えないとおもう...
for (i in c(3, 5:ncol(area))){
  area[,i] <- as.numeric(levels(area[,i]))[area[,i]]
}
# Incomeはノイズが含まれていてうまく変換できないので別で変換
str(area[,4])
incident_income <- area[,4]
subset(incident_income, incident_income=="250,000+" | incident_income=="2,500-")
incident_income <- gsub(",", "", incident_income)
incident_income <- gsub("+", "", incident_income, fixed = TRUE) # "+"はエスケープ必要
incident_income <- gsub("-", "", incident_income)
subset(incident_income, incident_income=="250000" | incident_income=="2500")
area[,4] <- as.numeric(incident_income)

str(area)





# incident(データ数少ない方)の結合処理
# --------------------------------------------------

# TrueとTRUEがごっちゃなのでどっちも0/1に変換して統一
data5$signs_of_mental_illness <- as.numeric(data5$signs_of_mental_illness)
data6$signs_of_mental_illness <- as.numeric(data6$signs_of_mental_illness) - 1 # True/Falseは1/2に変換されるので1/0にするため-1
data5$body_camera <- as.numeric(data5$body_camera)
data6$body_camera <- as.numeric(data6$body_camera) - 1
# "%d/%m/%y"と"%Y-%m-%d"がごっちゃなので統一
data5$date <- as.Date(data5$date, format = "%d/%m/%y")
data6$date <- as.Date(data6$date, format = "%Y-%m-%d")


# データフレームの結合
incident <- merge(data5, data6, all=T, sort=F)
# incident <- incident[, -c(1, 2)] # idとnameはいらないので削除
head(incident); dim(incident); class(incident)

dim(incident) # 処理前の次元数を確認 # 6337   12
sum(is.na(incident)) # 欠損値NAの個数を確認

# 各要素char型だったのをfactor型に変換(lapplyは遅いNG)
# incident <- as.data.frame(sapply(incident, as.factor)) 
# str(incident)

# 変数列が"-"や""の行の場合、どっかの列(今回は1列目)にNA代入(あとでまとめてNAが入ってる行削除する)
nrow(subset(incident, incident$armed == ""))
incident[rownames(subset(incident, incident$armed == "")), 1] <- NA
nrow(subset(incident, incident$gender == ""))
incident[rownames(subset(incident, incident$gender == "")), 1] <- NA
nrow(subset(incident, incident$race == ""))
incident[rownames(subset(incident, incident$race == "")), 1] <- NA
nrow(subset(incident, incident$flee == ""))
incident[rownames(subset(incident, incident$flee == "")), 1] <- NA

# 非数値が入ってる行にNA入れたのも含めて、NAが入ってる行を削除
incident <- na.omit(incident); dim(incident) # 5364   12

summary(incident); head(incident); str(incident); dim(incident)
colnames(incident)

# もう一回as.factorしないと因子から"-"とかが完全に消えないっぽいので
incident[, -6] <- as.data.frame(sapply(incident[, -6], as.factor))
str(incident);




# 後処理いろいろ
# --------------------------------------------------

# mergeの前後の変化はこれを実行するとわかりやすいかも
head(data1);
head(area);





# 南部だけでみた時期による頻度
south <- c("FL", "GA", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX") # 南部16州(by Wiki)を抽出
incident_merge_south <- incident_merge[incident_merge$state == south,]
dateFrequency_south <- data.frame(table(as.Date(incident_merge_south$date)))
dateFrequency_south # グラフ化するまでもなく特に違いなさそう(てかデータ数少ない)
# time_south<- ggplot(dateFrequency_south,
#                    aes(x = dateFrequency_south$Var1, y = dateFrequency_south$Freq, group=1)) + geom_line()
# time_south <- time_south + labs(x = "Time []", y = "Freq_sourh []")
# plot(time_south)





###############################
# 地域データと事件データの結合
###############################
head(incident); head(area); dim(incident); dim(area)
incident_orig <- incident

# 地域データの行インデックスを格納するareaIdという列を準備
incident <- data.frame(incident, areaId = rep(0, nrow(incident)))

# for以外でやるやり方が思いつかなかった...遅いですが、すみません
# 州分だけforループ

# for(i in c( 1 : 1 ) ){
for(i in c( 1 : length(unique(incident$state)) ) ){
  # 州を一つピックアップ
  pickedStates <- unique(incident$state)[i]
  # 事件データのうち特定の州を抽出
  oneStateIncident <- subset(incident, grepl(pickedStates, incident$state))
  # デバッグのためメモ出力、省略可
  print( paste(i, "個目の州", pickedStates, length(unique(oneStateIncident$city)), "個のcityがあります") )
  
  # 事件データのうち特定の州のうち市の分だけforループ
  for(j in c( 1:length(oneStateIncident$city) ) ){
    # 市を一つピックアップ
    pickedCity <- oneStateIncident$city[j]
    # 地域データのうち州が一致してるデータ、のうち、市が一致してるデータを抽出
    pickedAreaSet <- subset(subset(area, area$Geographic.Area == pickedStates), 
                            grepl(pickedCity, subset(area, area$Geographic.Area == pickedStates)$City))
    
    # 地域データのうち抽出したデータの行番号を登録
    # rowNum <- ifelse(length(rownames(pickedAreaSet)) == 1,
    #                  rownames(pickedAreaSet), NA)
    rowNum <- ifelse(length(rownames(pickedAreaSet)) == 1,
                     rownames(pickedAreaSet),
                     debugFun_countNonCoressponds(pickedCity, pickedAreaSet))

    # 個人データの中で見てるデータセットのareaIdに地域データの行番号を登録
    incident[(incident$state == pickedStates & incident$city == pickedCity), ncol(incident)] <- rowNum
    
  }# for j
  
}# for i
print(paste(dim(incident)[1], "個中, 合計", count, "個の事件データが不一致でスキップ"))
count <- 0

head(incident)
# incident_bak <- incident

# うまく抽出できなかった行に入ってるNA削除
sum(is.na(incident))
incident_omitNA <- na.omit(incident)
dim(incident_omitNA)

# 事件データに地域データを結合
incident_merge <- data.frame(incident_omitNA, area[incident_omitNA$areaId, ])
dim(incident_merge)
# ちゃんと行ごとに州名と市名で結合できていることを確認(^ω^)やった〜
data.frame(incident_merge$city, incident_merge$City, 
           incident_merge$Geographic.Area, incident_merge$state)
colnames(incident_merge); dim(incident_merge)


# その他データを綺麗にするなど
incident_merge <- incident_merge[, -c(1,2,15,16,17)] # id, name, 重複した市名と州名, 結合に使ったidなど削除
# armedをgunとそれ以外に分けた列、stateを南部とそれ以外に分けた列
armed_isGun <- ifelse(incident_merge$armed == "gun", TRUE, FALSE)
state_isSouth <- ifelse(incident_merge$state == south, TRUE, FALSE)
incident_merge <- data.frame(incident_merge, 
                             armed_isGun = armed_isGun,
                             state_isSouth = state_isSouth)

colnames(incident_merge); dim(incident_merge)
head(incident_merge); str(incident_merge)





# ↑ここまで前処理





###############################
# データを探索的に見てみてなんとなくデータ分布の感覚を得る
###############################


# ------------ 
# 数値データの分布を確認
# ------------

# 年齢の棒グラフ
hist_age <- ggplot(data = incident_merge, aes(x = incident_merge$age)) + geom_bar()

df <- data.frame(incident_merge$race, incident_merge$age)
df <- df[order(df$incident_merge.race),]
df <- df[df$race == "B" | df$race == "H" | df$race == "W", ]

colnames(df) <- c("race", "age")
temp <- melt(df, id = "race")
g <- ggplot(temp, aes(x = temp$value, fill=temp$race, colour=temp$race))
# g <- g + geom_histogram(position = "identity", alpha = 0.5, binwidth = 1)
# g <- g + scale_fill_npg()
# g <- g + geom_histogram(position = "identity", alpha = 0.5, binwidth = 1, aes(y=..density..))
g <- g + geom_density(alpha=0.1)
plot(g)




plot(hist_age)

head(incident_merge)

# $percent_completed_hs と $Median.Incomeの箱ひげ図
box_per <- ggplot(incident_merge, aes(x = "", y = incident_merge$percent_completed_hs)) + geom_boxplot()
box_income <- ggplot(incident_merge, aes(x = "",y = incident_merge$Median.Income)) + geom_boxplot()
grid.arrange(box_per, box_income, ncol = 2) # 2列に並べる

# $ share_raceの散布図
share_race <- rbind(
  data.frame(race = "white", share = incident_merge$share_white),
  data.frame(race = "asian", share = incident_merge$share_asian),
  data.frame(race = "black", share = incident_merge$share_black),
  data.frame(race = "na", share = incident_merge$share_native_american),
  data.frame(race = "hispanic", share = incident_merge$share_hispanic)
)
box_share <- ggplot(share_race, aes(x = race, y = share)) + geom_boxplot()
plot(box_share)



# ------------
# カテゴリカルデータの分布
# ------------

# いろいろな変数の棒グラフ
bar_1 <- ggplot(data = incident_merge, aes(x = incident_merge$manner_of_death)) + geom_bar()
bar_2 <- ggplot(data = incident_merge, aes(x = incident_merge$gender)) + geom_bar()
bar_3 <- ggplot(data = incident_merge, aes(x = incident_merge$race)) + geom_bar()
bar_4 <- ggplot(data = incident_merge, aes(x = incidentincident_merge$city)) + geom_bar()
bar_5 <- ggplot(data = incident_merge, aes(x = incident_merge$state)) + geom_bar()
bar_6 <- ggplot(data = incident_merge, aes(x = incident_merge$signs_of_mental_illness)) + geom_bar()
bar_7 <- ggplot(data = incident_merge, aes(x = incident_merge$threat_level)) + geom_bar()
bar_8 <- ggplot(data = incident_merge, aes(x = incident_merge$flee)) + geom_bar()
bar_9 <- ggplot(data = incident_merge, aes(x = incident_merge$body_camera)) + geom_bar()
# まとめて1枚に出力
grid.arrange(bar_1, bar_2, bar_3, bar_5, ncol = 2) # 2列に並べる
grid.arrange(bar_6, bar_7, bar_8, bar_9, ncol = 2)




# ------------
# 時系列での変遷(incident$date)を横軸に
# {人種}{Income}{Illness}{South/North}の時期の違い
# ------------


# {時期}による頻度
dateFrequency <- data.frame(table(as.Date(incident_merge$date)))
time_freq <- ggplot(dateFrequency,
                    aes(x = dateFrequency$Var1, y = dateFrequency$Freq, group = 1)) + geom_line()
time_freq <- time_freq + labs(x = "Time []", y = "Freq []")
plot(time_freq) # 別に違いない


# {収入}(と{貧困率})ごとに見た時期による頻度
time_income <- ggplot(incident_merge,
                      aes(x = incident_merge$date, y = incident_merge$Median.Income)) + geom_line()
time_income <- time_income + labs(x = "Time []", y = "Income [$]")
plot(time_income) # 別に違いない
# $poverty、もほぼ一緒、違いない


# {人種} ごとに見た時期による頻度
share.df <- data.frame(date = incident_merge$date,
                       white = incident_merge$share_white, black = incident_merge$share_black,
                       asian = incident_merge$share_asian, na = incident_merge$share_native_american,
                       hispanic = incident_merge$share_hispanic)
temp <- melt(share.df, id = "date", measure = c("white", "black", "asian", "na", "hispanic"))
time_share <- ggplot(temp, aes(x = date, y = value, group = variable, colour = variable)) + geom_line()
time_share <- time_share + labs(x = "Time []", y = "share [%]")
plot(time_share) # 時期による違いまるでない





# {病気}ごとに見た時期による違い
ill_dummy <- as.data.frame(predict(dummyVars(~., data = as.data.frame(incident_merge$signs_of_mental_illness)), 
                                   as.data.frame(incident_merge$signs_of_mental_illness)))
dateIll <- data.frame(date = incident_merge$date, illness = ill_dummy[,2])
dateFrequency_ill <- aggregate(dateIll$illness, by = list(dateIll$date), sum)
time_ill<- ggplot(dateFrequency_ill,
                  aes(x = dateFrequency_ill$Group.1, y = dateFrequency_ill$x, group=1)) + geom_line()
time_ill <- time_ill + labs(x = "Time []", y = "Freq_ill []")
plot(time_ill)





# ------------
# 相関
# ------------

# {貧困}と{収入}の相関
sc_povInc <- ggplot(incident_merge, 
                    aes(x = incident_merge$Median.Income, y = incident_merge$poverty_rate)) + geom_point()
# {貧困}と{人種}の相関
share.df <- data.frame(share.df, 
                       Poverty = incident_merge$poverty_rate, 
                       Income = incident_merge$Median.Income,
                       CompHS = incident_merge$percent_completed_hs)
temp <- melt(share.df, id = "date", measure = c("white", "black", "asian", "na", "hispanic"))
head(share.df)
temp <- data.frame(temp, 
                   Poverty = incident_merge$poverty_rate, 
                   Income = incident_merge$Median.Income,
                   CompHS = incident_merge$percent_completed_hs)
sc_povRace <- ggplot(temp, 
                     aes(x = temp$value, y = temp$Income, colour=variable)) + geom_point()
plot(sc_povRace)

# {学歴}と{人種}の相関
sc_compHSRace <- ggplot(temp, 
                     aes(x = temp$value, y = temp$CompHS, colour=variable)) + geom_point()
plot(sc_compHSRace)



# incident_merge <- incident_merge_south
# incident_bak <- incident_merge
# incident_merge <- incident_bak

# {学歴}と{被害者人種}、{収入}と{被害者人種}の相関
head(incident_merge)
victimB <- incident_merge[incident_merge$race == "B", 
                          c("armed", "signs_of_mental_illness", "percent_completed_hs", "Median.Income")]
victimW <- incident_merge[incident_merge$race == "W", 
                          c("armed", "signs_of_mental_illness", "percent_completed_hs", "Median.Income")]
victims <- rbind(
  data.frame(variable = rep("vicBlack", dim(victimB)[1]), victimB),
  data.frame(variable = rep("vicWhite", dim(victimW)[1]), victimW)
)
g_income <- ggplot(victims, aes(x = victims$Median.Income, fill = variable))
g_income <- g_income + geom_histogram(position = "identity", alpha = 0.5, binwidth = 1000)
g_income <- g_income + scale_fill_npg()
# plot(g_income)
g_compHS <- ggplot(victims, aes(x = victims$percent_completed_hs, fill = variable))
g_compHS <- g_compHS + geom_histogram(position = "identity", alpha = 0.5, binwidth = 1)
g_compHS <- g_compHS + scale_fill_npg()
# plot(g_compHS)
# 並べて表示
grid.arrange(g_income, g_compHS, ncol = 2)


# {貧困}と{人種}の相関
share.df <- data.frame(share.df, Poverty = incident_merge$poverty_rate, Income = incident_merge$Median.Income)
temp <- melt(share.df, id = "date", measure = c("white", "black", "asian", "na", "hispanic"))
head(share.df)
head(temp)
temp <- data.frame(temp, Poverty = incident_merge$poverty_rate, Income = incident_merge$Median.Income)
sc_povRace <- ggplot(temp, 
                     aes(x = temp$value, y = temp$Income, colour=variable)) + geom_point()
plot(sc_povRace)
grid.arrange(sc_povInc, sc_povRace, ncol = 2) # 2列に並べる





# ------------
# そのほか
# ------------

# {カメラ所持}と{人種}の関係
race_dummy <- as.data.frame(predict(dummyVars(~., data = as.data.frame(incident_merge$race)), 
                                    as.data.frame(incident_merge$race)))
colnames(race_dummy) <- c("A", "B", "H", "N", "O", "W")
body_dummy <- as.data.frame(predict(dummyVars(~., data = as.data.frame(incident_merge$body_camera)),
                                    as.data.frame(incident_merge$body_camera)))
colnames(body_dummy) <- c("camera_0", "camera_1")
raceCam <- data.frame(race_dummy, body_dummy)
raceCam[raceCam$camera_1 == "1", ] # カメラ所持時の犠牲者の人種
raceCam[raceCam$camera_0 == "1", ] # カメラ所持してない時の犠牲者の人種
raceCam_1 <- apply(raceCam[raceCam$camera_1 == "1", ], 2, sum)
raceCam_0 <- apply(raceCam[raceCam$camera_0 == "1", ], 2, sum)
raceCam_1 / raceCam_1[8]; raceCam_0 / raceCam_0[7] # カメラ所持時と未所持時の犠牲者の人種の確率
# グラフ化するまでもなさそう、特に違いなし




# だいたいの人種の総計
apply(share.df[,2:6], 2, sum)/100 # シェアの人種ごとの合計、意味ないけどなんとなくのシェアを確認するため
table(incident_merge$race) # 被害者の人種ごとの総計


# 人種ごとのヒストグラム
df <- data.frame(value = incident_merge[incident_merge$race == "B", "share_black"]); head(df)
g <- ggplot(df, aes(x = value)) + geom_histogram(binwidth = 5)
plot(g)

df <- data.frame(value = incident_merge[incident_merge$race == "W", "share_black"]); head(df)
g <- ggplot(df, aes(x = value)) + geom_histogram(binwidth = 5)
plot(g)

df <- data.frame(value = incident_merge[incident_merge$race == "B", "share_white"]); head(df)
g <- ggplot(df, aes(x = value)) + geom_histogram(binwidth = 5)
plot(g)

df <- data.frame(value = incident_merge[incident_merge$race == "W", "share_white"]); head(df)
g <- ggplot(df, aes(x = value)) + geom_histogram(binwidth = 5)
plot(g)

vicBshareB <- incident_merge[incident_merge$race == "B", "share_black"]
vicBshareW <- incident_merge[incident_merge$race == "B", "share_white"]
vicWshareW <- incident_merge[incident_merge$race == "W", "share_white"]
vicWshareB <- incident_merge[incident_merge$race == "W", "share_black"]
vicShare <- rbind(
  data.frame(variable = rep("vicB_shareBlack", length(vicBshareB)), value = vicBshareB),
  data.frame(variable = rep("vicB_shareWhite", length(vicBshareW)), value = vicBshareW),
  data.frame(variable = rep("vicW_shareWhite", length(vicWshareW)), value = vicWshareW),
  data.frame(variable = rep("vicW_shareBlack", length(vicWshareB)), value = vicWshareB)
)
g <- ggplot(vicShare, aes(x = value, fill = variable))
g <- g + geom_histogram(position = "identity", alpha = 0.5, binwidth = 1)
# g <- g + scale_fill_npg()
g <- g + scale_fill_npg() + ylim(0, 80)  # y軸の範囲指定
plot(g)

head(incident_merge)




# なにで使ってるか忘れた
victimA <- incident_merge[incident_merge$race == "A", 
                          c("armed", "signs_of_mental_illness", "percent_completed_hs", "Median.Income")]
victimH <- incident_merge[incident_merge$race == "H", 
                          c("armed", "signs_of_mental_illness", "percent_completed_hs", "Median.Income")]
victimN <- incident_merge[incident_merge$race == "N", 
                          c("armed", "signs_of_mental_illness", "percent_completed_hs", "Median.Income")]

victims_all <- rbind(
  data.frame(variable = rep("vicWhite", dim(victimW)[1]), victimW),
  data.frame(variable = rep("vicBlack", dim(victimB)[1]), victimB),
  data.frame(variable = rep("vicHispa", dim(victimH)[1]), victimH),
  data.frame(variable = rep("vicAsian", dim(victimA)[1]), victimA),
  data.frame(variable = rep("vicNaAm", dim(victimB)[1]), victimB)
)





# 南部とそれ以外の被害者の{人種}
# 総数
df <- data.frame(isSouthTRUE = table(incident_merge_south$race), 
                 isSouthFALSE = table(filter(incident_merge, state_isSouth == FALSE)$race))
colnames(df) <- c("race", "isSouth", "race", "nonSouth")
temp <- melt(df, id = "race")
g <- ggplot(temp, aes(x = temp$race, y = temp$value, fill = temp$variable))
g <- g + geom_bar(position = position_dodge(), stat = "identity")
plot(g)

# 割合
temp_isSouth <- temp[temp$variable == "isSouth", "value"]
temp[temp$variable == "isSouth", "value"] <- temp_isSouth / sum(temp_isSouth)
temp_nonSouth <- temp[temp$variable == "nonSouth", "value"]
temp[temp$variable == "nonSouth", "value"] <- temp_nonSouth / sum(temp_nonSouth)
g <- ggplot(temp, aes(x = temp$race, y = temp$value, fill = temp$variable))
g <- g + geom_bar(position = position_dodge(), stat = "identity")
plot(g)



###############################
# 人種別の全体の人口に対する被害者の割合
###############################
# allPeople <- data.frame(c(2039400, 38408000, 57560600, 17651200, 192336100))
# allPeople <- data.frame(c(14674252, 38929319, 50477594, 2932248, 223553265))
# allPeople <- data.frame(c(14674252, 38929319, 50477594, 223553265))
allPeople <- data.frame(c(38929319, 50477594, 223553265))
rownames(allPeople) <- c("Black", "Hispanic", "White")
# rownames(allPeople) <- c("Asian", "Black", "Hispanic", "White")
race_dummy_ABHW <- race_dummy[,-c(1, 4, 5)] # A,N,Oを除く
vicRaceSum <- apply(race_dummy_ABHW, 2, sum)
vicRaceProb <- (vicRaceSum / allPeople) * 1000000
colnames(vicRaceProb) <- "probability"
g <- ggplot(data = vicRaceProb, aes(x = rownames(vicRaceProb), y = vicRaceProb$probability, fill = rownames(vicRaceProb)))
g <- g + geom_bar(stat = "identity")
plot(g)



vicRaceSum



###############################
# 決定木 : 黒人が殺されてる時のTreeと白人が殺されてる時のTree
###############################

# データの結合の過程で行名がぐちゃぐちゃになってるのをリセット
rownames(incident_merge) <- c(1:nrow(incident_merge))


# # Tree用にデータセット作成
# colnames(incident_train)
# ic_train_victim <- data.frame(
#   select(incident_train, -c(date, gender, armed, race, city, state)), 
#   race_dummy_train) # いらないデータ削除、追加
# colnames(ic_train_victim)
# str(ic_train_victim)


# Tree用にデータセット作成
colnames(incident_merge)
ic_victim <- data.frame(
  select(incident_merge, -c(date, gender, armed, race, city, state)), 
  race_dummy) # いらないデータ削除、追加
colnames(ic_victim)
str(ic_victim)

ic_victim <- ic_victim[ic_victim$B == 1 | ic_victim$W == 1,]

# dim(ic_victim[ic_victim$share_black<14.15,])[1]

# BlackのTree
# ic_train_victim_B <- select(ic_train_victim, -c(A, H, N, O, W))
ic_victim_B <- select(ic_victim, -c(A, H, N, O, W, 
                                    share_white, share_native_american, 
                                    share_asian, share_hispanic))
ic_victim_B <- select(ic_victim, -c(A, H, N, O, W, share_black,
                                    share_white, share_native_american, 
                                    share_asian, share_hispanic))
# ic_victim_B <- select(ic_victim, c(B, age))
# ic_victim_B[ic_victim_B$B==1,]
# hist(ic_victim_B[ic_victim_B$B==1,"age"])
# ic_victim_B[ic_victim_B$B == 1, "B"] <- "Black"
# ic_victim_B[ic_victim_B$B == 0, "B"] <- "White"
ic_victim_B$B <- ifelse(ic_victim_B$B == 1, "Black", "White")

dim(ic_victim_B)[1]
class.tree_B <- rpart(B ~ ., data = ic_victim_B, minbucket = 50, 
                     control = rpart.control(maxdepth = 7), method = "class")
# class.tree_B <- rpart(B ~ age, data = ic_victim_B, method = "class")

# png("tree_victimB.png", width = 700, height = 1000) 
prp(class.tree_B, type = 4, extra = 1, split.font = 1, varlen = -10, digits = -22, cex=0.6) # 結果をプロット
# dev.off()
# 木を刈り込むのが必要であればあとで追加


# WhiteのTree
ic_victim_W <- select(ic_victim, -c(A, B, H, N, O,
                                          share_black, share_native_american,
                                          share_asian, share_hispanic))
ic_victim_W <- select(ic_victim, -c(A, B, H, N, O, share_white,
                                    share_black, share_native_american, 
                                    share_asian, share_hispanic))
dim(ic_victim_W)[1]

ic_victim_W$W <- ifelse(ic_victim_W$W == 1, "White", "Black")

class.tree_W <- rpart(W ~ ., data = ic_victim_W, minbucket = 50, 
                      control = rpart.control(maxdepth = 7), method = "class")
# png("tree_victimW.png", width = 700, height = 1000) 
prp(class.tree_W, type = 4, extra = 1, split.font = 1, varlen = -10, digits = -22, cex=0.6, xflip=TRUE) # 結果をプロット
# dev.off()


########################


# trainデータとvalidationデータに分割
incident_merge.trainIndex <- sample(row.names(incident_merge), 0.6*dim(incident_merge)[1])
incident_merge.validIndex <- setdiff(row.names(incident_merge), incident_merge.trainIndex)
incident_train <- incident_merge[incident_merge.trainIndex, ]
incident_valid <- incident_merge[incident_merge.validIndex, ]
race_dummy_train <- race_dummy[incident_merge.trainIndex, ]


# Tree用にデータセット作成
colnames(incident_train)
ic_victim_train <- data.frame(
  select(incident_train, -c(date, gender, armed, race, city, state)), 
  race_dummy_train) # いらないデータ削除、追加
colnames(ic_victim_train)
str(ic_victim_train)

ic_victim_train <- ic_victim_train[ic_victim_train$B == 1 | ic_victim_train$W == 1,]


# BlackのTree
# ic_train_victim_B <- select(ic_train_victim, -c(A, H, N, O, W))
ic_victim_train_B <- select(ic_victim_train, -c(A, H, N, O, W, 
                                    share_white, share_native_american, 
                                    share_asian, share_hispanic))
ic_victim_train_B <- select(ic_victim_train, -c(A, H, N, O, W, share_black,
                                    share_white, share_native_american, 
                                    share_asian, share_hispanic))
# ic_victim_B <- select(ic_victim, c(B, age))
# ic_victim_B[ic_victim_B$B==1,]
# hist(ic_victim_B[ic_victim_B$B==1,"age"])
# ic_victim_B[ic_victim_B$B == 1, "B"] <- "Black"
# ic_victim_B[ic_victim_B$B == 0, "B"] <- "White"
ic_victim_train_B$B <- ifelse(ic_victim_train_B$B == 1, "Black", "White")

dim(ic_victim_train_B)[1]
class.tree_B_train <- rpart(B ~ ., data = ic_victim_train_B, minbucket = 50, 
                      control = rpart.control(maxdepth = 7), method = "class")

# png("tree_victimB.png", width = 700, height = 1000) 
prp(class.tree_B_train, type = 4, extra = 1, split.font = 1, varlen = -10, digits = -22, cex=0.6) # 結果をプロット
# dev.off()
# 木を刈り込むのが必要であればあとで追加


# WhiteのTree
ic_victim_train_W <- select(ic_victim_train, -c(A, B, H, N, O,
                                    share_black, share_native_american,
                                    share_asian, share_hispanic))
ic_victim_train_W <- select(ic_victim_train, -c(A, B, H, N, O, share_white,
                                    share_black, share_native_american, 
                                    share_asian, share_hispanic))
ic_victim_train_W$W <- ifelse(ic_victim_train_W$W == 1, "White", "Black")
class.tree_train_W <- rpart(W ~ ., data = ic_victim_train_W, minbucket = 50, 
                      control = rpart.control(maxdepth = 7), method = "class")
# png("tree_victimW.png", width = 700, height = 1000) 
prp(class.tree_train_W, type = 4, extra = 1, split.font = 1, varlen = -10, digits = -22, cex=0.6, xflip=TRUE) # 結果をプロット
# dev.off()



# type="class"で分類学習のpredictをすることを明示的に書く
pred.train <- predict(class.tree_B, type = "class")
cm_tree <- confusionMatrix(as.factor(pred.train), as.factor(train.df$Competitive.))


plot(incident_merge$share_black, incident_merge$percent_completed_hs)
plot(incident_merge$share_black, incident_merge$percent_completed_hs)



###############################
# ロジスティック回帰 : 黒人が殺されてるかどうかの分類
###############################

### 全データでのロジスティック回帰

# データセット作成
colnames(incident_merge)
ic_victim <- data.frame(
  select(incident_merge, -c(date, gender, armed, race, city, state)), 
  race_dummy) # いらないデータ削除、追加
colnames(ic_victim)
str(ic_victim)

# ic_victim <- ic_victim[ic_victim$B == 1 | ic_victim$W == 1,]

dim(incident_merge); colnames(incident_merge)
dim(ic_victim); colnames(ic_victim)


ic_victim_B <- select(ic_victim, c("age",
                                   "signs_of_mental_illness",
                                   "percent_completed_hs",
                                   "poverty_rate",
                                   "Median.Income",
                                    "B"))
ic_victim_B <- select(ic_victim, -c(A, H, N, O, W))
ic_victim_W <- select(ic_victim, -c(A, H, N, O, B,
                                    share_white, share_black,
                                    share_native_american, share_asian,
                                    share_hispanic))

colnames(ic_victim_B)


# Black
logitReg_B <-glm(B ~ ., data = ic_victim_B, family = "binomial")
pred_logitReg_B <- predict(logitReg_B, ic_victim_B, type="response") # できたモデルを使って推定値を作ってみる

# ifelse(pred_logitReg > cutoff, 1, 0)は、
# predがcutoff値より大きい確率だったら1に分類、それ以外は0に分類としたデータ列
cm_logitReg_B <- confusionMatrix(as.factor(ifelse(pred_logitReg_B > cutoff, 1, 0)), 
                                  as.factor(ic_victim_B$B))
plot(pred_logitReg_B)
# White
logitReg_W <-glm(W ~ ., data = ic_victim_W, family = "binomial")
pred_logitReg_W <- predict(logitReg_W, ic_victim_W) # できたモデルを使って推定値を作ってみる
cm_logitReg_W <- confusionMatrix(as.factor(ifelse(pred_logitReg_W > cutoff, 1, 0)), 
                                  as.factor(ic_victim_W$W))


# 全データでのロジスティック回帰のまとめ
summary(logitReg_B)
summary(logitReg_W)
cm_logitReg_B
cm_logitReg_W






#### trainデータとvalidationデータに分割したロジスティック回帰

# trainデータとvalidationデータに分割
ic_victim.trainIndex <- sample(row.names(ic_victim), 0.6*dim(ic_victim)[1])
ic_victim.validIndex <- setdiff(row.names(ic_victim), incident_merge.trainIndex)
ic_victim_train <- ic_victim[ic_victim.trainIndex, ]
ic_victim_valid <- ic_victim[ic_victim.validIndex, ]
race_dummy_train <- race_dummy[ic_victim.trainIndex, ]

colnames(ic_victim_train)
colnames(ic_victim_valid)
cutoff <- 0.5

ic_victim_B_train <- select(ic_victim_train, -c(A, H, N, O, W))
ic_victim_B_valid <- select(ic_victim_valid, -c(A, H, N, O, W))

ic_victim_W_train <- select(ic_victim_train, -c(A, H, N, O, B))
ic_victim_W_valid <- select(ic_victim_valid, -c(A, H, N, O, B))
# ic_victim_B <- select(ic_victim_train, -c(A, H, N, O, W, share_black,
#                                     share_white, share_native_american, 
#                                     share_asian, share_hispanic))
# ic_victim_W <- select(ic_victim_train, -c(A, H, N, O, B, share_white,
#                                     share_white, share_native_american, 
#                                     share_asian, share_hispanic))


# Black
logitReg_B_train <-glm(B ~ ., data = ic_victim_B_train, family = "binomial")
pred_logitReg_B2 <- predict(logitReg_B_train, ic_victim_B_valid) # できたモデルを使って推定値を作ってみる

length(as.factor(ifelse(pred_logitReg_B2 > cutoff, 1, 0)))
dim(ic_victim_B_valid)[1]

# ifelse(pred_logitReg > cutoff, 1, 0)は、
# predがcutoff値より大きい確率だったら1に分類、それ以外は0に分類としたデータ列
cm_logitReg_B2 <- confusionMatrix(as.factor(ifelse(pred_logitReg_B2 > cutoff, 1, 0)), 
                                       as.factor(ic_victim_B_valid$B))

# White
logitReg_W_train <-glm(W ~ ., data = ic_victim_W_train, family = "binomial")
pred_logitReg_W2 <- predict(logitReg_W_train, ic_victim_W_valid) # できたモデルを使って推定値を作ってみる
cm_logitReg_W2 <- confusionMatrix(as.factor(ifelse(pred_logitReg_W2 > cutoff, 1, 0)), 
                                  as.factor(ic_victim_W_valid$W))

summary(logitReg_B_train)
summary(logitReg_W_train)
cm_logitReg_B2
cm_logitReg_W2



###############################
# デバッグ用の関数
###############################
debugFun_countNonCoressponds <- function(pickedCity.p, pickedAreaSet.p){
  count <<- count + 1
  print(paste(">>>" , count, "個目: ", pickedCity.p, "で検索したけど, 以下が該当"))
  print(as.character(pickedAreaSet.p$City))
  return (NA)
}



# 以下メモ

# 地域、カメラ所持、時期時間、
# 南部だと黒人殺されてる？
# 黒人の人に対する殺害率
# share
# 人口/比率 殺され度合い
# share100%の時の殺害数と
# share10%の時の殺害数 で重篤度が違う 10%しかいないのにわざわざ黒人殺したみたいなイメージ
# 全city分足し 合わせると 各人種が全米の100%を閉めたときにどれくらい殺されたかがわかる
# 
# complete_hc、incomeとの相関
# complete_hc、income低い -> 黒人とか関係なくそもそも犯罪犯しやすい -> 打たれやすい
# 黒人 -> 打たれやすい # 打たれた白人とかの収入やcompletehsが高かったら 黒人打たれやすいってこと
# [1] race.BのIncome

# [仮説1] 南部の被害者の人種は他の地域の被害者の人種より黒人より
# [仮説2] 白人被害者の収入学歴<黒人被害者の収入学歴 => 黒人被害者になりやすい
# [前提2] 被害者の収入学歴が低い、
# [前提] 学歴と収入の相関、人種ごと

# カメラつけてた時とつけてない時の被害者人種の違い
# 黒人シェア小さいところで被害者黒人の割合?
# 季節による{人種, 場所, illness}の違い


# 黒人が殺されてる時のTree
# 白人が?Tree
# 黒人{白人}かどうか1/0をClassification
# 黒人かどうかを求める計算が、計算精度が低かったら、他の要素関係なく黒人撃ってると言える



# EOF