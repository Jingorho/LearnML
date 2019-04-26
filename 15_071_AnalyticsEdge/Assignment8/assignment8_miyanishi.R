setwd("/Users/yukako/WorkSpace/LearnML/15_071_AnalyticsEdge/Assignment8/")
his <- read.csv("Dartboard_historical.csv")
split1 = (his$Year <= 2013)
split2 = (his$Year > 2013)
train = his[split1,]
test = his[split2,]
head(his)

# Week_NumとWeekの関係がよくわからなかったので調べてました。本編とは関係ない
# Week_Numは2012年からのカウント
summary(his$Week)
summary(his$Week_Num)
summary(train$Week)
summary(train$Week_Num)
summary(his$County.Name)
table(his$County.Name)/52 # 群ごとに3年分(52週間分)あるやつとそれ以上の奴があるじゃねーか！
# -> 異なる州で被ってる名前の群があるからでした。そして以下にはなんも関係なかった
table(w=his$Week, n=his$Week_Num)
table(train$County.Name, train$Week)
table(train$County.Name, train$Week_Num)




# 変数作る


# d   : denotes the demand in week t from county c , in dollars;
d = train$Sales

# pop : denotes the population in week t of county c;
barplot(train$Population)
pop = train$Population

# inc : denotes the income per capita in week t of county c in dollars;
inc = train$Income

# t   : denotes the number of weeks elapsed since the start of year 2012;
t = train$Week_Num

# u   : indicates if week t falls in “season” s of the year (=1); otherwise, 0;
table(week = train$Week_Num, season = train$Season) # 2つの個数の関係を見たかっただけ
data.frame(week = train$Week_Num, season = train$Season) # Week_NumとSeasonを並べて見たかっただけ

# pdfのヒント「Week5はSeason2に属するのでu(5,2)=1, あと0」とのことなので、
# 「numericをカテゴリに変換するときの処理(ダミー変数に変換するときの処理)」
# と同じ処理をすればよいのかと思いました。
# ex.) 
# 変数monthがApril, May, June, ...だったのを
# April  May  June ...
#   1    0     0
#   0    1     0
#   0    0     1
# にするみたいな
#
# 前回のApril消える問題で扱った時の課題では、カテゴリ型変数をlm()に
# そのまま突っ込めばRが勝手にダミー変数っぽくしてくれるってことでしたが、
# 今回はSeasonそのまま突っ込むとnumericとして認識しちゃうと思うので、
# 明示的にas.factor()でカテゴリ型ですよ〜と教えてあげる(April消える問題みたいなことも起こらない)
u = as.factor(train$Season) 
u = as.date(train$Season) 
table(u)


# 式に当てはめる
result = lm(log10(d/pop) ~ log10(inc) + t + u)
summary(result)


# オマケ
# ほんとにregressionしてるか、summaryで見た各項の係数を使ってなんとなくたしかめ
# (切片とuはめんどいので省略)
plot(-2.345e-01*log10(inc) + 2.285e-03*t, log10(d/pop)) # すぎょい




# もっと明示的にやるなら?
install.packages("dummies")
library(dummies)
u_dum = dummy(as.factor(train$Season))
u_dum

result2 = lm(log10(d/pop) ~ log10(inc) + t + u_dum)
summary(result2)
# モデルの結果(Residualsとか)は同じだけど、各項の係数が違う...なぜ...

plot(-0.2350912*log10(inc) + 0.0020721*t, log10(d/pop))


a = c(2,5,6,8,11,12)
b0 = c(1,2,3,4,5,6)
b1 = c(100,120,140,160,120,200)
c = c("jp", "us", "us", "en", "nk", "jp")
c_dum = dummy(c)
c_fac = as.factor(c)
r = lm(a ~ b0+b1+c)
r_dum = lm(a ~ b0+b1+c_dum)
r_fac = lm(a ~ b0+b1+c_fac)
summary(r)
summary(r_fac)
summary(r_dum)
a <- data.frame()

