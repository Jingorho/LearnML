setwd("/Users/yukako/WorkSpace/LearnML/15_071_AnalyticsEdge/Assignment8/")
his <- read.csv("Dartboard_historical.csv")
split1 = (his$Year <= 2013)
split2 = (his$Year > 2013)
train = his[split1,]
test = his[split2,]
head(his)

# Create variables

# d   : denotes the demand in week t from county c , in dollars;
d = train$Sales

# pop : denotes the population in week t of county c;
#barplot(train$Population)
pop = train$Population

# inc : denotes the income per capita in week t of county c in dollars;
inc = train$Income

# t   : denotes the number of weeks elapsed since the start of year 2012;
t = train$Week_Num

# u   : indicates if week t falls in “season” s of the year (=1); otherwise, 0;
u = as.factor(train$Season) 
table(u)


# -----------------------------------------------------------
# ↓ 編集部分1
# -----------------------------------------------------------
# 式に使う項で改めてデータフレーム作る
train_named <- data.frame(y = log10(d/pop), x0 = log10(inc), x1 = t, x2 = u)

# testデータでも同様に改めてデータフレーム作る
d_test = test$Sales
pop_test = test$Population
inc_test = test$Income
t_test = test$Week_Num
u_test = as.factor(test$Season) 
test_named <- data.frame(y = log10(d_test/pop_test), x0 = log10(inc_test), x1 = t_test, x2 = u_test)


# モデル構築時にy,x0,x1,x2という名前を明示的に使う
result_named <- lm(y ~ x0 + x1 + x2, data = train_named)
summary(result_named) # 結果はresultと同じ

# test_namedにもy,x0,x1,x2があるので、正確にtestデータに対してpredictできる
pred_named = predict(result_named, newdata = test_named)
pred_named
predSale_named = test$Population * 10^(pred_named)
# -----------------------------------------------------------
# ↑ 編集部分1 おわり
# -----------------------------------------------------------



# -----------------------------------------------------------
# ↓ おまけというか 確認
# -----------------------------------------------------------
# 正解データ(test$Sales)との差を見てみると、改めてデータセットを作って項の名前を揃えた_namedの方が小さげ
plot(test$Sales - predSale_named)
plot(test$Sales - predSale)

length(test$Sales) # 正解データが39780行なのに
length(predSale) # 79560行ある(trainの行数)
length(predSale_named) # 項の名前を揃えた方は正解データと同じ39780行

# 最初の5000個をプロットしてみる
plot(test$Sales[1:5000], ylim=c(0,2000000), cex=0.1) # 正解データ
par(new=T) # グラフ重ね書きの命令(実行しても何も表示されませんが次に進んでおk)
plot(predSale_named[1:5000], ylim=c(0,2000000), col="blue", cex=0.1) # 項の名前を揃えて予測したデータ
par(new=T)
plot(predSale[1:5000], ylim=c(0,2000000), col="red", cex=0.1) # 
# 最初の5000個は違いがなさそう

# 全部プロットしてみる
plot(test$Sales, xlim=c(0, length(predSale)), ylim=c(0,2000000), cex=0.1) # 正解データ
par(new=T)
plot(predSale_named, xlim=c(0, length(predSale)), ylim=c(0,2000000), col="blue", cex=0.1) # 項の名前を揃えて予測したデータ
par(new=T)
plot(predSale, xlim=c(0, length(predSale)), ylim=c(0,2000000), col="red", cex=0.1) # やっぱなんか数多い...
# -----------------------------------------------------------
# ↑ おまけというか 確認 おわり
# -----------------------------------------------------------




# Let's allocate the equation
result = lm(log10(d/pop) ~ log10(inc) + t + u, data = train)
# result = lm(log10(train$Sales/train$Population) ~ log10(train$Income) + train$Week_Num + as.factor(train$Season), data = train)
summary(result)

#c) MAPE
pred = predict(result, newdata = test)
pred

predSale=test$Population*10^(pred)

# install.packages("MLmetrics")
library(MLmetrics)
MAPE(y_pred = predSale, y_true = test$Sales)
# -----------------------------------------------------------
# ↓ 編集部分2
# -----------------------------------------------------------
MAPE(y_pred = predSale_named, y_true = test$Sales) # _namedにした
# -----------------------------------------------------------
# ↑ 編集部分2 おわり
# -----------------------------------------------------------













#d) i) total online sales in the Northeast region from mid-2015 to end-2017
fut <- read.csv("Dartboard_future.csv")

# -----------------------------------------------------------
# ↓ 編集部分3
# -----------------------------------------------------------
# futデータでも同様に改めてデータフレーム作る
# (わざわざtとかuとか定義するのめんどいので直接変数作った)
fut_named <- data.frame(x0 = log10(fut$Income), 
                        x1 = fut$Week_Num, 
                        x2 = as.factor(fut$Season))
head(test)
head(test_named)
head(fut)
head(fut_named)
pred_future_named = predict(result_named, newdata = fut_named)
pred_futureSale_named = fut$Population * 10^(pred_future_named)
sum(pred_futureSale_named)
# -----------------------------------------------------------
# ↑ 編集部分3 おわり
# -----------------------------------------------------------



pred_future = predict(result, newdata = fut)
pred_future

pred_futureSale = fut$Population * 10^(pred_future)

sum(pred_futureSale)




#d) ii)the last 8 weeks
sub_fut= subset(fut, subset = Week_Num>=305)
pred_future_8years = predict(result, newdata = sub_fut) # 8years?
pred_future_8years
pred_future_8yearsSale=sub_fut$Population*10^(pred_future_8years)
sum(pred_future_8yearsSale)
# -----------------------------------------------------------
# ↓編集部分4
# -----------------------------------------------------------
sub_fut = subset(fut, subset = Week_Num>=305) # おなじ
# モデルの変数名に合わせたデータフレーム作る
sub_fut_named <- data.frame(x0 = log10(sub_fut$Income), 
                            x1 = sub_fut$Week_Num, 
                            x2 = as.factor(sub_fut$Season))
pred_future_8weeks = predict(result_named, newdata = sub_fut_named) # 8years?となってたので8weeksにした
pred_future_8weeks
pred_future_8weeksSale = sub_fut$Population * 10^(pred_future_8weeks)
sum(pred_future_8weeksSale)
# -----------------------------------------------------------
# ↑編集部分4 おわり
# -----------------------------------------------------------




#d) iii)Suffolk County
sub_fut2= subset(fut, subset = County.Name=="Suffolk County")
pred_future_Suffolk = predict(result, newdata = sub_fut2)
pred_future_Suffolk
pred_future_SuffolkSale=sub_fut2$Population*10^(pred_future_Suffolk)
sum(pred_future_SuffolkSale)
# -----------------------------------------------------------
# ↓編集部分5
# -----------------------------------------------------------
sub_fut2 = subset(fut, subset = County.Name=="Suffolk County") # 同じ
# モデルの変数名に合わせたデータフレーム作る
sub_fut2_named <- data.frame(x0 = log10(sub_fut2$Income), 
                            x1 = sub_fut2$Week_Num, 
                            x2 = as.factor(sub_fut2$Season))
pred_future_Suffolk_named = predict(result_named, newdata = sub_fut2_named)
pred_future_Suffolk_named
pred_future_SuffolkSale_named = sub_fut2$Population * 10^(pred_future_Suffolk_named)
sum(pred_future_SuffolkSale_named)
# -----------------------------------------------------------
# ↑編集部分5
# -----------------------------------------------------------



# -----------------------------------------------------------
# 確認 (桁大きすぎるのでテキトーに /10^7 して見やすくしてます)
# -----------------------------------------------------------
# 修正前
sum(pred_futureSale) /10^7
sum(pred_future_8yearsSale) /10^7
sum(pred_future_SuffolkSale) /10^7

sum(pred_futureSale_named) /10^7
sum(pred_future_8weeksSale) /10^7
sum(pred_future_SuffolkSale_named) /10^7

