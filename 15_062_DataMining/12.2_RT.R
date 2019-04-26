rm(list = ls())
# setwd("C:/Users/ryuichi/Desktop/01 MIT SDM/2. class/DATA MINING/Individual Assignment/3")
# install.packages("DiscriMiner")
library(DiscriMiner) #for discriminant analysis
library(gains) # graphics for lift
admin.df <- read.csv("/Users/yukako/WorkSpace/DataMiningHW/data/SystemAdministrators.csv")

#12a
head(admin.df)
#pre-process; make dummies
#admin.df$Completed.task_Yes <- 1*(admin.df$Completed.task=="Yes")
#admin.df$Completed.task_No <- 1*(admin.df$Completed.task=="No")
yes<-subset(admin.df,admin.df$Completed.task=="Yes")
no<-subset(admin.df,admin.df$Completed.task=="No")
plot(yes$Experience, yes$Training,col="blue",xlab="",ylab = "", xlim=c(6,14))
par(new=T)
plot(no$Experience, no$Training,col="red",xlim=c(6,14),xlab="Experience", ylab = "Training")
legend("topleft", legend = c("yes","no"), col = c("blue","red"), pch = c(1,1),ncol=2)


#12b

da.reg <- linDA(admin.df[,1:2], admin.df[,3])
da.reg$functions
# compute probabilities manually (below); or, use lda() in package MASS with predict()
propensity <- exp(da.reg$scores[,2])/(exp(da.reg$scores[,1])+exp(da.reg$scores[,2]))
a<-data.frame(Actual=admin.df$Completed.task,
           da.reg$classification, da.reg$scores, propensity=propensity)
confusionMatrix(da.reg$classification,admin.df$Completed.task)
print(a)
#12c
da.reg$functions
da.reg$scores

# c
# (経験,資格)=(4,6)を、linDAで作ったモデル式(da.reg$functions)に代入
# 各クラスに分類される"score"を計算する(p298下)
newDataScore <- da.reg$functions[1,] + da.reg$functions[2,]*4 + da.reg$functions[3, ]*6
newDataScore # Noのスコアの方が高いのでNoっぽい。かっこよく出すならwhich.max(newDataScore)とか使ってもいいかも

# (任意) 説明のために必要であれば、線形判別直線を描いてみてもいいかも?
# lda()使うにはlibrary(MASS)必要
result <- lda(admin.df$Completed.task ~ admin.df$Experience + admin.df$Training, data = admin.df)
# lda_aとlda_bの定義はいろんなサイトでこれが使われてるからこれにしました。意味はまだよくわかってません!笑
lda_a <- apply(result$means%*%result$scaling, 2, mean) / result$scaling[2]
lda_b <- - result$scaling[1] / result$scaling[2]
abline(lda_a, lda_b, col = "black", lty = 1) # abline(a,b)は(a,b)を通る直線を描く関数。lty=2だと点線
# 凡例の上に重なってしまうので、凡例(legend関数)を上書きするか凡例の場所を変えるかするといいかも?

# d
# 判別直線から、Experience=8だとまだNoに判別されてることがわかるので、9年...とかでしょうか??
# 本当はかっこよく式で出す方法あるんでしょうけど、よくわかりませんでした!!!!笑

# スコア = 切片 + (Expericenceの係数)*経験年数 + (Trainingの係数)*資格の数



# 方法2 各クラスに分類される"probability"を計算する(p299下)
# 既存のデータのprobabilityの計算方法は教科書に載ってたが、新しいデータのprbobabilityの計算には
# 結局scoreの値が必要で、方法1で計算している。方法1で計算したscoreを使って
# 改めてprbobability計算してもいいが、方法1でもできるならやらなくていいかな...


#12e
reg <-glm(Completed.task ~ .,data = admin.df, family = "binomial")
summary(reg)
confusionMatrix(as.factor(ifelse(reg$fitted.values>0.5,"Yes","No")),admin.df$Completed.task)
