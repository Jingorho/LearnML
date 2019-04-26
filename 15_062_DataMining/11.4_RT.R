rm(list = ls())
# setwd("/Users/yukako/")

data <- read.csv("/Users/yukako/WorkSpace/DataMiningHW/data/EastWestAirlinesNN.csv")
head(data)

#11.4
data[,c(2,4,5,6,7,13,14,15,16)]<-sapply(data[,c(2,4,5,6,7,13,14,15,16)],as.numeric)
head(data)
data$Phone_sale_1 <- 1*(data$Phone_sale == 1)
data$Phone_sale_0 <- 1*(data$Phone_sale == 0)

library(caret)
normalization.model <- preProcess(data[,c(3,8,9,10,11,12)], method = "range")
data.norm <- predict(normalization.model, data)
head(data.norm)


training=sample(row.names(data.norm), dim(data.norm)[1]*0.6)
validation=setdiff(row.names(data.norm), training)
train.df <- data.norm[training,]
valid.df <- data.norm[validation,]
str(train.df)

#neural net
#install.packages("neuralnet")
#install.packages("nnet")
library(nnet)
library(neuralnet)
head(training)




###############################
# a
###############################


nn <- neuralnet(Phone_sale_1+Phone_sale_0~ +Topflight+Balance+
                  Balance+Qual_miles+cc1_miles.+cc2_miles.+cc3_miles.+Bonus_miles+Bonus_trans+Flight_miles_12mo+Flight_trans_12+Online_12+Email+Club_member+Any_cc_miles_12mo
                  , data = train.df, linear.output = F, hidden = 5)
head(nn)
plot(nn)
nn.pred <- prediction(nn)
nn$result.matrix
nn.pred
validation.prediction <- compute(nn, valid.df[, c("Topflight", 
                                                       "Balance", 
                                                       "Qual_miles",   
                                                       "cc1_miles.",
                                                       "cc2_miles.",     
                                                       "cc3_miles.", 
                                                       "Bonus_miles",
                                                       "Bonus_trans", 
                                                       "Flight_miles_12mo",
                                                       "Flight_trans_12", 
                                                       "Online_12", "Email", 
                                                       "Club_member",  
                                                       "Any_cc_miles_12mo")])
training.prediction <- compute(nn, train.df[, c("Topflight", 
                                                  "Balance", 
                                                  "Qual_miles",   
                                                  "cc1_miles.",
                                                  "cc2_miles.",     
                                                  "cc3_miles.", 
                                                  "Bonus_miles",
                                                  "Bonus_trans", 
                                                  "Flight_miles_12mo",
                                                  "Flight_trans_12", 
                                                  "Online_12", "Email", 
                                                  "Club_member",  
                                                  "Any_cc_miles_12mo")])



# Decile-wise Lift chart

#install.packages("gains")
library(gains)
validation.prediction$net.result
gain <- gains(valid.df$Phone_sale_1, validation.prediction$net.result[,1])
gain
barplot(gain$mean.resp / mean(valid.df$Phone_sale_1,na.rm = TRUE), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart with validation data",ylim = c(0,2), col="gold")

gain2 <- gains(train.df$Phone_sale_0, training.prediction$net.result[,2])
barplot(gain2$mean.resp / mean(train.df$Phone_sale_0,na.rm = TRUE), names.arg = gain2$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart with training data",ylim = c(0,2),col = "green")

hist(unlist(validation.prediction))
hist(validation.prediction$net.result, main = "Histogram of Predicted Normalized Prices", xlab = "Normalized Price")
hist(training.prediction$net.result, main = "Histogram of Predicted Normalized Prices",xlab = "Normalized Price with training data set")


training.class_1 = apply(training.prediction$net.result, 1, which.max) - 1

# training.class = round(training.prediction$net.result, digits=0) #四捨五入じゃないのかな...
confusionMatrix(as.factor(training.prediction$net.result), as.factor(train.df$Phone_sale_1))

dim(training.prediction$net.result)
length(training.prediction$net.result)
length(train.df$Phone_sale_1)


head(training.prediction$net.result)
head(train.df$Phone_sale_1)
head(round(training.prediction$net.result))



###############################
# b
###############################

v.pred <- validation.prediction$net.result
class(v.pred)#check the class
dim(v.pred)#converts matrix to vector
v.pred.vector <- as.vector(v.pred)
class(v.pred.vector)
dim(v.pred.vector) #vectors don't have a dimension
length(v.pred.vector)
head(rownames(v.pred)) 

library(forecast)
accuracy(v.pred.vector,valid.df$Phone_sale)


###############################
# c
###############################

# Hidden nodeを1で実行
nn_1 <- neuralnet(Phone_sale_1+Phone_sale_0~ +Topflight+Balance+
                  Balance+Qual_miles+cc1_miles.+cc2_miles.+cc3_miles.+Bonus_miles+Bonus_trans+Flight_miles_12mo+Flight_trans_12+Online_12+Email+Club_member+Any_cc_miles_12mo
                , data = train.df, linear.output = T, hidden = 1)
head(nn_1)
plot(nn_1)
nn_1.pred <- prediction(nn_1)
nn_1$result.matrix
nn_1.pred
validation.prediction1 <- compute(nn_1, valid.df[, c("Topflight", 
                                                  "Balance", 
                                                  "Qual_miles",   
                                                  "cc1_miles.",
                                                  "cc2_miles.",     
                                                  "cc3_miles.", 
                                                  "Bonus_miles",
                                                  "Bonus_trans", 
                                                  "Flight_miles_12mo",
                                                  "Flight_trans_12", 
                                                  "Online_12", "Email", 
                                                  "Club_member",  
                                                  "Any_cc_miles_12mo")])
training.prediction1 <- compute(nn_1, train.df[, c("Topflight", 
                                                "Balance", 
                                                "Qual_miles",   
                                                "cc1_miles.",
                                                "cc2_miles.",     
                                                "cc3_miles.", 
                                                "Bonus_miles",
                                                "Bonus_trans", 
                                                "Flight_miles_12mo",
                                                "Flight_trans_12", 
                                                "Online_12", "Email", 
                                                "Club_member",  
                                                "Any_cc_miles_12mo")])
v.pred1 <- validation.prediction1$net.result
class(v.pred1)#check the class
dim(v.pred1)#converts matrix to vector
v.pred.vector1 <- as.vector(v.pred1)
class(v.pred.vector1)
dim(v.pred.vector1) #vectors don't have a dimension
length(v.pred.vector1)
head(rownames(v.pred1)) 
# library(forecast)
accuracy(v.pred.vector1,valid.df$Phone_sale)
