head(video)
colnames(video)
video_reg <- video[c(17,18,19,20,23,24,25,26,27)] # 必要に応じて色々変える
head(video_reg)
colnames(video_reg)

split1 = sample(row.names(video_reg), 0.6*nrow(video_reg))
split2 = setdiff(row.names(video_reg), split1)
train = video_reg[split1,]
test = video_reg[split2,]

# model <- glm(superPop~., family=binomial, data = train) # ロジスティック回帰
model <- lm(lviews~., data = train) # 普通の重回帰
summary(model)
