# Recitation 6
# Text analytics and time series 

# Outline: 
# 1. Text analytics for analyzing Donald's tweets. 
# 2. Time series for predicting daily sales volume for drugstore chain. 

############################################################
# 1. Text analytics for analyzing Donald's tweets. 
############################################################

# Recall we want to predict if a tweet was posted from an
# Android (Trump) or an Iphone (Trump's staff). 

# Install/load the required packages. 
# install.packages("tm")
# install.packages("SnowballC")
library(tm) # Will use to create corpus and modify text therein.
library(SnowballC) # Will use for "stemming." 
library(rpart) # Will use to construct a CART model.
library(rpart.plot) # Will use to plot CART tree.

# Load the dataset (trump tweets posted between 6/1/15 and 3/1/17).
setwd("/Users/yukako/WorkSpace/LearnML/15_071_AnalyticsEdge")
tweets = read.csv("data/trump_tweets.csv", stringsAsFactors = FALSE)

# Let us recall the structure of the dataset.
str(tweets)
# Note that if TrumpWrote = 1 in an observation, then the 
# corresponding tweet was posted from an Android. 

# Let us also see how many tweets Donald posted.   
table(tweets$TrumpWrote)

# Next we will manipulate the text in Donald's tweets. To this end, 
# we will create a "corpus." 
corpus = Corpus(VectorSource(tweets$text))

# The tweets in the corpus are called "documents."
corpus[[1]]
strwrap(corpus[[1]])

# Let us start processing the text in the corpus! Here is a 
# summary of how we shall process the text.  
# 1. Change all the text to lower case.  
# 2. Remove stop words and particular words. 
# 3. "Stem" the documents. 
# 4. Remove infrequent words. 
# 5. Create new data frame that contains word counts. 

# 1. Let's change all the text to lower case. 
corpus = tm_map(corpus, tolower)
# The function tm_map applies an operation to every document in the 
# corpus. In this case, the operation is 'tolower" (i.e. to lowercase). 

# Let us check:
strwrap(corpus[[1]])

# Sometimes 'tolower' does strange stuff (?) to the documents. 
# We run the next command to fix the strange stuff. 
if (!("PlainTextDocument" %in% class(corpus[[1]]))) 
  corpus = tm_map(corpus, PlainTextDocument)

# 2. Let us remove some words. First, we remove stop words:  
corpus = tm_map(corpus, removeWords, stopwords("english"))
# stopwords("english") is a dataframe that constains a list of 
# stop words. Let us look at the first ten stop words. 
stopwords("english")[1:10]

# Checking again:  
strwrap(corpus[[1]])

# Next, we remove the two particular words: realdonaldtrump, donaldtrump. 
corpus = tm_map(corpus, removeWords, c("realdonaldtrump", "donaldtrump"))

# 3. Now we stem our documents. Recall that this corresponding to 
# removing the parts of words that are in some sense not 
# necessary (e.g. 'ing' and 'ed'). 
corpus = tm_map(corpus, stemDocument)
# We have: 
strwrap(corpus[[1]])

# 4. Let us "sparsify" the corpus and remove infrequent words. 
# First, we calculate the frequency of each words over all tweets. 
frequencies = DocumentTermMatrix(corpus)
frequencies
# Let us get a feel for what words occur the most. Words that appear 
# at least 200 times: 
findFreqTerms(frequencies, lowfreq=200)
# Words that appear at least 100 times: 
findFreqTerms(frequencies, lowfreq=100)
# Let us only keep words that appear in at least 1% of the tweets. We 
# create a list of these words as follows. 
sparse = removeSparseTerms(frequencies, 0.99)
sparse
# We now have 155 terms instead of 12790. 

# 5. We first create a new data frame. Each variable corresponds 
# to one of the 155 words, and each row corresponds to one of the tweets.
document_terms = as.data.frame(as.matrix(sparse))
str(document_terms)
# Lastly, we create a new column for the dependent variable: 
document_terms$TrumpWrote = tweets$TrumpWrote

# We have processed our data! Let us briefly construct a CART model. 

# Training and test set.
split1 = (tweets$created_at < "2016-06-01")
split2 = (tweets$created_at >= "2016-06-01")
train = document_terms[split1,]
test = document_terms[split2,]

# Constructing and plotting the CART model.  
cart = rpart(TrumpWrote ~ ., data=train, method="class", cp = .003)
prp(cart)

# We can calculate the model's TPR and FPR just like we learned 
# in the CART recitation.  

############################################################
# 2. Time series for predicting daily sales volume for drugstore chain. 
############################################################

# Recall we want to predict daily sales volume for a
# store in the Rossmann drug store chain (second largest drug store 
# chain in Germany). 

# Let us load the data and recall its structure. 
ross_sales <- read.csv("rossmann_sales.csv")
str(ross_sales)
# Note that Date is currently a factor variable. We will convert Date 
# to a Date variable. This will allows us to properly construct a
# testing and training set with date values. 
ross_sales$Date <- as.Date(strptime(ross_sales$Date, "%Y-%m-%d"))
str(ross_sales)

# We will construct three autoregressive time series models: 
# 1. Simple model with one independent lag variable.
# 2. Model with weekdays and months. 
# 3. Model with weekdays, months, promotions, and holidays. 

# We need to create a lag variable for Sales. We can create a lag variable 
# called SalesYesterday as follows. 
ross_sales$SalesYesterday = c(NA, head(ross_sales$Sales, -1))
# Let us take a look. 
str(ross_sales)

# We are done creating variables! Next, we construct our training 
# and test sets.
salesTrain = subset(ross_sales, Date < "2015-01-01")
salesTest = subset(ross_sales, Date >= "2015-01-01")

# Finally, we use linear regression to construct our three models. 

# Model with one lag variable:
model1 = lm(Sales~ SalesYesterday, data=salesTrain)
summary(model1)

# Model with weekdays and months: 
model2 = lm(Sales~ SalesYesterday + DayOfWeek + Month, data=salesTrain)
summary(model2)

# Model with weekdays, months, promotions, and holidays:
model3 = lm(Sales~ SalesYesterday + DayOfWeek + Month
                      + Promo + SchoolHoliday, data=salesTrain)
summary(model3)

