
###############################
# Text Analysis (Recitation 6のほぼパクリ)
###############################
library(tm) # Will use to create corpus and modify text therein.
library(SnowballC) # Will use for "stemming." 
library(rpart) # Will use to construct a CART model.
library(rpart.plot) # Will use to plot CART tree.

# Next we will manipulate the text in Donald's tweets. To this end, 
# we will create a "corpus." 

corpus = Corpus(VectorSource(video$description))
# タグで同じことやってもいい
# corpus = Corpus(VectorSource(video$tags))


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
strwrap(corpus[[1]])
# Sometimes 'tolower' does strange stuff (?) to the documents. 
# We run the next command to fix the strange stuff. 
if (!("PlainTextDocument" %in% class(corpus[[1]]))) 
  corpus = tm_map(corpus, PlainTextDocument)

# 2. Let us remove some words. First, we remove stop words:  
corpus = tm_map(corpus, removeWords, stopwords("english"))
# stopwords("english") is a dataframe that constains a list of stop words.
stopwords("english")[1:10]
strwrap(corpus[[1]])
# Next, we remove the two particular words: 
corpus = tm_map(corpus, removeWords, c("youtube", "video", "channel", "that")) # ほかあれば

# 3. Now we stem our documents. Recall that this corresponding to 
# removing the parts of words that are in some sense not necessary (e.g. 'ing' and 'ed'). 
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]])

# 4. Let us "sparsify" the corpus and remove infrequent words. 
# First, we calculate the frequency of each words over all tweets. 
frequencies = DocumentTermMatrix(corpus)
frequencies
# Let us get a feel for what words occur the most. Words that appear at least 200 times: 
findFreqTerms(frequencies, lowfreq=1000)
# Words that appear at least 100 times: 
findFreqTerms(frequencies, lowfreq=500)
# Let us only keep words that appear in at least 1% of the tweets. We 
# create a list of these words as follows. 
sparse = removeSparseTerms(frequencies, 0.99)
sparse
dim(sparse)
# We now have 155 terms instead of 12790. 

# 5. We first create a new data frame. Each variable corresponds 
# to one of the 155 words, and each row corresponds to one of the tweets.
document_terms = as.data.frame(as.matrix(sparse))
str(document_terms); dim(document_terms)
head(document_terms)
# Lastly, we create a new column for the dependent variable: 
# 目的変数はsuperPop(めちゃ人気かどうか)
document_terms$superPop = video$superPop
head(document_terms)

# We have processed our data! Let us briefly construct a CART model. 
# Training and test set.
# Reciataionでは日付を元にsplitしてたけど、今回は単にランダムに6:4でtrain:test
split1 = sample(row.names(document_terms), 0.6*nrow(document_terms))
split2 = setdiff(row.names(document_terms), split1)
train = document_terms[split1,]
test = document_terms[split2,]
head(train)

# Constructing and plotting the CART model.  
cart = rpart(superPop ~ ., data=train, method="class", cp = .003)
prp(cart, cex=0.6)
# We can calculate the model's TPR and FPR just like we learned 
# in the CART recitation.  