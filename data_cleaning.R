library(plyr)
library(stringr)
library(ggplot2)
library(sentiment)
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(igraph)
library(graph)
library(grid)
library(stringr)
library(rpart)

sandy4 = read.csv("D:/Project/sandydata6.csv",header = TRUE, stringsAsFactors = F)
sandy4_subset <- subset(sandy4, Language=='en')



# Clean tweets by replacing the unwanted text
sandy_clean = gsub("b'", "", sandy4_subset$Tweet)
sandy_clean = gsub('b"', "", sandy_clean)
sandy_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",sandy_clean)
sandy_clean = gsub("@\\w+","",sandy_clean)
sandy_clean = gsub("[[:punct:]]","",sandy_clean)
sandy_clean = gsub("[[:digit:]]","",sandy_clean)
sandy_clean = gsub("http\\w+","",sandy_clean)
sandy_clean = gsub("^\\s+|\\s+$", "",sandy_clean)
sandy_clean = gsub("[^\x20-\x7E]","",sandy_clean)

head(sandy_clean, n=10)
tail(sandy_clean, n=10)

#build a corpus and specify the source to be a character vector
sandy_Corpus = Corpus(VectorSource(sandy_clean))
sandy_Corpus[[4]]$content

#Transforming the text to lowercase
sandy_Corpus = tm_map(sandy_Corpus, content_transformer(tolower))
sandy_Corpus[[4]]$content
#Remove numbers
sandy_Corpus = tm_map(sandy_Corpus, removeNumbers)
sandy_Corpus[[4]]$content
#Remove punctuations
sandy_Corpus = tm_map(sandy_Corpus, removePunctuation)
sandy_Corpus[[4]]$content
#Remove Stopwords and unwanted words
sandy_Corpus = tm_map(sandy_Corpus, removeWords, c("sandy","hurricane","hurricanesandy","storm","us","going","w","go","getting","coming","people","take","ya","doesnt","well","preparing","duh","ok","okay","lol","gonna","xfxfxxaxfxfxxa","xfxfxx","hey","xexxc","Whatever","can", "come", "someone", "dont", "isnt", "like", "need", "now", "one", "thank", "thatleaves", "theteam", 
                                                   "just","really","im","dick","bitch","fuck", "youfor","will","that","oooooo", "still", "got", "get", "flight", "rt","whatever","didnt","lmao","wanna","made","cant","y","thats","xfxfxxb","pic","xfxfxxa","u",
                                                   "awesome" ,"youre","arent","havent","yes","oh","theres","saying","fuckyousandy","shit","wasnt" ,"whats","shes","know" ,"want","gets","also","yall","almost","ppl","let","says","say","ive",
                                                   "en","said","theyre","el","able","thru","damn" ,"o","esta","lets","aint","must","xfxfxxf","fuckin","gettin","til","till","da","dis","came","whos","hoe","niggas","ima","lmfao","fine","nigga",
                                                   "couldnt","bitches","happened","nd","hasnt","feels","eating","fucksandy","blew",stopwords("english")))
sandy_Corpus[1:10]$content
#Remove Stopwords
sandy_Corpus = tm_map(sandy_Corpus,removeWords, stopwords("english"))
sandy_Corpus[1:10]$content

#Remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
sandy_Corpus = tm_map(sandy_Corpus, content_transformer(removeURL))
sandy_Corpus[[4]]$content
#Remove NumPuncts
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","", x)
sandy_Corpus = tm_map(sandy_Corpus, content_transformer(removeNumPunct))
sandy_Corpus[[4]]$content

toSpace<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
sandy_Corpus<-tm_map(sandy_Corpus,toSpace,"Ã¢â¬Å~/|@|\\|")

#Remove Whitespace
sandy_Corpus = tm_map(sandy_Corpus, stripWhitespace)
sandy_Corpus[[4]]$content

#Remove stemmed words and replacing with original word -------------#Unsuccessful
sandy_Corpus_stemmed = tm_map(sandy_Corpus, stemDocument)
sandy_Corpus_stemmed[1:10]$content

#Document-Term Matrix: documents as the rows, terms/words as the columns, frequency of the term in the document.
# as the entries. Notice the dimension of the matrix

sandy_tdm = TermDocumentMatrix(sandy_Corpus, control = list(wordLengths = c(1, Inf),stopwords = TRUE))
sandy_dtm = DocumentTermMatrix(sandy_Corpus, control = list(wordLengths = c(1, Inf)))       #Full DocumentTermMatrix
sandy_dtm_limited = sandy_dtm[0:10000,0:50491]     #Limited DocumentTermMatrix
sandy_tdm_limited = sandy_tdm[0:10000,0:50491]     #Limited DocumentTermMatrix
 
inspect(sandy_dtm[0:10000,0:50491])

#inspect frequent words
freq.terms <- findFreqTerms(sandy_tdm_limited, lowfreq = 100)
term.freq <- rowSums(as.matrix(sandy_tdm_limited))
term.freq <- subset(term.freq, term.freq >= 750)
df <- data.frame(term = names(term.freq), freq = term.freq)


#plot the frequency with words
ggplot(df, aes(x=term , y=freq))

ggplot(df, aes(x=term , y=freq))+ geom_bar(stat = "identity")+
  xlab("Terms") + ylab("Count")+ coord_flip()+
  theme(axis.text=element_text(size=8))


#simple word cloud
findFreqTerms(sandy_dtm, 1000)
terms = colnames(sandy_dtm_limited)
freq = data.frame(sort(colSums(as.matrix(sandy_dtm_limited)),decreasing = TRUE))
freq_limited = freq[0:200,1]
#WordCloud
wordcloud(row.names(freq),freq[,1],min.freq=5, max.words = 50, colors = brewer.pal(8, "Dark2"))

score.sentiment <- function(sentence,pos.words, neg.words)
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
  sentence <- gsub('[[:punct:]]',"",sentence)
  sentence <- gsub('[[:cntrl:]]',"",sentence)
  sentence <- gsub('\\d+',"",sentence)
  sentence <- tolower(sentence)
  word.list <- str_split(sentence, '\\s+')
  words <- unlist(word.list)
  pos.matches <- match(words, pos.words)
  neg.matches <- match(words, neg.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- !is.na(neg.matches)
  score <- sum(pos.matches) - sum(neg.matches)
  return(score)
}, pos.words, neg.words, .progress=.progress)
  score.df <- data.frame(score = scores, text = sentences)
  return(score.df)
}

# Precompiled list of words with positive and negative meanings
# Source: http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
pos <- scan('D:/Project/Material/positive-words.txt', what='character', comment.char=';')
neg <- scan('D:/Project/Material/negative-words.txt', what='character', comment.char=';')
pos.words <- c(pos,"upgrade" )
neg.words <- c(neg,"wtf","duh")

analysis <- score.sentiment(sandy_clean,pos.words,neg.words)
table(analysis$score)

neutral  <- length(which(analysis$score == 0))
positive <- length(which(analysis$score  >  0))
negative <- length(which(analysis$score  <  0))
Sentiment <- c("Negative","Neutral","Positive")
Count <- c(negative, neutral, positive)
output <- as.data.frame(Sentiment, Count)

#Bar plot for Sentiment Analysis
qplot(Sentiment, Count, data = output, geom = 'histogram', fill = Sentiment,
      xlab = "Sentiment", ylab = "Count", main = "Sentiment Analysis")

ggplot(output, aes(x=Sentiment, y = Count, fill =  Sentiment)) + geom_bar(stat = "identity")
