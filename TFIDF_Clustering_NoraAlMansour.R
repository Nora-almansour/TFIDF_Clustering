# START : 

#_____________________________________________________
# 1. Loading Req Library :
#_____________________________________________________
library("tm")
#library("tmcn")
library("rJava")
#library("Rwordseg")
library("SnowballC")
library("slam")
library("ggplot2")
library("qdap")
library("wordcloud")
require("RColorBrewer")

#   Warning  "Package 'xxx' is not available for R version 'x.y.z' " , Solving:
# ~     http://stackoverflow.com/questions/25721884/how-should-i-deal-with-package-xxx-is-not-available-for-r-version-x-y-z-wa
#   Warning in install.packages : package ‘quantedaData’ is not available (for R version 3.3.1) , Solving: 
# ~     type in R Console:  ' devtools::install_github("kbenoit/quantedaData") ' 

# ****************************************
#I like  the following feature in R:
#demo( laibName) and example(funcName from that laibName) : Run examples and see how the function works
# Great Feature that helps beginner in R likes me :) 
# ****************************************

#_____________________________________________________
# 2.Copy data files from computer into R as a corups:
#_____________________________________________________

# - 2.1 Reading location of texts data input 
cname <- file.path("~", "Desktop", "texts") #Path for input location on my computer 
dir(cname)   # check texts have loaded. 


#................. Starting- NOTE ........................................................................
#From (tm document in R) ftp://cran.r-project.org/pub/R/web/packages/tm/vignettes/tm.pdf :
# "the corpus constructor, x must be a Source object which abstracts the input location. tm provides a
#set of predefined sources, e.g., DirSource, VectorSource, or DataframeSource, which handle a directory"
#................. END- NOTE ............................................................................

# - 2.2 Load step 2.1 into R as a corups: ( Based on NOTE): 
corpus  <-Corpus(DirSource(cname), readerControl = list(blank.lines.skip=TRUE));
summary(corpus) # To see a brief description of the corpus.
#_____________________________________________________
#3. Pre-Processing : 
#_____________________________________________________

#First thing to do is to normalize the texts, which means to remove punctuation, cases, and numbers. These are called “transformations” in R-
#The syntax for using the transformations is:
#The tm_map function allows you to perform the same transformations on all of your texts at once.

my.corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords,stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument, language="english") #Stemming a Document: This will ensure that different forms of the word are converted to the same form

#_____________________________________________________
# 4 : TF-IDF 
#_____________________________________________________

#tfidf -- norlization:
 dtm <- DocumentTermMatrix(corpus  , #Courps name  #TF + TFIDF 
                           control = list(weighting =
                                             function(x)
                                              weightTfIdf(x, normalize = TRUE),
                                           stopwords = TRUE))
 inspect(dtm)


#It is finds all words that appear at least twice in any document
#findFreqTerms(dtm , 2)

#_____________________________________________________
# 5.Similarity
#_____________________________________________________

# tdm_Similaruty <- as.matrix(tdm)
# require(proxy)
# cosine_dist_mat <- as.matrix(dist(t(tdm_Similaruty), method = "cosine"))
# #....I remove the diagonal of my cosine distance matrix (since I am not interested in the distance between a document and itself) and compute the average distance between each document and the other 19 document of the corpus
# #diag(cosine_dist_mat) <- NA
# cosine_dist <- apply(cosine_dist_mat, 2, mean, na.rm=TRUE)
# cosine_dist
# summary(cosine_dist)
# cosine_dist_SCORE<-as.matrix(cosine_dist)
#........................................     

# sorting 
# apply(dtm, 1, function(x) {
#   x2 <- sort(x, TRUE)
#   x2[x2 >= x2[1]]
# })

#_____________________________________________________
# 6.Clustering 
#_____________________________________________________
#I learned that from: https://cran.r-project.org/web/packages/fastcluster/fastcluster.pdf
#and http://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html

# A ) 
# my.df <- as.data.frame(inspect(dtm))
# my.df.scale <- scale(my.df)
# d <- dist(my.df.scale,method="euclidean") # distance matrix
# fit <- hclust(d, method="ward.D") # method that merge each pair of document , go back to paper , section IV.	Approach.
# groups <- cutree(fit, k=27)
# plot(fit)
# rect.hclust(fit, k=3, border="red")

# B )
my.df <- as.data.frame(inspect(dtm))
my.df.scale <- scale(my.df)
d <- dist(my.df.scale,method="manhattan")  # distance matrix
fit <- hclust(d, method="ward.D2") # method that merge each pair of document , go back to paper , section IV.	Approach.
groups <- cutree(fit, k=28)
plot(fit)
rect.hclust(fit, k=3, border="red")



# TREE: 
#pltree (rect.hclust(fit, k=2, border="red"))
#dagn  <- as.dendrogram(as.hclust(agn))
# dagn2 <- as.dendrogram(as.hclust(agn), hang = 0.2)
# op <- par(mar = par("mar") + c(0,0,0, 2)) # more space to the right
# plot(dagn2, horiz = TRUE)
# plot(dagn, horiz = TRUE, center = TRUE,
#      nodePar = list(lab.cex = 0.6, lab.col = "forest green", pch = NA),
#      main = deparse(agn$call))
# par(op)


# C )
# my.df <- as.data.frame(inspect(dtm))
# my.df.scale <- scale(my.df)
# d <- dist(my.df.scale,method="euclidean")
# fit <- hclust(d, method="ward.D2")
# groups <- cutree(fit, k=29)
# plot(fit)
# rect.hclust(fit, k=3, border="red")

#_____________________________________________________
#7. WordCloud of Frq 
#_____________________________________________________

pal2 <- brewer.pal(8,"Dark2")
wordcloud(corpus, scale=c(8,.2),min.freq=4, max.words=3, random.order=FALSE, rot.per=.15, colors=pal2)
#wordcloud(corpus, max.words = 10, random.order = FALSE, colors=pal2)


#END ! 




#__________________________________________________________________________________________________________
# No Neeed the code from Line , keep it in case I'll need it later 
#__________________________________________________________________________________________________________
# # Ward Hierarchical Clustering* 2  ..... Per at k =1 
# d <- dist(my.df.scale, method = "manhattan") # distance matrix
# fit2 <- hclust(d, method="ward.D") 
# plot(fit2) # display dendogram
# groups <- cutree(fit2, k=26) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters 
# rect.hclust(fit2, k=25, border="red")
# 
# # Ward Hierarchical Clustering* 2  ..... Per at k =5 
#  d <- dist(my.df.scale, method = "manhattan") # distance matrix
# fit2 <- hclust(d, method="ward.D2") 
# plot(fit2) # display dendogram
# groups <- cutree(fit2, k=26) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters 
# rect.hclust(fit2, k=25, border="red")
# 
# # Ward Hierarchical Clustering* 2  ..... Per at k =10 
# d <- dist(my.df.scale, method = "manhattan") # distance matrix
# fit2 <- hclust(d, method="ward.D2") 
# plot(fit2) # display dendogram
# groups <- cutree(fit2, k=26) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters 
# rect.hclust(fit2, k=25, border="red")
# 
# # Ward Hierarchical Clustering* 2  ..... Per at k =10 .. NEW
# d <- dist(my.df.scale, method = "euclidean") # distance matrix
# fit2 <- hclust(d, method="ward.D") 
# plot(fit2) # display dendogram
# groups <- cutree(fit2, k=10) # cut tree into 5 clusters
# # draw dendogram with red borders around the 5 clusters 
# rect.hclust(fit2, k=5, border="red")

#.... job title with CV's:
#query <- "web"
#term.doc.matrix.stm <- TermDocumentMatrix(corpus)
#my.docs <- VectorSource(c(corpus, query))
#my.docs$Names <- c(names(corpus), "query")
#my.corpus <- Corpus(my.docs)
#my.corpus
#colnames(term.doc.matrix.stm) <- c(names(doc.list), "query") #add Query col. 
#inspect(term.doc.matrix.stm)


#ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
#geom_bar(stat="identity") + coord_flip() + 
#xlab("Terms") + ylab("Frequency") +
#ggtitle("Term frequencies")


#freq=rowSums(as.matrix(tdm))
#head(freq,3)
#tail(freq,3)
#plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")
#tail(sort(freq),n=10)


#high.freq=tail(sort(freq),n=5)
#hfp.df=as.data.frame(sort(high.freq))
#hfp.df$names <- rownames(hfp.df) 

#ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
#geom_bar(stat="identity") + coord_flip() + 
#xlab("Terms") + ylab("Frequency") +
#ggtitle("Term frequencies")

#While silhouette() is intrinsic to the partition clusterings,Compute or Extract Silhouette Information from Clustering
#https://cran.r-project.org/web/packages/cluster/cluster.pdf
# pr4 <- pam(my.df.scale, 5)
# str(si <- silhouette(pr4))
# (ssi <- summary(si))
# plot(si) # silhouette plot
# plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring
# table(str)

