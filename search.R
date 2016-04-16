# Make all document/data into a single list
doc.list <- as.list(df$data)

# Size of doc.list
object.size(doc.list)

#Calculate length of doc.list
N.docs <- length(doc.list)

#Naming the doc.list
names(doc.list) <- paste0("doc", c(1:N.docs))

#Enter the query
query <- "R introduction"

#Making VectorSource
my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")

#Finding the Corpus
my.corpus <- Corpus(my.docs)

#Applying Operation on my.corpus
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, stemDocument)
my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, PlainTextDocument)
my.corpus <- tm_map(my.corpus, stripWhitespace)

#Making my.corpus into TermDocumentMatrix 
term.doc.matrix.stm <- TermDocumentMatrix(my.corpus)
str(term.doc.matrix.stm)

#Convert into matrix
term.doc.matrix <- as.matrix(term.doc.matrix.stm)
str(term.doc.matrix)

#Calculate size of testing
cat("Dense matrix representation costs", object.size(term.doc.matrix), "bytes.\n",
"Simple triplet matrix representation costs", object.size(term.doc.matrix.stm),
"bytes.")

View(term.doc.matrix)

#Calculates 

get.tf.idf.weights <- function(tf.vec, df) {
# Computes tfidf weights from a term frequency vector and a document
# frequency scalar
weight = rep(0, length(tf.vec))
weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
weight
}


get.weights.per.term.vec <- function(tfidf.row) {
term.df <- sum(tfidf.row[1:N.docs] > 0)
tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
return(tf.idf.vec)
}


tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))


colnames(tfidf.matrix) <- colnames(term.doc.matrix)

#Checking Cos Angle 
angle <- seq(-pi, pi, by = pi/16)
plot(cos(angle) ~ angle, type = "b", xlab = "angle in radians", main = "Cosine similarity by angle")
tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
query.vector <- tfidf.matrix[, (N.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:N.docs]
doc.scores <- t(query.vector) %*% tfidf.matrix
results.df <- data.frame(doc = names(doc.list), score = t(doc.scores), text = unlist(doc.list))
results.df <- results.df[order(results.df$score, decreasing = TRUE), ]

#Top search results
top <- head(results.df)
