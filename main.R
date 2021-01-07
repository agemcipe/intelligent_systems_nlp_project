# install.packages("rjson")
install.packages("here")
install.packages("tm")

# load required packages
library(rjson)
library(here)
library(tm)


# input file
json_file <- here("input", "head_arxiv.json")

# verify file exists
if (!file.exists(json_file)) {
    print("Input json file does not exist")
    quit(status = 1)
}

file_con <- file(json_file)
lines <- readLines(con = file_con)
close(file_con)

# create empty vector
vec <- vector(length = length(lines))
for (i in 1:length(lines)) {
    vec[i] <- fromJSON(json_str = lines[i])$abstract
}

# create corpus
corpus <- tm::SimpleCorpus(x = tm::VectorSource(vec))

# TODO: set id of corpus?
meta(corpus[[1]])

# clean corpus

# Create TermDocumentMatrix (Vector Word Model)
tdm <- tm::TermDocumentMatrix(
    corpus,
    control = list(
        removePunctuation = TRUE,
        stopwords = TRUE,
        stripWhitespace = TRUE
    )
)

inspect(tdm[1:100, 1:4])

# taken from https://stackoverflow.com/questions/29750519/r-calculate-cosine-distance-from-a-term-document-matrix-with-tm-and-proxy
cosine_dist_mat <- (
    1 - crossprod_simple_triplet_matrix(tdm)
    / (sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))
)

# to find minimum
which.min()