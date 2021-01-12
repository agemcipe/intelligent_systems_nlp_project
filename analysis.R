
library(here)
source(here("utils.R"))

########################## loading data file  ##########################
unzip_data("arxiv_archive.zip")

json_ls <- get_json_ls_from_file("head_arxiv.json")

####################### 2 most similar analysis ########################

tdm <- get_tdm_from_json_ls(json_ls[1:1000])

# remove terms that only occur in a single document
removeSparseTerms(tdm, sparse = (1 - 1 / (ncol(tdm))))

# The calculation of the cosine similarity matrix is
# taken from https://stackoverflow.com/questions/29750519/r-calculate-cosine-distance-from-a-term-document-matrix-with-tm-and-proxy

cosine_dist_mat <- (
    slam::crossprod_simple_triplet_matrix(tdm)
    / (sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))
)

most_similar <- which(
    cosine_dist_mat == max(
        cosine_dist_mat[which(cosine_dist_mat < 0.9999)]
    ),
    arr.ind = TRUE
)

print_doc_infos(row.names(most_similar))

m <- melt(as.matrix(tdm[, c(most_similar[1:2])]), value.name = "tf_idf")
m <- m[m$"tf_idf" > 0.01, ]
m$Terms <- droplevels(m$Terms)

ggplot(m, aes(
    x = as.factor(Docs),
    y = reorder(Terms, desc(Terms)),
    fill = tf_idf
)) +
    geom_tile(colour = "white") +
    scale_fill_gradient(high = "#FF0000", low = "#FFFFFF") +
    ylab("Terms") +
    xlab("Docs") +
    theme(panel.background = element_blank())

########################## random document analysis ##########################

tdm <- get_tdm_from_json_ls(json_ls)
# remove terms that only occur in a single document
removeSparseTerms(tdm, sparse = (1 - 1 / ncol(tdm)))

##### Pick a random doc_id
doc_id <- 57113

num_of_docs_to_show <- 3

most_similar <- find_similar_papers(doc_id, tdm)

print_doc_infos(most_similar$ix[1:(num_of_docs_to_show + 1)])

m <- melt(
    as.matrix(
        tdm[, c(most_similar$ix[1:(num_of_docs_to_show + 1)])]
    ),
    value.name = "tf_idf"
)
m <- m[m$"tf_idf" > 0.1, ]
m$Terms <- droplevels(m$Terms)

ggplot(m, aes(
    x = as.factor(Docs),
    y = reorder(Terms, desc(Terms)),
    fill = tf_idf
)) +
    geom_tile(colour = "white") +
    scale_fill_gradient(high = "#FF0000", low = "#FFFFFF") +
    ylab("Terms") +
    xlab("Docs") +
    theme(panel.background = element_blank())