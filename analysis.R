
library(here)

source(here("utils.R"))
########################## loading data file  ##########################

json_ls <- get_json_ls_from_file("head_arxiv.json")

########################## most similar analysis ##########################

tdm <- get_tdm_from_json_ls(json_ls[1:1000])

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

########################## random document analysis ##########################

tdm <- get_tdm_from_json_ls(json_ls)

doc_id <- 57113
doc_id <- 1
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








#######################################################
# let's create a scatter plot of the similar documents
tdm_no_sparse <- tm::removeSparseTerms(tdm, 0.90)

# tdm_no_sparse <- tdm

inspect(tdm_no_sparse[, c(result_ls[[doc_id]]$ix[1:5])])

tdm_no_sparse_matrix <- reshape2::melt(
    as.matrix(tdm_no_sparse[, c(result_ls[[doc_id]]$ix[1:5])]),
    value.name = "count"
)

head(tdm_no_sparse_matrix)

df <- as.data.frame(tdm_no_sparse_matrix)
grouped_df <- aggregate(count ~ Terms, df, sum)

df_to_plot <- df[!(df$Terms %in% grouped_df[which(grouped_df$count == 0), ]$Terms), ]
df_to_plot$Terms <- droplevels(df_to_plot$Terms)

df_to_plot$Terms <- factor(df_to_plot$Terms, levels = sort(levels(df_to_plot$Terms)))

ggplot(df_to_plot, aes(
    x = as.factor(Docs),
    y = reorder(Terms, desc(Terms)),
    fill = count
)) +
    geom_tile(colour = "white") +
    scale_fill_gradient(high = "#FF0000", low = "#FFFFFF") +
    ylab("Terms") +
    xlab("Docs") +
    theme(panel.background = element_blank())

################### The two most similar documents ###################



# create a list with each entry containing the sorted similar documents
result_ls <- vector("list", num_docs)
for (i in 1:num_docs) {
    result_ls[[i]] <- sort.int(cosine_dist_mat[, i], index.return = TRUE)
}


#### finding most similar papers based on quantile

which(
    cosine_dist_mat <= quantile(
        cosine_dist_mat[which(cosine_dist_mat < 0.999)],
        probs = c(.00001)
    ) & cosine_dist_mat < 0.999,
    arr.ind = TRUE
)