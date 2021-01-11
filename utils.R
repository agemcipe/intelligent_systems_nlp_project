########################## packages ##########################

library(rjson)
library(here)
library(tm)
library(slam)
library(ggplot2)
library(reshape2)
library(SnowballC)
library(dplyr)

########################## functions ##########################

unzip_data <- function(file_name) {
    # TODO: unzip file
}


get_json_ls_from_file <- function(file_name, num_lines = -1L) {
    json_file <- here("input", file_name)

    # verify file exists
    if (!file.exists(json_file)) {
        print("Input json file does not exist")
        quit(status = 1)
    }

    # read file
    print(paste("Reading file:", json_file))
    file_con <- file(json_file)
    lines <- readLines(con = file_con, n = num_lines)
    close(file_con)

    num_docs <- length(lines)

    print(paste("Number of Lines read:", num_docs))

    json_ls <- vector("list", length = num_docs)
    for (i in 1:num_docs) {
        json_ls[[i]] <- fromJSON(json_str = lines[i])
    }
    return(json_ls)
}

get_tdm_from_json_ls <- function(json_ls) {
    # create corpus
    abstract_vec <- vector("character", length = length(json_ls))
    for (i in 1:length(json_ls)) {
        abstract_vec[[i]] <- json_ls[[i]]$abstract
    }

    corpus <- tm::Corpus(x = tm::VectorSource(
        abstract_vec
    ))

    print(paste("Creating TDM from", length(json_ls), "Documents"))
    # Create TermDocumentMatrix (Vector Word Model)
    tdm <- tm::TermDocumentMatrix(
        corpus,
        control = list(
            weighting = tm::weightTfIdf,
            removePunctuation = TRUE,
            stopwords = TRUE,
            stripWhitespace = TRUE,
            stemming = TRUE,
            removeNumbers = TRUE
        )
    )

    return(tdm)
}


find_similar_papers <- function(doc_index, tdm) {
    # check that doc_index exists

    # calculate cosine similarity vector
    cosine_similarity <- (
        slam::crossprod_simple_triplet_matrix(x = tdm, y = tdm[, doc_index])
        / (sqrt((col_sums(tdm^2) %*% t(col_sums(tdm[, doc_index]^2)))))
    )

    cosine_similarity <- sort.int(
        cosine_similarity[, 1],
        index.return = TRUE,
        decreasing = TRUE
    )
}

print_doc_info <- function(doc_id) {
    cat(
        paste(
            "   doc_id:", doc_id, "\n",
            "   Title:", gsub(
                "\n",
                "\n         ",
                json_ls[[doc_id]]$title
            ), "\n",
            " Authors:", json_ls[[doc_id]]$authors, "\n",
            "Category:", json_ls[[doc_id]]$categories, "\n",
            " Journal:", json_ls[[doc_id]]$"journal-ref", "\n"
        )
    )
}


print_doc_infos <- function(doc_ids) {
    for (doc_id in doc_ids) {
        print_doc_info(as.numeric(doc_id))
        cat("-----------------------------\n")
    }
}