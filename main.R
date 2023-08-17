# Import Packages
library(igraph)
# Import the CSV Dataset
dataset <- read.csv("USPresidency.csv")
dataset_matrix <- as.matrix(dataset, nrow= nrow(data), ncol=ncol(data))
#remove the target column
dataset_matrix <- dataset_matrix[,colnames(dataset_matrix)!="Target"]

#make the row names the year column
formatted_matrix <- dataset_matrix[,-1]
rownames(formatted_matrix) <- dataset_matrix[,1]



calc_hamming_dist <- function(A,B){
    return(sum(A != B))
}
calc_jaccard_measure <- function(A,B){
    return(sum(A != B)/sum(A | B))
}

#calculate hamming row-wise (Question 1a)
calc_hamming_row <- function(input_matrix){
    hamming_distance_matrix <- matrix(0, nrow=nrow(input_matrix), ncol=nrow(input_matrix), dimnames = list(rownames(input_matrix), rownames(input_matrix)))
    for(x in 1:nrow(input_matrix)){
        for(y in 1:nrow(input_matrix)){
            hamming_distance_matrix[x,y] <- calc_hamming_dist(input_matrix[x,], input_matrix[y,])
        }
    }
    return (hamming_distance_matrix)
}
#calculate jaccard row-wise (Question 1a)
calc_jaccard_row <- function(input_matrix){
    jaccard_measure_matrix <- matrix(0, nrow=nrow(input_matrix), ncol=nrow(input_matrix), dimnames = list(rownames(input_matrix), rownames(input_matrix)))
    for(x in 1:nrow(input_matrix)){
        for(y in 1:nrow(input_matrix)){
            jaccard_measure_matrix[x,y] <- calc_jaccard_measure(input_matrix[x,], input_matrix[y,])
        }
    }
    return (jaccard_measure_matrix)
}
#calculate hamming column-wise (Question 1b)
calc_hamming_col <- function(input_matrix){
    hamming_distance_matrix <- matrix(0, nrow=ncol(input_matrix), ncol=ncol(input_matrix), dimnames = list(colnames(input_matrix), colnames(input_matrix)))
    for(x in 1:ncol(input_matrix)){
        for(y in 1:ncol(input_matrix)){
            hamming_distance_matrix[x,y] <- calc_hamming_dist(input_matrix[,x], input_matrix[,y])
        }
    }
    return (hamming_distance_matrix)
}
#calculate jaccard column-wise (Question 1b)
calc_jaccard_col <- function(input_matrix){
    jaccard_measure_matrix <- matrix(0, nrow=ncol(input_matrix), ncol=ncol(input_matrix), dimnames = list(colnames(input_matrix), colnames(input_matrix)))
    for(x in 1:ncol(input_matrix)){
        for(y in 1:ncol(input_matrix)){
            jaccard_measure_matrix[x,y] <- calc_jaccard_measure(input_matrix[,x], input_matrix[,y])
        }
    }
    return (jaccard_measure_matrix)
}
cat("Formatted Matrix:\n")
formatted_matrix
cat("Hamming Distance Matrix (Row-wise):\n")
calc_hamming_row(formatted_matrix)
cat("Hamming Distance Matrix (Column-wise):\n")
calc_hamming_col(formatted_matrix)
cat("Jaccard Measure Matrix (Row-wise):\n")
calc_jaccard_row(formatted_matrix)
cat("Jaccard Measure Matrix (Column-wise):\n")
calc_jaccard_col(formatted_matrix)
