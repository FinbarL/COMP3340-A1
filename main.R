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
formatted_matrix


calc_hamming_dist <- function(A,B){
    return(sum(A != B))
}
calc_jaccard_measure <- function(A,B){
    return(sum(A != B)/sum(A | B))
}

#calculate hamming row-wise (Question 1a)
calc_hamming_row <- function(input_matrix){
    hamming_distance_matrix <- matrix(0, nrow=nrow(input_matrix), ncol=nrow(input_matrix), dimnames = list(rownames(input_matrix), rownames(input_matrix)))
    for(row in 1:nrow(input_matrix)){
        for(col in 1:nrow(input_matrix)){
            hamming_distance_matrix[row,col] <- sum(input_matrix[row,] != input_matrix[col,])
        }
    }
    return (hamming_distance_matrix)
}

calc_hamming_row(formatted_matrix)

