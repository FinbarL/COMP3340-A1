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

cat("Ex. 1.a. Hamming Distance Matrix (Row-wise):\n")
hamming_row <- calc_hamming_row(formatted_matrix)
write.csv(hamming_row, file = "hamming_row.csv")
hamming_row

cat("Ex. 1.b. Hamming Distance Matrix (Column-wise):\n")
hamming_col <- calc_hamming_col(formatted_matrix)
write.csv(hamming_col, file = "hamming_col.csv")
hamming_col

cat("Ex. 1.a. Jaccard Measure Matrix (Row-wise):\n")
jaccard_row <- calc_jaccard_row(formatted_matrix)
write.csv(jaccard_row, file="jaccard_row.csv")
jaccard_row

cat("Ex 1.b. Jaccard Measure Matrix (Column-wise):\n")
jaccard_col <- calc_jaccard_col(formatted_matrix)
write.csv(jaccard_col, file="jaccard_col.csv")
jaccard_col

cat("Ex 1.c. MST for Hamming Distance Matrix (Row-wise):\n")
g_hamming_row <- graph_from_adjacency_matrix(jaccard_row)
hamming_row_mst <- mst(g_hamming_row)
hamming_row_mst

cat("Ex 1.c. MST for Hamming Distance Matrix (Column-wise):\n")
g_hamming_col <- graph_from_adjacency_matrix(jaccard_row)
hamming_col_mst <- mst(g_hamming_col)
hamming_col_mst
