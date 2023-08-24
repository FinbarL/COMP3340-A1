# Import Packages
library(igraph)
library(cccd)

get_formatted_matrix <- function(dataset) {
    dataset_matrix <- as.matrix(dataset, nrow= nrow(data), ncol=ncol(data))
    #remove the target column
    dataset_matrix <- dataset_matrix[,colnames(dataset_matrix)!="Target"]
    #make the row names the year column
    formatted_matrix <- dataset_matrix[,-1]
    rownames(formatted_matrix) <- dataset_matrix[,1]
    return(formatted_matrix)
}
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
calc_mst <- function(dist_matrix) {
    graph <- graph.adjacency(as.matrix(dist_matrix), mode = "undirected", weighted = TRUE)
    mst <- mst(graph)
    return(mst)
}
calc_rng <- function(dist_matrix) {
    rng_output <- rng(dist_matrix)
    rng_output
    return(rng_output)
}
get_graph_layout <- function(graph) {
    layout <- layout_with_fr(graph)
    return(layout)
}
print_line <- function(text) {
    cat(text, "\n")
}
save_graph <- function(graph) {
    filename <- deparse(substitute(graph))
    saveTo <- paste0("files/", filename, ".graphml")
    write_graph(graph, file=saveTo, format="graphml")
}
save_csv <- function(matrix) {
    filename <- deparse(substitute(matrix))
    saveTo <- paste0("files/", filename, ".csv")
    write.csv(matrix, file=saveTo)
}
plot_graph <- function(graph) {
    layout <- layout_with_fr(graph)
    V(graph)$label <- rownames(graph)
    plot(graph, layout = layout)
}

dataset <- read.csv("USPresidency.csv")
formatted_matrix <- get_formatted_matrix(dataset)
print_line("Formatted Matrix:")
formatted_matrix


print_line("Ex. 1.a. Hamming Distance Matrix (Row-wise):")
hamming_row <- calc_hamming_row(formatted_matrix)
save_csv(hamming_row)
hamming_row

print_line("Ex. 1.b. Hamming Distance Matrix (Column-wise):")
hamming_col <- calc_hamming_col(formatted_matrix)
save_csv(hamming_col)
hamming_col

print_line("Ex. 1.a. Jaccard Measure Matrix (Row-wise):")
jaccard_row <- calc_jaccard_row(formatted_matrix)
save_csv(jaccard_row)
jaccard_row

print_line("Ex 1.b. Jaccard Measure Matrix (Column-wise):")
jaccard_col <- calc_jaccard_col(formatted_matrix)
save_csv(jaccard_col)
jaccard_col

print_line("Ex 1.c. MST for Hamming Distance Matrix (Row-wise):")
hamming_mst_row <- calc_mst(hamming_row)
plot(hamming_mst_row, layout = get_graph_layout(hamming_mst_row),  main="MST for Hamming Distance Matrix (Row-wise)",)
save_graph(hamming_mst_row)
hamming_mst_row

print_line("Ex 1.c. MST for Hamming Distance Matrix (Column-wise):")
hamming_mst_col <- calc_mst(hamming_col)
plot(hamming_mst_col, layout = get_graph_layout(hamming_mst_col), main="MST for Hamming Distance Matrix (Column-wise)",)
save_graph(hamming_mst_col)
hamming_mst_col

print_line("Ex 2. RNG for Hamming Distance Matrix (Row-wise):")
hamming_rng_row <- calc_rng(hamming_row)
plot(hamming_rng_row, layout = get_graph_layout(hamming_rng_row), main="RNG for Hamming Distance Matrix (Row-wise)",)
save_graph(hamming_rng_row)
hamming_rng_row

print_line("Ex 2. RNG for Hamming Distance Matrix (Column-wise):")
hamming_rng_column <- calc_rng(hamming_col)
plot(hamming_rng_column, layout = get_graph_layout(hamming_rng_column), main="RNG for Hamming Distance Matrix (Column-wise)",)
save_graph(hamming_rng_column)
hamming_rng_column

print_line("Ex 3. RNG for Jaccard Distance Matrix (Row-wise):")
jaccard_rng_row <- calc_rng(jaccard_row)
plot(jaccard_rng_row, layout = get_graph_layout(jaccard_rng_row), main="RNG for Jaccard Distance Matrix (Row-wise)",)
save_graph(jaccard_rng_row)
jaccard_rng_row

print_line("Ex 3. RNG for Jaccard Distance Matrix (Column-wise):")
jaccard_rng_column <- calc_rng(jaccard_col)
plot(jaccard_rng_column, layout = get_graph_layout(jaccard_rng_column),  main="RNG for Jaccard Distance Matrix (Column-wise)",)
save_graph(jaccard_rng_column)
jaccard_rng_column

print_line("Ex 4. MST for Jaccard Measure (Column-wise):")
jaccard_mst_col <- calc_mst(jaccard_col)
plot(jaccard_mst_col, layout = get_graph_layout(jaccard_rng_column),  main="MST for Jaccard Measure (Column-wise)",)
save_graph(jaccard_mst_col)
jaccard_mst_col

print_line("Ex 4. MST for Jaccard Measure (Row-wise):")
jaccard_mst_row <- calc_mst(jaccard_row)
plot(jaccard_mst_row, layout = get_graph_layout(jaccard_rng_column), main="MST for Jaccard Measure (Row-wise)",)
save_graph(jaccard_mst_row)
jaccard_mst_row


print_line("Ex. 5 Hamming Distance Matrix, k-NN graph with k=2 (row-wise)")
hamming_knn_row <- nng(hamming_row, k=2)
plot(hamming_knn_row, layout = get_graph_layout(jaccard_rng_column), main="k-NN graph with k=2 for Hamming Distance Matrix, (row-wise)")
save_graph(hamming_knn_row)
hamming_knn_row

print_line("Ex. 6 Hamming Distance Matrix, k-NN graph with k=2 (column-wise)")
hamming_knn_col <- nng(hamming_col, k=2)
plot(hamming_knn_col, layout = get_graph_layout(jaccard_rng_column), main="k-NN graph with k=2 for Hamming Distance Matrix, (column-wise)")
save_graph(hamming_knn_col)
hamming_knn_col

