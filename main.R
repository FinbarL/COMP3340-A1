library(igraph)
library(cccd)
install.packages("FNN")
library(FNN)

get_formatted_matrix <- function(dataset) {
  dataset_matrix <- as.matrix(dataset, nrow = nrow(data), ncol = ncol(data))
  #remove the target column
  dataset_matrix <- dataset_matrix[, colnames(dataset_matrix) != "Target"]
  #make the row names the year column
  formatted_matrix <- dataset_matrix[, -1]
  rownames(formatted_matrix) <- dataset_matrix[, 1]
  return(formatted_matrix)
}

calc_hamming_dist <- function(A, B) {
  return(sum(A != B))
}

calc_jaccard_measure <- function(A, B) {
  return(sum(A != B) / sum(A | B))
}

#calculate hamming row-wise (Question 1a)
calc_hamming_row <- function(input_matrix) {
  hamming_distance_matrix <- matrix(0, nrow = nrow(input_matrix), ncol = nrow(input_matrix), dimnames = list(rownames(input_matrix), rownames(input_matrix)))
  for (x in 1:nrow(input_matrix)) {
    for (y in 1:nrow(input_matrix)) {
      hamming_distance_matrix[x, y] <- calc_hamming_dist(input_matrix[x,], input_matrix[y,])
    }
  }
  return(hamming_distance_matrix)
}

#calculate jaccard row-wise (Question 1a)
calc_jaccard_row <- function(input_matrix) {
  jaccard_measure_matrix <- matrix(0, nrow = nrow(input_matrix), ncol = nrow(input_matrix), dimnames = list(rownames(input_matrix), rownames(input_matrix)))
  for (x in 1:nrow(input_matrix)) {
    for (y in 1:nrow(input_matrix)) {
      jaccard_measure_matrix[x, y] <- calc_jaccard_measure(input_matrix[x,], input_matrix[y,])
    }
  }
  return(jaccard_measure_matrix)
}

#calculate hamming column-wise (Question 1b)
calc_hamming_col <- function(input_matrix) {
  hamming_distance_matrix <- matrix(0, nrow = ncol(input_matrix), ncol = ncol(input_matrix), dimnames = list(colnames(input_matrix), colnames(input_matrix)))
  for (x in 1:ncol(input_matrix)) {
    for (y in 1:ncol(input_matrix)) {
      hamming_distance_matrix[x, y] <- calc_hamming_dist(input_matrix[, x], input_matrix[, y])
    }
  }
  return(hamming_distance_matrix)
}

#calculate jaccard column-wise (Question 1b)
calc_jaccard_col <- function(input_matrix) {
  jaccard_measure_matrix <- matrix(0, nrow = ncol(input_matrix), ncol = ncol(input_matrix), dimnames = list(colnames(input_matrix), colnames(input_matrix)))
  for (x in 1:ncol(input_matrix)) {
    for (y in 1:ncol(input_matrix)) {
      jaccard_measure_matrix[x, y] <- calc_jaccard_measure(input_matrix[, x], input_matrix[, y])
    }
  }
  return(jaccard_measure_matrix)
}

calc_mst <- function(dist_matrix) {
  graph <- graph.adjacency(as.matrix(dist_matrix), mode = "undirected", weighted = TRUE)
  mst <- mst(graph)
  E(mst)$weight <- round(E(mst)$weight, 3)
  return(mst)
}

calc_rng <- function(dist_matrix) {
  rng_output <- custom_rng(dist_matrix)
  print(rng_output)
  return(rng_output)
}

get_graph_layout <- function(graph) {
  layout <- layout_with_fr(graph)
  return(layout)
}

print_line <- function(text) {
  cat(text, "\n")
}

save_csv <- function(matrix) {
  filename <- deparse(substitute(matrix))
  saveTo <- paste0("files/", filename, ".csv")
  write.csv(matrix, file = saveTo)
}

save_graph <- function(graph, filename) {
  saveTo <- paste0("files/", filename, ".graphml")
  write_graph(graph, file = saveTo, format = "graphml")
}

plot_graph <- function(graph) {
  filename <- deparse(substitute(graph))
  layout <- layout_with_fr(graph)
  plot(graph, layout = layout, main = filename, edge.label = E(graph)$weight)
  save_graph(graph, filename)
  #graph
}

# Taken from the 'rng()' function in the cccd package
# was originally just using the package directly, but it was replacing my node labels with numbers
# Same code but modified to include rownames
# https://search.r-project.org/CRAN/refmans/cccd/html/rng.html
custom_rng <- function(x = NULL, dx = NULL, r = 1, method = NULL, usedeldir = TRUE, open = TRUE, k = NA,
                       algorithm = 'cover_tree')
{
  if (is.na(k)) {
    if (is.null(dx)) {
      if (is.null(x)) stop("One of x or dx must be given.")
      dx <- as.matrix(proxy::dist(x, method = method))
    } else {
      usedeldir <- FALSE
    }
    n <- nrow(dx)
    A <- matrix(0, nrow = n, ncol = n)

    # Set rownames and colnames
    if (!is.null(rownames(x))) {
      rownames(A) <- rownames(x)
      colnames(A) <- rownames(x)
    }


    if (is.vector(x)) x <- matrix(x, ncol = 1)
    if (usedeldir && ncol(x) == 2) {
      del <- deldir(x[, 1], x[, 2])
      for (edge in 1:nrow(del$delsgs)) {
        i <- del$delsgs[edge, 5]
        j <- del$delsgs[edge, 6]
        d <- min(apply(cbind(dx[i, -c(i, j)], dx[j, -c(i, j)]), 1, max))
        if (open) {
          if (r * dx[i, j] < d) {
            A[i, j] <- 1
            A[j, i] <- 1
          }
        } else {
          if (r * dx[i, j] <= d) {
            A[i, j] <- 1
            A[j, i] <- 1
          }
        }
      }
    }
    else {
      diag(dx) <- Inf
      for (i in 1:n) {
        for (j in setdiff(1:n, i)) {
          d <- min(apply(cbind(dx[i, -c(i, j)], dx[j, -c(i, j)]), 1, max))
          if (open) {
            if (r * dx[i, j] < d) {
              A[i, j] <- 1
              A[j, i] <- 1
            }
          } else {
            if (r * dx[i, j] <= d) {
              A[i, j] <- 1
              A[j, i] <- 1
            }
          }
        }
      }
    }
    diag(A) <- 0
    out <- graph_from_adjacency_matrix(A, mode = "undirected")
  } else {
    if (is.null(x)) stop("x must not be null")
    n <- nrow(x)
    k <- min(k, n - 1)
    dx <- get.knn(x, k = k, algorithm = algorithm)
    edges <- NULL
    for (i in 1:n) {
      i.indices <- dx$nn.index[i,]
      i.dists <- dx$nn.dist[i,]
      for (j in 1:k) {
        rd <- r * i.dists[j] / 2
        j.indices <- dx$nn.index[i.indices[j],]
        j.dists <- dx$nn.dist[i.indices[j],]
        rd <- r * i.dists[j]
        S <- setdiff(intersect(i.indices, j.indices), c(i, i.indices[j]))
        if (length(S) > 0) {
          d <- Inf
          for (si in S) {
            a <- which(i.indices == si)
            b <- which(j.indices == si)
            d <- min(d, max(i.dists[a], j.dists[b]))
          }
          if (rd < d) {
            edges <- cbind(edges, c(i, i.indices[j]))
          }
        }
      }
    }
    out <- simplify(make_graph(edges = edges, n = n, directed = FALSE))
    #Set rownames when k is provided
    if (!is.null(rownames(x))) {
      V(out)$name <- rownames(x)
    }
  }
  if (!is.null(x)) {
    out$layout <- x
  }
  out$r <- r
  out
}

dataset <- read.csv("USPresidency.csv")
formatted_matrix <- get_formatted_matrix(dataset)
print_line("Formatted Matrix:")
#formatted_matrix


print_line("Ex. 1.a. Hamming Distance Matrix (Row-wise):")
hamming_row <- calc_hamming_row(formatted_matrix)
save_csv(hamming_row)
#hamming_row

print_line("Ex. 1.b. Hamming Distance Matrix (Column-wise):")
hamming_col <- calc_hamming_col(formatted_matrix)
save_csv(hamming_col)
#hamming_col

print_line("Ex. 1.a. Jaccard Measure Matrix (Row-wise):")
jaccard_row <- calc_jaccard_row(formatted_matrix)
save_csv(jaccard_row)
#jaccard_row

print_line("Ex 1.b. Jaccard Measure Matrix (Column-wise):")
jaccard_col <- calc_jaccard_col(formatted_matrix)
save_csv(jaccard_col)
#jaccard_col

print_line("Ex 1.c. MST for Hamming Distance Matrix (Row-wise):")
hamming_mst_row <- calc_mst(hamming_row)
hamming_plot <- plot_graph(hamming_mst_row)

print_line("Ex 1.c. MST for Hamming Distance Matrix (Column-wise):")
hamming_mst_col <- calc_mst(hamming_col)
plot_graph(hamming_mst_col)

print_line("Ex 2. RNG for Hamming Distance Matrix (Row-wise):")
hamming_rng_row <- calc_rng(hamming_row)
plot_graph(hamming_rng_row)

print_line("Ex 2. RNG for Hamming Distance Matrix (Column-wise):")
hamming_rng_column <- calc_rng(hamming_col)
plot_graph(hamming_rng_column)

print_line("Ex 3. RNG for Jaccard Distance Matrix (Row-wise):")
jaccard_rng_row <- calc_rng(jaccard_row)
plot_graph(jaccard_rng_row)

print_line("Ex 3. RNG for Jaccard Distance Matrix (Column-wise):")
jaccard_rng_column <- calc_rng(jaccard_col)
plot_graph(jaccard_rng_column)

print_line("Ex 4. MST for Jaccard Measure (Column-wise):")
jaccard_mst_col <- calc_mst(jaccard_col)
plot_graph(jaccard_mst_col)

print_line("Ex 4. MST for Jaccard Measure (Row-wise):")
jaccard_mst_row <- calc_mst(jaccard_row)
plot_graph(jaccard_mst_row)

print_line("Ex. 5 Hamming Distance Matrix, k-NN graph with k=2 (row-wise)")
hamming_knn_row <- custom_rng(hamming_row, k = 2)
plot_graph(hamming_knn_row)

print_line("Ex. 6 Jaccard Distance Matrix, k-NN graph with k=2 (column-wise)")
jaccard_knn_row <- custom_rng(jaccard_row, k = 2)
plot_graph(jaccard_knn_row)

