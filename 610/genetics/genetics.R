library(readr)

#Reading in Germline
germline <- read_file("germline.txt")
germline <- unlist(strsplit(germline,""))

#Reading in Sequences and separating by environments
clones <- read.csv("sequences.csv")
clones_a <- clones[[1]][clones[[2]] == "a"]
clones_a <- lapply(clones_a, function(n_seq) unlist(strsplit(toString(n_seq),"")))
clones_b <- clones[[1]][clones[[2]] == "b"]
clones_b <- lapply(clones_b, function(n_seq) unlist(strsplit(toString(n_seq),"")))

#Creating the probability matrix
transition_nums <- c(0.93, 0.05, 0.01, 0.01,
                     0.05, 0.93, 0.01, 0.01,
                     0.01, 0.01, 0.93, 0.05,
                     0.01, 0.01, 0.05, 0.93)
transition_matrix <- matrix(transition_nums, nrow = 4, ncol = 4)
colnames(transition_matrix) <- c("A", "G", "T", "C")
rownames(transition_matrix) <- c("A", "G", "T", "C")

#Count Mutations Function (Problem 1)
count_mut = function(n_seq, germ) {
  if((length(germ) != length(n_seq)) || (length(grep("A|G|C|T", n_seq)) != length(n_seq))) {
    print("This is not a correct sequence.")
    return(NaN)
  }
  
  num_mut = length(((germ == n_seq))[(germ == n_seq) == FALSE])
  
  return(num_mut)
}

#Sequence Divergence Function (Problem 2)
seq_divergence = function (n_seq, germ, trans_mat) {
  if (is.nan(count_mut(n_seq, germ))) {
    return(NaN)
  }
  
  if ((nrow(trans_mat) != 4) || (ncol(trans_mat) != 4) || any(trans_mat < 0) || any(rowSums(trans_mat) != 1)) {
    print("The matrix you have provided is invalid.")
    return(NaN)
  }
  
  likelihood <- 0
  for (i in 1:length(germ)) {
    likelihood = likelihood + log10(trans_mat[germ[i], n_seq[i]])
  }
  return(likelihood)
}

#Running Functions on Clones
seq_div_clones_a <- unlist(lapply(clones_a, seq_divergence, germ = germline, trans_mat = transition_matrix))
mut_clones_a <- unlist(lapply(clones_a, count_mut, germ = germline))
seq_div_clones_b <- unlist(lapply(clones_b, seq_divergence, germ = germline, trans_mat = transition_matrix))
mut_clones_b <- unlist(lapply(clones_b, count_mut, germ = germline))

#Creating Table for Stats
stats_table <- matrix(0, nrow = 4, ncol = 2)
colnames(stats_table) <- c("clones_a", "clones_b")
rownames(stats_table) <- c("mut_mean", "mut_sd", "seq_div_mean", "seq_div_sd")

#Saving Stats
stats_table["mut_mean", "clones_a"] <- mean(mut_clones_a)
stats_table["mut_sd", "clones_a"] <- sd(mut_clones_a)
stats_table["seq_div_mean", "clones_a"] <- mean(seq_div_clones_a)
stats_table["seq_div_sd", "clones_a"] <- sd(seq_div_clones_a)
stats_table["mut_mean", "clones_b"] <- mean(mut_clones_b)
stats_table["mut_sd", "clones_b"] <- sd(mut_clones_b)
stats_table["seq_div_mean", "clones_b"] <- mean(seq_div_clones_b)
stats_table["seq_div_sd", "clones_b"] <- sd(seq_div_clones_b)
stats_table

#Running T Test on Data
t.test(mut_clones_a, mut_clones_b)
t.test(seq_div_clones_a, seq_div_clones_b)
