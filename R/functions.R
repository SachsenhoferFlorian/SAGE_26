cramers_v_matrix <- function(data) {
  n <- ncol(data)
  mat <- matrix(NA, n, n)
  colnames(mat) <- colnames(data)
  rownames(mat) <- colnames(data)
  
  for (i in 1:n) {
    for (j in 1:n) {
      tbl <- table(data[[i]], data[[j]])
      mat[i, j] <- CramerV(tbl)
    }
  }
  
  return(mat)
}
