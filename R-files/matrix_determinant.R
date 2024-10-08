A_mat <- matrix(
    c(1, 15, 14, 4, 12, 6, 7, 9, 8, 10, 11, 5, 13, 3, 2, 16),
    byrow = FALSE,
    nrow = 4
)

print(A_mat)

determinant(A_mat, logarithm = FALSE)
