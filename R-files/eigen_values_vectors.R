A_mat <- matrix(
    c(0.5, 0, 0, 0.1, 0.1, 0.3, 0, 0.2, 0.3),
    byrow = TRUE,
    nrow = 3
)

print(A_mat)

eigen(A_mat)
