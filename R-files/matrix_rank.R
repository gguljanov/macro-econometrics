A_mat <- matrix(c(2, -7, -9, 5, 4, 3), nrow = 3, byrow = TRUE)

res <- qr(A_mat)

print(res$rank)
