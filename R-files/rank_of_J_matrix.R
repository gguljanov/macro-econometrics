# Parameter values
b_021 <- 0.1
sigma2_o <- 0
sigma2_p <- 0.1

# Set up J-matrix
J_matrix <- matrix(
    c(0, 1, 0, -sigma2_o, -b_021, 0, -2 * b_021 * sigma2_o, -b_021^2, 1),
    byrow = TRUE,
    nrow = 3
)

print(J_matrix)

# Get the rank
print(qr(J_matrix)$rank)
