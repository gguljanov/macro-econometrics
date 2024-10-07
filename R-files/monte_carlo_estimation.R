# Set up model
sample_len <- 2500

param_true <- c(0.3, 0.2, 0.1, 0.4, 0.23, -0.1, 0.32)

Phi1 <- matrix(param_true[1:4], nrow = 2)

V_mat <- matrix(
    c(param_true[5], param_true[6], param_true[6], param_true[7]),
    nrow = 2
)
V_chol <- chol(V_mat)

# Generate data
y_vec <- matrix(, nrow = 2, ncol = sample_len)
y_vec[, 1] <- matrix(c(0, 0), nrow = 2)

nu_vec <- matrix(rnorm(2 * sample_len), nrow = 2)
nu_vec <- t(V_chol) %*% nu_vec

for (tt in 2:sample_len) {
    y_vec[, tt] <- Phi1 %*% y_vec[, (tt - 1)] + nu_vec[, tt]
}

# Derive log-likelihood
nn <- dim(Phi1)[1]

nlog_liki <- function(param, TT, nn, pp, data) {
    Phi1 <- matrix(param[1:(2 * nn)], nrow = nn)

    # If eigenvalues are greater than one, cancel
    if (any(eigen(Phi1)$values) >= 1) {
        res <- -Inf
        return
    }

    V_mat <- matrix(
        c(
            param[2 * nn + 1], param[2 * nn + 2],
            param[2 * nn + 2], param[2 * nn + 3]
        ),
        nrow = nn
    )

    err <- data[, 2:sample_len] - Phi1 %*% data[, 1:(sample_len - 1)]

    # Log-lileihood
    res <- -(TT - pp) * nn / 2 * log(2 * pi) +
        (TT - pp) / 2 * determinant(solve(V_mat))$modulus -
        (0.5) * sum(t(err) %*% solve(V_mat) %*% err)

    # Negative log-likelihood
    res <- -res
}

# Minimize the log-likelihood
res <- optim(
    # par = param_true,
    par = c(1, 0.1, 0.1, 1, 0.4, 0.1, 0.6),
    fn = nlog_liki,
    method = "BFGS",
    # method = "Nelder-Mead",
    # method = "SANN",
    hessian = TRUE,
    TT = sample_len,
    nn = nn,
    pp = 1,
    data = y_vec
)

res
