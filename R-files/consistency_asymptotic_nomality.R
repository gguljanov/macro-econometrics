# Simulation details
RR <- 1e3

TT <- c(25e1, 25e2, 25e3)

# Set up model
nlag <- 1

param_true <- c(0.3, 0.2, 0.1, 0.4, 0.23, -0.1, 0.32)

Phi1 <- matrix(param_true[1:4], nrow = 2)

V_mat <- matrix(
    c(param_true[5], param_true[6], param_true[6], param_true[7]),
    nrow = 2
)
V_chol <- chol(V_mat)

par(mfcol = c(4, length(TT)))

for (ii in seq_along(TT)) {
    param_hat <- matrix(, nrow = 4, ncol = RR)

    for (rr in seq_len(RR)) {
        # Generate data
        sample_len <- TT[ii]

        y_vec <- matrix(, nrow = 2, ncol = sample_len)
        y_vec[, 1] <- matrix(c(0, 0), nrow = 2)

        nu_vec <- matrix(rnorm(2 * sample_len), nrow = 2)
        nu_vec <- t(V_chol) %*% nu_vec

        for (tt in 2:sample_len) {
            y_vec[, tt] <- Phi1 %*% y_vec[, (tt - 1)] + nu_vec[, tt]
        }

        # OLS estimation of transition matrix, Phi_1
        y_dep <- t(y_vec[, (nlag + 1):sample_len])
        X_mat <- t(rbind(rep(1, sample_len - 1), y_vec[, 1:(sample_len - 1)]))

        Phi1_hat <- solve(t(X_mat) %*% X_mat) %*% (t(X_mat) %*% y_dep)

        param_hat[, rr] <- c(Phi1_hat[2:3, ])
    }

    for (jj in seq_len(4)) {
        hist(
            param_hat[jj, ],
            main = paste("TT-", sample_len),
            breaks = 25,
            xlab = paste("param-", jj),
            xlim = c(0.0, 0.5)
        )
    }
}
