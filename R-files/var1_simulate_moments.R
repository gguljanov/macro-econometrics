library(MASS)
library(netcontrol)

# Set up model
Phi <- matrix(c(0.2, 0.3, -0.6, 1.1), byrow = TRUE, nrow = 2)
V_mat <- matrix(c(0.9, 0.2, 0.2, 0.5), byrow = TRUE, nrow = 2)

dim_Phi <- dim(Phi)[1]

# Theoretical Moments
mu <- array(0, c(2, 1))
Gamma_0 <- dlyap(A = t(Phi), W = V_mat)

# Simulate data
RR <- 1e2
TT <- 1e4

# Initialize storage for Monte-Carlo
mean_VAR <- array(numeric(), c(dim_Phi, 1, RR))
cov_VAR <- array(numeric(), c(dim_Phi, dim_Phi, RR))

for (rr in seq_len(RR)) {
    # Inititalize storage observations in Monte Carlo run rr
    # First observation are zeros, i.e. equal to expectation
    yy <- array(0, c(2, TT))
    vv <- t(mvrnorm(n = TT, mu = c(0, 0), Sigma = V_mat))

    for (tt in 2:TT) {
        yy[, tt] <- Phi %*% yy[, tt - 1] + vv[, tt]
    }

    mean_VAR[, , rr] <- apply(yy, 1, mean)
    cov_VAR[, , rr] <- cov(t(yy))
}

# Compare theoretical with simulated moments for VAR(1)
sprintf("The theoretical mean is:")
print(mu)

sprintf("The simulated mean is: ")
sim_mu <- apply(mean_VAR, c(1, 2), mean)
print(sim_mu)

sprintf("The absolute difference is")
print(abs(mu - sim_mu))

sprintf("The theoretical covariance matrix is ")
print(Gamma_0)

sprintf("The simulated covariance matrix is ")
sim_Gamma_0 <- apply(cov_VAR, c(1, 2), mean)

sprintf("The absolute difference is")
print(abs(Gamma_0 - sim_Gamma_0))
