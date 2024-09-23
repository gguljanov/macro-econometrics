# Length of simulation
sim_len <- 250

# Transition matrix
Phi_1 <- matrix(data = c(-0.66, 0.26, -0.30, -0.58), nrow = 2, byrow = TRUE)

# Allocate space for simulation
yy <- matrix(, nrow = 2, ncol = sim_len)

# Initial point is zero vector
yy[, 1] <- c(0, 0)

# Simulate shocks
shocks <- matrix(rnorm(2 * sim_len), nrow = 2)

# Simulate VAR model
for (tt in 2:sim_len) {
    yy[, tt] <- Phi_1 %*% yy[, tt - 1] + shocks[, tt]
}

# Prepare plot layout
par(mfrow = c(2, 1))

# Plot first element
plot(yy[1, ], type = "l", xlab = "time", ylab = "First element")

# Plot second element
plot(yy[2, ], type = "l", xlab = "time", ylab = "Second element")

# Correct the plot layout
par(mfrow = c(1, 1))
