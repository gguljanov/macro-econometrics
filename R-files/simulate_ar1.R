sim_len <- 2500

phi_vec <- c(0.66, 0.99, 1, 1.01)

nu_t <- rnorm(sim_len)

y_t <- rep(NA, sim_len)
y_t[1] <- 0

colors <- c("green", "red", "black")

par(mfrow = c(3, 2))

for (ii in seq_len(length(phi_vec))) {
    for (tt in 2:sim_len) {
        y_t[tt] <- phi_vec[ii] * y_t[tt - 1] + nu_t[tt]
    }

    plot(y_t, type = "l", col = colors[ii])
    abline(h = 0, col = "blue")
    acf(y_t)

    y_t[1] <- 0
}
