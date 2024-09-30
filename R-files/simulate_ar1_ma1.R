theta <- 0.45
max_lag <- 15

par(mfrow = c(2, 2))

# ACF of AR(1)
ar1_acf <- ARMAacf(ar = theta, ma = 0, lag.max = max_lag, pacf = FALSE)

plot(
    ar1_acf[2:length(ar1_acf)],
    col = "blue",
    type = "h",
    lwd = 2,
    main = "ACF for AR(1)"
)
abline(h = 0)

# PACF of AR(1)
ar1_pacf <- ARMAacf(ar = theta, ma = 0, lag.max = max_lag, pacf = TRUE)

plot(
    ar1_pacf,
    col = "red",
    type = "h",
    lwd = 2,
    main = "PACF for AR(1)"
)
abline(h = 0)

# ACF of MA(1)
ma1_acf <- ARMAacf(ar = 0, ma = theta, lag.max = max_lag, pacf = FALSE)

plot(
    ma1_acf[2:length(ma1_acf)],
    col = "blue",
    type = "h",
    lwd = 2,
    main = "ACF for MA(1)"
)
abline(h = 0)

# PACF of MA(1)
ma1_pacf <- ARMAacf(ar = 0, ma = theta, lag.max = max_lag, pacf = TRUE)

plot(
    ma1_pacf,
    col = "red",
    type = "h",
    lwd = 2,
    main = "PACF for MA(1)"
)
abline(h = 0)
