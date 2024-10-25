library(readr)
library(vars)
library(xtable)
library(ggplot2)
library(gridExtra)

# Unemployment for Germany, from FRED, seasonlly adjusted, Quarterly, percent
unemp <- read_csv("../Data/LRHUADTTDEQ156S.csv")

head(unemp)
tail(unemp)

len <- dim(unemp)[1]

# Percentage change
unemp$delta <- rep(NA, len)
unemp$delta[2:len] <- diff(unemp$LRHUADTTDEQ156S, lag = 1)

# Real Gross Domestic Product for Germany, from FRED, sesonally adj, Quarterly
real_gdp <- read_csv("../Data/CLVMNACSCAB1GQDE.csv")

head(real_gdp)
tail(real_gdp)

len <- dim(real_gdp)[1]

# Growth rate
real_gdp$growth <- rep(NA, len)

real_gdp$growth[2:len] <- 100 * (
    real_gdp$CLVMNACSCAB1GQDE[2:len] /
        real_gdp$CLVMNACSCAB1GQDE[1:(len - 1)] -
        1
)

# Plot the real GDP and its growth
real_gdp_pl <- ggplot(data = real_gdp, aes(x = DATE, y = CLVMNACSCAB1GQDE)) +
    geom_line() +
    xlab("date") +
    ylab("real GDP")

gdp_growth_pl <- ggplot(data = real_gdp, aes(x = DATE, y = growth)) +
    geom_line() +
    xlab("date") +
    ylab("GDP growth")

grid.arrange(real_gdp_pl, gdp_growth_pl)

# Plot the unemployment and its change
unemp_pl <- ggplot(data = unemp, aes(x = DATE, y = LRHUADTTDEQ156S)) +
    geom_line() +
    xlab("date") +
    ylab("Unemployment in percentage")

unemp_delta_pl <- ggplot(data = unemp, aes(x = DATE, y = delta)) +
    geom_line() +
    xlab("date") +
    ylab("Change in unemployment")

grid.arrange(unemp_pl, unemp_delta_pl)

# Estimate
y_vec <- cbind(
    unemployment = unemp$delta[2:len],
    growth = real_gdp$growth[2:len]
)

res <- VAR(y_vec, p = 2, type = "const")

summary(res)

reg_unemployment <- xtable(res$varresult$unemployment)
print(reg_unemployment, file = "../Tex-files-auto/reg_unemployment.tex")

reg_growth <- xtable(res$varresult$growth)
print(reg_growth, file = "../Tex-files-auto/reg_growth.tex")

# Granger causality
causality(x = res, cause = "unemployment")

# Impulse Response Function

# unemp <- get_eurostat(
#     id = "lfsq_ugad",
#     filters = list(
#         age = "Y15-74", # From 15 to 74 years old
#         duration = "TOTAL", # no matter how long unemployed
#         geo = "DE", # Germany
#         sex = "T", # Total = female + male
#         sinceTimePeriod = "1998-Q1",
#         untilTimePeriod = "2024-Q2",
#         freq = "Q", # Quarterly
#         unit = "THS_PER" # Thousand persons
#     ),
#     cache = FALSE
# )

# pop <- get_eurostat(
#     id = "lfsq_pgaed",
#     filters = list(
#         age = "Y15-74", # From 15 to 74 years old
#         geo = "DE", # Germany
#         isced11 = "TOTAL", # total, any education level
#         sex = "T", # Total = female + male
#         sinceTimePeriod = "1998-Q1",
#         untilTimePeriod = "2024-Q2",
#         freq = "Q", # Quarterly
#         unit = "THS_PER" # Thousand persons
#     ),
#     cache = FALSE
# )
