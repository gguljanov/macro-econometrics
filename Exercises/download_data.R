library(eurostat)

# Gross Domestic Product
gdp <- get_eurostat(
    id = "namq_10_gdp",
    filters = list(
        geo = c("EA", "DE", "IT", "FR"),
        untilTimePeriod = 2024,
        na_item = "B1GQ",
        unit = "CLV_I10",
        s_adj = "SCA"
    ),
    cache = FALSE
)

# Producer Price Index
ppi_search <- search_eurostat(
    pattern = "price index",
    type = "dataset",
    fixed = TRUE
)

print(ppi_search$title)

ppi <- get_eurostat(
    id = "enpe_sts_inppd",
    filters = list(
        freq = "A", # Annual
        untilTimePeriod = 2024,
        unit = "I15", # Index, 2015=100
        geo = "AM" # Armenia
    ),
    cache = FALSE
)

# Consumer Price Index

# Short Term Interest Rate

# Total Government Spending

# Total Government Revenue

# Oil Price

# Net Lending Government

# Stock Market Index

# Real GDP
