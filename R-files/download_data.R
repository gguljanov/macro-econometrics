library(eurostat)

# Gross Domestic Product
gdp <- get_eurostat(
    id = "namq_10_gdp",
    filters = list(
        geo = "DE", # Germany
        sinceTimePeriod = "1991-01-01",
        untilTimePeriod = "2024-04-01",
        na_item = "B1GQ", # Gross domestic product at market prices
        unit = "CLV_I10", # Chain linked volumes, index 2010=100
        s_adj = "SCA", # Seasonally and calendar adjusted data
        freq = "Q" # Quarterly data
    ),
    cache = FALSE
)

print(gdp)

# Producer Price Index
ppi_search <- search_eurostat(
    pattern = "price index",
    type = "dataset",
    fixed = FALSE
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
cpi_search <- search_eurostat(
    pattern = "price index",
    type = "dataset",
    fixed = FALSE
)

print(cpi_search$title)

print(cpi_search$code[4])

cpi <- get_eurostat(
    id = "prc_ipc_g20",
    filters = list(
        freq = "M", # Annual
        untilTimePeriod = 2024,
        unit = "I15", # Index, 2015=100
        geo = "DE" # Germany
    ),
    cache = FALSE
)

# Short Term Interest Rate
short_int_search <- search_eurostat(
    pattern = "interest rate",
    type = "dataset",
    fixed = FALSE
)

print(short_int_search$title)

print(short_int_search$code[5])

short_int <- get_eurostat(
    id = "irt_st_m",
    filters = list(
        sinceTimePeriod = "1994-01-01",
        # untilTimePeriod = 2024,
        int_rt = "IRT_DTD", # Day-to-day rate
        geo = "EA" # Euro Area
    ),
    cache = FALSE
)

print(short_int)

# Total Government Spending
gov_spend_sear <- search_eurostat(
    pattern = "government.expenditure",
    type = "dataset",
    fixed = FALSE
)

print(gov_spend_sear$title)

print(gov_spend_sear$code)

gov_spend <- get_eurostat(
    id = "gov_10a_exp",
    filters = list(
        freq = "A",
        unit = "MIO_EUR", # Million euro
        sector = "S13", # General government
        cofog99 = "TOTAL", # Total expenditure across government functions
        na_item = "TE", # Total expenditure across national accounts
        sinceTimePeriod = "1995-01-01",
        # untilTimePeriod = 2024,
        geo = "DE" # Germany
    ),
    cache = FALSE
)

print(gov_spend)

# Total Government Revenue
gov_rev_sear <- search_eurostat(
    pattern = "revenue",
    type = "dataset",
    fixed = FALSE
)

print(gov_rev_sear$title)

print(gov_rev_sear$code[1])

gov_rev <- get_eurostat(
    id = "gov_10a_main",
    filters = list(
        freq = "A",
        unit = "MIO_EUR", # Million euro
        sector = "S13", # General government
        # sector = "S1", # Total economy
        na_item = "TR", # Total general government revenue
        # sinceTimePeriod = "1995-01-01",
        # untilTimePeriod = 2024,
        geo = "DE" # Germany
    ),
    cache = FALSE
)

print(gov_rev)

# Oil Price
oil_pr_sear <- search_eurostat(
    pattern = "oil.*supply",
    type = "dataset",
    fixed = FALSE
)

print(oil_pr_sear$title)

print(oil_pr_sear$code[1])

oil_pr <- get_eurostat(
    id = "nrg_cb_cosm",
    filters = list(
        freq = "M", # Monthly frequency
        nrg_bal = "IMP", # Imports
        indic_nrg = "AVGPRC_USD_BBL", # Average price - USD per barrel
        geo = "DE" # Germany
    ),
    cache = FALSE
)

print(oil_pr)


# Net Lending Government
net_lend_sear <- search_eurostat(
    pattern = "main aggregates",
    type = "dataset",
    fixed = FALSE
)

print(net_lend_sear$title)

print(net_lend_sear$code[3])

net_lend <- get_eurostat(
    id = "gov_10a_main",
    filters = list(
        geo = "DE", # Germany
        na_item = "B9", # Net lending (+) / net borrowing (-)
        sector = "S13", # General government
        # sector = "S1", # Total economy
        freq = "A", # Annual frequency
        unit = "MIO_EUR", # Million euro
        # untilTimePeriod = 2024,
        sinceTimePeriod = "1995-01-01"
    ),
    cache = FALSE
)

print(net_lend)

# Stock Market Index
smi_search <- search_eurostat(
    pattern = "stock.*market.*index",
    type = "dataset",
    fixed = FALSE
)

print(smi_search$title)

print(smi_search$code[3])

# I could not find anything

# Real GDP
real_gdp_search <- search_eurostat(
    pattern = "Real GDP per capita",
    type = "dataset",
    fixed = FALSE
)

print(real_gdp_search$title)

print(real_gdp_search$code[3])

real_gdp <- get_eurostat(
    id = "sdg_08_10",
    filters = list(
        geo = "DE", # Germany
        na_item = "B1GQ", # Gross domestic product at market prices
        freq = "A", # Annual frequency
        unit = "CLV10_EUR_HAB" # Chain linked volumes (2010), euro per capita
        # untilTimePeriod = 2024,
        # sinceTimePeriod = "1995-01-01"
    ),
    cache = FALSE
)

print(real_gdp)
