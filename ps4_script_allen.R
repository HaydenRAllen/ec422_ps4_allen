library(tidyverse)
library(fpp3)
library(BVAR)


liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)


liquor |>
  autoplot(sales) +
  labs(
    title = "U.S. Liquor, Beer, and Wine Sales",
    x = "Year/Month", 
    y = "Millions of Dollars", 
    color = NULL
  ) +
  theme_minimal()

liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Seasonal Pattern in SA Liquor, Beer, and Wine Sales",
    x = "Year/Month",
    y = "Millions of dollars"
  ) +
  theme_minimal()


liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

gg_tsresiduals()
