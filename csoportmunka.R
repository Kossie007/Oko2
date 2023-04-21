setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(quantmod)

eur = getSymbols("EURHUF=X", src = "yahoo", from = "2021-01-01", to = "2022-12-31", auto.assign = F)

