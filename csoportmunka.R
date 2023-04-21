setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(quantmod)
library(tidyverse)

eurxts = getSymbols("EURHUF=X", src = "yahoo", from = "2021-01-01", to = "2022-12-31", auto.assign = F)

eur = data.frame(time = index(eurxts), eurhuf = as.numeric(eurxts$`EURHUF=X.High`))

ggplot(eur, aes(x = time))+
  geom_line(aes(y = eurhuf, color = "EUR/HUF árfolyam"))+
  theme_minimal()
