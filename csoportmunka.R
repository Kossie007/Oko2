# working directory beállítása
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# libraryk
library(quantmod)  #Yahoo finance adatokhoz
library(gtrendsR)  #Google trends adatokhoz
library(tidyverse) #ggplot2 + más packegek, ha kellenének

# Yahoo finance-rők az árfolyam lekérése 2021. jan 1-től 2022. dec 31-ig
eurxts = getSymbols("EURHUF=X", src = "yahoo", from = "2021-01-01", to = "2022-12-31", auto.assign = F)

# a Yahoo finance-es adatokből dataframe-ként az idő és az 'highest'(ezt közösen beszéljük meg melyik legyen)
eur = data.frame(time = index(eurxts), eurhuf = as.numeric(eurxts$`EURHUF=X.High`))

# még nem működik, otthon megcsinálom
# Google trends-ről 'korányinfó' keresési trend
kinfo = gtrends(keyword = "Kormányinfó", geo = "HU", time = "2021-01-01 2022-12-31")

# alap vonaldiagramm, 'theme_minimal'-al
ggplot(eur, aes(x = time))+
  geom_line(aes(y = eurhuf, color = "EUR/HUF árfolyam"))+
  theme_minimal()
