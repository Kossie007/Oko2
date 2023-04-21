# working directory beállítása
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# libraryk
library(quantmod)  #Yahoo finance adatokhoz
library(gtrendsR)  #Google trends adatokhoz
library(tidyverse) #ggplot2 + más packegek, ha kellenének

#Adatok----
#********************************************************************************************
# Adatok összegyűjtése
#********************************************************************************************

# Yahoo finance-rők az árfolyam lekérése 2021. jan 1-től 2022. dec 31-ig
eurxts = getSymbols("EURHUF=X", src = "yahoo", from = "2021-01-01", to = "2022-12-31", auto.assign = F)

# a Yahoo finance-es adatokből dataframe-ként az idő és az 'highest'(ezt közösen beszéljük meg melyik legyen)
eur = data.frame(time = index(eurxts), eurhuf = as.numeric(eurxts$`EURHUF=X.High`))

# EUR/HUF alap vonaldiagramm, 'theme_minimal'-al
ggplot(eur, aes(x = time))+
  geom_line(aes(y = eurhuf, color = "EUR/HUF árfolyam"))+
  theme_minimal()


# Google trends-ről 'korányinfó' keresési trend
#   Itt egy olyan probléma lép fel, hogy max ~90 napos intervallumban vannak napi adatok, annál nagyobbakban már hetiek
#   ezt úgy lehetne orvosolni, hogy sokszor kérek le 90 naposat.
#   Azonban ez ugye arányosítva van, ezért minden ~90 napban biztosan van egy 100-as érték -> minimum 8db ilyenünk lesz

# a 90 napos intervallumokat tartalmazó vektor
t = c("2021-01-01 2021-04-01","2021-04-01 2021-06-30","2021-06-30 2021-10-01","2021-10-01 2022-01-01",
      "2022-01-01 2022-04-01","2022-04-01 2022-06-30","2022-06-30 2022-10-01","2022-10-01 2023-01-01")

# 'kinfo' dataframe létrehozása
kinfo = data.frame()

# kinfo dataframe feltöltése
#   itt van egy Sys.sleep(2) függvény meghívva azért, hogy a google ne higyje azt,
#   hogy DDOS-olni szeretném a rövid időn belüli sok lekéréséert,
#   ezért a gépállat pihen minden lekérés után 2 másodpercet(nem, az 1 mp még nem jó)
#                   |
#                   V
#   szóval lassan fog lefutni, ezzel együtt kell élni

for (i in 1:length(t)) {
  kinfo = rbind(kinfo, gtrends(keyword = "kormányinfó", time = t[i])$interest_over_time)
  Sys.sleep(2)
}

# kitörlöm a felesleges oszlopokat, ami marad átnevezem
kinfo = kinfo[,1:2]
colnames(kinfo) =  c("time", "trend")

# lementem a 'kinfo'-t, hogy ne kelljen sokat várni legközelebb
write.csv(kinfo, "KormanyinfoGoogleTrend.csv", row.names = F)

kinfo = read.csv("KormanyinfoGoogleTrend.csv")
kinfo$time = as.Date(kinfo$time)

# kormányinfó trend alap vonaldiagramm, 'theme_minimal'-al
ggplot(kinfo, aes(x = time))+
  geom_line(aes(y = trend, color = "kormányinfó keresési trend"))+
  theme_minimal()

#----
#********************************************************************************************
# 
#********************************************************************************************

#----
#********************************************************************************************
# 
#********************************************************************************************

#Box-Jenkins----
#********************************************************************************************
# Box-Jenkins módszer
#********************************************************************************************

#----
#********************************************************************************************
# 
#********************************************************************************************





