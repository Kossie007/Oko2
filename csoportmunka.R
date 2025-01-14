# working directory beállítása
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# libraryk
library(quantmod)  #Yahoo finance adatokhoz
# library(gtrendsR)  #Google trends adatokhoz
library(tidyverse) #ggplot2 + más packegek, ha kellenének
library(aTSA) # adf.test-hez
library(lmtest) # bgtest, coeftest...
library(vars) # var modellhez

#Adatok----
#********************************************************************************************
# Adatok összegyűjtése
#********************************************************************************************

# Yahoo finance-rők az árfolyam lekérése 2021. jan 1-től 2022. dec 31-ig
eurxts = getSymbols("EURHUF=X", src = "yahoo", from = "2020-01-01", to = "2023-05-14", auto.assign = F)
usdxts = getSymbols("USDHUF=X", src = "yahoo", from = "2020-01-01", to = "2023-05-14", auto.assign = F)

# Kormányinfó dátumok
kinfo = read.csv("Kormanyinfo.csv")

# a Yahoo finance-es adatokből dataframe-ként az idő és az 'highest'(ezt közösen beszéljük meg melyik legyen)
arfolyam = data.frame(time = index(eurxts), eurhuf = as.numeric(eurxts$`EURHUF=X.High`), usdhuf = as.numeric(usdxts$`USDHUF=X.High`), kinfo = kinfo$Kormanyinfo)
rm(eurxts, usdxts)

# EUR/HUF, USD/HUF alap vonaldiagramm, 'theme_minimal'-al
kinfo = arfolyam %>% filter(kinfo == 1) # azért van ilyen data frame, hogy tudjam ábrázolni a kormányinfó pontokat

ggplot()+
  geom_line(data = arfolyam, aes(x = time, y = eurhuf, color = "EUR/HUF árfolyam"))+
  geom_line(data = arfolyam, aes(x = time, y = usdhuf, color = "USD/HUF árfolyam"))+
  geom_point(data = kinfo, aes(x = time, y = eurhuf, color = "Kormányinfó"))+
  geom_point(data = kinfo, aes(x = time, y = usdhuf, color = "Kormányinfó"))+
  theme_minimal()

rm(kinfo)

# végül ez nem kell----------------
# # Google trends-ről 'korányinfó' keresési trend
# #   Itt egy olyan probléma lép fel, hogy max ~90 napos intervallumban vannak napi adatok, annál nagyobbakban már hetiek
# #   ezt úgy lehetne orvosolni, hogy sokszor kérek le 90 naposat.
# #   Azonban ez ugye arányosítva van, ezért minden ~90 napban biztosan van egy 100-as érték -> minimum 8db ilyenünk lesz
# 
# # a 90 napos intervallumokat tartalmazó vektor
# t = c("2021-01-01 2021-04-01","2021-04-01 2021-06-30","2021-06-30 2021-10-01","2021-10-01 2022-01-01",
#       "2022-01-01 2022-04-01","2022-04-01 2022-06-30","2022-06-30 2022-10-01","2022-10-01 2023-01-01")
# 
# # 'kinfo' dataframe létrehozása
# kinfo = data.frame()
# 
# # kinfo dataframe feltöltése
# #   itt van egy Sys.sleep(2) függvény meghívva azért, hogy a google ne higyje azt,
# #   hogy DDOS-olni szeretném a rövid időn belüli sok lekéréséert,
# #   ezért a gépállat pihen minden lekérés után 2 másodpercet(nem, az 1 mp még nem jó)
# #                   |
# #                   V
# #   szóval lassan fog lefutni, ezzel együtt kell élni
# 
# for (i in 1:length(t)) {
#   kinfo = rbind(kinfo, gtrends(keyword = "kormányinfó", time = t[i])$interest_over_time)
#   Sys.sleep(2)
# }
# 
# # kitörlöm a felesleges oszlopokat, ami marad átnevezem
# kinfo = kinfo[,1:2]
# colnames(kinfo) =  c("time", "trend")
# 
# # lementem a 'kinfo'-t, hogy ne kelljen sokat várni legközelebb
# write.csv(kinfo, "KormanyinfoGoogleTrend.csv", row.names = F)
# 
# kinfo = read.csv("KormanyinfoGoogleTrend.csv")
# kinfo$time = as.Date(kinfo$time)
# 
# # kormányinfó trend alap vonaldiagramm, 'theme_minimal'-al
# ggplot(kinfo, aes(x = time))+
#   geom_line(aes(y = trend, color = "kormányinfó keresési trend"))+
#   theme_minimal()
# --------------

# https://telex.hu/gazdasag/2022/10/14/rendkivuli-bejelentest-tesz-az-mnb-csak-azt-nem-tudni-meg-hogy-mikor

#Box-Jenkins----
#********************************************************************************************
# Box-Jenkins módszer
#********************************************************************************************

# 1. lépés --------------------------


adf.test(arfolyam$eurhuf)
adf.test(arfolyam$usdhuf)
# nem stacionerek

# 2. lépés -------------------------

arfolyam$d_eurhuf = c(NA, diff(arfolyam$eurhuf))
arfolyam$d_usdhuf = c(NA, diff())

adf.test(diff(arfolyam$eurhuf))
adf.test(diff(arfolyam$usdhuf))
# stacionerek

# 3. lépés -------------------------

bgtest(diff(arfolyam$eurhuf) ~ 1, order = 29)
bgtest(diff(arfolyam$usdhuf) ~ 1, order = 29)
# nem fehérzajok még

# 4. lépés -------------------------


# 5. lépés -------------------------


# 6. lépés -------------------------


#----
#********************************************************************************************
# VAR/VACM modell
#********************************************************************************************





