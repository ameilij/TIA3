# TIA3Semana3Code.R
# Ariel E. Meilij
# July-08-2018

# Codificacion de Datos TIA3
# El siguiente codigo es un analisis y codificacion de variables

# Carga de librerias necesarias
library(Quandl)
library(tseries)
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(reshape2)
library(ggpubr)

# Llave autorización API
Quandl.api_key("KzzS8Vfxkw1ZgTWgU4jH")

# lectura de variables trm, coffee, oil, coal
trm <- Quandl("CURRFX/USDCOP")
wti <- Quandl("EIA/PET_RWTC_D")
coffee <- Quandl("CHRIS/ICE_KC1")
coal <- Quandl("EIA/COAL")
gold <- Quandl("WGC/GOLD_DAILY_USD")
banana <- Quandl("ODA/PBANSOP_USD")
palma <- Quandl("ODA/PPOIL_USD")
nickel <- Quandl("CHRIS/MCX_NI1")
gasoil <- Quandl("NASDAQOMX/NQCIGOER")

# Limpiar serie de tiempo TRM en su data.frame
trm <- trm[, 1:2]
trm <- subset(trm, trm$Date > "2009-12-31")
trm <- subset(trm, trm$Rate > 1500)
colnames(trm) <- c("Date", "trm")
ggplot(trm, aes(x = Date, y = trm)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion TRM")

# Limpiar serie de tiempo WTI en su data.frame
wti <- subset(wti, wti$Date > "2009-12-31")
colnames(wti) <- c("Date", "wti")
ggplot(wti, aes(x = Date, y = wti)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion WTI")

# Limpiar serie de tiempo COAL en su data.frame
coal <- coal[, 1:2]
coal <- subset(coal, coal$`Week Ended` > "2009-12-31")
colnames(coal) <- c("Date", "coal")
ggplot(coal, aes(x = Date, y = coal)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Carbon")

# Limpiar serie de tiempo COFFEE en su data.frame
coffee <- coffee[,c(1,5)]
coffee <- subset(coffee, coffee$Date > "2009-12-31")
coffee <- subset(coffee, coffee$Settle > 0)
colnames(coffee) <- c("Date", "coffee")
ggplot(coffee, aes(x = Date, y = coffee)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Cafe")

# Limpiar serie de tiempo GOLD en su data.frame
gold <- subset(gold, gold$Date > "2009-12-31")
colnames(gold) <- c("Date", "gold")
ggplot(gold, aes(x = Date, y = gold)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Oro")

# Limpiar serie de tiempo BANANA en su data.frame
banana <- subset(banana, banana$Date > "2009-12-31")
colnames(banana) <- c("Date", "banana")
ggplot(banana, aes(x = Date, y = banana)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Oro")

# Limpiar serie de tiempo PALMA en su data.frame
palma <- subset(palma, palma$Date > "2009-12-31")
colnames(palma) <- c("Date", "palma")
ggplot(palma, aes(x = Date, y = palma)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Aceite de Palma")

# Limpiar serie de tiempo NICKEL en su data.frame
nickel <- nickel[, c(1,5)]
nickel <- subset(nickel, nickel$Date > "2009-12-31")
colnames(nickel) <- c("Date", "nickel")
ggplot(nickel, aes(x = Date, y = nickel)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Ferroniquel")

# Limpiar serie de tiempo GASOIL en su data.frame
gasoil <- gasoil[, c(1:2)]
gasoil <- subset(gasoil, gasoil$`Trade Date` > "2009-12-31")
gasoil <- subset(gasoil, gasoil$`Index Value` > 0)
colnames(gasoil) <- c("Date", "gasoil")
ggplot(gasoil, aes(x = Date, y = gasoil)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Indice Gasoil")

# Limpiar serie de tiempo POLYPROPYLENE en su data.frame
polypropylene <- subset(polypropylene, polypropylene$Date > "2009-12-31")
colnames(polypropylene) <- c("Date", "polypropylene")
ggplot(polypropylene, aes(x = Date, y = polypropylene)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Indice Polipropileno")

# Limpiar serie de tiempo HULLA TERMINCA en su data.frame
thermal <- Quandl("CHRIS/SGX_CFF3")
thermal <- thermal[, c(1,6)]
thermal <- subset(thermal, thermal$Date > "2009-12-31")
colnames(thermal) <- c("Date", "thermal")
ggplot(thermal, aes(x = Date, y = thermal)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Indice Hulla Térmica")

# Merge data frames
df1 <- merge(trm, gold)
df1 <- merge(df1, wti)
df1 <- merge(df1, coal)
df1 <- merge(df1, coffee)
df1 <- merge(df1, nickel)
df1 <- merge(df1, gasoil)
df1 <- merge(df1, thermal)
summary(df1)

# Test potential correlations
corrTest2 <- rcorr(as.matrix(df1[,-1]), type = c("pearson","spearman"))
corrTest2

chart.Correlation(df1[,-1], histogram=TRUE, pch=19)
corrplot(corrTest2$r, type="upper", order="hclust", p.mat = corrTest2$P, sig.level = 0.01, insig = "blank")

# New correlogram prototype
# Cite STHDA http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corrTest2$r, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = corrTest2$P, sig.level = 0.05, insig = "blank",  
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

# Low volume time series correlation
df2 <- merge(trm, polypropylene)
df2 <- merge(df2, banana)
df2 <- merge(df2, palma)

corrTest1 <- rcorr(as.matrix(df1[,-1]), type = c("pearson","spearman"))
chart.Correlation(df2[,-1], histogram=TRUE, pch=19)

# Multiple Graph
graf1 <- ggplot(trm, aes(x = Date, y = trm)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion TRM")
graf2 <- ggplot(wti, aes(x = Date, y = wti)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion WTI")
graf3 <- ggplot(coal, aes(x = Date, y = coal)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Carbon")
graf4 <- ggplot(coffee, aes(x = Date, y = coffee)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Cafe")
graf5 <- ggplot(gold, aes(x = Date, y = gold)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Oro")
graf6 <- ggplot(nickel, aes(x = Date, y = nickel)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Ferroniquel")
graf7 <- ggplot(gasoil, aes(x = Date, y = gasoil)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Indice Gasoil")
graf8 <- ggplot(thermal, aes(x = Date, y = thermal)) + geom_line(color = "#00AFBB", size = 1) + 
  xlab("Fechas") + ylab("Cotizacion Indice Hulla Térmica")

ggarrange(graf1, graf2, graf3, graf4, graf5, graf6, graf7, graf8 + rremove("x.text"), 
          labels = c("TRM", "WTI", "CARBON", "CAFE", "ORO", "FERRONIQUEL", "GASOIL", "HULLA TERMICA" ),
          ncol = 4, nrow = 2)
