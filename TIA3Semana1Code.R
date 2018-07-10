# TIA3Semana1Code.R
# Ariel E. Meilij
# July-08-2018

# Prueba Piloto TIA3
# El siguiente código accede a las bases de datos Quandl y extrae los valores para
# las variables de TRM, petróleo, café y carbón

# Carga de librerias necesarias
library(Quandl)
library(tseries)
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(reshape2)

# Llave autorización API
Quandl.api_key("KzzS8Vfxkw1ZgTWgU4jH")

# lectura de variables trm, coffee, oil, coal
start_time <- Sys.time()
trm <- Quandl("CURRFX/USDCOP", collapse = "monthly", type = "ts")
wti <- Quandl("EIA/PET_RWTC_D", collapse = "monthly", type = "ts")
coffee <- Quandl("ODA/PCOFFOTM_USD", collapse = "monthly", type = "ts")
coal <- Quandl("EIA/COAL", collapse = "monthly", type = "ts")
end_time <- Sys.time()
end_time - start_time

# Verificar estructura de trm
summary(trm)
autoplot(window(trm, 2010), main = "ANALISIS SERIE DE TIEMPO TRM 2010-2018")

# Verificar estructura de wti
summary(wti)
autoplot(window(wti, 2010), main = "ANALISIS SERIE DE TIEMPO PETROLEO 2010-2018")

# Verificar estructura de coffee
summary(coffee)
autoplot(window(coffee, 2010), main = "ANALISIS SERIE DE TIEMPO CAFE 2010-2018")

# Verificar estructura de coal
summary(coal)
autoplot(window(coal, 2010), main = "ANALISIS SERIE DE TIEMPO CARBON 2010-2018")

# Convertir las cuatro series de tiempo en un data frame NO TIME CASTING
# La serie de tiempo de carbón solo necesita la cotización Northern Appalachian
coal <- coal[,2]

# La serie de tiempo de TRM solo utilizará la cotización de cierre
trm <- trm[,1]

df_coffee <- data.frame(date=index(coffee), coffee = melt(coffee)$value)
df_coal <- data.frame(date=index(coal), coal = melt(coal)$value)
df_wti <- data.frame(date=index(wti), wti = melt(wti)$value)
df_trm <- data.frame(date=index(trm), trm = melt(trm)$value)

ggplot(df_coffee, aes(x = date, y = coffee)) + geom_line(color = "#00AFBB", size = 1) + xlab("Fechas") + ylab("Cotizacion Cafe")
ggplot(df_coal, aes(x = date, y = coal)) + geom_line(color = "#00AFBB", size = 1) + xlab("Fechas") + ylab("Cotizacion Carbon")
ggplot(df_wti, aes(x = date, y = wti)) + geom_line(color = "#00AFBB", size = 1) + xlab("Fechas") + ylab("Cotizacion Petroleo")
ggplot(df_trm, aes(x = date, y = trm)) + geom_line(color = "#00AFBB", size = 1) + xlab("Fechas") + ylab("Cotizacion TRM")

# Merge data frames
df1 <- merge(df_trm, df_wti)
df1 <- merge(df1, df_coal)
df1 <- merge(df1, df_coffee)
summary(df1)

# Test potential correlations
corrTest2 <- rcorr(as.matrix(df1), type = c("pearson","spearman"))
corrTest2

chart.Correlation(df1, histogram=TRUE, pch=19)
corrplot(corrTest2$r, type="upper", order="hclust", p.mat = corrTest2$P, sig.level = 0.01, insig = "blank")


