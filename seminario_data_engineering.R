# MAE5870 - Análise de Séries Temporais
# Rodrigo da Silva Cunha
# NUSP 7631302
# ------------------------------------------------------------------------------
# Bibliotecas necessárias
library(latex2exp)
library(astsa)
library(gridExtra)
library(tidyr)
library(dplyr)
library(lubridate)
library(zoo)
# ------------------------------------------------------------------------------

################################################################################

# 1) Carregando os dados

# Exchange Rate Real/US
dexrealus_path = "/home/rodra/workspace/MAT5870/Seminário/Dados/DEXREALUS.csv"
dexrealus = read.csv(dexrealus_path, dec=".")
dexrealus$date = as.Date(as.character(dexrealus$DateTime),format="%Y-%m-%d")
dexrealus$dexrealus = suppressWarnings(as.numeric(dexrealus$Taxa.de.Câmbio.Nominal))
dexrealus = dexrealus[c('date','dexrealus')]

# Exchange Rate US/Euro
dexuseu_path = "/home/rodra/workspace/MAT5870/Seminário/Dados/DEXUSEU.csv"
dexuseu = read.csv(dexuseu_path, dec=".")
dexuseu$date = as.Date(as.character(dexuseu$DATE),format="%Y-%m-%d")
dexuseu$dexuseu = suppressWarnings(as.numeric(dexuseu$DEXUSEU))
dexuseu = dexuseu[c('date','dexuseu')]

# Exchange Rate CH/US
dexchus_path = "/home/rodra/workspace/MAT5870/Seminário/Dados/DEXCHUS.csv"
dexchus = read.csv(dexchus_path, dec=".")
dexchus$date = as.Date(as.character(dexchus$DATE),format="%Y-%m-%d")
dexchus$dexchus = suppressWarnings(as.numeric(dexchus$DEXCHUS))
dexchus = dexchus[c('date','dexchus')]

# Crude Oil ILW
dcoilwtico_path = "/home/rodra/workspace/MAT5870/Seminário/Dados/DCOILWTICO.csv"
dcoilwtico = read.csv(dcoilwtico_path, dec=".")
dcoilwtico$date = as.Date(as.character(dcoilwtico$DATE),format="%Y-%m-%d")
dcoilwtico$dcoilwtico = suppressWarnings(as.numeric(dcoilwtico$DCOILWTICO))
dcoilwtico = dcoilwtico[c('date','dcoilwtico')]

# IBovespa
ibov_dir = "/home/rodra/workspace/MAT5870/Seminário/Dados/IBOV/"

read_ibov_file <- function(ibov_path) {
  (ibov_path)
  ibov_data = read.csv(text=paste0(
                          head(readLines(ibov_path), -2), collapse="\n"
                       ),
                       col.names=c('Dia','01','02','03','04','05',
                                   '06','07','08','09','10','11','12'),
                       skip = 1, sep=';',dec = ',')
  
  ibov = ibov_data %>%
    pivot_longer(!Dia, names_to = "Mes", values_to = "IBOV")
  
  spath = strsplit(ibov_path, "/")
  syear = strsplit(spath[[1]][length(spath[[1]])], '_')
  year = syear[[1]][[1]]
  Date = paste(year,ibov$Mes,ibov$Dia, sep="-")
  ibov$date = as.Date(as.character(Date),format="%Y-X%m-%d")
  
  ibov$IBOV = gsub('[,]', '.',gsub('[.]', '',ibov$IBOV))
  ibov$ibov = suppressWarnings(as.numeric(ibov$IBOV))
  
  ibov = na.omit(ibov)
  ibov = ibov[c('date','ibov')]
  ibov = ibov[order(ibov$date),]
  
  return(ibov)
  
}

read_ibov_dir <- function(ibov_dir) {
  fnames = list.files(ibov_dir)
  dataframe = data.frame(date=c(), ibov=c())
  
  for(i in 1:length(fnames)) {
    path = paste(ibov_dir, fnames[[i]], sep='')
    f = read_ibov_file(path)
    dataframe = rbind(dataframe,f)
  }
  
  return(dataframe)
}

# Carregar dados IBovespa
ibov = read_ibov_dir(ibov_dir)

# Merge dos dados
seminario <- left_join(ibov, dexuseu, by=c("date"))
seminario <- left_join(seminario, dexrealus, by=c("date"))
seminario <- left_join(seminario, dexchus, by=c("date"))
seminario <- left_join(seminario, dcoilwtico, by=c("date"))


# Filtrar por data
seminario <- seminario %>%
  select(date, ibov, dexuseu, dexrealus, dexchus, dcoilwtico) %>% 
    filter(date >= as.Date("2011-07-12"), date <= as.Date("2021-06-09"))

# Interpolar NA values
seminario$ibov = na.approx(seminario$ibov)
seminario$dexuseu = na.approx(seminario$dexuseu)
seminario$dexeuus = 1/seminario$dexuseu
seminario$dexrealus = na.approx(seminario$dexrealus)
seminario$dexchus = na.approx(seminario$dexchus)
seminario$dcoilwtico = na.approx(seminario$dcoilwtico)

# Salvar Dados
seminario_path = "/home/rodra/workspace/MAT5870/Seminário/Dados/seminario.csv"
write.csv(seminario, seminario_path)

# Visualização
par(mfrow=c(3,1))
plot(seminario$date, seminario$dexeuus, type='l', main="EU/US")
plot(seminario$date, seminario$dexrealus, type='l', main="REAL/US")
plot(seminario$date, seminario$dexchus, type='l', main="YUAN/US")
par(mfrow=c(1,1))
plot(seminario$date, seminario$dcoilwtico, type='l', main="Crue Oil Price")
plot(seminario$date, seminario$ibov, type='l', main="IBovespa")
