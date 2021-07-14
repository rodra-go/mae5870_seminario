# MAE5870 - Análise de Séries Temporais
# Rodrigo da Silva Cunha
# NUSP 7631302
# ------------------------------------------------------------------------------
# Bibliotecas necessárias
library(latex2exp)
library(astsa)
library(gridExtra)
library(dplyr)
# ------------------------------------------------------------------------------

# Carregando os dados
seminario_path = "/home/rodra/workspace/MAT5870/Seminário/Dados/seminario.csv"
seminario = read.csv(seminario_path)
seminario = seminario[c('ibov','dexeuus','dexchus','dexrealus','dcoilwtico')]
ibov = diff(ts(seminario$ibov))
dexeuus = diff(ts(seminario$dexeuus))
dexchus = diff(ts(seminario$dexchus))
dexrealus = diff(ts(seminario$dexrealus))
dcoilwtico = diff(ts(seminario$dcoilwtico))
seminario_ts = data.frame(ibov, dexeuus, dexchus, dexrealus, dcoilwtico)

# ------------------------------------------------------------------------------
# Visualização das Séries
plot.ts(seminario, main="Análise de Séries Financeiras")
plot.ts(seminario_ts, main="Séries Diferenciadas")

# ------------------------------------------------------------------------------
# Análise Espectral
Y = seminario_ts   # Y holds the transformed series
Nt = length(Y[,1])

L = 401; M = 800; alpha = .001; fdr = .001; tp = .1; 
H = 31 # Horizonte de previsão
nq = 3              # number of inputs (exchange rates)

## Spectral Matrix
Yspec = mvspec(Y, spans=L, kernel="daniell", detrend=TRUE, demean=FALSE,
               taper=tp, plot=FALSE)
n = Yspec$n.used # effective sample size
Fr = Yspec$freq # fundamental freqs
n.freq = length(Fr) # number of frequencies
Yspec$bandwidth*sqrt(12) # = 0.050 - the bandwidth

Pks = cbind(c('IBovespa','topleft',.0619,.1390,.22,.381,.4828), #ibov
       c('EU/US','topright',.017,.112,.157, .193,.335), #euus
       c('CH/US','bottomright',.019,.06,.12, .32,.407), #chus
       c('REAL/US','topright',0.0475,.087,.182, .315,.393), #realus
       c('Crude Oil','topleft',.07,.21, .345,.3865,.438)) #coil

# Periodogramas
for (i in 5:5){
  pks = as.numeric(Pks[3:length(Pks[,i]),i])
  tm = Nt/(n*pks)
  lplc = Pks[2,i]
  name = Pks[1,i]
  lgds = paste(as.character(round(tm,2)),'dias',sep=' ')
  ltys = c(1:length(pks))
  plot(Fr, Yspec$spec[,i], type="l", ylab="Espectro", xlab="Frequencia",
       main=paste('Periodograma',name, '| Taper=', tp, sep=' '))
  abline(v = pks, lty=ltys)
  legend(lplc,legend=lgds,lty=ltys,bty='o', cex = .8)
}

## Coherencies
Fq = qf(1-alpha, 2, L-2)
cn = Fq/(L-1+Fq)
plt.name = c("(a)","(b)","(c)","(d)","(e)","(f)")
par(mfrow=c(2,2), cex.lab=1.2)
# The coherencies are listed as 1,2,...,10=choose(5,2)
for (i in 2:5){
  idx = 1 + (i - 1) * (i - 2)/2
  plot(Fr, Yspec$coh[,idx], type="l", ylab="Sq Coherence", xlab="Frequency",
       ylim=c(0,.4), main=c("Ibov with", names(seminario[i])))
  abline(h = cn); text(.45,.98, plt.name[i-1], cex=1.2) }

## Multiple Coherency
par(mfrow=c(3,2))
coh.23 = stoch.reg(Y, cols.full = c(2,3), cols.red = NULL, alpha, L, M,
                   plot.which = "coh")
title(main = c("IBovespa com", "EU/US e CH/US"))
coh.24 = stoch.reg(Y, cols.full = c(2,4), cols.red = NULL, alpha, L, M,
                   plot.which = "coh")
title(main = c("IBovespa com", "EU/US e REAL/US"))
coh.25 = stoch.reg(Y, cols.full = c(2,5), cols.red = NULL, alpha, L, M,
                   plot.which = "coh")
title(main = c("IBovespa com", "EU/US e Crude Oil"))
coh.34 = stoch.reg(Y, cols.full = c(3,4), cols.red = NULL, alpha, L, M,
                   plot.which = "coh")
title(main = c("IBovespa com", "CH/US e REAL/US"))
coh.35 = stoch.reg(Y, cols.full = c(3,5), cols.red = NULL, alpha, L, M,
                   plot.which = "coh")
title(main = c("IBovespa com", "CH/US e Crude Oil"))
coh.45 = stoch.reg(Y, cols.full = c(4,5), cols.red = NULL, alpha, L, M,
                   plot.which = "coh")
title(main = c("IBovespa com", "REAL/US e Crude Oil"))

par(mfrow=c(2,2))
coh.234 = stoch.reg(Y, cols.full = c(2,3,4), cols.red = NULL, alpha, L, M,
                   plot.which = "coh")
title(main = c("IBovespa com", "REAL/US, EU/US, CH/US"))
coh.345 = stoch.reg(Y, cols.full = c(3,4,5), cols.red = NULL, alpha, L, M,
                    plot.which = "coh")
title(main = c("IBovespa com", "EU/US, CH/US, Crude Oil"))
coh.245 = stoch.reg(Y, cols.full = c(2,4,5), cols.red = NULL, alpha, L, M,
                    plot.which = "coh")
title(main = c("IBovespa com", "EU/US, REAL/US, Crude Oil"))
coh.235 = stoch.reg(Y, cols.full = c(2,3,5), cols.red = NULL, alpha, L, M,
                    plot.which = "coh")
title(main = c("IBovespa com", "EU/US, CH/US, Crude Oil"))

par(mfrow=c(1,1))
coh.2345 = stoch.reg(Y, cols.full = c(2,3,4,5), cols.red = NULL, alpha, L, M,
                    plot.which = "coh")
title(main = c("IBovespa com", "REAL/US, EU/US, CH/US e Crude Oil"))

# ------------------------------------------------------------------------------
# Regressão no Domínio da Frequência

## Partial F
numer.df = 2*nq; denom.df = Yspec$df-2*nq
par(mfrow=c(4,1), mar=c(3,3,2,1)+.5, mgp = c(1.5,0.4,0), cex.lab=1.2)
out.234 = stoch.reg(Y, cols.full = c(2,3,4), cols.red = 4, alpha, L, M,
                   plot.which = "F.stat")
eF = out.234$eF
pvals = pf(eF, numer.df, denom.df, lower.tail = FALSE)
pID = FDR(pvals, fdr); abline(h=c(eF[pID]), lty=2)
title(main = "Partial F Statistic")

# Regression Coefficients
S = seq(from = -M/2+1, to = M/2 - 1, length = M-1)
plot(S, coh.234$Betahat[,1], type = "h", xlab = "", ylab = names(seminario[2]),
     ylim = c(-20, 12), lwd=2)
abline(h=0); title(main = "Impulse Response Functions")
plot(S, coh.234$Betahat[,2], type = "h", xlab = "Index", ylab =
       names(seminario[3]), ylim = c(-7, 3), lwd=2)
abline(h=0)
plot(S, coh.234$Betahat[,3], type = "h", xlab = "Index", ylab =
       names(seminario[4]), ylim = c(-7, 5), lwd=2)
abline(h=0)

# Selecionando os coeficientes
euus_betas = data.frame(lag=S, beta=coh.234$Betahat[,1])
chus_betas = data.frame(lag=S, beta=coh.234$Betahat[,2])
realus_betas = data.frame(lag=S, beta=coh.234$Betahat[,3])

euus_b = euus_betas %>% filter(Mod(beta) > 8)
chus_b = chus_betas %>% filter(Mod(beta) > 3)
realus_b = realus_betas %>% filter(Mod(beta) > 3)

# Regressão
sem = ts.intersect(
  It=Y[,1], 
  E0=Y[,2],
  C0=Y[,3],
  C1=stats::lag(Y[,3],-1),
  R0=Y[,4],
  R1=stats::lag(Y[,4],-1),
  R2=stats::lag(Y[,4],-2),
  R3=stats::lag(Y[,4],-3))
(u = lm(sem[,1]~sem[,2:8], na.action=NULL))
acf2(ts(resid(u)), main="ACF e PACF - Vt")
mymodel = sarima(sem[,1],  1,0,1,  1,0,0,7,  xreg=sem[,2:8])

# Previsão
N = length(sem[,1]) # Tamanho do dataset de treino
train = sem[1:(N-H),]
test = sem[(N-H+1):N,]

par(mfrow=c(1,1))
prev = sarima.for(
  train[,1],  H, 
  1,0,1,  
  1,0,0,7,  
  xreg=train[,2:8], 
  newxreg=test[,2:8])

ibov_hat = diffinv(rbind(data.matrix(train[,1]), data.matrix(prev$pred))) 
se_hat = diffinv(prev$se)
se_hat = se_hat[2:length(se_hat)]
up_hat = ts(ibov_hat[(N-H+1):N,] + se_hat + mean(seminario$ibov), 
            start=(N-H+1), 
            frequency=1)
lo_hat = ts(ibov_hat[(N-H+1):N,] - se_hat + mean(seminario$ibov),
            start=(N-H+1), 
            frequency=1)

ibov1 = ibov_hat[1:(N-H),]
pred = ts(ibov_hat[(N-H+1):N,], start=(N-H+1), frequency=1)

# Regressão com 3 variaveis: primeiro plot, com menos zoom
plot(ibov1 + mean(seminario$ibov), 
     ylim=c(50000, 175000), 
     xlim=c(2000,2441), 
     type='l',
     main="Regressão Modelo 1 - IBov ~ REAL/US + CH/US + EU/US",
     xlab="Dia", ylab="IBovespa")
xx = c(time(up_hat), rev(time(lo_hat))); yy = c(lo_hat, rev(up_hat))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(pred + mean(seminario$ibov), type="p", col=2)
grid(10, 5, lwd = 1) # grid only in y-direction

# Regressãoc com 3 variáveis: Segundo plot, com mais zoom
plot(ibov1 + mean(seminario$ibov), 
     ylim=c(50000, 175000), 
     xlim=c(2300,2441), 
     type='l',
     main="Regressão Modelo 1 - IBov ~ REAL/US + CH/US + EU/US",
     xlab="Dia", ylab="IBovespa")
xx = c(time(up_hat), rev(time(lo_hat))); yy = c(lo_hat, rev(up_hat))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(pred + mean(seminario$ibov), type="p", col=2)
grid(10, 5, lwd = 1) # grid only in y-direction


# Regressão com apenas REAL/US
# 5) Regressão
lr = LagReg(Y[,4], Y[,1], L=L, M=M, threshold =200)

## Ajuste
sem2 = ts.intersect(
  I=Y[,1], 
  T0=Y[,4], 
  T1=stats::lag(Y[,4],-1),
  T2=stats::lag(Y[,4],-2),
  T3=stats::lag(Y[,4],-3),
  T4=stats::lag(Y[,4],-4),
  T9=stats::lag(Y[,4],-9),
  T11=stats::lag(Y[,4],-11))
(u2 = lm(sem2[,1]~sem2[,2:8], na.action=NULL))

# Avaliação do erro final
acf2(ts(resid(u2)), main="ACF e PAFC - Vt")

# Ajuste de SARIMA para o erro
mymodel2 = sarima(sem2[,1], 1, 0, 1,  1, 0, 0, 7,  xreg=sem2[,2:8])


# Previsão
N = length(sem2[,1]) # Tamanho do dataset de treino
train2 = sem2[1:(N-H),]
test2 = sem2[(N-H+1):N,]

par(mfrow=c(1,1))
prev2 = sarima.for(
  train2[,1],  H, 
  1,0,1,  
  1,0,0,7,  
  xreg=train2[,2:8], 
  newxreg=test2[,2:8])

ibov_hat2 = diffinv(rbind(data.matrix(train2[,1]), data.matrix(prev2$pred))) 
se_hat2 = diffinv(prev2$se)
se_hat2 = se_hat2[2:length(se_hat2)]
up_hat2 = ts(ibov_hat2[(N-H+1):N,] + se_hat2 + mean(seminario$ibov), 
            start=(N-H+1+8), 
            frequency=1)
lo_hat2 = ts(ibov_hat2[(N-H+1):N,] - se_hat2 + mean(seminario$ibov),
            start=(N-H+1+8), 
            frequency=1)

ibov2 = ibov_hat2[1:(N-H),]
pred2 = ts(ibov_hat2[(N-H+1):N,], start=(N-H+1+8), frequency=1)


# Regressão com 1 variavel: primeiro plot, com menos zoom
plot(ibov1 + mean(seminario$ibov), 
     ylim=c(50000, 175000), 
     xlim=c(2000,2441), 
     type='l',
     main="Regressão Modelo 2 - IBov ~ REAL/US",
     xlab="Dia", ylab="IBovespa")
xx2 = c(time(up_hat2), rev(time(lo_hat2))); yy2 = c(lo_hat2, rev(up_hat2))
polygon(xx2, yy2, border = 8, col = gray(.6, alpha = .2))
lines(pred2 + mean(seminario$ibov), type="p", col=2)
grid(10, 5, lwd = 1) # grid only in y-direction

# Regressão com 1 variável: Segundo plot, com mais zoom
plot(ibov1 + mean(seminario$ibov), 
     ylim=c(50000, 175000), 
     xlim=c(2300,2441), 
     type='l',
     main="Regressão Modelo 2 - IBov ~ REAL/US",
     xlab="Dia", ylab="IBovespa")
xx2 = c(time(up_hat2), rev(time(lo_hat2))); yy2 = c(lo_hat2, rev(up_hat2))
polygon(xx2, yy2, border = 8, col = gray(.6, alpha = .2))
lines(pred2 + mean(seminario$ibov), type="p", col=2)
grid(10, 5, lwd = 1) # grid only in y-direction


# Comparação dos dois modelos
plot(ibov1 + mean(seminario$ibov), 
     ylim=c(50000, 175000), 
     xlim=c(2300,2441), 
     type='l',
     main="Regressão - Modelos 1 e 2",
     xlab="Dia", ylab="IBovespa")
polygon(xx, yy, border = 8, col = gray(.9, alpha = .2))
lines(pred + mean(seminario$ibov), type="p", col=2)

xx2 = c(time(up_hat2), rev(time(lo_hat2))); yy2 = c(lo_hat2, rev(up_hat2))
polygon(xx2, yy2, border = 8, col = gray(.3, alpha = .2))
lines(pred2 + mean(seminario$ibov), type="p", col=3)
grid(10, 5, lwd = 1) # grid only in y-direction
legend('topleft',legend=c('Modelo 1', 'Modelo 2'),col=c(2,3), lty=c(1,1), cex = .8)


rmse1 = sqrt(sum((test[,1] - pred)*(test[,1] - pred))/length(test[,1]))
rmse2 = sqrt(sum((test2[,1] - pred2)*(test2[,1] - pred2))/length(test2[,1]))
