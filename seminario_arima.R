acf2(diff(seminario$dexeuus,2), 50, plot="TRUE")
acf2(diff(seminario$dexrealus,2), 50, plot="TRUE")
acf2(diff(seminario$dexchus,2), 50, plot="TRUE")
acf2(diff(seminario$dcoilwtico), 50, plot="TRUE")
acf2(diff(seminario$ibov,2), 50, plot="TRUE")

par(mfrow=c(3,1))
sarima.for(seminario$dexeuus, 20, 1,1,0, 0,0,0)
sarima.for(seminario$dexrealus, 20, 0,1,1, 0,0,0)
sarima.for(seminario$dexchus, 20, 1,1,3, 0,0,0)
par(mfrow=c(1,1))
sarima.for(seminario$dcoilwtico, 20, 0,1,1, 0,0,0)
sarima.for(seminario$ibov, 20, 4,1,1, 0,0,0)

auto.arima(
  seminario$dexeuus,
  max.p = 10,
  max.q = 10,
  stepwise = FALSE
)

auto.arima(
  seminario$dexrealus,
  max.p = 10,
  max.q = 10,
  stepwise = FALSE
)

auto.arima(
  seminario$dexchus,
  max.p = 10,
  max.q = 10,
  stepwise = FALSE
)

auto.arima(
  seminario$dcoilwtico,
  max.p = 10,
  max.q = 10,
  stepwise = FALSE
)

auto.arima(
  seminario$ibov,
  max.p = 10,
  max.q = 10,
  stepwise = FALSE
)