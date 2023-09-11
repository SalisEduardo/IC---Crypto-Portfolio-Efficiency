

# Load packages:

library(MFDFA)
library(readxl)
library(plotly)


# ----

N = 1126 # number of data points (732 prices, 731 returns)
no = 10000 # number of series

# ----

# Generate random walk processes:

precos = matrix(0,nrow = N,ncol = no)
precos[1,] = 300 # initial price

for(j in 1:no){
  for(i in 2:N){
    precos[i,j] = precos[i-1,j] + rnorm(1,0,1)
  }
}

# Compute returns:

retornos <- diff(log(precos))

# ----

# Set MF-DFA parameters:

scale <- 10:((N-1)/4) # literature suggestion
q <- -4:4
m <- 1 # degree polinomial (avoid overfitting)

# ----

# RUN MF-DFA:

Resultados = matrix(0,nrow = no,10)

for(j in 1:no){
  
  b <- MFDFA(retornos[,j], scale, m, q)
  
  Resultados[j,1:9] = b[["Hq"]]
  Resultados[i,10] = ((b[["spec"]][["hq"]][4]-min(b[["spec"]][["hq"]]))-(max(b[["spec"]][["hq"]])-b[["spec"]][["hq"]][4]))/((b[["spec"]][["hq"]][4]-min(b[["spec"]][["hq"]]))+(max(b[["spec"]][["hq"]])-b[["spec"]][["hq"]][4])) # Theta
  
}

deltah = matrix(0,nrow = no,1)
for(i in 1:no){
  deltah[i,1]=max(Resultados[i,1:9])- min(Resultados[i,1:9])
}

hist(deltah)

write.csv(deltah,file = "deltas")
quantile(deltah,probs = c(0.80,0.9,0.95,0.99))

quantile(Resultados[,7],probs = c(0.01,0.05,0.10,0.90,0.95,0.99))
hist(Resultados[,7])

# q 90 --> 0,33