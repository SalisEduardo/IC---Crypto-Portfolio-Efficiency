source('utils_R/fractality.R')
setwd("~/Documents/Faculdade/IC - Crypto Portfolio Efficiency")

teste <- generate_experiments_parallel(1000,100)
teste_v2 <- generate_experiments_parallel_v2(1000,100)



rw_deltaH_360_10000 <- generate_experiments_parallel_v2(360,10000)

rw_deltaH_720_10000 <- generate_experiments_parallel_v2(720,10000)

rw_deltaH_1080_10000 <- generate_experiments_parallel_v2(1080,10000)


saveRDS(rw_deltaH_360_10000, file = "deltaH_simulations/rw_deltaH_360_10000.rds")
saveRDS(rw_deltaH_720_10000, file = "deltaH_simulations/rw_deltaH_720_10000.rds")
saveRDS(rw_deltaH_1080_10000, file = "deltaH_simulations/rw_deltaH_1080_10000.rds")
