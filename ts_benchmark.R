# Simple return exercise: Daily Prices to Monthly Returns
# Input: Nx2 data.frame with columns (day, price) 
# Output: Nx2 object with columns (month, return)
# Three different functions: 1. xts/TTR, 2. quantmod, 3. data.table

rm(list = ls()); gc()

library(data.table) 
library(zoo)
library(xts)
library(ggplot2)
library(quantmod)

# Random stock params
spot = 100
r = 0.01
sigma = 0.02
N = 1e5

pmat = data.frame( # Input: Nx2 data.frame (date, price)
    date = seq.Date(as.Date('1970-01-01'), by = 1, length.out = N),
    price = spot * exp(cumsum((r - 0.5 * sigma**2) * 1/N + (sigma * (sqrt(1/N)) * rnorm(N, mean = 0, sd = 1))))
)

      # 1. xts standalone function
      xtsfun = function(mat){
        xtsdf = as.xts(mat[, 2], order.by = mat[, 1])
        eom_prices = to.monthly(xtsdf)[, 4]
        mret = eom_prices/lag.xts(eom_prices) - 1; mret[1] = eom_prices[1]/xtsdf[1] - 1
        mret
      }
      
      # 2. data.table standalone function
      dtfun = function(mat){
        dt = setNames(as.data.table(mat), c('V1', 'V2'))
        dt[, last(V2), .(Month = as.yearmon(V1))][, .(Month, Return = V1/shift(V1, fill = first(mat[, 2])) - 1)]
      }
      
      # 3. quantmod (black box external function)
      qmfun = function(mat){
        qmdf = as.xts(mat[, 2], order.by = mat[, 1])
        monthlyReturn(qmdf)
      }

# Check 1 == 2 == 3:
all.equal(
    unlist(dtfun(pmat[1:1000,])[, Return]),
    as.numeric(xtsfun(pmat[1:1000,])),
    as.numeric(qmfun(pmat[1:1000,])),
    scale = NULL
)
    
# Benchmark
library(microbenchmark)
gc()

mbm = microbenchmark(
  xts = xtsfun(pmat),
  data.table = dtfun(pmat),
  quantmod = qmfun(pmat),
  times = 50
)

mbm

png('benchmark.png', width = 800, height = 600)
autoplot(mbm, log = F, cex.lab = 1.5)
dev.off()
