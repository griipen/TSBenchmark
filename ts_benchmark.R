# Simple return exercise: Daily Prices to Monthly Returns
# Input: Nx2 data.frame with columns (day, price) 
# Output: Nx2 object with columns (month, return)
# Three different functions: 1. xts/TTR, 2. quantmod, 3. data.table

rm(list = ls()); gc()

library(data.table) 
library(zoo)
library(xts)
library(TTR)
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

      # 1. xts + TTR
      xtsfun = function(mat){
          xtsdf = as.xts(mat[, 2], order.by = mat[, 1])
          dailyRet = ROC(xtsdf, type = 'discrete', na.pad = F) # same as xtsdf/lag.xts(xtsdf, na.pad = F) - 1
          apply.monthly(dailyRet, function(x) tail(cumprod(x + 1) - 1, 1))
      }
      
      # 2. quantmod (black box external function)
      qmfun = function(mat){
        qmdf = as.xts(mat[, 2], order.by = mat[, 1])
        monthlyReturn(qmdf)
      }
      
      # 3. data.table standalone (no helper functions)
      dtfun = function(mat){
        dt = setNames(as.data.table(mat, key = colnames(mat)[1]), c('V1', 'V2'))
        dt[, dailyRet := V2/shift(V2, fill = V2[1]) - 1][, .(last(cumprod(1+dailyRet)-1)), .(as.yearmon(V1))]
      }

# Check 1 == 2 == 3:
all.equal(
    as.numeric(unlist(dtfun(pmat[1:100,])[, 2])),
    as.numeric(xtsfun(pmat[1:100,])),
    as.numeric(qmfun(pmat[1:100,]))
)
    
# Benchmark
library(microbenchmark)
gc()

mbm = microbenchmark(
  data.table = dtfun(pmat),
  quantmod = qmfun(pmat),
  xtsTTR = xtsfun(pmat),
  times = 200
)

mbm
autoplot(mbm, log = F)

