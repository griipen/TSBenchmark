# Simple return exercise: Daily Prices to Monthly Returns
# Input: Nx2 data.frame with columns (day, price) 
# Output: Nx2 object with columns (month, return)
# Three different functions: 1. xts, 2. data.table, 3. quantmod

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
N = 1e6

# Input data: Nx2 data.frame (date, price)
pmat = data.frame( 
    date = seq.Date(as.Date('1970-01-01'), by = 1, length.out = N),
    price = spot * exp(cumsum((r - 0.5 * sigma**2) * 1/N + (sigma * (sqrt(1/N)) * rnorm(N, mean = 0, sd = 1))))
)

# Output functions

      # 1. xts standalone function
      xtsfun = function(mat){
        xtsdf = as.xts(mat[, 2], order.by = mat[, 1])
        eom_prices = to.monthly(xtsdf)[, 4]
        mret = eom_prices/lag.xts(eom_prices) - 1; mret[1] = eom_prices[1]/xtsdf[1] - 1
        mret
      }
      
      # 2. data.table standalone function
      dtfun = function(mat){
        dt = setNames(as.data.table(mat), c('V1', 'V2')); dt[, Month := as.yearmon(V1)]; setkey(dt, "Month")
        dt[, .(EOM = last(V2)), Month][, .(Month, Return = EOM/shift(EOM, fill = first(mat[, 2])) - 1)]
      }
      
      # 3. quantmod (black box library)
      qmfun = function(mat){
        qmdf = as.xts(mat[, 2], order.by = mat[, 1])
        monthlyReturn(qmdf)
      }

# Check 1 == 2 == 3:
all.equal(
    unlist(dtfun(pmat)[, Return]),
    as.numeric(xtsfun(pmat)),
    as.numeric(qmfun(pmat)),
    scale = NULL
)
    
# Benchmark
library(microbenchmark)
gc()

runs = 50

mbm = microbenchmark(
  xts = xtsfun(pmat),
  data.table = dtfun(pmat),
  quantmod = qmfun(pmat),
  times = runs
)

# Visualisation
setDT(mbm)
mbm[, time := time*1e-6]
nfun = mbm[, length(unique(expr))]
trange = mbm[, c(0.8*min(time), 1.2*max(time))]
headers = c("Min", "Q1", "Median", "Mean", "Q3", "Max")
options(scipen = 999)

png('benchmark.png')
par(mfrow = c(nfun, 1), mar = c(3,1,3,11))
mbm[, {
  d = density(time)
  stats = as.numeric(summary(time))
  plot(x = d$x, y = d$y, xlim = trange, t = 'l', ylab = NULL, lwd = 2, col = 'coral3', main = paste0(expr, ' (', runs, ' runs, N = ', N, ')'))
  mtext('Runtime (ms)', line = -2, adj = 1.25, cex = 0.8)
  for (i in 1:length(stats)){mtext(headers[i], line = -(i+3), adj = 1.1, cex = 0.7)}
  for (i in 1:length(stats)){mtext(round(stats[i], 2), line = -(i+3), adj = 1.25, cex = 0.7)}
}, expr]

dev.off()
