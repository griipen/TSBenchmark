# Simple return exercise: Daily Prices to Monthly Returns
# Input: Nx2 data.frame with columns (day, price) 
# Output: Nx2 object with columns (month, return)
# Three different functions: 1. xts, 2. data.table, 3. quantmod

rm(list = ls()); gc()
setwd("~/Documents/R/TSBenchmark")

library(data.table) 
library(zoo)
library(xts)
library(quantmod)
library(plotly)
library(htmlwidgets)

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
        dt = setNames(as.data.table(mat), c('V1', 'V2'))
        dt[, .(EOM = last(V2)), .(Month = as.yearmon(V1))][, .(Month, Return = EOM/shift(EOM, fill = first(mat[, 2])) - 1)]
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

runs = 100

mbm = microbenchmark(
  xts = xtsfun(pmat),
  data.table = dtfun(pmat),
  quantmod = qmfun(pmat),
  times = runs
)

# Visualisation
setDT(mbm)
mbm[, `:=`(time = as.numeric(time*1e-6), expr = as.character(expr))]
ds = mbm[, .(list(density(time))), expr]

tit = paste0('TS Benchmark Runtime Distributions (ms), ', 'N = ', N, ', ', runs, ' Runs.')
p = plot_ly(type = 'scatter', mode = 'lines') %>% 
    layout(title = tit, legend = list(orientation = 'h')) %>%
    config(displaylogo = F)

for (i in ds$expr){
  x = ds[expr == i, V1[[1]]$x]
  y = ds[expr == i, V1[[1]]$y]
  p = p %>% add_trace(x = x, y = y, name = i, fill = 'tozeroy')
}

p

saveWidget(p, file = 'benchmark.html')
