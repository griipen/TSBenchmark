# TSBenchmark: Performance Benchmarking of TS libraries in R
# --- https://github.com/griipen/TSBenchmark
# --- Author: Jacob GRAPENDAL (jacob@grapendal.eu)

# Simple period return conversion: Daily Prices to Monthly Returns
# --- Input: Nx2 object with columns (day, price) 
# --- Output: Nx2 object with columns (month, return)

rm(list = ls()); gc()
setwd("~/Documents/R/TSBenchmark")

library(data.table) 
library(zoo)
library(xts)
library(quantmod)
library(plotly)
library(htmlwidgets)
library(dplyr)
library(tictoc)

# Output functions

      # 1. xts standalone function
      xtsfun = function(xtsdf){
        eom_prices = to.monthly(xtsdf)[, 4]
        mret = eom_prices/lag.xts(eom_prices) - 1; mret[1] = eom_prices[1]/xtsdf[1] - 1
        mret
      }
      
      # 2. data.table standalone function
      dtfun = function(dt){
        dt[, .(EOM = last(V2)), .(Month = as.integer(V1/100L))][, .(Month, Return = EOM/shift(EOM, fill = dt[, first(V2)]) - 1)]
      }
      
      # 3. dplyr standalone function
      tidyfun = function(df){
        df %>% 
          group_by(Month = as.yearmon(V1)) %>% 
          slice(n()) %>% 
          ungroup() %>%
          mutate(Return = V2/lag(V2, default = df[1, 2]) - 1) %>% 
          select(Month, Return)
      }
      
      # 4. base R
      basefun = function(df){
        out = aggregate(df, list(Month = as.yearmon(df$V1)), function(x) tail(x, 1))[, -2]
        out$Return = out$V2/c(df[1,2], out$V2[-nrow(out)]) - 1
        out[, -2]
      }
      
      # 5. quantmod (black box)
      qmfun = function(xtsdf){
        monthlyReturn(xtsdf)
      }

# Benchmark size
size = c(1e4, 1e5, 1e6)

# Initiate Benchmark
bmlist = rbindlist(lapply(size, function(N){
    
      # Asset params
      spot = 100
      r = 0.01
      sigma = 0.02
      
      pmat = data.frame( # base/dplyr input format
        V1 = seq.Date(as.Date('1970-01-01'), by = 1, length.out = N),
        V2 = spot * exp(cumsum((r - 0.5 * sigma**2) * 1/N + (sigma * (sqrt(1/N)) * rnorm(N, mean = 0, sd = 1))))
      )
      
      pmat_dt = data.table(pmat)[, V1 := as.integer(gsub('-', '', V1))] # data.table input format
      pmat_xts = xts(pmat[,2], order.by = pmat[,1]) # xts/quantmod input format
      
      # Verify output equivalence:
      eqv = all.equal(
        as.numeric(xtsfun(pmat_xts)),
        unlist(dtfun(pmat_dt)[, Return]),
        tidyfun(pmat)$Return,
        basefun(pmat)$Return,
        as.numeric(qmfun(pmat_xts)),
        scale = NULL
      )
      
      if((eqv)) {
        print('Function outputs equivalent, starting benchmark')} else {print('Function outputs differ, please revise'); return()}
      
      # Benchmark
      library(microbenchmark)
      options(scipen = 999)
      gc()
      
      runs = 100
      
      mbm = microbenchmark(
        xts = xtsfun(pmat_xts),
        data.table = dtfun(pmat_dt),
        dplyr = tidyfun(pmat),
        base = basefun(pmat),
        quantmod = qmfun(pmat_xts),
        times = runs
      )
      
      setDT(mbm)
      mbm[, `:=`(time = as.numeric(time*1e-6), expr = as.character(expr), N = N)]
}))

x = factor(bmlist$expr, levels = c('data.table', 'quantmod', 'xts', 'dplyr', 'base'))
y = bmlist$time
col = as.factor(bmlist$N)

p = plot_ly(x = ~x, y = ~y, color = ~col, type = 'box') %>% 
    layout(title = 'TS Benchmark Results (100 runs)', boxmode = 'group', 
           xaxis = list(title = ''), 
           yaxis = list(type = "log", title = 'ms, log scale'),
           margin = list(t = 100),
           legend = list(title = list(text = '<b>   Input N</b>')))

saveRDS(p, 'fig.RDS')
saveWidget(p, 'TSBenchmark.html')

