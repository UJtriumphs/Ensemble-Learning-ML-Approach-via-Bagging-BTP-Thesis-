out <- read.csv("C:/Users/ashish_pc/Desktop/BTP mod/BAG.csv")


library('Metrics')
rmse(out$REAL,out$AVERAGE)
mae(out$REAL,out$AVERAGE)
mse(out$REAL,out$AVERAGE)