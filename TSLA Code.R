library(tidyquant)
library(timetk)
library(bayestestR)
library(rstanarm)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(data.table)
library(triangle)

# First, estimate relationship g_TSLA ~ g_QCOM for available data on TSLA
# Then, using QCOM data after tech bubble, find predicitve distribution of E[g_TSLA | g_QCOM] to then get predictive distribution of terminal values

financials <- read.csv("TSLA_Financials.csv")
finance.qcom <- subset(financials, Company == "QCOM")
finance.tsla <- subset(financials, Company == "TSLA")
QCOM <- tq_get("QCOM",
               from = '1998-01-01',
               to = '2003-01-01',
               get = "stock.prices")
TSLA <- tq_get("TSLA",
               from = '2019-01-01',
               to = '2021-07-01',
               get = "stock.prices")


g_qcom <- 27L
g_tsla <- 9L

time.norm <- 1:6

# Create data.frame for regression with time-adjusted variables
for(t in 1:27){
  g_qcom[t] <- (finance.qcom$UFCF[t+1] - finance.qcom$UFCF[t])/finance.qcom$UFCF[t]
}
for(t in 1:9){
  g_tsla[t] <- (finance.tsla$UFCF[t+1] - finance.tsla$UFCF[t])/finance.tsla$UFCF[t]
}

# Create data.frame for time-normalized growth rates (Centered around t_TSLA_2021 = t_QCOM_2000)
gs.df <- data.frame("Time" = time.norm,
                    "TSLA" = g_tsla[4:9],
                    "QCOM" = g_qcom[1:6],
                    "indicator.t3" = ifelse(time.norm == 3,1,0))

# Sample size n = 4000
model1 <- stan_glm(TSLA ~ -1 + QCOM + indicator.t3,
                   data = gs.df)
model1.posteriors <- as.data.frame(model1)
describe_posterior(model1.posteriors)

# Get predictive distribution over "future" QCOM growth rates
newdat <- data.frame("Time" = 7:26,
                     "QCOM" = g_qcom[7:26],
                     "indicator.t3" = rep(0,20))
model1.predict <- posterior_predict(model1, newdata = newdat)

# Sample WACC from triangle distribution
WACC <- rtriangle(n=4000, a = 0.075, b = 0.085)

# Create value function
market_value <- function(FCF,fc_length,g_predict,g_mean,w){
  g.sum <- 0
  for(t in 1:fc_length){
    g.prod <- prod(g_predict[1:t])
    g.sum <- g.sum + (g.prod - (1+g_mean)^t)/((1+w)^t)
  }
  V <- FCF*(g.sum + ((1+g_mean)/(w-g_mean)))
  return(V)
}

# Generate predictive distribution of market cap
# Sample size is initialized in Market_Values

Market_Values <- 10000L
for(i in 1:10000){
  g_values <- 20L
  for(j in 1:20){
    g_values[j] <- sample(model1.predict[,j],size=1)
  }
  Market_Values[i] <- market_value(FCF = finance.tsla$UFCF[10],
                                   fc_length = 20,
                                   g_predict = g_values,
                                   g_mean = 0.01,
                                   w = sample(WACC,size=1))
}
Share_Values <- Market_Values/963.33
Share_Values <- subset(Share_Values, Share_Values > 0)
describe_posterior(Share_Values, centrality = c("Median","MAP"))

map_estimate(Share_Values)








