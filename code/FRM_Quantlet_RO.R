## 0. Preparation

rm(list = ls(all = TRUE))

#----------------------------------------START UPDATE----------------------------------------

wdir = "D:\\PROIECTE\\FRM RO\\FRM"

#Choose between "Americas", "Europe", "Crypto", "SP500", "ER", "Asia", "EM"
channel = "RO"
#Data source
date_end_source =  20220718


#Data source
date_start = 20220103
date_end =   20220718


#Network output, fixed companies
date_start_fixed = 20220103
date_end_fixed =   20220718


#Note: fixed companies are needed to produce network gif and for analysis
#Note: allow min of s days in between date_start_source 
#and date_start, date_start_fixed

quantiles = c(0.99, 0.95, 0.90, 0.85, 0.80, 0.75, 0.70, 0.65, 0.60, 0.55, 0.50, 0.25)

#Estimation window size (63)
s = 63
#Tail risk level (0.05)
tau = 0.05

#Number of iterations (25)
I = 25
#CoStress top and bottom L (5)
L = 5

#Number of largest companies, highlighted node for network graph,
#plot parameter defined based on the outliers

  date_start_source = 20050104
  stock_main = "ROTLV.BX"
  J = 20

#-----------------------------------------END UPDATE-----------------------------------------

setwd(wdir)

input_path = paste0("Input/", channel, "/", date_start_source, "-", date_end_source)
if (tau == 0.05 & s == 63) output_path = paste0("Output/", channel) else 
  output_path = paste0("Output/", channel, "/Sensitivity/tau=", 100*tau, "/s=", s)

#TODO choose between quantile and expectile in the header
source("FRM_Statistics_Algorithm.R")

library(ggplot2)
library(data.table)
library(igraph)
require(timeDate)
library(stringr)
library(graphics)
library(magick)
library(scales)
library(tidyr)
library(dplyr)
library(zoo)
library(plotly)
library(tidyverse)
options(digits=6)


## 1. Data Preprocess

#Note: requires additional preprocessing for i.a. Crypto channel


ro_names= read.csv(file = 
        "D:\\PROIECTE\\FRM RO\\FRM\\Input\\RO\\tickers.csv", header = TRUE)

stock_prices = read_csv(file = paste0(input_path, "/", channel, "_Price_", 
                                      date_end_source, ".csv"),guess_max =20000)

stock_prices<-stock_prices%>%rename('ticker'='Date')


select<-ro_names%>% filter(str_detect(Market, "BVB"))

stock_prices<-stock_prices %>% select(dput(as.character("ticker"))|
                                        one_of(dput(as.character(select$ticker))))

valid_prices<-stock_prices %>% 
  
  summarise_all(~ sum(!is.na(.)))%>% select(-'ticker')

valid_prices<-valid_prices/nrow(stock_prices)

mktcap = read_csv(file = paste0(input_path, "/", channel, "_Mktcap_", 
                                date_end_source, ".csv"), guess_max =20000)

macro = read_csv(file = paste0(input_path, "/", channel, "_Macro_", 
                             date_end_source, ".csv"), guess_max =1000)



colnames(mktcap)[1] = "ticker"
colnames(stock_prices)[1] = "ticker"
colnames(macro)[1] = "ticker"


mktcap<-mktcap %>% select(dput(as.character("ticker"))|
                                        one_of(dput(as.character(select$ticker))))

mktcap<-mktcap%>% as.matrix()

if (!all(sort(colnames(mktcap)) == sort(colnames(stock_prices)))) 
  stop("columns do not match")
M_stock = ncol(mktcap)-1
M_macro = ncol(macro)-1
M = M_stock+M_macro


#Can potentially cause LHS==0 in the regression
#but almost certainly it will be excluded wrt mktcap 
stock_prices = na.locf(stock_prices, na.rm = FALSE)
mktcap = na.locf(mktcap, na.rm = FALSE)
write.csv((tail(mktcap,1)), paste0(output_path,  "sample.csv"), 
          row.names = FALSE, quote = FALSE)
#If missing market caps are kept NA, the column will be excluded 
#from top J  => do not interpolate in mktcap
mktcap[is.na(mktcap)] = 0

#Load the stock prices and macro-prudential data matrix
#Macros on days when stock is not traded are excluded
all_prices = merge(stock_prices, macro, by = "ticker", all.x = TRUE)
all_prices<-all_prices %>%
  mutate(ticker_str = ticker %>% 
           as.Date(format = "%m/%d/%Y"))%>%
  mutate(ticker_str = as.numeric(gsub("-", "", ticker_str)))

all_prices<- arrange(all_prices,ticker_str)
all_prices<-subset(all_prices, select = -ticker_str)

#Fill up macros on the missing days
all_prices[, (M_stock+2):(M+1)] = all_prices[,
                            (M_stock+2):(M+1)] %>% na.locf(fromLast=T)

colSums(is.na(all_prices))
#TODO: exceptions that break crypto algorithm and result in large lambda
#all_prices = all_prices[-c(377,603,716,895,896),]

ticker_str = all_prices$ticker[-1]
if (channel == "RO") ticker_str = ticker_str %>% 
  as.Date(format = "%m/%d/%Y") %>% sort()
ticker <- as.numeric(gsub("-", "", ticker_str))

N = length(ticker_str)

#Calculate the daily return and differences matrix of all selected financial 
#companies and macro-prudential variables; use exponential function for selected
#macro-prudential variables that are expressed in first order differences

all_prices$`EURIBOR3MD=`= exp(all_prices$`EURIBOR3MD=`)
#all_prices$SPBDUB3T = exp(all_prices$SPBDUB3T)
all_prices$`RORON3MD=` = exp(all_prices$`RORON3MD=` )


all_prices[, -1] = sapply(all_prices[, -1], as.numeric)

all_return = diff(log(as.matrix(all_prices[, -1])))
all_return[is.na(all_return)] = 0
all_return[is.infinite(all_return)] = 0
stock_return = all_return[, 1:M_stock]
macro_return = all_return[, (M_stock+1):M]

#Sorting the market capitalization data
FRM_sort = function(data) {sort(as.numeric(data), decreasing = TRUE,
                                index.return = TRUE)}
#Determining the index number of each company
#according to decreasing market capitalization
mktcap_index = matrix(0, N, M_stock)
mktcap_sort = apply(mktcap[-1, -1], 1, FRM_sort)
for (t in 1:N) mktcap_index[t,] = mktcap_sort[[t]]$ix
mktcap_index = cbind(ticker, mktcap_index)

## 2. Estimation


#Row index corresponding to date_start and date_end
N0 = which(ticker == date_start)
N0
N1 = which(ticker == date_end)
N1

N0_fixed = which(ticker == date_start_fixed)
N0_fixed
N1_fixed = which(ticker == date_end_fixed)

N_upd = N1-N0+1
N_fixed = N1_fixed-N0_fixed+1

## 2.1 Varying companies or coins

FRM_individ = vector(mode = "list")
J_dynamic = matrix(0, 1, N_upd)

for (t in N0:N1) { 
  #Biggest companies at each time point
  biggest_index = as.matrix(mktcap_index[t, 2:(J+1)])
  data = cbind(stock_return[(t-s+1):t, biggest_index], 
               macro_return[(t-s):(t-1),])
  #J_dynamic needed for data available for less than J stocks:

  data = data[, colSums(data != 0) > 0]
  M_t = ncol(data)
  J_t = M_t - M_macro
  J_dynamic[t-N0+1] = J_t
  #Initialize adjacency matrix
  adj_matrix = matrix(0, M_t, M_t) 
  est_lambda_t = vector()
  #FRM quantile regression
  for (k in 1:M_t) { 
    est = FRM_Quantile_Regression(as.matrix(data), k, tau, I)
    est_lambda = abs(data.matrix(est$lambda[which(est$Cgacv == min(est$Cgacv))]))
    est_beta = t(as.matrix(est$beta[which(est$Cgacv == min(est$Cgacv)),]))
    adj_matrix[k, -k] = est_beta
    est_lambda_t = c(est_lambda_t, est_lambda)
  }
  #List of vectors of different size with different column names
  est_lambda_t = t(data.frame(est_lambda_t[1:J_t]))
  colnames(est_lambda_t) = colnames(data)[1:J_t]
  FRM_individ[[t-N0+1]] = est_lambda_t
  #Save adjacency matrix
  colnames(adj_matrix) = colnames(data)
  rownames(adj_matrix) = colnames(data)
  write.csv(adj_matrix, paste0(output_path, "/Adj_Matrices/adj_matrix_", 
                              ticker[t], ".csv"), quote = FALSE)
}


## 2.2 Fixed companies or coins 

#Make companies constant, select the biggest companies 
biggest_index_fixed = as.matrix(mktcap_index[N0_fixed, 2:(J+1)])

#Note: dependent variable cannot be all 0
M_J = J+M_macro
adj_matrix_fixed = matrix(0, M_J, M_J) 
FRM_individ_fixed = matrix(0, N_fixed, J+1)
FRM_individ_fixed[, 1] = ticker[N0_fixed:N1_fixed]

for (t in N0_fixed:N1_fixed) { 
  data_fixed = cbind(stock_return[(t-s+1):t, biggest_index_fixed], 
                     macro_return[(t-s):(t-1),])
  if(all(colSums(data_fixed != 0) > 0)) {
    #FRM quantile regression
    for (k in 1:M_J) { 
      est_fixed = FRM_Quantile_Regression(as.matrix(data_fixed), k, tau, I)
      est_lambda_fixed = abs(data.matrix(est_fixed$lambda[
        which(est_fixed$Cgacv == min(est_fixed$Cgacv))]))
      est_beta_fixed = t(as.matrix(est_fixed$beta[
        which(est_fixed$Cgacv == min(est_fixed$Cgacv)),]))
      adj_matrix_fixed[k, -k] = est_beta_fixed
      if (k <= J) FRM_individ_fixed[t-N0_fixed+1, k+1] = est_lambda_fixed  
    }
    #Save adjacency matrix
    colnames(adj_matrix_fixed) = colnames(data_fixed)
    rownames(adj_matrix_fixed) = colnames(data_fixed)
    write.csv(adj_matrix_fixed, paste0(output_path, "/Adj_Matrices/Fixed/adj_matrix_", 
                                      ticker[t], ".csv"), quote = FALSE) 
  } else {
    warning("column with 0 return, check input correctness")
  }
}


## 3. Updated FRM index

names(FRM_individ) = ticker_str[N0:N1]

#Append R dataset to the historical file
{if (file.exists(paste0(output_path, "/Lambda/FRM_", channel, ".rds"))) {
  FRM_history_prev = readRDS(paste0(output_path, "/Lambda/FRM_", channel, ".rds"))
  #Uncomment if appending to the older format output
  #names(FRM_history_prev) = as.Date(names(FRM_history_prev), format = "%Y %m %d")
  #Delete to be able to overwrite
  N0_del = which(names(FRM_history_prev) == ticker_str[N0])
  N1_del = which(names(FRM_history_prev) == ticker_str[N1])
  if (length(N1_del) == 0) N1_del = length(FRM_history_prev)
  if (length(N0_del) != 0) FRM_history_prev[N0_del:N1_del] = NULL
  FRM_history = c(FRM_history_prev, FRM_individ)} 
else FRM_history = FRM_individ}
#order and unique just in case
FRM_history = FRM_history[order(unique(names(FRM_history)))]
N_h = length(FRM_history)
saveRDS(FRM_history, paste0(output_path, "/Lambda/FRM_", channel, ".rds"))


#Transform the list of lambdas into a wide dataset
stock_names = vector()
for (t in 1:N_h) stock_names = c(stock_names, attributes(FRM_history[[t]])$dimnames[[2]])
stock_names = unique(stock_names)
N_names = length(stock_names)
lambdas_wide = matrix(0, N_h, N_names+1)
for (k in 1:N_names) 
  for (t in 1:N_h) 
    if (stock_names[k] %in% attributes(FRM_history[[t]])$dimnames[[2]]) 
      lambdas_wide[t, k+1] = FRM_history[[t]][, stock_names[k]]
lambdas_wide = round(lambdas_wide, digits = 6)
lambdas_wide[, 1] = names(FRM_history)
colnames(lambdas_wide) = c("date", stock_names)
write.csv(lambdas_wide, paste0(output_path, "/Lambda/lambdas_wide.csv"), 
          row.names = FALSE, quote = FALSE)

#Saved fixed lambdas for the specified period
colnames(FRM_individ_fixed) = c("date", colnames(data_fixed)[1:J])
write.csv(FRM_individ_fixed, paste0(output_path, "/Lambda/Fixed/lambdas_fixed_", 
                            date_start_fixed, "_", date_end_fixed, ".csv"),
          row.names = FALSE, quote = FALSE)

#Calculate FRM index as the average
FRM_index = sapply(1:N_h, function(i) mean(FRM_history[[i]]))
FRM_index = round(FRM_index, digits = 6)
FRM_index = data.frame(date = names(FRM_history), frm = FRM_index)

write.csv(FRM_index, paste0(output_path, "/Lambda/FRM_", channel, "_index.csv"),
          row.names = FALSE, quote = FALSE)

########### plot FRM  ################
FRM<-FRM_index
FRM$date<-as.Date(FRM$date)
 

#FRM = FRM %>% na.locf()
FRM$risk_level <- (100 * ecdf(FRM_index$frm)(FRM_index$frm)) %>% round(digits = 2)
FRM$risk <- as.factor(ifelse(FRM$risk_level < 10, '1. Low risk',
                             ifelse(FRM$risk_level < 40, '2. General risk', 
                                    ifelse(FRM$risk_level < 70, '3. Elevated risk', 
                                           ifelse(FRM$risk_level < 90, '4. High risk', 
                                          ifelse(FRM$risk_level <= 100, '5. Severe risk',0))))))

p<-ggplot(data = FRM,aes(x=date,y=frm,text=risk_level))+
  labs(x = "Date", y="FRM@RO")+
  scale_x_date(date_breaks = "1 year" , date_labels = "%Y")+
    scale_color_manual(values = c("1. Low risk" = "green",
                                  "2. General risk"="blue",
                                  "3. Elevated risk"="yellow",
                                  "4. High risk"="orange",
                                  "5. Severe risk"="red")) +
  geom_point(aes(color =risk),size=1)
ggplotly(p)
  


p1<-ggplot(data = FRM,aes(x=date,y=frm))+
  labs(x = "Date", y="FRM@RO")+
  scale_x_date(date_breaks = "1 year" , date_labels = "%Y")+
  geom_line(color="blue")+
  annotate(geom="text", x=as.Date("2008-12-09"), y=0.1, 
 label="2008 Financial crisis", color="red")+ 

  annotate(geom="text", x=as.Date("2020-04-09"), y=0.04, 
           label="2020 Covid-19 crisis", color="red") +

  annotate(geom="text", x=as.Date("2010-07-01"), y=0.045, 
           label="2010 Budget crisis", color="red") 
  #annotate(geom="point", x=as.Date("2020-04-09"), y=0.1,
           #size=20, shape=21, fill="transparent")
  
p1


#Daily maximum
FRM_max = sapply(1:N_h, function(i) max(FRM_history[[i]]))
name_max = sapply(1:N_h, function(i) 
  attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_max[i])[1]])
FRM_max = data.frame(date = names(FRM_history), name = name_max, lambda = FRM_max)
write.csv(FRM_max, paste0(output_path, "/Lambda/max_lambda.csv"), 
          row.names = FALSE, quote = FALSE)

#Daily minimum
FRM_min = sapply(1:N_h, function(i) min(FRM_history[[i]]))
name_min = sapply(1:N_h, function(i) 
  attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_min[i])[1]])
FRM_min = data.frame(date = names(FRM_history), name = name_min, lambda = FRM_min)
write.csv(FRM_min, paste0(output_path, "/Lambda/min_lambda.csv"), 
          row.names = FALSE, quote = FALSE)

#Quantiles
for (q in quantiles) {
  FRM_q = sapply(1:N_h, function(i) quantile(FRM_history[[i]], q))
  FRM_q = data.frame(date = names(FRM_history), quantile = FRM_q)
  write.csv(FRM_q, paste0(output_path, "/Lambda/Quantiles/q", q*100, "_lambda.csv"), 
            row.names = FALSE, quote = FALSE)
}

#Risk level for the website

risk_level = (100 * ecdf(FRM_index$frm)(FRM_index$frm[N_h])) %>% round(digits = 2)
rl = data.frame(date = as.Date(as.character(date_end), format = "%Y %m %d")
                %>% format("%d/%m/%Y"), risk = risk_level)
write.csv(rl, paste0(output_path, "/Lambda/risk_level_", channel, ".csv"),
          row.names = FALSE, quote = FALSE)


## 4. Top 10 companies based on lambda at date_end for the website

top_10 <- FRM_individ[[N_upd]]
top_10 = top_10[, order(top_10, decreasing = T)]
top_10 = top_10[1:10]
top_10 = round(top_10, digits = 6)
top_10 = cbind(names(top_10), unname(top_10))


if (channel == "Crypto") colnames(top_10) = c("Coin", "Risk") else 
  colnames(top_10) = c("ticker", "Risk")
top_10=as.data.frame(top_10)

#Read a file which stores full names and abbreviations of companies

  fullnames = read.csv(file = paste0("Input/", channel, "/", 
                                     channel,"_names.csv"), header = TRUE)
  fullnames$ticker = gsub(" ", ".", fullnames$ticker)
  top_10_fullnames <-fullnames %>%
    filter(ticker %in% top_10$ticker)
top_10=merge(top_10,top_10_fullnames,by="ticker")

top_10<-top_10%>%select(-c(4,5))


 
write.csv(top_10, paste0(output_path, "/Top/top10_", date_end, "_", 
                         channel, ".csv"), row.names = FALSE, quote = FALSE)


## 5.1 Boxplot

png(paste0(output_path, "/Boxplot/Boxplot_", date_start, "_", date_end, "_", 
           channel, ".png"), width = 900, height = 600, bg = "transparent")

#outliers = which(FRM_max$lambda > lambda_cutoff)
boxplot(FRM_individ, col = "white", xaxt = "n")
lines(tail(FRM_index$frm, N_upd), col = "blue", lwd = 2)
lines(tail(FRM_max$lambda, N_upd), col = "red", lwd = 2)

div = floor(N_upd/5)
ll = c(1, div, 2*div, 3*div, 4*div, N_upd)
axis(1, at = ll, labels = names(FRM_individ)[ll])

dev.off()

## TODO: 5.2 Accumulated boxplot


## 6. Network

FRM_history = readRDS(paste0(output_path, "/Lambda/FRM_", channel, ".rds"))
FRM_individ_fixed = read.csv(paste0(output_path, "/Lambda/Fixed/lambdas_fixed_", 
                      date_start_fixed, "_", date_end_fixed, ".csv"), header = TRUE) %>% as.matrix()
FRM_index = read.csv(paste0(output_path, "/Lambda/FRM_", channel, "_index.csv"), header = TRUE)

N0_fixed_net = which(gsub("-", "", names(FRM_history)) == date_start_fixed)
N1_fixed_net = which(gsub("-", "", names(FRM_history)) == date_end_fixed)
# 
# scale_net = 500
# 
# fig = image_graph(width = 1000, height = 1000, res = 96, bg = "transparent")
# options(show.error.messages = FALSE)
# 
# for (t in N0_fixed_net:N1_fixed_net) try( {
#   adj0 = read.csv(file=paste0(output_path, "/Adj_Matrices/Fixed/adj_matrix_", 
#                               gsub("-", "", names(FRM_history)[t]), ".csv"), 
#                   header = TRUE, sep = "," , row.names = 1)
#   
#   adj0 = as.matrix(adj0)[1:J, 1:J] 
#   adj0 = apply(adj0, 2, as.numeric)
#   netw1 = graph_from_adjacency_matrix(adj0, mode = "directed", weighted = T)
#   V(netw1)$color = ifelse(V(netw1)$name == stock_main, "orange", "lightgrey")
#   colors = rep("Gray", alpha.f = .8, length(E(netw1)))
#   colors = ifelse(head_of(netw1, E(netw1))$name == stock_main, 'blue', colors) #inflow
#   colors = ifelse(tail_of(netw1, E(netw1))$name == stock_main, 'orange', colors) #outflow
#   if (channel == "SP500") colnames(adj0) = colnames(adj0) %>% substr(1, nchar(colnames(adj0))-7)
#   plot(netw1, layout = layout_in_circle, vertex.label = colnames(adj0), 
#        edge.width = 0.8, 
#        edge.color = colors, edge.arrow.size = 0.9, edge.arrow.width = 1, 
#        vertex.size = scale_net*FRM_individ_fixed[t-N0_fixed_net+1, -1])
#   title(xlab = paste0(FRM_index$date[t], "\n FRM: ", round(FRM_index$frm[t], 5)), 
#         cex.lab = 1.15, font.lab = 2, line = -0.5)
# })
# options(show.error.messages = TRUE)
# dev.off()
# 
# animation <- image_animate(fig, fps = 5)
# image_write(animation, paste0(output_path, "/Network/Network_", date_start_fixed, "_", 
#                               date_end_fixed, "_", channel, ".gif"))


## 7. Macro influence

macro_inf = matrix(0, N_h, M_macro+1)
macro_inf[, 1] = names(FRM_history)
colnames(macro)[1] = "date"
colnames(macro_inf) = colnames(macro)

for (t in 1:(N_h-1)) {
  adj0 = read.csv(file=paste0(output_path, "/Adj_Matrices/adj_matrix_", 
                              gsub("-", "", names(FRM_history)[t]), ".csv"), 
                  header = TRUE, sep = "," , row.names = 1)
  k1 = ncol(adj0)-M_macro
  for (k in 1:M_macro) macro_inf[t, k+1] = sum(adj0[1:k1, k1+k]!=0)/k1
}

write.csv(macro_inf, paste0(output_path, "/Macro/macro_influence.csv"), 
          row.names = FALSE, quote = FALSE)

macro_inf_long = gather(as.data.frame(macro_inf), macro, inf_idx, -date, 
                               convert = TRUE, factor_key = TRUE)

div = floor(N_h/7)
plot_labels_macro = names(FRM_history)[c(1, div, 2*div, 3*div, 4*div, 5*div, 6*div, N_h)]

png(paste0(output_path, "/Macro/macro_inf.png"), 
    width = 900, height = 600, bg = "transparent")

ggplot(macro_inf_long, aes(date, as.numeric(inf_idx), group=macro)) +
  geom_line(aes(color = macro), size = 1) + ylab("normalised # of non-zero betas") +
  scale_x_discrete(breaks = plot_labels_macro, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.border = element_blank(),
        legend.key=element_blank(),
        panel.grid=element_blank())

dev.off()

#Smooth the values
macro_inf_smooth = macro_inf

for (k in 1:M_macro) {
  ss = smooth.spline(gsub("-", "", macro_inf[,"date"]), macro_inf[, k+1])$y
  macro_inf_smooth[, k+1] = ifelse(ss > 0, ss, 0)
}

macro_inf_long_smooth = gather(as.data.frame(macro_inf_smooth), macro, inf_idx, -date, 
                        convert = TRUE, factor_key = TRUE)

png(paste0(output_path, "/Macro/macro_inf_smooth.png"), 
    width = 900, height = 600, bg = "transparent")

ggplot(macro_inf_long_smooth, aes(date, as.numeric(inf_idx), group=macro)) +
  geom_line(aes(color = macro), size = 1) + ylab("normalised # of non-zero betas") +
  scale_x_discrete(breaks = plot_labels_macro, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.border = element_blank(),
        legend.key=element_blank(),
        panel.grid=element_blank())

dev.off()


save.image(file='BVB_20_07_2022_5%.RData')
