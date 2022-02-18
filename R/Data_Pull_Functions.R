library(MASS)
library(scales)
library(ghql)
library(jsonlite)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

#############Factory Stats
factory_stats_v3 <- function(user_address = "0x0000000000000000000000000000000000000000")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]
  fromJSON(con$exec(qry$queries$factory_stats,list(userAdd = user_address)))$data$factories%>%as_tibble()
}


###Ticks_stats

Tick_stats_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]
  pair_data<-fromJSON(con$exec(qry$queries$Tick_stats,list(poolAdd = pool_address)))$data$pools$ticks[[1]]
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:11)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}

####UniswapDayStats##############
#################################
uniswap_day_stats_v3 <- function()
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  uni_data <- data.frame()
  while(TRUE)
  {
    uni_data_t <- fromJSON(con$exec(qry$queries$uni_stats_hist,list(timestamp=c_timestamp)))$data$uniswapDayDatas
    if(length(uni_data_t)==0) break()
    uni_data <- bind_rows(uni_data,uni_data_t)
    c_timestamp <- as.numeric(tail(uni_data_t$date,1))
    message(paste0("Fetched ",nrow(uni_data)," Entries"))
  }
  uni_data<-data.frame(apply(uni_data, 2, as.numeric))
  return(uni_data)
}


################################################################################
###Uniswap Functions
################################################################################


token_stats_v3 <- function(token_address = "0x1f9840a85d5af5bf1d1762f925bdaddc4201f984")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]
  fromJSON(con$exec(qry$queries$Token_stats,list(tokenAdd = token_address)))$data$tokens%>%str()
}


#Daily Token Stats

Token_stats_daily_v3 <- function(token_address = "0x1f9840a85d5af5bf1d1762f925bdaddc4201f984")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$Token_daily_stats,list(tokenAdd = token_address,timestamp=c_timestamp)))$data$tokens$tokenDayData[[1]]
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$date,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:14)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}


#Token Hourly Stats
token_stats_hourly_v3 <- function(token_address = "0x0001fcbba8eb491c3ccfeddc5a5caba1a98c4c28")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$Token_Hourly_stats,list(tokenAdd = token_address,timestamp=c_timestamp)))$data$tokenHourDatas
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$hourStartUnix,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:14)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}

################Swaps, Mints, Burns
pool_swap_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$swaps_pool,list(poolAdd = pool_address,timestamp=c_timestamp)))$data$pools$swaps[[1]]
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$timestamp,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:9)]
  df2<-data.frame(apply(pair_data[,c(10:14)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}



pool_mint_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$mints_pool,list(poolAdd = pool_address,timestamp=c_timestamp)))$data$pools$mints[[1]]
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$timestamp,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:7)]
  df2<-data.frame(apply(pair_data[,c(8:13)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}




pool_burn_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$mints_pool,list(poolAdd = pool_address,timestamp=c_timestamp)))$data$pools$mints[[1]]
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$timestamp,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:7)]
  df2<-data.frame(apply(pair_data[,c(8:13)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}



########Pool Hourly stats #########################################################################

pool_hourly_Stats_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$pool_stats_hist_hourly,list(poolAdd = pool_address,timestamp=c_timestamp)))$data$pools$poolHourData[[1]]
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$periodStartUnix,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:18)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}


##############Collects based on Pool Data ########################################

Collect_stats <- function(pool_address = "0x0001fcbba8eb491c3ccfeddc5a5caba1a98c4c28")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  id_last = ""
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$Collect_stats,list(poolAdd = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8",idlast=id_last)))$data$pools$collects[[1]]
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    id_last <- tail(pair_data_t$id,1)
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  return(pair_data)
}

###################Swaps , Mints , Burns and flashes based on Transaction#####################

swap_gas_v3 <- function(pool_address = "0x0001fcbba8eb491c3ccfeddc5a5caba1a98c4c28")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$swap_Transaction_stats,list(poolAdd = pool_address,timestamp=c_timestamp)))$data$transactions
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$timestamp,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:5)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}



mint_gas_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$Mint_Transaction_stats,list(poolAdd = pool_address,timestamp=c_timestamp)))$data$transactions
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$timestamp,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:5)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}




burn_gas_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$Burn_Transaction_stats,list(poolAdd = pair_address,timestamp=c_timestamp)))$data$transactions
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$timestamp,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:5)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}



flash_gas_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$Flash_Transaction_stats,list(poolAdd = pool_address,timestamp=c_timestamp)))$data$transactions
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$timestamp,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:5)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}



#####################################################################################################
########Daily Tick Statistics
tick_stats_daily_v3 <- function(pool_address = "0xf00e80f0de9aea0b33aa229a4014572777e422ee")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]

  ## Loop historical
  c_timestamp <- as.integer(Sys.time())
  pair_data <- data.frame()
  while(TRUE)
  {
    pair_data_t <- fromJSON(con$exec(qry$queries$Tick_Daily_stats,list(poolAdd = pool_address,timestamp=c_timestamp)))$data$tickDayDatas
    if(length(pair_data_t)==0) break()
    pair_data <- bind_rows(pair_data,pair_data_t)
    c_timestamp <- as.numeric(tail(pair_data_t$date,1))
    message(paste0("Fetched ",nrow(pair_data)," Entries"))
  }
  df1<-pair_data[,c(1:3)]
  df2<-data.frame(apply(pair_data[,c(4:11)], 2, as.numeric))
  pair_data<-cbind(df1,df2)
  return(pair_data)
}

#####################################################################################################
##############User current position

user_current_position_v3 <- function(user_address = "0xb650131a49cb1193a359c2674e3564eb366fcfaf")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]
  ## Loop historical
  id_last = ""
  lp_data <- data.frame()
  while(TRUE)
  {
    lp_data_t <- fromJSON(con$exec(qry$queries$Position_user_current,list(userAdd = user_address,idlast=id_last)))$data$position%>%as_tibble()
    if(length(lp_data_t)==0) break()
    lp_data <- bind_rows(lp_data,lp_data_t)
    id_last <- tail(lp_data_t$id,1)
    message(paste0("Fetched ",nrow(lp_data)," Entries"))
  }
  return(lp_data)
}



user_position_history_v3 <- function(user_address = "0xb650131a49cb1193a359c2674e3564eb366fcfaf")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]
  ## Loop historical
  id_last = ""
  lp_data <- data.frame()
  while(TRUE)
  {
    lp_data_t <- fromJSON(con$exec(qry$queries$Position_hist_user,list(userAdd = user_address,idlast=id_last)))$data$positionSnapshots
    if(length(lp_data_t)==0) break()
    lp_data <- bind_rows(lp_data,lp_data_t)
    id_last <- tail(lp_data_t$id,1)
    message(paste0("Fetched ",nrow(lp_data)," Entries"))
  }
  return(lp_data)
}

######################################################################################################
#########Swap, Mints, burns & Flash Transactions

user_swaps.Trans_v3 <- function(user_address = "0x56178a0d5f301baf6cf3e1cd53d9863437345bf9")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]
  ## Loop historical
  id_last = ""
  tx_data <- data.frame()
  while(TRUE)
  {
    tx_data_t <- fromJSON(con$exec(qry$queries$swap_user,list(userAdd = user_address,idlast=id_last)))$data$swaps
    if(length(tx_data_t)==0) break()
    tx_data <- bind_rows(tx_data,tx_data_t)
    id_last <- tail(tx_data$id,1)
    message(paste0("Fetched ",nrow(tx_data)," Entries"))
  }
  return(tx_data)
}



user_mint.Trans_v3 <- function(user_address = "0x56178a0d5f301baf6cf3e1cd53d9863437345bf9")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]
  ## Loop historical
  id_last = ""
  tx_data <- data.frame()
  while(TRUE)
  {
    tx_data_t<-fromJSON(con$exec(qry$queries$mint_user,list(userAdd = user_address,idlast=id_last)))$data$mints
    if(length(tx_data_t)==0) break()
    tx_data <- bind_rows(tx_data,tx_data_t)
    id_last <- tail(tx_data$id,1)
    message(paste0("Fetched ",nrow(tx_data)," Entries"))
  }
  return(tx_data)
}



user_burn.Trans_v3 <- function(user_address = "0x56178a0d5f301baf6cf3e1cd53d9863437345bf9")
{
  qcon <- initialize_queries()
  con <- qcon[[1]]
  qry <- qcon[[2]]
  ## Loop historical
  id_last = ""
  tx_data <- data.frame()
  while(TRUE)
  {
    tx_data_t<-fromJSON(con$exec(qry$queries$burn_user,list(userAdd = user_address,idlast=id_last)))$data$burns
    if(length(tx_data_t)==0) break()
    tx_data <- bind_rows(tx_data,tx_data_t)
    id_last <- tail(tx_data$id,1)
    message(paste0("Fetched ",nrow(tx_data)," Entries"))
  }
  return(tx_data)
}


########################################################################################################
########################################################################################################
