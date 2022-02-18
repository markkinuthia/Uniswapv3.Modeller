
################Vis uniswap_stats_hist#######################

vis_uniswap_stats_hist_v3 <- function()
{
  plot_data <- uniswap_day_stats_v3()
  plot_data$Date <- as_date(as_datetime(plot_data$date))

  plot_data_long <- gather(plot_data, key="met_nam", value="met_val", c( "dailyVolumeETH" , "dailyVolumeUSD" ,"totalLiquidityETH", "totalLiquidityUSD" ,"txCount"))
  plot_data_long <- plot_data_long[,c("Date","met_nam","met_val")]
  plot_data_long$met_val <- as.numeric(plot_data_long$met_val)
  variable_names <- list(
    "volumeETH" = "Daily Volume (ETH)" ,
    "volumeUSD" = "Daily Volume (USD)" ,
    "feesUSD" = "Fees charged (USD)",
    "tvlUSD" = "Total Value Locked (USD)",
    "txCount"  = "Transaction Count"
  )
  variable_labeller <- function(variable,value) return(variable_names[value])

  ggplot(plot_data_long, aes(x=Date, y=met_val)) +
    geom_area()+
    facet_wrap(~met_nam, scales="free_y", ncol=1, labeller= variable_labeller)+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
    labs(x = "Date", y = "Value")+
    ggtitle("Uniswap Platform Growth")+
    theme(plot.title = element_text(hjust = 0.5))

}

###############################Token Stats

#####vis_token_stats_hist_v2(token_address = "0x1f9840a85d5af5bf1d1762f925bdaddc4201f984")######

vis_token_stats_hist_v2 <- function(token_address = "0x1f9840a85d5af5bf1d1762f925bdaddc4201f984")
{
  plot_data <- Token_stats_daily_v3(token_address)
  plot_data$Date <- as_date(as_datetime(plot_data$date))

  plot_data_long <- gather(plot_data, key="met_nam", value="met_val", c( "volume" , "volumeUSD" ,"totalValueLocked" , "untrackedVolumeUSD" , "totalValueLockedUSD" ,"priceUSD" ,"feesUSD" ,
                                                                         "open" , "high" , "low" ,"close" ))
  plot_data_long <- plot_data_long[,c("Date","met_nam","met_val")]
  plot_data_long$met_val <- as.numeric(plot_data_long$met_val)
  variable_names <- list(
    "volume" = "Volume of Token 1" ,
    "volumeUSD" = "Volume of Token 1 (USD)" ,
    "totalValueLocked" = "Total Value Locked",
    "untrackedVolumeUSD" = "Untracked Volume (USD)",
    "totalValueLockedUSD" = "Total Value Locked (USD)",
    "priceUSD" = "Price of Token (USD)",
    "feesUSD" = "fees charge in USD",
    "open"  = "open",
    "high"  = "high",
    "low"   = "low",
    "close" = "close"
  )
  variable_labeller <- function(variable,value) return(variable_names[value])

  ggplot(plot_data_long, aes(x=Date, y=met_val)) +
    geom_area()+
    facet_wrap(~met_nam, scales="free_y", ncol=1, labeller= variable_labeller)+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
    #scale_y_log10(breaks = trans_breaks("log10", function(y) 10^y),
    #labels = trans_format("log10", math_format(10^.y)))
    labs(x = "Date", y = "Value")+
    ggtitle("Token Growth")+
    theme(plot.title = element_text(hjust = 0.5))

}


######################Pool Hourly stats visualization
vis_Pool_Daily_Ave_stats_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  plot_data <- pool_hourly_Stats_v3(pool_address)
  plot_data$Date <- as_date(as_datetime(plot_data$periodStartUnix))
  Token0_Sym <- unique(plot_data$pool$token0$symbol)
  Token1_Sym <- unique(plot_data$pool$token1$symbol)


  plot_data_long <- gather(plot_data, key="met_nam", value="met_val", c( "liquidity" , "sqrtPrice" ,"token0Price" , "token1Price","feeGrowthGlobal0X128" ,"feeGrowthGlobal1X128","tvlUSD" , "open" , "high"))
  plot_data_long <- plot_data_long[,c("Date","met_nam","met_val")]
  plot_data_long$met_val <- as.numeric(plot_data_long$met_val)
  variable_names <- list(
    "liquidity" = "Pool liquidity" ,
    "sqrtPrice" = "Square-root of Price" ,
    "token0Price" = paste0(Token0_Sym,"\'s Price"),
    "token1Price" = paste0(Token1_Sym,"\'s Price"),
    "feeGrowthGlobal0X128" = paste0(Token0_Sym,"\'s Fees Growth"),
    "feeGrowthGlobal1X128" = paste0(Token1_Sym,"\'s Fees Growth"),
    "tvlUSD" = "Total Value Locked (USD)",
    "open" =  "Open Price",
    "high" =  "High Price"
  )
  variable_labeller <- function(variable,value) return(variable_names[value])
  plot_data_long<-group_by(plot_data_long,Date,met_nam)%>%summarise(mean(met_val))
  colnames(plot_data_long)<-c("Date","met_nam","met_val")

  ggplot(plot_data_long, aes(x=Date, y=met_val)) +
    geom_line()+
    facet_wrap(~met_nam, scales="free_y", ncol=1, labeller= variable_labeller)+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
    #scale_y_log10(breaks = trans_breaks("log10", function(y) 10^y),
    #labels = trans_format("log10", math_format(10^.y)))
    labs(x = "Date", y = "Value")+
    ggtitle("Pool Daily Average Stats")+
    theme(plot.title = element_text(hjust = 0.5))

}


###########################################################################################
############################################################################################
####Swap , Burn and Mint visualizations #####

vis_SMB_v3 <- function(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")
{
  plot_data_1<- pool_swap_v3(pool_address)
  plot_data_3<- pool_mint_v3(pool_address)
  plot_data_1$Date <- as_date(as_datetime(as.numeric(plot_data_1$timestamp)))
  plot_data_3$Date <- as_date(as_datetime(as.numeric(plot_data_3$timestamp)))
  Token0_Sym <- unique(plot_data_1$token0$symbol)
  Token1_Sym <- unique(plot_data_1$token1$symbol)


  plot_data_long_1 <- gather(plot_data_1, key="met_nam", value="met_val", c( "amount0" , "amount1" , "amountUSD"))
  plot_data_long_3 <- gather(plot_data_3, key="met_nam", value="met_val", c( "amount0" , "amount1" , "amountUSD"))
  plot_data_long_1 <- plot_data_long_1[,c("Date","met_nam","met_val")]
  plot_data_long_3 <- plot_data_long_3[,c("Date","met_nam","met_val")]

  plot_data_long_1$met_val <- as.numeric(plot_data_long_1$met_val)
  plot_data_long_3$met_val <- as.numeric(plot_data_long_3$met_val)
  plot_data_long_1<-group_by(plot_data_long_1,Date,met_nam)%>%summarise(sum(met_val))
  colnames(plot_data_long_1)<-c("Date","met_nam","met_val")
  plot_data_long_3<-group_by(plot_data_long_3,Date,met_nam)%>%summarise(sum(met_val))
  colnames(plot_data_long_3)<-c("Date","met_nam","met_val")



  variable_names <- list(
    "amount0" = paste0(Token0_Sym,"\ Net Amount") ,
    "amount1" = paste0(Token1_Sym,"\ Net Amount") ,
    "amountUSD" = "Aggregate Amount in (USD)"
  )
  variable_labeller <- function(variable,value) return(variable_names[value])

  ggplot(plot_data_long_1, aes(x=Date, y=met_val)) +
    geom_line()+
    facet_wrap(~met_nam, scales="free_y", ncol=1, labeller= variable_labeller)+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
    labs(x = "Date", y = "Number of Pairs")+
    ggtitle("Swap Daily Net Amounts")+
    theme(plot.title = element_text(hjust = 0.5))

  ggplot(plot_data_long_3, aes(x=Date, y=met_val)) +
    geom_line()+
    facet_wrap(~met_nam, scales="free_y", ncol=1, labeller= variable_labeller)+
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
    labs(x = "Date", y = "Number of Pairs")+
    ggtitle("Mint Daily Net Amounts")+
    theme(plot.title = element_text(hjust = 0.5))

}

#####################################################################################


##################PolygonStats_Transactions
Polygon_Addresses <- c("0x1f98431c8ad98523631ae4a59f267346ea31f984",
                       "0x91ae842a5ffd8d12023116943e72a606179294f3",####Uniswap V3: LP Descriptor NFT##
                       "0xa5644e29708357803b5a882d272c41cc0df92b34",####Uniswap V3: Migrator##
                       "0x42b24a95702b9986e82d421cc3568932790a48ec",####Uniswap V3: NFT Descriptor##
                       "0xc36442b4a4522e871399cd717abdd847ab11fe88",####Uniswap V3: Positions NFT##
                       "0xb753548f6e010e7e680ba186f9ca1bdab2e90cf2",####Uniswap V3: Proxy Admin##
                       "0xe592427a0aece92de3edee1f18e0157c05861564")####Uniswap V3: Router##

#####################################################################################

#' vis_pair_liq_positions_v2(pair_address = "0xf00e80f0de9aea0b33aa229a4014572777e422ee")
vis_pair_liq_positions_v2 <- function(pair_address = "0xf00e80f0de9aea0b33aa229a4014572777e422ee")
{
  plot_data <- pair_liq_positions_v2(pair_address)
  plot_data$liquidityTokenBalance <- as.numeric(plot_data$liquidityTokenBalance)
  plot_data <- plot_data[plot_data$liquidityTokenBalance>0,]

  ggplot(plot_data, aes(x=liquidityTokenBalance)) +
    geom_histogram()+
    scale_x_continuous(breaks = pretty(plot_data$liquidityTokenBalance, n = 20))+
    labs(x = "Liquidity Token Balance", y = "Number of Holders")+
    ggtitle("Liquidity Token Distribution")+
    theme(plot.title = element_text(hjust = 0.5))

}


