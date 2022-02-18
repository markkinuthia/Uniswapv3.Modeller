# Uniswapv3.Modeller
A Package for analysing UniswapV3 Data .

This is an improvement of the uniswappeR package by Omnia Analytics (https://github.com/Omni-Analytics-Group/uniswappeR), which analyszes data from V2.

I have applied the same concept and created a package for V3 that analyzes various aspects of the Uniswap V3 protocol.


##Walkthrough
Ensure the below packages are pre-intalled 
###library(MASS)
###library(scales)
###library(ghql)
###library(jsonlite)
###library(tidyr)
###library(dplyr)
###library(ggplot2)
###library(lubridate)


###Example 1
factory_stats_v3(user_address = "0x0000000000000000000000000000000000000000")

Result: Information below 
 owner       id    poolCount txCount totalVolumeUSD totalVolumeETH totalFeesUSD
  <chr>       <chr> <chr>     <chr>   <chr>          <chr>          <chr>       
1 0x00000000~ 0x1F~ 5863      116977~ 426285832403.~ 133231342.340~ 802287125.5~

 ###Example 2
Tick_stats_v3(pool_address = "0xc2e9f25be6257c210d7adf0d4cd6e3e881ba25f8")

Result: Column names->
[1] "id"             "poolAddress"    "pool"           "tickIdx"       
[5] "liquidityGross" "liquidityNet"   "price0"         "price1"        
[9] "volumeToken0"   "volumeToken1"   "volumeUSD"

###Example 3
uniswap_day_stats_v3() 

 Result: Column names->  
[1] "id"                 "date"               "volumeUSD"         
[4] "volumeETH"          "volumeUSDUntracked" "feesUSD"           
[7] "txCount"            "tvlUSD"
  
 ###Example 4
 token_stats_v3(token_address = "0x1f9840a85d5af5bf1d1762f925bdaddc4201f984")
 
 Result : Summary 
  'data.frame':	1 obs. of  14 variables:
 $ id                          : chr "0x1f9840a85d5af5bf1d1762f925bdaddc4201f984"
 $ symbol                      : chr "UNI"
 $ name                        : chr "Uniswap"
 $ decimals                    : chr "18"
 $ totalSupply                 : chr "18776"
 $ volume                      : chr "133679251.051730122489432702"
 $ untrackedVolumeUSD          : chr "3143460824.007341324588614067893697"
 $ feesUSD                     : chr "9450362.568624226792239932600553117"
 $ txCount                     : chr "105504"
 $ poolCount                   : chr "0"
 $ totalValueLocked            : chr "2312204.768539120523324687"
 $ totalValueLockedUSD         : chr "23726048.22037924850503726020019942"
 $ totalValueLockedUSDUntracked: chr "0"
 $ derivedETH                  : chr "0.003675081597148297925570648492286579"
  
  
 ###Example 5
 Token_stats_daily_v3(token_address = "0x1f9840a85d5af5bf1d1762f925bdaddc4201f984")
  
  Result : Column names
 [1] "id"                  "date"                "token"              
 [4] "volume"              "volumeUSD"           "untrackedVolumeUSD" 
 [7] "totalValueLocked"    "totalValueLockedUSD" "priceUSD"           
[10] "feesUSD"             "open"                "high"               
[13] "low"                 "close"
