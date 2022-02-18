
initialize_queries <- function()
{
  ## Initialize Client Connection
  con <- GraphqlClient$new("https://api.thegraph.com/subgraphs/name/ianlapham/uniswap-v3-subgraph")

  ## Prepare New Query
  qry <- Query$new()

  ##################################################################
  ##################################################################
  ## Stats of a Token
  ##################################################################
  qry$query(
    'Token_stats',
    'query Token_stats($tokenAdd: String!)
        {
            tokens(where: {id:$tokenAdd})
            {
                   id
                   symbol
                   name
                   decimals
                   totalSupply
                   volume
                   untrackedVolumeUSD
                   feesUSD
                   txCount
                   poolCount
                   totalValueLocked
                   totalValueLockedUSD
                   totalValueLockedUSDUntracked
                   derivedETH

            }
        }'
  )
  ########################################################################
  ########################################################################
  ####Daily Token Stats
  #######################################################################
  qry$query(
    'Token_daily_stats',
    'query Token_daily_stats($tokenAdd: String!,$timestamp: Int!)
        {
           tokens(where:{id:$tokenAdd})
              {
                tokenDayData(orderBy: date, orderDirection: desc,first:1000,where:{date_lt:$timestamp})
                {
                       id
                       date
                       token {
                            id
                            symbol
                            name
                             }
                       volume
                       volumeUSD
                       untrackedVolumeUSD
                       totalValueLocked
                       totalValueLockedUSD
                       priceUSD
                       feesUSD
                       open
                       high
                       low
                       close
                     }
                }
        }'
  )
  #####################################################################################
  #####################################################################################
  ###Token hourly stats
  #####################################################################################
  qry$query(
    'Token_Hourly_stats',
    'query Token_Hourly_stats($tokenAdd: String!,$timestamp: Int!)
             {
                tokenHourDatas(orderBy: periodStartUnix, orderDirection: desc,first:1000,where:{periodStartUnix_lt:$timestamp})
                {
                id
                periodStartUnix
                token(where: {id: $tokenAdd})
                    {
                id
                symbol
                name
                    }
                volume
                volumeUSD
                untrackedVolumeUSD
                totalValueLocked
                totalValueLockedUSD
                priceUSD
                feesUSD
                open
                high
                low
                close
            }
        }'
  )
  #########################################################################################################
  ##################################################################
  ## Swaps based on Pool Data
  ##################################################################
  qry$query(
    'swaps_pool',
    'query swaps_pool($poolAdd: String!,$timestamp: Int!)
        {
            pools(where:{id:$poolAdd})
            {
                swaps(orderBy: timestamp, orderDirection: desc,first:1000,where:{timestamp_lt:$timestamp})
                {
                        id
                        transaction {
                                      id
                                     }
                        timestamp
                        pool {
                              id
                             }
                        token0 {
                                id
                                symbol
                                name
                               }
                        token1 {
                                id
                                symbol
                                name
                               }
                        sender
                        recipient
                        origin
                        amount0
                        amount1
                        amountUSD
                        sqrtPriceX96
                        tick
                }
            }
        }'
  )
  ##################################################################
  ##################################################################
  ##################################################################
  ## Mints based on Pool Data
  ##################################################################
  qry$query(
    'mints_pool',
    'query mints_pool($poolAdd: String!,$timestamp: Int!)
        {
            pools(where:{id:$poolAdd})
            {
                mints(orderBy: timestamp, orderDirection: desc,first:1000,where:{timestamp_lt:$timestamp})
                {
                    id
                    transaction
                    timestamp
                    pool {
                           id
                         }
                    token0 {
                           id
                           symbol
                           name
                           }
                    token1 {
                           id
                           symbol
                           name
                           }
                    owner
                    origin
                    amount
                    amount0
                    amount1
                    amountUSD
                    tickLower
                    tickUpper

                }
            }
        }'
  )
  ##################################################################
  ##################################################################


  ##################################################################
  ## Burn based on Pool Data
  ##################################################################
  qry$query(
    'burns_pool',
    'query burns_pool($poolAdd: String!,$timestamp: Int!)
        {
            pools(where:{id:$poolAdd})
            {
                burns(orderBy: timestamp, orderDirection: desc,first:1000,where:{timestamp_lt:$timestamp})
                {
                        id
                        transaction {
                                      id
                                     }
                        timestamp
                        pool {
                              id
                             }
                        token0 {
                                id
                                symbol
                                name
                               }
                        token1 {
                                id
                                symbol
                                name
                               }
                        owner
                        origin
                        amount0
                        amount1
                        amountUSD
                        sqrtPriceX96
                        tick
                }
            }
        }'
  )
  ##################################################################
  ##################################################################
  ####PoolHour Data summary
  ##################################################################
  qry$query(
    'pool_stats_hist_hourly',
    'query pool_stats_hist_hourly($poolAdd: String!,$timestamp: Int!)
        {
            pools(where:{id:$poolAdd})
            {
                poolHourData(orderBy: periodStartUnix, orderDirection: desc,first:1000,where:{periodStartUnix_lt:$timestamp })
                {
                    id
                    periodStartUnix
                    pool {
                          id
                    token0 {
                           id
                           symbol
                           name
                           }
                    token1 {
                           id
                           symbol
                           name
                           }
                          }
                   liquidity
                   sqrtPrice
                   token0Price
                   token1Price
                   tick
                   feeGrowthGlobal0X128
                   feeGrowthGlobal1X128
                   tvlUSD
                   volumeUSD
                   volumeToken0
                   volumeToken1
                   feesUSD
                   txCount
                   open
                   high

                }
            }
        }'
  )
  ##################################################################
  ## PoolDay data summary
  ##################################################################
  qry$query(
    'Pool_stats_hist_daily',
    'query Pool_stats_hist_daily($poolAdd: String!,$timestamp: Int!)
        {
           pools(where:{id:$poolAdd})
          {
            poolDayDatas(orderBy: date, orderDirection: desc,first:1000,where:{date_lt:$timestamp, id:$poolAdd})
            {
                  id
                  date
                  pool {
                        id
                         }
                  liquidity
                  sqrtPrice
                  token0Price
                  token1Price
                  tick
                  feeGrowthGlobal0X128
                  feeGrowthGlobal1X128
                  tvlUSD
                  volumeUSD
                  volumeToken0
                  volumeToken1
                  feesUSD
                  txCount
               }
            }
        }'
  )
  ##########################################################################################
  ##########################################################################################
  ##################################################################
  ## Tick based on pool data
  ##################################################################
  qry$query(
    'Tick_stats',
    'query Tick_stats($poolAdd: String!)
        {
          pools(where: {id: $poolAdd})
           {
            ticks
            {
                id
                poolAddress
                pool{
                    id
                    }
                tickIdx
                pool
                liquidityGross
                liquidityNet
                price0
                price1
                volumeToken0
                volumeToken1
                volumeUSD
                untrackedVolumeUSD
                feesUSD
                collectedFeesToken0
                collectedFeesToken1
                collectedFeesUSD
                createdAtTimestamp
                createdAtBlockNumber
                liquidityProviderCount
                feeGrowthOutside0X128
                feeGrowthOutside1X128
              }
           }
        }'
  )
  #######################################################################################
  #######################################################################################
  ##################################################################
  ## Collects based on pool data
  ##################################################################
  qry$query(
    'Collect_stats',
    'query Collect_stats($poolAdd: String!)
        {
          pools(where: {id: $poolAdd})
           {
            collects
            {
              id
              transaction {
                           id
                          }
              timestamp
              pool {
                    id
                   }
              owner
              amount0
              amount1
              amountUSD
              tickLower
              tickUpper
              }
           }
        }'
  )
  #####################################################################################
  ###Flash loans based on pools
  ####################################################################################
  qry$query(
    'Flash',
    'query Flash
        {
            pools(where: {id: $poolAdd})
             {
               flashes
               {
                id
                transaction {
                             id
                            }
                pool {
                id
                     }
                sender
                recipient
                amount0
                amount1
                amountUSD
                amount0Paid
                amount1Paid
               }
           }
        }'
  )
  #####################################################################################
  ##Swaps based on Transaction
  #####################################################################################
  qry$query(
    'swap_Transaction_stats',
    'query swap_Transaction_stats($poolAdd: String!,$timestamp: Int!)
             {

               swaps(where: {id: $poolAdd})
                {
                 pool {
                      id
                      token0{
                            symbol
                            name
                            }
                      token1{
                            symbol
                            name
                            }
                      }
                   }
                transactions(orderBy: timestamp, orderDirection: asc,first:1000,where:{timestamp_gt:$timestamp})
                {
                 id
                 blockNumber
                 timestamp
                 gasUsed
                 gasPrice

            }
        }'
  )
  #####################################################################################
  #####################################################################################
  #Mints based on Transactions
  ######################################################################################
  qry$query(
    'Burn_Transaction_stats',
    'query Burn_Transaction_stats($poolAdd: String!,$timestamp: Int!)
             {
               burns(where: {id: $poolAdd})
                {
                 pool {
                      id
                      token0{
                            symbol
                            name
                            }
                      token1{
                            symbol
                            name
                            }
                      }
                   }
                transactions(orderBy: timestamp, orderDirection: asc,first:1000,where:{timestamp_gt:$timestamp})
                {
                 id
                 blockNumber
                 timestamp
                 gasUsed
                 gasPrice

            }
        }'
  )
  #####################################################################################
  #####################################################################################
  ###Burns based on Transactions
  #####################################################################################
  qry$query(
    'Mint_Transaction_stats',
    'query Mint_Transaction_stats($poolAdd: String!,$timestamp: Int!)
             {

               mints(where: {id: $poolAdd})
                {
                 pool {
                      id
                      token0{
                            symbol
                            name
                            }
                      token1{
                            symbol
                            name
                            }
                      }
                   }
                transactions(orderBy: timestamp, orderDirection: asc,first:1000,where:{timestamp_gt:$timestamp})
                {
                 id
                 blockNumber
                 timestamp
                 gasUsed
                 gasPrice

            }
        }'
  )
  ###################################################################################
  ###################################################################################
  #### Flash based on Transactions
  ###################################################################################
  qry$query(
    'Flash_Transaction_stats',
    'query Flash_Transaction_stats($poolAdd: String!,$timestamp: Int!)
             {

               flashes(where: {id: $poolAdd})
                {
                 pool {
                      id
                      token0{
                            symbol
                            name
                            }
                      token1{
                            symbol
                            name
                            }
                      }
                   }
                transactions(orderBy: timestamp, orderDirection: asc,first:1000,where:{timestamp_gt:$timestamp})
                {
                 id
                 blockNumber
                 timestamp
                 gasUsed
                 gasPrice

            }
        }'
  )
  #####################################################################################
  ###Tick Day Data
  #####################################################################################
  qry$query(
    'Tick_Daily_stats',
    'query Tick_Daily_stats($poolAdd: String!,$timestamp: Int!)
             {

                tickDayDatas(orderBy: date, orderDirection: desc,first:1000,where:{date_lt:$timestamp})
                {
                 id
                 date
                 pool(where: {id: $poolAdd})
                    {
                 id
                 tick {
                      id
                      }
                    }
                 liquidityNet
                 liquidityGross
                 volumeUSD
                 volumeToken0
                 volumeToken1
                 feesUSD
                 feeGrowthOutside0X128
                 feeGrowthOutside1X128
            }
        }'
  )
  #########################################################################################################
  #########################################################################################################
  ##################################################################
  ## User Liquidity Positions Current
  ##################################################################
  qry$query(
    'Position_user_current',
    'query Position_user_current($userAdd: String!,$idlast: String!)
        {
            positions(orderBy: id, orderDirection: asc,first:1000,where:{id_gt:$idlast,owner:$userAdd})
            {
                id
                owner
                pool {
                     id
                     }
                token0 {
                     id
                       }
                token1 {
                     id
                       }
                tickLower {
                          id
                           }
                tickUpper {
                          id
                          }
                liquidity
                depositedToken0
                depositedToken1
                withdrawnToken0
                withdrawnToken1
                collectedFeesToken0
                collectedFeesToken1
                transaction {
                             gasUsed
                             gasPrice
                              }
                feeGrowthInside0LastX128
                feeGrowthInside1LastX128
            }
        }'
  )
  ##################################################################
  ##################################################################


  ##################################################################
  ## User Liquidity Positions Historical
  ##################################################################
  qry$query(
    'Position_hist_user',
    'query Position_hist_user($userAdd: String!,$idlast: String!)
        {
            positionSnapshots(orderBy: id, orderDirection: asc,first:1000,where:{id_gt:$idlast,owner:$userAdd})
            {
                id
                owner
                pool {
                     id
                     totalValueLockedETH
                     totalValueLockedUSD
                     }
                position
                blockNumber
                timestamp
                liquidity
                depositedToken0
                depositedToken1
                withdrawnToken0
                withdrawnToken1
                collectedFeesToken0
                collectedFeesToken1
                transaction {
                            gasUsed
                            gasPrice
                            }
                feeGrowthInside0LastX128
                feeGrowthInside1LastX128
            }
        }'
  )
  ##################################################################
  ##################################################################


  ##################################################################
  ## User Swap Transactions
  ##################################################################
  qry$query(
    'swap_user',
    'query swap_user($userAdd: String!,$idlast: String!)
    {
      swaps(orderBy: id, orderDirection: asc,first:1000,where:{id_gt:$idlast,recipient:$userAdd})
      {
        id
        transaction {
              gasUsed
              gasPrice
              }
        timestamp
        pool {
           id
           }
        token0 {
            id
            symbol
            name
            totalSupply
            volume
            volumeUSD
            totalValueLocked
            totalValueLockedUSD
            }
        token1 {
            id
            symbol
            name
            totalSupply
            volume
            volumeUSD
            totalValueLocked
            totalValueLockedUSD
           }
        sender
        recipient
        origin
        amount0
        amount1
        amountUSD
        sqrtPriceX96
        tick
      }
    }'
  )
  ##################################################################
  ##################################################################


  ##################################################################
  ## User Mint Transactions
  ##################################################################
  qry$query(
    'mint_user',
    'query mint_user($userAdd: String!,$idlast: String!)
    {
      mints(orderBy: id, orderDirection: asc,first:1000,where:{id_gt:$idlast,origin:$userAdd})
      {
       id
       transaction {
            gasUsed
            gasPrice
              }
       timestamp
       pool {
          id
          }
       token0 {
       id
       symbol
       name
       totalSupply
       volume
       volumeUSD
       totalValueLocked
       totalValueLockedUSD
           }
       token1 {
       id
       symbol
       name
       totalSupply
       volume
       volumeUSD
       totalValueLocked
       totalValueLockedUSD
           }
       sender
       origin
       owner
       amount
       amount1
       amountUSD
       tickLower
       tickUpper
      }
    }'
  )
  ##################################################################
  ##################################################################


  ##################################################################
  ## User Burn Transactions
  ##################################################################
  qry$query(
    'burn_user',
    'query burn_user($userAdd: String!,$idlast: String!)
    {
      burns(orderBy: id, orderDirection: asc,first:1000,where:{id_gt:$idlast,sender:$userAdd})
       {
       id
       transaction {
            gasUsed
            gasPrice
              }
       timestamp
       pool {
          id
          }
       token0 {
       id
       symbol
       name
       totalSupply
       volume
       volumeUSD
       totalValueLocked
       totalValueLockedUSD
           }
       token1 {
       id
       symbol
       name
       totalSupply
       volume
       volumeUSD
       totalValueLocked
       totalValueLockedUSD
           }
       sender
       origin
       owner
       amount
       amount1
       amountUSD
       tickLower
       tickUpper
      }
    }'
  )
  ##################################################################
  ##################################################################
  ### Stats of Uniswap Factory
  ##################################################################
  qry$query(
    'factory_stats',
    'query factory_stats($userAdd: String!)
        {
            factories(where:{owner:$userAdd})
            {
                 owner
                 id
                 poolCount
                 txCount
                 totalVolumeUSD
                 totalVolumeETH
                 totalFeesUSD
                 totalFeesETH
                 untrackedVolumeUSD
                 totalValueLockedUSD
                 totalValueLockedETH
                 totalValueLockedUSDUntracked
                 totalValueLockedETHUntracked
            }
        }'
  )
  ##################################################################
  ##################################################################
  ##################################################################
  ## Historical Stats of Uniswap Platform
  ##################################################################
  qry$query(
    'uni_stats_hist',
    'query uni_stats_hist($timestamp: Int!)
        {
            uniswapDayDatas(orderBy: date, orderDirection: desc,first:1000,where:{date_lt:$timestamp})
            {
                id
                date
                volumeUSD
                volumeETH
                volumeUSDUntracked
                feesUSD
                txCount
                tvlUSD
            }
        }'
  )
  ##################################################################
  ##################################################################

  return(list(con, qry))
}


initialize_queries_Polygon <- function()
{
  ## Initialize Client Connection
  con <- GraphqlClient$new("https://api.thegraph.com/subgraphs/name/maticnetwork/mainnet-root-subgraphs")

  ## Prepare New Query
  qry <- Query$new()

  ##################################################################
  ##################################################################
  ## Pool Uniswap Data from Polgon
  ##################################################################
  qry$query(
    'matic_stats',
    'query matic_stats($userAdd: String!)
        {
            maticTransfers(orderBy: id, orderDirection: desc,first:1000,where:{timestamp_lt:$timestamp,sender:$userAdd})
            {
                 id
                 token
                 from
                 to
                 value
                 block
                 timestamp
                 transactionHash
            }
        }'
  )




return(list(con, qry))
}
