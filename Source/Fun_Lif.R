library(tidyverse)

# ENG_68H <- tibble(Year = rep(1968, 19),
#                   Local = rep("ENG",19),
#                   NUTS3 = rep("16", 19),
#                   Sex = rep("H", 19),
#                   AG = c(0,1,seq(5,85,5)),
#                   nKx = c(414700, 1716200, 1980500, 1716000, 1734800, 1857200,
#                           1538700, 1504100, 1508800, 1540800, 1626400, 1411600,
#                           1458300, 1292800, 991000, 631100, 399200, 206600, 101100),
#                   nBx = rep(0,19),
#                   # nBx = c(0, 0, 0, 222, 81853, 295946, 240807, 125316, 58083,
#                   #         15904, 1140, 1, 0, 0, 0, 0, 0, 0, 0),
#                   nDx = c(8707, 1490, 869, 677, 1545, 1712, 1347, 1554, 2447, 4653,
#                           8275, 13023, 23368, 34785, 43710, 43426, 42011, 32675,
#                           27037))
# 
# ENG_68M <- tibble(Year = rep(1968, 19),
#                   Local = rep("ENG",19),
#                   NUTS3 = rep("16", 19),
#                   Sex = rep("M", 19),
#                   AG = c(0,1,seq(5,85,5)),
#                   nKx = c(393600, 1631200, 1882500, 1633100, 1681800, 1838900,
#                           1500800, 1426100, 1443300, 1522100, 1644900, 1488800,
#                           1585300, 1483800, 1280500, 1033400, 753600, 464300,
#                           275100),
#                   nBx = c(0, 0, 0, 222, 81853, 295946, 240807, 125316, 58083,
#                           15904, 1140, 1, 0, 0, 0, 0, 0, 0, 0),
#                   nDx = c(6275, 1194, 573, 431, 624, 740, 794, 1077, 1739, 3153,
#                           5569, 7848, 12601, 19196, 27233, 37936, 47724, 49949,
#                           58913))
# 
# ENG_68 = rbind(ENG_68H, ENG_68M) %>%
#   mutate(Sex = factor(Sex),
#          Local = factor(Local),
#          AG = factor(AG)) %>%
#   arrange(Sex, Local, AG)
# 
# LifeTable = ENG_68
# LifeTable = PopCenso2011 %>%
#   filter(NUTS3 == "PT")
# YearI = 1968
# SexI = "M"
# NUTS3I = "16"


Lif = function(LifeTable, SexI, YearI, NUTS3I){
  require(tidyverse)
  #
  # YearI = unique(LifeTable$Year)
  # NUTS3I = unique(LifeTable$NUTS3)
  #
  df_LT = LifeTable %>%
    filter(AG != "Total" & Sex == SexI & Year == YearI & NUTS3 == NUTS3I) %>%
    droplevels()
    # filter(Year == YearI & Sex == SexI & NUTS3 == NUTS3I)
  #
  nKx = df_LT$nKx
  NPop = length(nKx)
  NPopT = NPop + 1
  nKx[NPopT] = 0
  nKxAux = nKx + 0.5
  nKx = ifelse(nKxAux == 0, 1, nKx)
  nKx = ifelse(nKx == 0, 1, nKx)
 
  nDx = df_LT$nDx
  nDx[NPopT] = 0

  nMx = nDx/nKx
  #
  # Sex = dim(nKx)[1]
  # Local = dim(nKx)[2]
  # AG = dim(nKx)[3]
  #
# Set initial values
  rx = rep(0,NPopT) 
  rx[1] = 0.000001
  SEP = 0.07 + 1.7 * nMx[1]
  lx = rep(0,NPopT) 
  nLx = rep(0,NPopT) 
  #
  lx[1] = 100000
  lx[2] <- lx[1] * (1 - SEP * nMx[1])/
   (1 + (1 - SEP) * nMx[1])
  lx[3] <- lx[2] * (1 - 1.5 * nMx[2])/
   (1 + 2.5 * nMx[2])
 
  for (i in 3:(NPop)) {
    lx[i+1] <- lx[i] * 
      (1 - 2.5 * nMx[i])/
      (1 + 2.5 * nMx[i])
    #
    nLx[i] = 2.5 * (lx[i] + lx[i+1])
  }
  lx17 = lx[17]
 
  # Coeficients
  CNSTA = 10/3
  CNSTB = 5/12
  CNSTC = 65/24
  CNSTD = 5/24
 
  for (j in 1:10) {
    # Revise nLx for ages 5-9 and 85+
    nLx[3] = 2.5 * (lx[3] + lx[4]) / exp(5 * rx[3])
    nLx[NPop] = CNSTA * lx[NPop] - CNSTB * lx[NPop-1]
    #
    for(i in 4:(NPop-1)){
      S = exp(5 * rx[i])
      # Interate for nLx
      nLx[i] = CNSTC * (lx[i] + lx[i+1] / S) -
        CNSTD * (lx[i-1] * S + lx[i+2] / S^2)
      # Interate for lx
      lx[i+1] = S * (lx[i] - (nMx[i] + rx[i]) *
                       (nLx[i] - CNSTC * lx[i+1]/S)) /
        (1 + CNSTC * (nMx[i] + rx[i]))
      # Iterate for rx
      WW = (nKx[i-1] / nLx[i-1]) / (nKx[i+1] / nLx[i+1])
      rx[i] = 0.000001
      if(WW > 1){
        rx[i] = 0.1 * log(WW)
      }
      if(rx[i] > 0.04){
        rx[i] = 0.04
      }
    }
    #Test for convergence
    if(abs(lx17 - lx[17]) < 1) break
    lx17 = lx[17]
  }
 
  Iage = rep(0,NPop)
  nqx = rep(0,NPop)
  ndx = rep(0,NPop)
  nTx = rep(0,NPop)
  nmx = rep(0,NPop)
  nax = rep(0,NPop)
  nEx = rep(0,NPop)
 
  for (i in 1:NPop) {
    Iage[i] = 5 * i - 10
    nqx[i] = 1 - lx[i+1]/lx[i]
    ndx[i] = lx[i] - lx[i+1] + 0.5
    if(i < 4 | i == (NPop)){
      # break
      1
    } else{
      nLx[i] = CNSTC * (lx[i] + lx[i+1]) - 
        CNSTD * (lx[i-1] + lx[i+2])
    }
  }
  #
  # Modification for the lowest and highest age group
  ndx[NPop] =lx[NPop] + 0.5
  nqx[NPop] = 1.
  Iage[1] = 0
  Iage[2] = 1
  nLx[1] = SEP * lx[1] + (1 - SEP) * lx[2]
  nLx[2] = 1.5 * lx[2] + 2.5 * lx[3]
  nLx[3] = 2.5 * (lx[3] + lx[4])
  nLx[NPop] = lx[NPop] / nMx[NPop]
  nTx[NPop] = nLx[NPop]
  #
  for(i in NPop:1){
    nmx[i] = (lx[i] - lx[i+1]) / nLx[i]
    nax[i] = (nLx[i] - 5 * lx[i+1]) / (lx[i] - lx[i+1])
    if(i != NPop){
      nTx[i] = nTx[i+1] + nLx[i]
    }
    nEx[i] = nTx[i] / lx[i]
  }
  #
  # Some other ajudments
  nax[1] = SEP
  nax[2] = 1.5
  nax[3] = 2.5
  nax[NPop] = nEx[NPop] 
  #
 
  nKx = nKx[-NPopT]
  nDx = nDx[-NPopT]
  nLx = nLx[-NPopT]
  nMx = nMx[-NPopT]
  rx = rx[-NPopT]
  lx = lx[-NPopT]
 
  df_LifeTable_1 = df_LT %>%
    # select(-nBx) %>%
    mutate(nqx = nqx,
           lx = round(lx, digits = 0),
           ndx = round(ndx, digits = 0),
           nLx = round(nLx, digits = 0))
  #
  df_LifeTable_2 = df_LT %>%
    select(-c(nBx, nKx, nDx)) %>%
    mutate(nmx = nmx,
           nax = nax,
           nTx = nTx,
           rx = rx,
           nEx = nEx,
           nMx = nMx)
  #
  return(list(df_LifeTable_1, df_LifeTable_2))
}

# teste = NetMig %>%
#   filter(NUTS3 == "16B" & Year == 1991)
# 
# # teste1 = Lif(ENG_68, SexI = "M")[[1]]
# # teste2 = Lif(ENG_68, SexI = "M")[[2]]
# 
# teste1 = Lif(teste %>%
#                filter(NUTS3 == "118"), SexI = "H", YearI = 2001, NUTS3I = "118")[[1]]
# teste2 = Lif(teste %>%
#                filter(NUTS3 == "118"), SexI = "H", YearI = 2001, NUTS3I = "118")[[2]]







