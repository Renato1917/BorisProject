library(tidyverse)
  

Project = function(LifeTable, YearI, NUTS3I, Nproj = 20){
  require(tidyverse)
  source("./Fun_Lif.R")
  #
  LifeTable = LifeTable %>%
    filter(Year == YearI & NUTS3 == NUTS3I) %>%
    droplevels()
  #
  S = LifeTable$nBx_H[LifeTable$Sex == "M" & LifeTable$AG == "Total"] /
    LifeTable$nBx_M[LifeTable$Sex == "M" & LifeTable$AG == "Total"]
  # YearI = unique(LifeTable$Year)
  # NUTS3I = unique(LifeTable$NUTS3)
  #
  LifeTable = LifeTable %>%
    filter(AG != "Total") %>%
    droplevels()
  #
  df_LTM = Lif(LifeTable, SexI = "M", YearI = YearI, NUTS3I = NUTS3I)[[1]] #%>%
    # select(Sex:nBx, nLx) %>%
    # mutate(nKx = ifelse(AG == 0, nKx + lead(nKx), nKx),
    #        nLx = ifelse(AG == 0, nLx + lead(nLx), nLx),
    #        nFx = ifelse(nBx == 0, 0, nBx/nKx)) %>%
    # filter(AG != 1)
  df_LTH = Lif(LifeTable, SexI = "H", YearI = YearI, NUTS3I = NUTS3I)[[1]]
  # nKx
  nKxM = df_LTM$nKx
  NPopSex = length(nKxM)
  NPopSexT = NPopSex + 1
  nKxM[NPopSexT] = 0
  nKxH = df_LTH$nKx
  nKxH[NPopSexT] = 0
  nKx = c(nKxH,nKxM)
  NPop = length(nKx)
  # nLx
  nLxM = df_LTM$nLx
  nLxM[NPopSexT] = 0
  nLxH = df_LTH$nLx
  nLxH[NPopSexT] = 0
  nLx = c(nLxH,nLxM)
  # nFx
  nFx = df_LTM$nBx/df_LTM$nKx
  # nFx[NPopSexT] = 0
  
  # 15
  # N = 1 + (LYear - IYear-1) / (LYear - IYear)
  # if(LYear == 0){
  #   N = 1
  # }
  
  nPx = array(rep(0, (NPop+1)*Nproj), dim=c((NPop+1),Nproj))
  IAge = rep(0, NPop+1)
  # Set age in array IAge
  for (i in 3:(NPopSex)) {
    IAge[i] = IAge[i-1] + 5
    IAge[i+NPopSexT] = IAge[i]
  }
  # IAge[1] = 0
  # IAge[2] = 1
  
  # 38
  nPx[1:NPop,1] = nKx
  
  # Combination of the Age groups 0 and 1
  nLx[2] = nLx[2] + nLx[1]
  nLx[NPopSexT+2] = nLx[NPopSexT+2] + nLx[NPopSexT+1]
  nPx[2,1] = nPx[2,1] + nPx[1,1]
  nPx[NPopSexT+2,1] = nPx[NPopSexT+2,1] + nPx[NPopSexT+1,1]
  
  # 46
  # for(k in 1:20){
  for(j in 2:Nproj){
    for (i in 3:NPopSex){
      nPx[i,j] = nPx[i-1,j-1] * nLx[i]/nLx[i-1]
      nPx[i+NPopSexT,j] = nPx[i+NPopSexT-1,j-1] * nLx[i+NPopSexT]/nLx[i+NPopSexT-1]
    }
    #
    nPx[NPopSexT+2,j] = 0
    # for(i in 1:NPopSex){
    for(i in 4:12){
      # if(nFx[i] != 0){
        nPx[NPopSexT+2,j] = nPx[NPopSexT+2,j] + 
          0.5 * nLx[NPopSexT+2] * (nPx[i+NPopSexT,j-1] + nPx[i+NPopSexT,j]) * 
          nFx[i] / ((1+S) * 100000)
      # }
    } # i
    nPx[2,j] = S * nPx[NPopSexT+2,j] * nLx[2]/nLx[NPopSexT+2]
    nPx[NPopSexT,j] = 0
    nPx[NPop,j] = 0
  } # j
  
  Years = seq(YearI, YearI + (Nproj-1)*5,5)
  Years = as.character(Years)
  
  df_nPx = as_tibble(nPx) 
  # names(df_nPx) = paste0("Y",Years)
  names(df_nPx) = Years
  df_nPx = df_nPx %>%
    filter(df_nPx[,2] != 0) %>%
    mutate_all(funs(round(.)))
  
  df_project = LifeTable %>%
    select(Year, NUTS3_DSG,NUTS3, Sex,AG) %>%
    filter(AG != "AG1")
  df_project = bind_cols(df_project,df_nPx)
  
  return(df_project)
}
# Test
# Proje = Project(LifeTable = NetMig, YearI = 2001, NUTS3I = "PT", Nproj = 2)

ProjectNUTS = function(LifeTable, YearI, Nproj = 20){
  require(tidyverse)
  df_LT = LifeTable %>%
    filter(Year == YearI)
  NUTS3levels = unique(df_LT$NUTS3)
  #
  df_proj = Project(df_LT, YearI, NUTS3I = NUTS3levels[1], Nproj)
  for (i in NUTS3levels[-1]){
    # i = NUTS3levels[1:7]
    df_projAux = Project(df_LT, YearI, NUTS3I = i, Nproj)
    df_proj = bind_rows(df_proj,df_projAux)
  }
  return(df_proj)
}
# Test
# ProjeNUTS = ProjectNUTS(LifeTable = NetMig, YearI = 2001, Nproj = 2)

# LifeTable = NetMig #%>%
#   # filter(!Year %in% c(1991,1996))
# NUTS3I = "PT"
# Nproj = 3

# LifeTable = NetMig
ProjectNUTSYear = function(LifeTable){
  require(tidyverse)
  Nproj = 3
  df_LT = LifeTable %>%
    # filter(NUTS3 == NUTS3I) %>%
    ungroup()
  Yearlevels = unique(df_LT$Year)
  NUTS3levels = unique(df_LT$NUTS3)
  #
  # df_proj = Project(df_LT, YearI = Yearlevels[1], NUTS3I , Nproj)
  lt_projNUTS3 = list()
  for (i in NUTS3levels) {
    lt_projYear = list()
    for (j in Yearlevels){
      lt_projYear[[j]] = Project(df_LT, YearI = j, NUTS3I = i, Nproj) %>%
        select(-Year) %>%
        gather(key = Year, value = nKx, -c(NUTS3_DSG:AG)) %>%
        mutate(nOx = nKx,
               nKx = ifelse(Year == (j + 10), 0, nKx),
               nOx = ifelse(Year == j, 0, nOx)
               )
    }
    lt_projNUTS3[[i]] = lt_projYear %>% do.call(rbind,.)
  }
  df_proj = lt_projNUTS3 %>% do.call(rbind,.) %>%
    group_by(Year, Sex, NUTS3_DSG, NUTS3, AG) %>%
    summarize(nKx = sum(nKx),
              nOx = sum(nOx)) %>%
    arrange(Year, NUTS3, AG, Sex)
  
  return(df_proj)
}

# ProjeNUTSYear = ProjectNUTSYear(LifeTable = NetMig)

# Proj = teste[[2]]
