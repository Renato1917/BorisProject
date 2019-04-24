library(tidyverse)

# Paths to inputs
pathCenso1991 = "../../Data/Censos/Censos/Censo1991.csv"
pathCenso2001 = "../../Data/Censos/Censos/Censo2001.csv"
pathCenso2011 = "../../Data/Censos/Censos/Censo2011.csv"
pathTransform91_11 = "../../Data/Censos/TransformFreg/Transform91_11.csv"
pathTransform01_11 = "../../Data/Censos/TransformFreg/Transform01_11.csv"
pathCensoNUTS2011 = "../../Data/Censos/Censos/CensoNUTS3_2011.csv"
pathPopAgeNUTS2 = "../../Data/EstimativasINE/Pop_Age_NUTS2.csv"
pathBirths = "../../Data/Births/Nados_vivos.csv"
pathDeaths = "../../Data/Deaths/Obitos.csv"
pathNUTSIndex = "../../Data/Index/NUTS_Index.csv"
# Paths to outputs
pathCensus_91_11 = "../../Data/PreFormat/"

# Read and prepar Census from INE data -----------------------------------------------

ReadCensus = function(){
  Censos11 = read_delim(pathCenso2011,
                        delim = ";",
                        col_types = cols(NUTS2 = col_character(),
                                         NUTS3 = col_character(),
                                         FF = col_character()),
                        locale = locale(encoding = 'ISO-8859-1'),
                        skip = 0) %>%
    mutate(AG50 = ifelse(NUTS3 %in% c("171", "172"), AG55, AG50),
           AG55 = ifelse(NUTS3 %in% c("171", "172"), AG60, AG55),
           AG60 = ifelse(NUTS3 %in% c("171", "172"), AG65, AG60),
           AG65 = ifelse(NUTS3 %in% c("171", "172"), AG70, AG65),
           AG70 = ifelse(NUTS3 %in% c("171", "172"), AG75, AG70),
           AG75 = ifelse(NUTS3 %in% c("171", "172"), AG80, AG75),
           AG80 = ifelse(NUTS3 %in% c("171", "172"), AG85, AG80),
           AG85 = ifelse(NUTS3 %in% c("171", "172"), AG90, AG85),
           AG90 = ifelse(NUTS3 %in% c("171", "172"), AG95, AG90),
           AG95 = ifelse(NUTS3 %in% c("171", "172"), AG100, AG95),
           AG100 = ifelse(NUTS3 %in% c("171", "172"), Resto, AG100)) %>%
    select(-Resto) %>%
    group_by(NUTS1, NUTS1_DSG, NUTS2, NUTS2_DSG, NUTS3, NUTS3_DSG, CC, CC_DSG,
             FF, FF_DSG, Sex) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    ungroup() %>%
    arrange(NUTS3, Sex)
  #
  Censos01 = read_delim(pathCenso2001,
                        delim = ";",
                        col_types = cols(NUTS2 = col_character(),
                                         NUTS3 = col_character(),
                                         FF = col_character()),
                        locale = locale(encoding = 'ISO-8859-1'),
                        skip = 0) %>%
    group_by(NUTS1, NUTS1_DSG, NUTS2, NUTS2_DSG, NUTS3, NUTS3_DSG, CC, CC_DSG,
             FF, FF_DSG, Sex) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    ungroup() %>%
    arrange(NUTS3, Sex)
  #
  Censos91 = read_delim(pathCenso1991,
                        delim = ";",
                        col_types = cols(NUTS2 = col_character(),
                                         NUTS3 = col_character(),
                                         FF = col_character()),
                        locale = locale(encoding = 'ISO-8859-1'),
                        skip = 1) %>%
    group_by(NUTS1, NUTS1_DSG, NUTS2, NUTS2_DSG, NUTS3, NUTS3_DSG, CC, CC_DSG,
             FF, FF_DSG, Sex) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    ungroup() %>%
    arrange(NUTS3, Sex)
  #
  Census_91_11 = list()
  Census_91_11[[1991]] = Censos91
  Census_91_11[[2001]] = Censos01
  Census_91_11[[2011]] = Censos11
  #
  save(Census_91_11, file = c(paste0(pathCensus_91_11,"pathCensus_91_11.rda")))
  #
  return(Census_91_11)
}

PTcensos = function(PreFormat = TRUE){
  require(tidyverse)
  #
  if(PreFormat == TRUE){
    load(paste0(pathCensus_91_11,"pathCensus_91_11.rda"))
  }else{
    Census_91_11 = ReadCensus()
  }
  # Census_91_11 = ReadCensus()
  
  df_Transform91_11 = read_delim(pathTransform91_11,
                        delim = ",",
                        col_types = cols(NUTS1 = col_character(),
                                         NUTS2 = col_character(),
                                         NUTS3 = col_character(),
                                         CC = col_character(),
                                         FF = col_character()),
                        locale = locale(encoding = 'ISO-8859-1'),
                        skip = 0)
    
  df_Transform01_11 = read_delim(pathTransform01_11,
                                delim = ",",
                                col_types = cols(NUTS1 = col_character(),
                                                 NUTS2 = col_character(),
                                                 NUTS3 = col_character(),
                                                 CC = col_character(),
                                                 FF = col_character()),
                                locale = locale(encoding = 'ISO-8859-1'),
                                skip = 0)
    
  # Census 1991
  
  Census91 = Census_91_11[[1991]] %>%
    select(-c(NUTS1:CC_DSG, FF_DSG)) %>%
    left_join(df_Transform91_11, by = c("FF" = "FREGUESIA_91")) %>%
    select(-FF) %>%
    rename(FF = FF.y) %>%
    select(NUTS1:FF_DSG, Sex:AG100) %>%
    mutate(Year = 1991) %>%
    group_by(Year,Sex,NUTS2,NUTS3,NUTS3_DSG) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    ungroup()
  
  # Census 2001
  
  Census01 = Census_91_11[[2001]] %>%
    select(-c(NUTS1:CC_DSG, FF_DSG)) %>%
    left_join(df_Transform01_11, by = c("FF" = "FREGUESIA_01")) %>%
    select(-FF) %>%
    rename(FF = FF.y) %>%
    select(NUTS1:FF_DSG, Sex:AG100) %>%
    mutate(Year = 2001) %>%
    group_by(Year,Sex,NUTS2,NUTS3,NUTS3_DSG) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    ungroup()
  
  # Census 2011
   
  Census11 = Census_91_11[[2011]] %>%
    mutate(Year = 2011) %>%
    group_by(Year,Sex,NUTS2,NUTS3,NUTS3_DSG) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    ungroup()
  # Test census 2011
  # df_Test2001 = Censos11 %>%
  #   mutate(Total2 = Total - rowSums(.[7:27]))
  #
  
  # Data from INE with AG 0 and 1-4 years old
  CensosNUTS11 = read_delim(pathCensoNUTS2011,
                        delim = ";",
                        col_types = cols(#NUTS2 = col_character(),
                                         NUTS3 = col_character()),
                        locale = locale(encoding = 'ISO-8859-1'),
                        skip = 0
  )
  
  Census11 = Census11 %>%
    left_join(CensosNUTS11, by = c("Year", "Sex", "NUTS3", "NUTS3_DSG")) %>%
    mutate(Teste = Total.x - Total.y) %>%
    select(-c(Total.x:AG85.x,AG90.y)) %>%
    rename_at(vars(AG5.y:AG85.y), ~ paste0("AG", seq(5,85,5))) %>%
    rename(AG90 = AG90.x,
           Total = Total.y,
           AG0 = AG0.y) %>%
    select(Year:NUTS3_DSG, Total:AG85, AG90:AG100) %>%
    filter(Sex != "HM")
  #
  # Some operations to the same NUTS 3
  # NUTSAux = Census01 %>%
  #   filter(Sex == "H") %>%
  #   select(NUTS2,NUTS3, NUTS3_DSG)
  #
  PopAgeNUTS2 = read_delim(pathPopAgeNUTS2,
                           delim = ";",
                           col_types = cols(NUTS2 = col_character()),
                           locale = locale(encoding = 'ISO-8859-1'),
                           skip = 7,
                           comment = "#"
  )
  
  colnames(PopAgeNUTS2) = c("YearL", "NUTS2", "Sex", "Total", paste0("AGL", seq(0,85,1)))
  PopAgeNUTS2 = PopAgeNUTS2 %>%
    separate(NUTS2, c("NUTS2", "Names"), sep = ":") %>%
    select(YearL:AGL4) %>%
    mutate(Year = ifelse(YearL < 2001, 1991, 
                         ifelse(YearL > 2010, 2011, 2001))) %>%
    group_by(Year, NUTS2, Sex) %>%
    summarize(Total = sum(Total),
              AGL0 = sum(AGL0),
              AGL1 = sum(AGL1),
              AGL2 = sum(AGL2),
              AGL3 = sum(AGL3),
              AGL4 = sum(AGL4)) %>%
    mutate(Total = AGL0 + AGL1 + AGL2 + AGL3 + AGL4,
           AGI0 = AGL0 / Total,
           AGI1 = (AGL1+AGL2+AGL3+AGL4) / Total) %>%
    filter(Year != 2011) %>%
    select(Year:Sex,AGI0:AGI1) %>%
    drop_na()
  
  CensosAux = bind_rows(Census91,Census01) %>%
    filter(Sex != "HM") %>%
    arrange(Year, Sex, NUTS3) %>%
    mutate(AG1 = NA) %>%
    select(Year:AG0, AG1, AG5:AG100) %>%
    left_join(PopAgeNUTS2, by = c("Year", "NUTS2", "Sex")) %>%
    mutate(AG1 = round(AG0 * AGI1, digits = 0),
           AG0 = round(AG0 * AGI0, digits = 0)) %>%
    select(-c(AGI0,AGI1))
  
  CensoT = bind_rows(CensosAux, Census11)%>%
    group_by(Year,Sex) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    mutate(NUTS2 = "PT0",
           NUTS3 = "PT0",
           NUTS3_DSG = "Portugal") %>%
    select(Year, Sex, NUTS2, NUTS3, NUTS3_DSG, Total:AG100)
  
  CensoC = bind_rows(CensosAux, Census11) %>%
    filter(!NUTS3 %in% c("200", "300")) %>%
    group_by(Year,Sex) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    mutate(NUTS2 = "PT1",
           NUTS3 = "PT1",
           NUTS3_DSG = "Continente") %>%
    select(Year, Sex, NUTS2, NUTS3, NUTS3_DSG, Total:AG100)
  
  NUTSAux = read_delim(pathNUTSIndex,
                       delim = ";",
                       col_types = cols(NUTS2 = col_character(),
                                        NUTS3 = col_character(),
                                        NUTS3_DSG = col_character()),
                       locale = locale(encoding = 'ISO-8859-1'),
                       skip = 0) %>%
    select(NUTS3, Region)
  
  df_Census = bind_rows(CensoT,CensoC, CensosAux,Census11) %>%
    left_join(NUTSAux, by = "NUTS3") %>%
    mutate(Type = "nKx")%>%
    select(Year:NUTS2,Region,NUTS3:NUTS3_DSG, Type, Total:AG100) %>%
    arrange(Year, Region, NUTS3, Sex) %>%
    ungroup()
  #
  # df_Census = df_Census %>%
  #   mutate(#NUTS2 = factor(NUTS2, levels = unique(NUTSAux$NUTS2)),
  #          Region = factor(Region, levels = unique(NUTSAux$Region)),
  #          NUTS3 = factor(NUTS3, levels = NUTSAux$NUTS3),
  #          Sex = factor(Sex, levels = c("H", "M"))
  #          ) %>%
  #   arrange(Year, Sex, Region, NUTS3)
  
  #Teste
  # df_Teste = Censos %>%
  #   ungroup() %>%
  #   mutate(Resto = Total - rowSums(.[8:29])) %>%
  #   # group_by(Year, Sex, NUTS3, Type) %>%
  #   summarize(Resto = sum(Resto))
  # Teste = df_Teste$Resto
  # ifelse(Teste != 0, print("A"), print("B"))
  return(df_Census)
  
}

# Read and prepar Maternity from INE data -----------------------------------------------

PTbirths = function(){
  require(tidyverse)
  
  Births = read_csv2(pathBirths,
                     locale = locale(encoding = 'ISO-8859-1'),
                     # col_names = c("AG", "Year", "Local", "nBx_H", "nBx_M"),
                     skip = 7,
                     col_types = cols(
                       NUTS3 = col_character(),
                       Type = col_character()),
                     comment = "#")
  
  NUTSAux = read_delim(pathNUTSIndex,
                       delim = ";",
                       col_types = cols(NUTS2 = col_character(),
                                        NUTS3 = col_character(),
                                        NUTS3_DSG = col_character()),
                       locale = locale(encoding = 'ISO-8859-1'),
                       skip = 0)
  
  BirthsAux = Births %>%
    separate(NUTS3, c("NUTS3", "Names"), sep = ":") %>%
    mutate(Type = ifelse(Type == "HM", "nBx",
                         ifelse(Type == "H", "nBx_H", "nBx_M")),
           Sex = "M") %>%
    mutate(Total2 = rowSums(select(., contains("AG")))) %>%
    mutate_at(.vars = vars(AG0:AG100),
              .funs = funs(round((1 + UNKNOWN/Total2)*., digits = 0))) %>%
    mutate(Total = rowSums(select(., contains("AG")))) %>%
    mutate(Year = ifelse(YearL < 2001, 1991,
                         ifelse(YearL > 2010, 2011, 2001))) %>%
    select(-c(UNKNOWN, Total2)) %>%
    filter(!NUTS3 %in% c(0, "YY", "99", "00")) %>%
    group_by(Year, NUTS3, Type, Sex) %>%
    summarize_at(vars(Total:AG100), .funs = funs(mean)) %>%
    summarize_at(vars(Total:AG100), .funs = funs(round(., digits = 0))) %>%
    left_join(NUTSAux, by = c("NUTS3")) %>%
    mutate(Sex = "M") %>%
    select(Year, Sex,NUTS2, Region, NUTS3, NUTS3_DSG, Type, Total:AG100)
  
  BirthsT = BirthsAux %>%
    group_by(Year, Type) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    mutate(NUTS2 = "PT0",
           Region = "PT0",
           NUTS3 = "PT0",
           NUTS3_DSG = "Portugal",
           Sex = "M") %>%
    select(Year, Sex, NUTS2, Region, NUTS3, NUTS3_DSG, Type, Total:AG100)
  
  BirthsC = BirthsAux %>%
    filter(!NUTS3 %in% c("200", "300")) %>%
    group_by(Year, Type) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    mutate(NUTS2 = "PT1",
           Region = "PT1",
           NUTS3 = "PT1",
           NUTS3_DSG = "Continente",
           Sex = "M") %>%
    select(Year, Sex, NUTS2, Region, NUTS3, NUTS3_DSG, Type, Total:AG100)
  
  df_Births = bind_rows(BirthsT, BirthsC, BirthsAux) %>%
    arrange(Year, Region, NUTS3, Sex) %>%
    ungroup()
  
  return(df_Births)
}

# Read and prepar Deaths from INE data -----------------------------------------------

PTdeaths = function(){
  Deaths = read_csv2(pathDeaths,
                     locale = locale(encoding = 'ISO-8859-1'),
                     # col_names = c("AG", "Year", "Local", "nBx_H", "nBx_M"),
                     skip = 7,
                     col_types = cols(
                       NUTS3 = col_character(),
                       Sex = col_character()),
                     comment = "#")
  
  NUTSAux = read_delim(pathNUTSIndex,
                       delim = ";",
                       col_types = cols(NUTS2 = col_character(),
                                        NUTS3 = col_character(),
                                        NUTS3_DSG = col_character()),
                       locale = locale(encoding = 'ISO-8859-1'),
                       skip = 0)
  
  DeathsAux = Deaths %>%
    separate(NUTS3, c("NUTS3", "Names"), sep = ":") %>%
    mutate(Type = "nDx") %>%
    mutate(Total2 = rowSums(select(., contains("AG")))) %>%
    filter(Total2 != 0) %>%
    mutate_at(.vars = vars(AG0:AG120),
              .funs = funs(round((1 + UNKNOWN/Total2)*., digits = 0))) %>%
    mutate(Total = rowSums(select(., contains("AG")))) %>%
    mutate(Year = ifelse(YearL < 2001, 1991,
                         ifelse(YearL > 2010, 2011, 2001))) %>%
    select(-c(UNKNOWN, Total2)) %>%
    filter(!NUTS3 %in% c(0, "YY", "999", "000")) %>%
    group_by(Year, NUTS3, Type, Sex) %>%
    summarize_at(vars(Total:AG120), .funs = funs(mean)) %>%
    left_join(NUTSAux, by = c("NUTS3")) %>%
    mutate(AG100 = AG100 + AG105 + AG110 + AG115 + AG120) %>%
    select(Year, Sex,NUTS2, Region, NUTS3, NUTS3_DSG, Type, Total:AG100)
  
  DeathsT = DeathsAux %>%
    group_by(Year, Sex, Type) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    mutate(NUTS2 = "PT0",
           Region = "PT0",
           NUTS3 = "PT0",
           NUTS3_DSG = "Portugal") %>%
    select(Year, Sex, NUTS2, Region, NUTS3, NUTS3_DSG, Type, Total:AG100)
  
  DeathsC = DeathsAux %>%
    filter(!NUTS3 %in% c("200", "300")) %>%
    group_by(Year, Sex, Type) %>%
    summarize_at(vars(Total:AG100), sum) %>%
    mutate(NUTS2 = "PT1",
           Region = "PT1",
           NUTS3 = "PT1",
           NUTS3_DSG = "Continente") %>%
    select(Year, Sex, NUTS2, Region, NUTS3, NUTS3_DSG, Type, Total:AG100)
  
  df_Deaths = bind_rows(DeathsT, DeathsC,DeathsAux) %>%
    arrange(Year, Region, NUTS3, Sex) %>%
    ungroup()
  
  return(df_Deaths)
}

# Preformat = T

LTinput = function(PreFormat = TRUE){
  require(tidyverse)
  AGlevels = c(paste0("AG", c(0,1,seq(5,100,5))), "Total")
  PTcensosAux = PTcensos(PreFormat = TRUE)
  PTdeathsAux = PTdeaths()
  PTbirthsAux = PTbirths()
  LifeTable = bind_rows(PTcensosAux,
                        PTbirthsAux,
                        PTdeathsAux)
  # LifeTable = bind_rows(Censos,Births,Deaths)
  
  InputLTAux = LifeTable %>%
    gather(key = AG, value = Mesurement, Total:AG100) %>%
    mutate(Mesurement = round(Mesurement, digits = 0))
  
  df_LTinput = InputLTAux %>%
    spread(Type, Mesurement) %>%
    mutate_at(.vars = vars(nBx:nKx),
              .funs = funs(ifelse(is.na(.), 0, .))) %>%
    mutate(AG = factor(AG, levels = AGlevels))
  #
  NUTSAux = read_delim(pathNUTSIndex,
                       delim = ";",
                       col_types = cols(NUTS2 = col_character(),
                                        NUTS3 = col_character(),
                                        NUTS3_DSG = col_character()),
                       locale = locale(encoding = 'ISO-8859-1'),
                       skip = 0)
  #
  df_LTinput = df_LTinput %>%
    mutate(AG = factor(AG),
           Region = factor(Region, levels = unique(NUTSAux$Region)),
           NUTS3 = factor(NUTS3, levels = NUTSAux$NUTS3),
           Sex = factor(Sex, levels = c("H", "M"))
           ) %>%
    arrange(Year, Region, NUTS3, AG, Sex)
  
  # %>%
  #   mutate()
  return(df_LTinput)
}

# teste = InputLT()%>%
#   write.csv("./teste.csv", row.names = FALSE)
# 
# df_teste_ori = read_delim("./teste_original.csv",
#                       delim = ",",
#                       locale = locale(encoding = 'ISO-8859-1'),
#                       skip = 0
# )
# 
# df_teste = read_delim("./teste.csv",
#                           delim = ",",
#                           locale = locale(encoding = 'ISO-8859-1'),
#                           skip = 0
# )
# 
# df_teste_ori2 = df_teste_ori %>%
#   filter(AG == "Total") %>%
#   group_by(Year,NUTS3,Local) %>%
#   summarize(nKx = sum(nKx))
# 
# df_teste2 = df_teste  %>%
#   filter(AG == "Total") %>%
#   group_by(Year,NUTS3,Local) %>%
#   summarize(nKx = sum(nKx)) %>%
#   left_join(df_teste_ori2, by = c("Year","NUTS3")) %>%
#   mutate(Teste = nKx.x - nKx.y)
