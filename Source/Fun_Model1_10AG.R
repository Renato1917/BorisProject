library(R2WinBUGS)
Path2BUGS = "C:/Users/rguedes/WinBUGS14/"
library(data.table)
library(lattice)
library(stringr)
library(reshape2)
library(mcmcplots)
library(ggplot2)
library(grid)
library(ggmcmc)
library(tidyverse)

source("./Fun_LTinput.R")
source("./Fun_Projection.R")

NetMig = LTinput(PreFormat = TRUE) %>%
  arrange(Year, NUTS3, Sex, AG)

df_Projection = ProjectNUTSYear(LifeTable = NetMig) 

Idade = paste0("AG", seq(0,75,5))
Anos = c(2001,2011)

df_InputData = df_Projection %>%
  filter(Year %in% Anos & AG %in% Idade) %>%
  droplevels() %>%
  mutate(DM = nKx - nCx,
         DMN =DM/nCx) %>%
  arrange(AG, Sex, Year, NUTS3)

# Model description -------------------------------------------------------

## Parameters to adjust#####
Path2Wdir = "C:/Users/rguedes/Documents/WORK/Population/BKproject/MigParameters"
ModelName = "Model1_10AG"
Limits = 0 # 0 - No limits; 1 - Max and Min limits; 2 - Hist?rical limits 
Sd = 2
UpInitial = 0
############################
# mainDir = "../../MigParameters"
subDir = paste0(ModelName,"_",Limits)
dir.create(file.path(Path2Wdir, subDir), showWarnings = FALSE)
WDir = paste0(Path2Wdir,"/", subDir, "/")
#
# subDir2 = paste0(ModelName,"_",1)
# WDir2 = paste0(getwd(),"/", subDir2, "/")

# Built data input --------------------------------------------------------

df2array = function(DataFrame, Formula){
  Array = xtabs(Formula, data = DataFrame)
  Array = unname(Array)
  Array = as(Array, Class = "array")
  return(Array)
}

data.DM = DM ~ AG + Sex + Year + NUTS3
data.DMN = DMN ~ AG + Sex + Year + NUTS3
data.CP = nCx ~ AG + Sex + Year + NUTS3

DM = df2array(DataFrame = df_InputData, Formula = data.DM)
DMN = df2array(DataFrame = df_InputData, Formula = data.DMN)
CP = df2array(DataFrame = df_InputData, Formula = data.CP)

# Input data --------------------------------------------------------------

input.data = list(DMN = DMN, CP = CP)
input.data$AG = nlevels(df_InputData$AG)
input.data$Sex = nlevels(df_InputData$Sex)
input.data$Year = nlevels(as.factor(df_InputData$Year))
input.data$NUTS3 = nlevels(df_InputData$NUTS3)

# Model Building---------------------------------------------------------

Modelo = function(){
  for(i in 1:AG){
    for(j in 1:Sex){
      for(k in 1:Year){
        for(l in 1:NUTS3){
          # =====================================
          # LIKELIHOOD
          # =====================================
          # OP[i,j,k,l] <- CP[i,j,k,l] * (1 + DMN.p[i,j,k,l])
          # DMN.p[i,j,k,l] ~ dnorm(mu.M[i,j,k,l],tau.M[i,j])
          DMN[i,j,k,l] ~ dnorm(mu.M[i,j,k,l],tau.M[i,j])
          mu.M[i,j,k,l] <- Mgamma[i,j]
        } #NUTS3
      } # Year
      Mgamma[i,j] ~ dnorm(0.0,tau.gamma[i,j])
      #
      tau.gamma[i,j] ~ dgamma(0.000001,0.000001)
      var.gamma[i,j] <- 1 / tau.gamma[i,j]
      sigma.gamma[i,j] <- sqrt(var.gamma[i,j])
      #
      tau.M[i,j] ~ dgamma(0.001,0.001)
      var.M[i,j] <- 1 / tau.M[i,j]
      sigma.M[i,j] <- sqrt(var.M[i,j])
    } # Sex
  } # AG
} # Modelo


# Init data function ------------------------------------------------------

inits.data <- function(i){
  AG <- nlevels(as.factor(df_InputData$AG))
  Sex <- nlevels(as.factor(df_InputData$Sex))
  # NUTS2 <- nlevels(as.factor(df_InputData$NUTS2))
  NUTS3 <- nlevels(as.factor(df_InputData$NUTS3))
  Year <- 2
  
  # Migration
  set.seed(i)
  tau.gamma = array(rgamma(AG*Sex, 1, 1), dim=c(AG,Sex))
  set.seed(i)
  tau.M = array(rgamma(AG*Sex, 1, 1), dim=c(AG,Sex))
  return(list(tau.gamma = tau.gamma,
              tau.M = tau.M))
}
inits = list(inits.data(i=1), inits.data(i=2), inits.data(i=3))


# Parameters to save ------------------------------------------------------

parameters = c("Mgamma",
               "sigma.gamma",
               "sigma.M"#,
               # "DMN.p"
               )

write.model(Modelo, paste0(WDir ,"Modelo.txt"))
model.file = paste0(WDir ,"Modelo.txt")

# Bugs function -----------------------------------------------------------

Model = bugs(input.data,
             inits,
             parameters,
             model.file,
             n.chains = 3,
             n.iter = 11000, # 11000 60000
             n.burnin = 1000, #1000 10000
             n.thin = 5,
             bugs.directory = Path2BUGS,
             codaPkg=TRUE,
             bugs.seed = 1,
             # debug=TRUE,
             working.directory = WDir)

attach.all(Model$sims.list)


# Data analysis -----------------------------------------------------------

# Model.sim = read.bugs(c(paste0(WDir,"/coda1.txt"),
#                         paste0(WDir,"/coda2.txt"),
#                         paste0(WDir,"/coda3.txt")))
# 
# parameter_names = varnames(Model.sim)
# 
# library(tidybayes)
# df_Parameters = Model.sim %>%
#   spread_draws(Mgamma[AG,Sex], sigma.gamma[AG,Sex], sigma.M[AG,Sex]) %>%
#   group_by(.iteration, AG, Sex) %>%
#   summarize(Mgamma = mean(Mgamma),
#             sigma.gamma = mean(sigma.gamma),
#             sigma.M = mean(sigma.M))
# 
# df_DMN = Model.sim %>%
#   spread_draws(DMN.p[AG,Sex, Year, NUTS3])
# 
AG = input.data$AG
Sex = input.data$Sex
Year = input.data$Year
NUTS3 = input.data$NUTS3
# 
# 
DMN.p = array(NA, c(2000, AG,Sex, Year, NUTS3))
for(i in 1:2000){
  DMN.p[i,,,,] = rnorm(AG*Sex*Year*NUTS3,
                   Mgamma[i,,],
                   sigma.M[i,,])
}
# df_DMN.p2 = DMN.p2 %>%
#   spread_draws(DMN[AG,Sex,Year,NUTS3])
#   
#   as_tibble(DMN.p2)
  
  
  
  
  
  
  
  
  
  
  
# DMN.p2 = rnorm(AG*Sex*Year*NUTS3, Mgamma[])


# 
# # attach.all(Model.sim$sims)
# 
# library(tidybayes)
# df_Parameters = Model.sim %>%
#   spread_draws(Mgamma[AG,Sex], sigma.gamma[AG,Sex], sigma.M[AG,Sex]) %>%
#   group_by(AG,Sex) %>%
#   summarize(
#     MgammaoutI = quantile(Mgamma, 0.025),
#     MgammainnI = quantile(Mgamma, 0.16),
#     Mgammamean = quantile(Mgamma, 0.5),
#     MgammainnS = quantile(Mgamma, 0.84),
#     MgammaoutS = quantile(Mgamma, 0.975),
#     #
#     sigma.gammaoutI = quantile(sigma.gamma, 0.025),
#     sigma.gammainnI = quantile(sigma.gamma, 0.16),
#     sigma.gammamean = quantile(sigma.gamma, 0.5),
#     sigma.gammainnS = quantile(sigma.gamma, 0.84),
#     sigma.gammaoutS = quantile(sigma.gamma, 0.975),
#     #
#     sigma.MoutI = quantile(sigma.M, 0.025),
#     sigma.MinnI = quantile(sigma.M, 0.16),
#     sigma.Mmean = quantile(sigma.M, 0.5),
#     sigma.MinnS = quantile(sigma.M, 0.84),
#     sigma.MoutS = quantile(sigma.M, 0.975)
#   ) %>%
#   ungroup() %>%
#   mutate(AG = factor(levels(df_InputData$AG)[AG]),
#          Sex = factor(levels(df_InputData$Sex)[Sex]))
# 
# ####
# library(ggstance)
# tema <- theme(axis.line = element_line(colour = "black"),
#               panel.grid.major = element_blank(),
#               # panel.grid.major.y = element_line(colour = "black",
#               #                                   linetype = "dashed"),
#               panel.grid.minor = element_blank(),
#               # panel.grid.minor.y = element_line(colour = "black",
#               #                                   linetype = "dashed"),
#               panel.border = element_blank(),
#               panel.background = element_blank(),
#               plot.title = element_text(family = "Trebuchet MS",
#                                         color="#666666",
#                                         face="bold",
#                                         size=32,
#                                         hjust=0)) +
#   theme(axis.title = element_text(family = "Trebuchet MS",
#                                   color="#666666",
#                                   face="bold",
#                                   size=18)) +
#   theme(axis.text = element_text(color = "grey20",
#                                  size = 12,
#                                  angle = 0,
#                                  hjust = .5,
#                                  vjust = .5,
#                                  face = "plain"))
# ########
# 
# ggplot(df_Parameters, aes(x = Mgammamean, y = AG, color = Sex)) +
#   geom_point(size = 2, position=position_dodgev(height=-2)) +
#   geom_errorbarh(aes(xmin=MgammaoutI, xmax=MgammaoutS, width = 0), size = 0.5,
#                  position=position_dodgev(height=-2)) +
#   geom_errorbarh(aes(xmin=MgammainnI, xmax=MgammainnS, width = 0), size = 1,
#                  position=position_dodgev(height=-2)) +
#   scale_y_discrete(limits = rev(df_Parameters$AG)) +
#   labs(title = expression(paste(delta, "M", " by age group")),
#        x = expression(paste(delta, "M")),
#        y = "Ages groups") +
#   tema
