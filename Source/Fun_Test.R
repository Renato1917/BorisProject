library(tidyverse)

source("./Fun_LTinput.R")
source("./Fun_Projection.R")

NetMig = LTinput(PreFormat = FALSE) %>%
  arrange(Year, NUTS3, Sex, AG)




df_Projection = ProjectNUTSYear(LifeTable = NetMig) 

# JMM comparetion ---------------------------------------------------------

Idade = levels(NetMig$AG)[6:17] #%>%
# droplevels()

load("../../JMMData/Data/InputData.RData")
df_InputData = df_InputData %>%
  filter(Year %in% c(1990:2010)) %>%
  select(AG:DM) %>%
  mutate(AG = factor(AG, labels = Idade)) %>%
  droplevels()

df_ProjectMig = df_Projection %>%
  filter(Year %in% c(1991:2011) & AG %in% Idade) %>%
  mutate(DMig = nKx - nOx) %>%
  filter(NUTS3 %in% unique(df_InputData$NUTS)) %>%
  rename(NUTS = NUTS3) %>%
  ungroup() %>%
  droplevels() %>%
  mutate(Year = factor(as.numeric(Year)-1),
         # Sex = factor(Sex, levels = levels(df_InputData$Sex)),
         NUTS = factor(NUTS, levels = levels(df_InputData$NUTS))) 

df_Compare = df_InputData %>%
  left_join(df_ProjectMig, by = c("AG", "Sex", "Year", "NUTS")) %>%
  select(Year, AG, Sex, NUTS3_DSG, NUTS2, NUTS, 
         OpenPop, nKx, ClosePop, nOx, DM, DMig)

df_Z = df_Compare %>%
  filter(Year != 1990) %>%
  mutate(Z = DM / DMig#,
         # RT1 = Z / DM,
         # RT2 = Z / DMig
         ) %>%
  rename(JM = DM,
         RG = DMig) %>%
  select(Year:NUTS,nKx, JM:Z) %>%
  gather(key = Type, value = Mig, JM:Z)



# df_Z1 = df_Z %>%
#   filter(Type %in% c("MigJM","MigRG") & Year == 2010 & Sex == "H" & NUTS == "111")

ggplot(df_Z %>%
         filter(Type %in% c("JM","RG") &
                  AG != "AG75" &
                  Year == 2000 &
                  Sex == "M" #&
                # NUTS == "16B"
         ),
       aes(x = AG, y = Mig, fill = Type, color = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NUTS + NUTS3_DSG, scales = "free_y")

ggplot(df_Z %>%
         filter(Type %in% c("JM","RG") &
                  AG != "AG75" &
                  Year == 2010 &
                  Sex == "M" #&
                # NUTS == "171"
         ),
       aes(x = AG, y = Mig, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ NUTS + NUTS3_DSG, scales = "free_y")

df_Zmean = df_Z %>%
  filter(Type == "Z"  & AG != "AG75") %>%
  group_by(Year,NUTS,Sex) %>%
  summarize(#Zmean = mean(Mig),
            Zmean2 = weighted.mean(Mig, nKx),
            ZoutI = quantile(Mig, 0.025),
            ZinnI = quantile(Mig, 0.16),
            Zmean = quantile(Mig, 0.5),
            ZinnS = quantile(Mig, 0.84),
            ZoutS = quantile(Mig, 0.975))


# Plots net migration -----------------------------------------------------

