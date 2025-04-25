


rm(list=ls())
results_tbl3 <- read.csv("D:/GITHUB/Elevation_as_an_cue_for_PAD/Elevation_PAD_analisys/data/rawdataind.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
results_tbl <- read.csv("./DatosUnificados/Dresults_without_outliers_slope_and_intercepto.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
# results_tbl3 <- results_tbl %>% mutate(block = recode(block, "NORMAL" = "FIXED"))
# results_tbl3 <- results_tbl3 %>% mutate(block = recode(block, "NORMAL" = "FIXED"))

# PARADOS
tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "NORMAL", location == "standing") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_unsigned ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mBiasUnsigned,threshold = 2 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,]

idx = results_tbl$subject == "S003" & results_tbl$condition == "Ear level" & results_tbl$type == "NORMAL" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]


tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "NORMAL", location == "standing") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_unsigned,threshold = 3 ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mBiasUnsigned ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Floor$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,]

idx = results_tbl$subject == "S001" & results_tbl$condition == "Floor level" & results_tbl$type == "NORMAL" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "ROVED", location == "standing") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_unsigned ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mBiasUnsigned,threshold = 2 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

idx = results_tbl$subject == "S003" & results_tbl$condition == "Ear level" & results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S012" & results_tbl$condition == "Ear level" & results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S019" & results_tbl$condition == "Ear level" & results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]


tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "ROVED", location == "standing") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_unsigned,threshold = 3 ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mBiasUnsigned ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Floor$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,]

idx = results_tbl$subject == "S001" & results_tbl$condition == "Floor level" & results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S003" & results_tbl$condition == "Floor level" & results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

#SENTADO
tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "ROVED", location == "sitting") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_signed ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mBiasUnsigned,threshold = 5 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

# idx = results_tbl$subject == "T005" & results_tbl$condition == "Ear level" & results_tbl$type == "ROVED" & results_tbl$location == "sitting"
# results_tbl = results_tbl[!idx,]
# idx = results_tbl$subject == "T017" & results_tbl$condition == "Ear level" & results_tbl$type == "ROVED" & results_tbl$location == "sitting"
# results_tbl = results_tbl[!idx,]

tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "ROVED", location == "sitting") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_signed,threshold = 3 ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mBiasUnsigned ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Floor$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,]

# idx = results_tbl$subject == "T005" & results_tbl$condition == "Floor level" & results_tbl$type == "ROVED" & results_tbl$location == "sitting"
# results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "T005"
results_tbl = results_tbl[!idx,]

results_tbl <- results_tbl %>% select(-block)
names(results_tbl)[names(results_tbl) == "type"] <- "block"

setdiff(names(results_tbl), names(results_tbl3))
write_csv(results_tbl, "D:/GITHUB/Elevation_as_an_cue_for_PAD/Elevation_PAD_analisys/data/rawdataindLOG.csv")
