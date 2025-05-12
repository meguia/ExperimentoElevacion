library(tidyverse)
library(Routliers)

rm(list=ls())
results_tbl <- read.csv("./DatosUnificados/Dresults_without_outliers_slope_and_intercepto.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "NORMAL", location == "standing") %>% 
  group_by(subject,condition,target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_un_mean ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

idx = results_tbl$subject == "S001" & results_tbl$condition == "Ear level" & 
  results_tbl$target_distance != 6 &  
  results_tbl$type == "NORMAL" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "S013" & results_tbl$condition == "Ear level" & 
  results_tbl$target_distance != 2 &  
  results_tbl$type == "NORMAL" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "NORMAL", location == "standing") %>% 
  group_by(subject,condition,target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_un_mean,threshold = 3 ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mBiasUnsigned ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Floor$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,]

idx = results_tbl$subject == "S001" & results_tbl$condition == "Floor level" &
  results_tbl$type == "NORMAL" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "S002" & results_tbl$condition == "Floor level" &
  results_tbl$target_distance == 6 & 
  results_tbl$type == "NORMAL" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

#ROVED
tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "ROVED", location == "standing") %>% 
  group_by(subject,condition,target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_un_mean ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

idx = results_tbl$subject == "S001" & results_tbl$condition == "Ear level" &
  results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S013" & results_tbl$condition == "Ear level" &
  results_tbl$target_distance == 6 &
  results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "ROVED", location == "standing") %>% 
  group_by(subject,condition,target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_un_mean,threshold = 3 ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mBiasUnsigned ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Floor$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,]

idx = results_tbl$subject == "S001" & results_tbl$condition == "Floor level" & 
  results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "S002" & results_tbl$condition == "Floor level" &
  results_tbl$target_distance == 6 &
  results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "S013" & results_tbl$condition == "Floor level" &
  results_tbl$target_distance == 2 &
  results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "S013" & results_tbl$condition == "Floor level" &
  results_tbl$target_distance == 4.2 &
  results_tbl$type == "ROVED" & results_tbl$location == "standing"
results_tbl = results_tbl[!idx,]


tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "ROVED", location == "sitting") %>% 
  group_by(subject,condition,target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_un_mean,threshold = 3 ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mBiasUnsigned ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Floor$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,]

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "ROVED", location == "sitting") %>% 
  group_by(subject,condition,target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_un_mean ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

idx = results_tbl$subject == "T013" & results_tbl$condition == "Ear level" &
  results_tbl$target_distance == 2 &
  results_tbl$type == "ROVED" & results_tbl$location == "sitting"
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "T015" & results_tbl$condition == "Ear level" &
  results_tbl$target_distance != 4.2 &
  results_tbl$type == "ROVED" & results_tbl$location == "sitting"
results_tbl = results_tbl[!idx,]


rm("res3", "tabla.ind.Floor", "tabla.ind.Eye")

write_csv(results_tbl, "D:/GITHUB/Elevation_as_an_cue_for_PAD/Elevation_PAD_analisys/data/rawdataindLOGNUEVO.csv")


