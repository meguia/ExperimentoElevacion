library(tidyverse)
library(Routliers)


rm(list=ls())
results_tbl <- read.csv("./DatosUnificados/Dresults.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

# Analysis Outliers 

# Unsigned bias
cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

f2 <- filter(results_tbl,type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_unsigned)) %>%
  ungroup() %>%
  ggplot(aes(x = condition,y = 100*mBiasUnsigned,colour = condition, fill = condition)) +
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "bar", 
               alpha = .4, 
               position = position_dodge(width = 1)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=2, 
               position = position_dodge(width = 1)) + 
  labs(x = "Condition", 
       y = "Relative signed \nbias [%]") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")

f2

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_unsigned ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mBiasUnsigned ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mBiasUnsigned  = mean(rel_bias_unsigned ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mBiasUnsigned ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Floor$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,]

idx = results_tbl$subject == "S001"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S003"
results_tbl = results_tbl[!idx,]

rm("res3", "tabla.ind.Floor", "tabla.ind.Eye")

write_csv(results_tbl, "./DatosUnificados/Dresults_without_outliers.csv")
