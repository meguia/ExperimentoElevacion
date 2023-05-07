library(tidyverse)
library(lme4)
library(janitor)
library(nlme)
library(lmerTest)
library(emmeans)
library(jtools)
library(broom)
library(ggstatsplot)
library(gmodels)
library(ggpubr)
library(Routliers)

rm(list=ls())
tabla.raw <- read.csv("./DatosUnificados/datacruda.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
tabla.raw$abs_bias <-  tabla.raw$percived_distance - tabla.raw$target_distance
tabla.raw$rel_bias <- (tabla.raw$percived_distance - tabla.raw$target_distance) / tabla.raw$target_distance
idx = tabla.raw$subject == "S013" & tabla.raw$percived_distance == 0.05
tabla.raw[idx,]$percived_distance = 0.5


f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

results_tbl <- tibble(aggregate(cbind(percived_distance,rel_bias,abs_bias) ~ subject*block*condition*target_distance*type,
                              data = tabla.raw,
                              FUN  = f_promedio,na.action = NULL))


results_tbl %>%
  clean_names() %>%
  mutate(subject = factor(subject),
         condition = factor(condition),
         type = factor(type),
         block = factor(block),
         perc_dist_sd = percived_distance[,"sd"],
         perc_dist_sem = percived_distance[,"sem"],
         perc_dist_var = percived_distance[,"var"],
         perc_dist_n = percived_distance[,"n"],
         perc_dist = percived_distance[,"mean"],
         rel_bias_signed_sd = rel_bias[,"sd"],
         rel_bias_signed_sem = rel_bias[,"sem"],
         rel_bias_signed_var = rel_bias[,"var"],
         rel_bias_signed_n = rel_bias[,"n"],
         rel_bias_signed = rel_bias[,"mean"],
         abs_bias_sd = abs_bias[,"sd"],
         abs_bias_sem = abs_bias[,"sem"],
         abs_bias_var = abs_bias[,"var"],
         abs_bias_n = abs_bias[,"n"],
         abs_bias = abs_bias[,"mean"]) %>%
  write_csv("./DatosUnificados/results.csv")

results_tbl <- read.csv("./DatosUnificados/results.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

# Outliers ----

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mSesgoRel ,na.rm=TRUE)
#plot_outliers_mad(res3,x=tabla.ind.Eye$mSesgoRel,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mSesgoRel ,na.rm=TRUE)
#plot_outliers_mad(res3,x=tabla.ind.Floor$mSesgoRel,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,] 

# 1 - ABS bias

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoAbs  = mean(abs_bias,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mSesgoAbs ,na.rm=TRUE)
#plot_outliers_mad(res3,x=tabla.ind.Eye$mSesgoAbs,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoAbs  = mean(abs_bias,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mSesgoAbs ,na.rm=TRUE)
#plot_outliers_mad(res3,x=tabla.ind.Floor$mSesgoAbs,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,]

### Graf-----

#Distance
#percived_distance and target_distance in log10()
results_tbl$percived_distance_log <-  log10(results_tbl$percived_distance)
results_tbl$target_distance_log <-  log10(results_tbl$target_distance)

m.ReachingDist <- lmer(mDist_perc ~ blind_cat*dist_fis + (dist_fis|subject), 
                       data = filter(tabla_ADP.ind.summ_dist, 
                                     (block=="reaching") & (reach_cat=="Within")) )
ggcoefstats(m.ReachingDist, output = "tidy") %>% select(-label)
anova(m.ReachingDist)

m.Dist <-  lmer(percived_distance_log ~ target_distance_log*condition + (target_distance_log|subject),
               data = filter(results_tbl,type == "NORMAL"))

ggcoefstats(m.Dist, output = "tidy") %>% select(-label)
anova(m.Dist)


results_tbl$Predsubject = fitted(m.Dist, level=1)
results_tbl$PredPob    = fitted(m.Dist, level=0)



eq1 <- substitute("Ear level"~~~~~~italic(y) == a %.% italic(X)^italic(b), 
                  list(a = 0.43,
                       b = 1.01))
eq2 <- substitute("Floor level"~~~italic(y) == a %.% italic(X)^italic(b), 
                  list(a = 0.44,
                       b = 1.02))


tabla.pob = results_tbl %>% group_by(target_distance,condition) %>%
  summarise(Mperc_dist  = mean(percived_distance))  %>%
  ungroup()


cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
f1 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = condition, color = condition)) + 
  
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  #geom_line(data = results_tbl, mapping = aes(x=target_distance, y=percived_distance,group = interaction(subject,condition), color = condition ) , alpha=.4, size=0.4)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
  #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
  #scale_x_log10(name="Distance source (m)", breaks=c(0,1,2,2.9,4.2,6,7), labels=c("",1,2,3.9,4.2,6,""), minor_breaks=NULL, limits = c(-2.3,6.1)) +
  #scale_y_log10(name="Perceived distance (m)",  breaks=c(0,1,2,2.9,4.2,6,7), labels=c("",1,2,3.9,4.2,6,""), minor_breaks=NULL, limits = c(-2,8)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f1
## Bias signed
f2 <- results_tbl %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias)) %>%
  ungroup() %>%
  ggplot(aes(x = condition,y = 100*mSesgoRel,colour = condition, fill = condition)) +
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

# Response variability
## Intra-subject
tabla.ind.var <- results_tbl %>% 
  group_by(target_distance,condition) %>%
  summarise(mSD = mean(perc_dist_sd),
            SdSd = sd(perc_dist_sd),
            n = n())  %>%
  ungroup()

f3 <- ggplot(tabla.ind.var, aes(x=target_distance, y =mSD, group = condition, color = condition)) + 
  geom_point()+ 
  geom_line(size = 1)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_errorbar(data=tabla.ind.var,alpha = 2, width=0, size=1,
                mapping=aes(ymin = mSD - (SdSd/sqrt(n)), 
                            ymax = mSD + (SdSd/sqrt(n)),
                            color=condition))+ 
  geom_abline(intercept = 0, slope = 0, linetype=2) +
  scale_y_log10(name="Standard deviation (m)\n +/- SEM Intra-subject", breaks=c(0,0.2,0.3,0.4,0.5), labels=c(0,0.2,0.3,0.4,0.5), minor_breaks=NULL, limits = c(-0.1,0.5)) +
  scale_x_log10(name="Distance source (m)",  breaks=c(2.4,3.6,4.8,6), labels=c(2.4,3.6,4.8,6), minor_breaks=NULL, limits = c(2.3,6.1)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())


## Intra-Sujeto colapsado
f4 <- results_tbl %>% 
  group_by(subject,condition) %>%
  summarise(mDist_perc = mean(percived_distance),
            mSesgoRel  = mean(rel_bias),
            mSD = mean(perc_dist_sd))  %>%
  ungroup() %>%
  ggplot(aes(x = condition,y = 100*mSD, colour = condition, fill = condition)) +
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
       y = "Collapsed standard\ndeviation [%] Intra-subject") +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")


# Unifico graficos en una sola figura
Figure2 = ggarrange(f1,f2,f3,f4,
                    labels = c("a", "b","c","d"),
                    ncol = 2, nrow = 2,
                    common.legend = TRUE, legend="top", align = "hv")
Figure2



