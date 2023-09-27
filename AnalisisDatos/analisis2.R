library(tidyverse)
library(lme4)
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
figures_folder = "figuras"

#Data entry -----
tabla.raw <- read.csv("./DatosUnificados/datacrudafinal2.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
tabla.raw$abs_bias <-  tabla.raw$percived_distance - tabla.raw$target_distance
tabla.raw$rel_bias <- (tabla.raw$percived_distance - tabla.raw$target_distance) / tabla.raw$target_distance
idx = tabla.raw$subject == "13" | tabla.raw$percived_distance == 0.05
tabla.raw[idx,]$percived_distance = 0.5


f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

results_tbl <- tibble(aggregate(cbind(percived_distance,rel_bias,abs_bias) ~ subject*block*condition*target_distance*type,
                              data = tabla.raw,
                              FUN  = f_promedio,na.action = NULL))

# Graf datos crudos
cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
f.all = ggplot(results_tbl, aes(x = target_distance, y = percived_distance[,"mean"], colour = condition, fill = condition))+
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .1,
                                             jitter.height = 0,
                                             dodge.width = .1 )) +
  geom_point(data = tabla.raw, aes(x=target_distance, y = percived_distance, color = condition),
                                       alpha = 0.4, size = .6,
             position = position_jitterdodge(jitter.width = .1,
                                             jitter.height = 0,
                                             dodge.width = .1 )) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 

  geom_abline(slope = 1, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  # stat_summary(fun.data = "mean_se", 
  #              geom = "pointrange", 
  #              alpha = .4, 
  #              position = position_dodge(width = 1)) +
  # stat_summary(fun.data = "mean_se", 
  #              geom = "linerange",  
  #              size=2, 
  #              position = position_dodge(width = 1)) + 
  #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
  #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
  scale_x_continuous(name="Distance source (m)", limits = c(-1,20)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(-1,20)) +
  facet_grid(type ~ subject) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())
f.all
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1 All data", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f.all, width=90, height=10, units="cm", limitsize=FALSE, dpi=600)


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

### Analysis Slopes -----
results_tbl$slope = 0
results_tbl$intercepto = 0
fig_normal = list()
for (i in 1:length(levels(results_tbl$subject))) {
  print(i)
  sub = levels(results_tbl$subject)[i]
  print(sub)
  m.pend = lm(percived_distance ~ target_distance*condition,
              data = filter(results_tbl,type == "NORMAL", subject == sub))

  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$slope = m.pend$coefficients[[2]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$slope = m.pend$coefficients[[2]]+m.pend$coefficients[[4]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$intercepto = m.pend$coefficients[[1]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$intercepto = m.pend$coefficients[[1]]+m.pend$coefficients[[3]]
  
  cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
  

  eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                    list(a = round(m.pend$coefficients[[2]],digits = 2),
                         b = round(m.pend$coefficients[[1]], digits = 2)))
  eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                    list(a = round(m.pend$coefficients[[2]]+m.pend$coefficients[[4]], digits = 2),
                         b = round(m.pend$coefficients[[1]]+m.pend$coefficients[[3]], digits = 2)))
  eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                    list(b = round(summary(m.pend)$r.squared, digits = 2)))
  
  fig1 = ggplot(filter(results_tbl,type == "NORMAL", subject == sub), 
                  aes(x = target_distance, y = perc_dist, ymin = perc_dist-perc_dist_sem, ymax = perc_dist+perc_dist_sem, 
                      colour = condition, fill = condition, group = condition))+
    geom_pointrange(alpha = 0.4, 
                    position = position_jitterdodge(jitter.width = .1,
                                                    jitter.height = 0,
                                                    dodge.width = .1 ))+
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    geom_abline(slope = 1, 
                intercept = 0, 
                alpha = 0.5, 
                linetype = "dashed") +
    geom_abline(slope = m.pend$coefficients[[2]], 
                intercept = m.pend$coefficients[[1]], 
                alpha = 0.5,
                color = "#000000") +
    geom_abline(slope = m.pend$coefficients[[2]]+m.pend$coefficients[[4]], 
                intercept = m.pend$coefficients[[1]]+m.pend$coefficients[[3]], 
                alpha = 0.5,
                color = "#E69F00") +
    geom_text(x = 0.2, y = 6.6, label = sub, hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#999999")+
    geom_text(x = 0.2, y = 6.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
    geom_text(x = 0.2, y = 5.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
    geom_text(x = 0.2, y = 5.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
    scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  fig_normal[[i]] = fig1

}

Figure1 = ggarrange(fig_normal[[1]],fig_normal[[2]],fig_normal[[3]],fig_normal[[4]],fig_normal[[5]],fig_normal[[6]],fig_normal[[7]],fig_normal[[8]],fig_normal[[9]],
                    fig_normal[[10]],fig_normal[[11]],fig_normal[[12]],fig_normal[[13]],fig_normal[[14]],fig_normal[[15]],fig_normal[[16]],fig_normal[[17]],fig_normal[[18]],
                    fig_normal[[19]],fig_normal[[20]],fig_normal[[21]],fig_normal[[22]],
                    ncol = 5, nrow = 5,
                    common.legend = TRUE, legend="top", align = "hv")
# Figure1
mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "1. Lm for subject NORMAL", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure1, width=50, height=50, units="cm", limitsize=FALSE, dpi=600)

# Figure lm for ROVED
fig_roved = list()
for (i in 1:length(levels(results_tbl$subject))) {
  print(i)
  sub = levels(results_tbl$subject)[i]
  print(sub)
  m.pend = lm(percived_distance ~ target_distance*condition,
              data = filter(results_tbl,type == "ROVED", subject == sub))
  
  results_tbl[(results_tbl$type == "ROVED" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$slope = m.pend$coefficients[[2]]
  results_tbl[(results_tbl$type == "ROVED" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$slope = m.pend$coefficients[[2]]+m.pend$coefficients[[4]]
  results_tbl[(results_tbl$type == "ROVED" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$intercepto = m.pend$coefficients[[1]]
  results_tbl[(results_tbl$type == "ROVED" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$intercepto = m.pend$coefficients[[1]]+m.pend$coefficients[[3]]
  
  
  cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
  
  
  eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                    list(a = round(m.pend$coefficients[[2]],digits = 2),
                         b = round(m.pend$coefficients[[1]], digits = 2)))
  eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                    list(a = round(m.pend$coefficients[[2]]+m.pend$coefficients[[4]], digits = 2),
                         b = round(m.pend$coefficients[[1]]+m.pend$coefficients[[3]], digits = 2)))
  eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                    list(b = round(summary(m.pend)$r.squared, digits = 2)))
  
  fig2 = ggplot(filter(results_tbl,type == "ROVED", subject == sub), 
                aes(x = target_distance, y = perc_dist, ymin = perc_dist-perc_dist_sem, ymax = perc_dist+perc_dist_sem, 
                    colour = condition, fill = condition, group = condition))+
    geom_pointrange(alpha = 0.4, 
                    position = position_jitterdodge(jitter.width = .1,
                                                    jitter.height = 0,
                                                    dodge.width = .1 ))+
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    geom_abline(slope = 1, 
                intercept = 0, 
                alpha = 0.5, 
                linetype = "dashed") +
    geom_abline(slope = m.pend$coefficients[[2]], 
                intercept = m.pend$coefficients[[1]], 
                alpha = 0.5,
                color = "#000000") +
    geom_abline(slope = m.pend$coefficients[[2]]+m.pend$coefficients[[4]], 
                intercept = m.pend$coefficients[[1]]+m.pend$coefficients[[3]], 
                alpha = 0.5,
                color = "#E69F00") +
    geom_text(x = 0.2, y = 6.6, label = sub, hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#999999")+
    geom_text(x = 0.2, y = 6.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
    geom_text(x = 0.2, y = 5.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
    geom_text(x = 0.2, y = 5.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
    scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  fig_roved[[i]] = fig2
  
}

Figure2 = ggarrange(fig_roved[[1]],fig_roved[[2]],fig_roved[[3]],fig_roved[[4]],fig_roved[[5]],fig_roved[[6]],fig_roved[[7]],fig_roved[[8]],fig_roved[[9]],
                    fig_roved[[10]],fig_roved[[11]],fig_roved[[12]],fig_roved[[13]],fig_roved[[14]],fig_roved[[15]],fig_roved[[16]],fig_roved[[17]],fig_roved[[18]],
                    fig_roved[[19]],fig_roved[[20]],fig_roved[[21]],fig_roved[[22]],
                    ncol = 5, nrow = 5,
                    common.legend = TRUE, legend="top", align = "hv")
mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "2. Lm for subject ROVED", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure2, width=50, height=50, units="cm", limitsize=FALSE, dpi=600)


# Slope for condition

f3 =  ggplot(results_tbl, aes(x = condition,y = slope, colour = condition, fill = condition)) +
  geom_line(aes(group = subject), alpha = 0.3)+ 
  geom_point(alpha = 1) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se",
               geom = "pointrange",
               alpha = 1,
               size = 1,
               # position = position_dodge(width = 1)
               position = position_jitterdodge(jitter.width = 0.6,
                                               jitter.height = 0,
                                               dodge.width = 0 )) +
  # stat_summary(fun.data = "mean_se",
  #              geom = "linerange",
  #              size=2,
  #              position = position_dodge(width = 1)) +
  labs(x = "Condition", 
       y = "Slope with LM") +
  facet_grid(. ~ type) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")

f3
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "3. Lm Slope", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f3, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

# Intercepto for condition

f4 =  ggplot(results_tbl, aes(x = condition,y = intercepto, colour = condition, fill = condition)) +
  geom_line(aes(group = subject), alpha = 0.3)+ 
  geom_point(alpha = 1) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se",
               geom = "pointrange",
               alpha = 1,
               size = 1,
               # position = position_dodge(width = 1)
               position = position_jitterdodge(jitter.width = 0.6,
                                               jitter.height = 0,
                                               dodge.width = 0 )) +
  # stat_summary(fun.data = "mean_se",
  #              geom = "linerange",
  #              size=2,
  #              position = position_dodge(width = 1)) +
  labs(x = "Condition", 
       y = "Intercepto with LM") +
  facet_grid(. ~ type) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")

f4
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "4. Lm Intercepto", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f4, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)












# Analysis Outliers ----

# Slope

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mslope  = mean(slope,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mslope ,na.rm=TRUE)
#plot_outliers_mad(res3,x=tabla.ind.Eye$mSesgoRel,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mslope  = mean(slope,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$mslope ,na.rm=TRUE)
#plot_outliers_mad(res3,x=tabla.ind.Floor$mSesgoRel,pos_display=TRUE)
tabla.ind.Floor[res3$outliers_pos,] 

# Intercepto

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(minter  = mean(intercepto,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$minter ,na.rm=TRUE)
#plot_outliers_mad(res3,x=tabla.ind.Eye$mSesgoRel,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 

tabla.ind.Floor <- results_tbl %>% 
  filter(condition == "Floor level", type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(minter  = mean(intercepto,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Floor$minter ,na.rm=TRUE)
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

idx = results_tbl$subject == "S003"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S012"
results_tbl = results_tbl[!idx,]

rm("res3", "tabla.ind.Floor", "tabla.ind.Eye", "tabla.raw")


#Distance ------
#percived_distance and target_distance lin
m.Dist.lin <-  lmer(percived_distance ~ target_distance*condition + (target_distance|subject),
                data = filter(results_tbl,type == "NORMAL"))

# m.Dist1 <-  lme(percived_distance ~ target_distance*condition, random = ~target_distance|subject,
#                 data = filter(results_tbl,type == "NORMAL"))
# extract_stats(ggcoefstats(m.Dist1))
# anova(m.Dist1)

extract_stats(ggcoefstats(m.Dist.lin))
anova(m.Dist.lin)
 
# results_tbl$Predsubject = fitted(m.Dist1, level=1)
# results_tbl$PredPob    = fitted(m.Dist1, level=0)

results_tbl$Predsubject.lin = fitted(m.Dist.lin, level=1)
# results_tbl$PredPob   = fitted(m.Dist, level=0)

# eq1 <- substitute("Ear level"~~~~~~italic(y) == a %.% italic(X)^italic(b), 
#                   list(a = 0.51,
#                        b = 0.98))
# eq2 <- substitute("Floor level"~~~italic(y) == a %.% italic(X)^italic(b), 
#                   list(a = 0.44,
#                        b = 0.59))


tabla.pob = filter(results_tbl,type == "NORMAL") %>% group_by(target_distance,condition) %>%
  summarise(Mperc_dist  = mean(percived_distance))  %>%
  ungroup()
tabla.pob$PredPob.lin = 0
idx = tabla.pob$condition == "Ear level"
tabla.pob[idx,]$PredPob.lin = 0.3322*c(2,2.9,4.2,6)+0.9702
idx = tabla.pob$condition == "Floor level"
tabla.pob[idx,]$PredPob.lin = (0.3322+0.3351)*c(2,2.9,4.2,6)+(0.9702-0.8352)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
f1 <- ggplot(tabla.pob, aes(x=target_distance, y =PredPob.lin, group = condition, color  = condition)) + 
  geom_line(size = 2)+
  # geom_line(data = results_tbl, aes(x=target_distance, y = PredPob.lin, color = condition), size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  geom_line(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x=target_distance, y=Predsubject.lin, group = interaction(subject,condition)) ,
            alpha=.4, size=0.4)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  ggtitle("Lineal Normal")+
  #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
  #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
  scale_x_continuous(name="Distance source (m)", breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,8)) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,8)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

  f1
  mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Lineal-Normal", ".png", sep = '')
  ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)
  
  #por sujeto
  f1 <- ggplot(tabla.pob, aes(x=target_distance, y =PredPob.lin, group = condition, color  = condition)) + 
    # geom_line(size = 2)+
    # geom_line(data = results_tbl, aes(x=target_distance, y = PredPob.lin, color = condition), size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype=2) +
    geom_line(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x=target_distance, y=Predsubject.lin, group = interaction(subject,condition)) ,
              alpha=1, size=0.4)+
    geom_point(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x = target_distance, y = perc_dist),
               alpha = 1, 
               position = position_jitterdodge(jitter.width = .3,
                                               jitter.height = 0,
                                               dodge.width = 1 )) +
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    
    #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
    #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
    scale_x_continuous(name="Distance source (m)", breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,7)) +
    scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,7)) +
    facet_wrap(. ~ subject) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  
  f1
  mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Lineal-Normal.ALL", ".png", sep = '')
  ggsave(mi_nombre_de_archivo, plot=f1, width=30, height=30, units="cm", limitsize=FALSE, dpi=600)
  
  # Poblacional
  
  #por sujeto
  f1.p <- ggplot(tabla.pob, aes(x=target_distance, y =PredPob.lin, group = condition, color  = condition)) + 
    geom_line(size = 1)+
    # geom_line(data = results_tbl, aes(x=target_distance, y = PredPob.lin, color = condition), size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype=2) +
    geom_point(aes(x = target_distance, y = Mperc_dist),
               alpha = 1)+
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    
    #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
    #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
    scale_x_continuous(name="Distance source (m)", breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,7)) +
    scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,7)) +
    # facet_wrap(. ~ subject) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  
  f1.p
  mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Lineal-Normal.pob", ".png", sep = '')
  ggsave(mi_nombre_de_archivo, plot=f1.p, width=10, height=10, units="cm", limitsize=FALSE, dpi=600)
  
  
 #--
  #percived_distance and target_distance in log10()
  results_tbl$percived_distance_log <-  log10(results_tbl$percived_distance)
  results_tbl$target_distance_log <-  log10(results_tbl$target_distance)
  

  m.Dist.log <-  lmer(percived_distance_log ~ target_distance_log*condition + (target_distance_log|subject),
                  data = filter(results_tbl,type == "NORMAL"))
  
  extract_stats(ggcoefstats(m.Dist.log))
  anova(m.Dist.log)
  
  
  results_tbl$Predsubject.log = fitted(m.Dist.log, level=1)
  # results_tbl$PredPob    = fitted(m.Dist, level=0)
  
  #   eq1 <- substitute("Ear level"~~~~~~italic(y) == a %.% italic(X)^italic(b), 
  #                   list(a = 0.51,
  #                        b = 0.98))
  # eq2 <- substitute("Floor level"~~~italic(y) == a %.% italic(X)^italic(b), 
  #                   list(a = 0.44,
  #                        b = 0.59))
  
  
  tabla.pob = filter(results_tbl,type == "NORMAL") %>% group_by(target_distance,condition) %>%
    summarise(Mperc_dist  = mean(percived_distance))  %>%
    ungroup()
  tabla.pob$PredPob.log = 0
  idx = tabla.pob$condition == "Ear level"
  tabla.pob[idx,]$PredPob.log = 0.61245*log10(c(2,2.9,4.2,6))+(-0.06628)
  idx = tabla.pob$condition == "Floor level"
  tabla.pob[idx,]$PredPob.log = (0.61245+0.34018)*log10(c(2,2.9,4.2,6))+(-0.06628-0.11821)
  
  cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
  f1 <- ggplot(tabla.pob, aes(x=target_distance, y =10^PredPob.log, group = condition, fill = condition, color  = condition)) + 
    geom_line(size = 2)+
   # geom_line(data = results_tbl, aes(x=target_distance, y = 10^PredPob, color = condition), size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype=2) +
    geom_line(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x=target_distance, y=10^Predsubject.log, group = interaction(subject,condition)) ,
              alpha=.4, size=0.4)+
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    ggtitle("Log Normal")+
    #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
    #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
    scale_x_continuous(name="Distance source (m)", breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,8)) +

    # scale_x_log10(name="Distance source (m)") +
    # scale_y_log10(name="Perceived distance (m)") +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  
  f1
  mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Log-Normal", ".png", sep = '')
  ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)
  
  
# ROVED DISTANCE  ------
  #Distance
  #percived_distance and target_distance lin
  m.Dist.lin.r <-  lmer(percived_distance ~ target_distance*condition + (target_distance|subject),
                      data = filter(results_tbl,type == "ROVED"))
  
  # m.Dist1 <-  lme(percived_distance ~ target_distance*condition, random = ~target_distance|subject,
  #                 data = filter(results_tbl,type == "NORMAL"))
  # extract_stats(ggcoefstats(m.Dist1))
  # anova(m.Dist1)
  
  extract_stats(ggcoefstats(m.Dist.lin.r))
  anova(m.Dist.lin.r)
  
  # results_tbl$Predsubject = fitted(m.Dist1, level=1)
  # results_tbl$PredPob    = fitted(m.Dist1, level=0)
  
  results_tbl$Predsubject.lin.r = fitted(m.Dist.lin.r, level=1)
  # results_tbl$PredPob   = fitted(m.Dist, level=0)
  
  # eq1 <- substitute("Ear level"~~~~~~italic(y) == a %.% italic(X)^italic(b), 
  #                   list(a = 0.51,
  #                        b = 0.98))
  # eq2 <- substitute("Floor level"~~~italic(y) == a %.% italic(X)^italic(b), 
  #                   list(a = 0.44,
  #                        b = 0.59))
  
  
  tabla.pob = filter(results_tbl,type == "NORMAL") %>% group_by(target_distance,condition) %>%
    summarise(Mperc_dist  = mean(percived_distance))  %>%
    ungroup()
  tabla.pob$PredPob.lin.r = 0
  idx = tabla.pob$condition == "Ear level"
  tabla.pob[idx,]$PredPob.lin.r = 0.1058*c(2,2.9,4.2,6)+2.0402
  idx = tabla.pob$condition == "Floor level"
  tabla.pob[idx,]$PredPob.lin.r = (0.1058+0.3997)*c(2,2.9,4.2,6)+(2.0402-1.3759)
  
  cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
  f1 <- ggplot(tabla.pob, aes(x=target_distance, y =PredPob.lin.r, group = condition, color  = condition)) + 
    geom_line(size = 2)+
    # geom_line(data = results_tbl, aes(x=target_distance, y = PredPob.lin, color = condition), size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype=2) +
    geom_line(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x=target_distance, y=Predsubject.lin.r, group = interaction(subject,condition)) ,
              alpha=.4, size=0.4)+
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    ggtitle("Lineal Roved")+
    #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
    #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
    scale_x_continuous(name="Distance source (m)", breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,8)) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  
  f1
  mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Lineal-Roved", ".png", sep = '')
  ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)
  
  #--
  #percived_distance and target_distance in log10()
  results_tbl$percived_distance_log <-  log10(results_tbl$percived_distance)
  results_tbl$target_distance_log <-  log10(results_tbl$target_distance)
  
  
  m.Dist.log.r <-  lmer(percived_distance_log ~ target_distance_log*condition + (target_distance_log|subject),
                      data = filter(results_tbl,type == "ROVED"))
  
  extract_stats(ggcoefstats(m.Dist.log.r))
  anova(m.Dist.log.r)
  
  
  results_tbl$Predsubject.log.r = fitted(m.Dist.log.r, level=1)
  # results_tbl$PredPob    = fitted(m.Dist, level=0)
  
  #   eq1 <- substitute("Ear level"~~~~~~italic(y) == a %.% italic(X)^italic(b), 
  #                   list(a = 0.51,
  #                        b = 0.98))
  # eq2 <- substitute("Floor level"~~~italic(y) == a %.% italic(X)^italic(b), 
  #                   list(a = 0.44,
  #                        b = 0.59))
  
  
  tabla.pob = filter(results_tbl,type == "NORMAL") %>% group_by(target_distance,condition) %>%
    summarise(Mperc_dist  = mean(percived_distance))  %>%
    ungroup()
  tabla.pob$PredPob.log.r = 0
  idx = tabla.pob$condition == "Ear level"
  tabla.pob[idx,]$PredPob.log.r = 0.2700*log10(c(2,2.9,4.2,6))+(0.1506)
  idx = tabla.pob$condition == "Floor level"
  tabla.pob[idx,]$PredPob.log.r = (0.2700+0.5060)*log10(c(2,2.9,4.2,6))+(0.1506-0.2460)
  
  cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
  f1 <- ggplot(tabla.pob, aes(x=target_distance, y =10^PredPob.log.r, group = condition, fill = condition, color  = condition)) + 
    geom_line(size = 2)+
    # geom_line(data = results_tbl, aes(x=target_distance, y = 10^PredPob, color = condition), size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype=2) +
    geom_line(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x=target_distance, y=10^Predsubject.log.r, group = interaction(subject,condition)) ,
              alpha=.4, size=0.4)+
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    ggtitle("Log Roved")+
    #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
    #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
    scale_x_continuous(name="Distance source (m)", breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2,2.9,4.2,6,7), labels=c("",2,2.9,4.2,6,""), minor_breaks=NULL, limits = c(0,8)) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  
  f1
  mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Log-Roved", ".png", sep = '')
  ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)
  
  
# Slops graf ------
  
tabla.pob.slope <- results_tbl %>% 
      group_by(subject, condition, type) %>%
      summarise(mSesgoRel  = mean(rel_bias)) %>%
      ungroup()
  tabla.pob.slope$slope = 0
  idx = tabla.pob.slope$condition == "Ear level" & tabla.pob.slope$type == "NORMAL"
  tabla.pob.slope[idx,]$slope = coef(m.Dist.lin)$subject$target_distance
  idx = tabla.pob.slope$condition == "Floor level"& tabla.pob.slope$type == "NORMAL"
  tabla.pob.slope[idx,]$slope = coef(m.Dist.lin)$subject$target_distance + coef(m.Dist.lin)$subject$`target_distance:conditionFloor level`
  
  
  idx = tabla.pob.slope$condition == "Ear level" & tabla.pob.slope$type == "ROVED"
  tabla.pob.slope[idx,]$slope = coef(m.Dist.lin.r)$subject$target_distance
  idx = tabla.pob.slope$condition == "Floor level"& tabla.pob.slope$type == "ROVED"
  tabla.pob.slope[idx,]$slope = coef(m.Dist.lin.r)$subject$target_distance + coef(m.Dist.lin.r)$subject$`target_distance:conditionFloor level`
  
  
 f2 =  ggplot(tabla.pob.slope, aes(x = condition,y = slope, colour = condition, fill = condition)) +
   geom_line(aes(group = subject), alpha = 0.3)+ 
   geom_point(alpha = 1) +
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    geom_abline(slope = 0,
                intercept = 0,
                alpha = 0.5,
                linetype = "dashed") +
    stat_summary(fun.data = "mean_se",
                 geom = "pointrange",
                 alpha = 1,
                 size = 1,
                 # position = position_dodge(width = 1)
                 position = position_jitterdodge(jitter.width = 0.6,
                                                 jitter.height = 0,
                                                 dodge.width = 0 )) +
    # stat_summary(fun.data = "mean_se",
    #              geom = "linerange",
    #              size=2,
    #              position = position_dodge(width = 1)) +
    labs(x = "Condition", 
         y = "Slope with LMER") +
   facet_grid(. ~ type) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "none")
  
  f2
  mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Slope", ".png", sep = '')
  ggsave(mi_nombre_de_archivo, plot=f2, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)
  
  
  
  
  
  
  
  
  
  
  
  
## Bias signed-----
f2 <- filter(results_tbl,type == "NORMAL") %>% 
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

f2
aaa <- filter(results_tbl,type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias)) %>%
  ungroup()
m.RelativBias <- lm(mSesgoRel ~ condition, 
                    data = aaa)
anova(m.RelativBias)


# Response variability
## Intra-subject
tabla.ind.var <- filter(results_tbl) %>% 
  group_by(target_distance,condition,type) %>%
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
  facet_grid(.~ type )+
  scale_y_log10(name="Standard deviation (m)\n +/- SEM Intra-subject", breaks=c(0,0.2,0.3,0.4,0.5,0.6), labels=c(0,0.2,0.3,0.4,0.5,0.6), minor_breaks=NULL, limits = c(-1.1,1.85)) +
  scale_x_log10(name="Distance source (m)",  breaks=c(2,2.9,4.2,6), labels=c(2,2.9,4.2,6), minor_breaks=NULL, limits = c(1.9,6.1)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f3

tabla.ind.var <- filter(results_tbl,type == "ROVED") %>% 
  group_by(target_distance,condition) %>%
  summarise(mSD = mean(perc_dist_sd),
            SdSd = sd(perc_dist_sd),
            n = n())  %>%
  ungroup()

f31 <- ggplot(tabla.ind.var, aes(x=target_distance, y =mSD, group = condition, color = condition)) + 
  geom_point()+ 
  geom_line(size = 1)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_errorbar(data=tabla.ind.var,alpha = 2, width=0, size=1,
                mapping=aes(ymin = mSD - (SdSd/sqrt(n)), 
                            ymax = mSD + (SdSd/sqrt(n)),
                            color=condition))+ 
  geom_abline(intercept = 0, slope = 0, linetype=2) +
  scale_y_log10(name="Standard deviation (m)\n +/- SEM Intra-subject", breaks=c(0,0.2,0.3,0.4,0.5,0.6), labels=c(0,0.2,0.3,0.4,0.5,0.6), minor_breaks=NULL, limits = c(-1.1,1.85)) +
  scale_x_log10(name="Distance source (m)",  breaks=c(2,2.9,4.2,6), labels=c(2,2.9,4.2,6), minor_breaks=NULL, limits = c(1.9,6.1)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f31
## Intra-Sujeto colapsado
f4 <- filter(results_tbl,type == "NORMAL") %>% 
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

f4
# Unifico graficos en una sola figura
Figure2 = ggarrange(f1,f2,f3,f4,
                    labels = c("a", "b","c","d"),
                    ncol = 2, nrow = 2,
                    common.legend = TRUE, legend="top", align = "hv")
Figure2
mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "Figura1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure2, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)


tabla.ind.summ <- results_tbl %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias),
            mSesgoAbs = mean(abs_bias),
            mSD = mean(perc_dist_sd))  %>%
  ungroup()

tabla.ind.summ$SignedBias = tabla.ind.summ$mslope

# Signed Bias

m.RelativBias <- lm(SignedBias ~ condition, 
                    data = tabla.ind.summ)
ggcoefstats(m.RelativBias, output = "tidy") %>% select(-label)
anova(m.RelativBias)

