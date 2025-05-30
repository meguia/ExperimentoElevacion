library(tidyverse)
library(lme4)
library(nlme)
library(sjPlot)
library(MuMIn)
library(ggstatsplot)
library(ggpubr)
library(ggpp)
library(PupillometryR)
library(effects)
library(lmerTest)
library(jtools)
library(gdtools)
library(broom)
library(modelsummary)
library(flextable)
library(webshot)
library(officer)
library(lattice)

rm(list=ls())
figures_folder = "figuras"
results_tbl <- read.csv("./DatosUnificados/Dresults_without_outliers_slope_and_intercepto.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

idx = results_tbl$subject == "T005"
results_tbl = results_tbl[!idx,]
# idx = results_tbl$subject == "S013"
# results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S001" & results_tbl$condition == "Floor level"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S003" & results_tbl$type == "ROVED"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S003"& results_tbl$condition == "Ear level" & results_tbl$type == "NORMAL"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S012"& results_tbl$condition == "Ear level"& results_tbl$type == "ROVED"
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "S019"& results_tbl$condition == "Ear level"& results_tbl$type == "ROVED"
results_tbl = results_tbl[!idx,]
# EXPERIMENTO 1 ----
## PAD Lineal ----

tu_data = filter(results_tbl, location == "standing")

tu_data$target_distance <- as.numeric(tu_data$target_distance)

modelo <- lmer(perc_dist ~ target_distance * condition * block + 
                 (target_distance | subject), data = tu_data)

library(emmeans)
predicciones <- emmeans(modelo, ~ target_distance * condition * type, 
                        at = list(target_distance = unique(tu_data$target_distance)))

predicciones <- emmeans(modelo, ~ condition * type, 
                        at = list(target_distance = seq(min(tu_data$target_distance), 
                                                        max(tu_data$target_distance), length.out = 100)))
predicciones_df <- as.data.frame(predicciones)

ggplot(predicciones_df, aes(x = target_distance, y = emmean, color = condition)) +
  geom_line() +
  geom_point() +
  geom_point(data = tabla.pob, aes(x = target_distance, y =Mperc_dist,
                                   group = condition,
                                   color = condition),size = 5)+
  facet_grid(. ~ type) +
  labs(x = "Distance source", y = "Perceived distance") +
  theme_minimal() +
  theme(legend.position = "bottom")



#-------------


m.Dist1 <-  lmer(perc_dist ~ target_distance*condition*type+(1+target_distance|subject)+(0+condition|subject)+(0+type|subject),
                 data = filter(results_tbl,location == "standing"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)


tabla.pob = filter(results_tbl, location == "standing") %>% group_by(target_distance,condition,type) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()

Final.Fixed <- effect(
  term = "target_distance*condition*type",
  mod = m.Dist1,
  xlevels = list(target_distance = c(2, 2.9, 4.2, 6))
)

a = filter(results_tbl, location == "standing" & subject != "S001")

m.Dist1 <-  lmer(perc_dist ~ target_distance*condition*type+(1+target_distance|subject)+(0+condition|subject)+(0+type|subject),
                 data = a)
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)
a$Model.2.fitted<-predict(m.Dist1)
# Final.Fixed<-as.data.frame(Final.Fixed)
# Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =fit, fill = condition, group=condition))+
#   geom_line(aes(color=condition), size=2)+
# FittedlmPlot2 <-ggplot()+
#   facet_grid(subject ~ type, labeller=label_both)+
#   geom_line(data = HappyData, aes(x = TimeStep, y=Model.2.fitted))+
#   geom_point(data = HappyData, aes(x = TimeStep, y =HappyPercent, group=subject,colour = subject), size=3)+
#   #  coord_cartesian(ylim = c(.03,.074))+ 
#   xlab("Time Step")+ylab("Happiness")
# FittedlmPlot2


# Final.Fixed<-effect(c("target_distance:condition:type"), m.Dist1,
#                     xlevels=list(target_distance=c(2,2.9,4.2,6)))

# You have to convert the output to a dataframe

Final.Fixed<-as.data.frame(Final.Fixed)
Final.Fixed.Plot <-ggplot(data = a, aes(x = target_distance, y =Model.2.fitted, fill = condition, group=condition))+
  geom_line(aes(color=condition), size=1)+
  # geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=condition,group = condition),alpha=.2)+
  geom_point(data = a, aes(x = target_distance, y =perc_dist,
                                   group = condition,
                                   color = condition),size = 1)+
  scale_x_continuous(name="Distance source (cm)", limits = c(0,12)) +
  scale_y_continuous(name="Perceived distance (cm)",   limits = c(0,12)) +
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  facet_grid(subject~type)+
  theme_bw()+
  theme(text=element_text(face="bold", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = "top")
        # legend.position = c(.2, .82))
Final.Fixed.Plot
f1 = Final.Fixed.Plot
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ALL SUBJETC standing NORMAL", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Final.Fixed.Plot, width=15, height=100, units="cm", limitsize=FALSE, dpi=600)

# por sujetos

Final.Fixed<-as.data.frame(Final.Fixed)
Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =fit, fill = condition, group=condition))+
  geom_line(aes(color=condition), size=2)+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=condition,group = condition),alpha=.2)+
  geom_point(data = tabla.pob, aes(x = target_distance, y =Mperc_dist,
                                   group = condition,
                                   color = condition),size = 5)+
  scale_x_continuous(name="Distance source (cm)", limits = c(0.9,7)) +
  scale_y_continuous(name="Perceived distance (cm)",   limits = c(0.9,7)) +
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  facet_grid(subject.~type)+
  theme_bw()+
  theme(text=element_text(face="bold", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = c(.2, .82))
Final.Fixed.Plot
f1 = Final.Fixed.Plot
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ALL standing NORMAL", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Final.Fixed.Plot, width=10, height=10, units="cm", limitsize=FALSE, dpi=600)




m.Dist1 <-  lmer(perc_dist ~ target_distance*condition*type+(1+target_distance|subject)+(0+condition|subject),
                 data = results_tbl)
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)

anova(m.Dist1)
anov1 = anova(m.Dist1)


eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                  list(a = round(m.Dist1$coefficients$fixed[[2]],digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[1]], digits = 2)))
eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                  list(a = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[5]], digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], digits = 2)))
eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))

tabla.pob = filter(results_tbl,type == "NORMAL") %>% group_by(target_distance,condition) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()

f1 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = condition, color  = condition)) + 
  geom_pointrange(aes(x = target_distance, y = Mperc_dist, ymin = Mperc_dist-SDperc_dist, ymax = Mperc_dist+SDperc_dist),size = .9,alpha = 1, 
                  position = position_jitterdodge(jitter.width = 0,
                                                  jitter.height = 0,
                                                  dodge.width = 0 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope =m.Dist1$coefficients$fixed[[2]], 
              intercept =m.Dist1$coefficients$fixed[[1]], 
              alpha = 0.5,
              size = 1.2,
              color = "#000000") +
  geom_abline(slope =m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[5]], 
              intercept =m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], 
              alpha = 0.5,
              size = 1.2,
              color = "#E69F00") +
  geom_text(x = 1.1, y = 6.6, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 3.5, color = "#000000")+
  geom_text(x = 1.1, y = 6.1, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 3.5, color = "#E69F00")+
  #geom_text(x = 0.2, y = 6, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Source distance (m)", limits = c(0,7)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,7)) +
  ggtitle("Experiment 1")+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f1

# Slope ----
# m.Dist1 <-  lme(perc_dist ~ target_distance*condition, random = ~target_distance|subject,
#                 method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
#                 data = filter(results_tbl,type == "NORMAL"))


m.Dist1 <-  lmer(perc_dist ~ target_distance*condition+(target_distance:condition|subject),
                 data = filter(results_tbl,type == "NORMAL"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)

# coefmodelpob = fixef(m.Dist1)
# coefmodelind = ranef(m.Dist1)
# a = filter(results_tbl,type == "NORMAL", condition == "Ear level")
# b = filter(results_tbl,type == "NORMAL", condition == "Floor level")
# 
# a$slopeLMEL = coefmodelind$subject[[2]]+coefmodelpob[[2]]
# b$slopeLMEL = coefmodelind$subject[[2]]+coefmodelpob[[2]]+coefmodelpob[[4]]
# 
# results_tblslop = merge(a, b, all=TRUE)

results_tblp <- filter(results_tbl,type == "NORMAL") %>% 
  group_by(condition) %>%
  summarise(mslope  = mean(slope,na.rm=TRUE),
            SDslope  = sd(slope,na.rm=TRUE)/sqrt(length(slope)),
            mintercepto  = mean(intercepto,na.rm=TRUE),
            SDintercepto  = sd(intercepto,na.rm=TRUE),)  %>%
  ungroup()

f2 =  ggplot(results_tblp, aes(x = condition,y = mslope,colour = condition)) +
  geom_pointrange(aes(x=condition, y=mslope, ymin=mslope-SDslope, ymax=mslope+SDslope),
                  position = position_dodgenudge(direction = "split", width = 3.2), size = 1.2, shape = 0)+
  geom_line(position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_line(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x = condition,y = slope, group = subject, colour = condition, fill = condition),alpha = 0.6)+
  geom_point(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x = condition,y = slope, colour = condition, fill = condition), size = 2.4,alpha = 1)+
  # geom_violin(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x = condition,y = slope, colour = condition, fill = condition),
  #                  position = position_nudge_center(x = .3,
  #                    y = 0,
  #                    center_x = 2,
  #                    direction = "split",
  #                    kept.origin = c("original", "none")),trim = FALSE, alpha = .2)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Slope with LM") +
  # facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 2,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 1.9, yend = 1.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")
f2
#Bias signed

results_tbls <- filter(results_tbl,type == "NORMAL") %>% 
  group_by(condition,subject) %>%
  summarise(mBiasSigned  = mean(rel_bias_signed,na.rm=TRUE),
            SdBiasSigned  = sd(rel_bias_signed,na.rm=TRUE)/sqrt(length(rel_bias_signed)),
            mBiasUnSigned  = mean(rel_bias_unsigned,na.rm=TRUE),
            SdBiasUnSigned  = sd(rel_bias_unsigned,na.rm=TRUE)/sqrt(length(rel_bias_unsigned)))  %>%
  ungroup()

results_tblp <- results_tbls %>% 
  group_by(condition) %>%
  summarise(MBiasSigned  = mean(mBiasSigned,na.rm=TRUE),
            SDBiasSigned  = sd(mBiasSigned,na.rm=TRUE)/sqrt(length(mBiasSigned)),
            MBiasUnSigned  = mean(mBiasUnSigned,na.rm=TRUE),
            SDBiasUnSigned  = sd(mBiasUnSigned,na.rm=TRUE)/sqrt(length(mBiasUnSigned)))  %>%
  ungroup()

f3 =  ggplot(results_tblp, aes(x = condition,y = MBiasSigned, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=MBiasSigned, ymin=MBiasSigned-SDBiasSigned, ymax=MBiasSigned+SDBiasSigned),
                  position = position_dodgenudge(direction = "split", width = 3.2), size = 1.2, shape = 0)+
  geom_line(position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_line(data = results_tbls, mapping = aes(x = condition,y = mBiasSigned, group = subject, colour = condition, fill = condition),alpha = 0.6)+
  geom_point(data = results_tbls, mapping = aes(x = condition,y = mBiasSigned, colour = condition, fill = condition), size = 2.4, alpha = 1)+
  # geom_violin(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x = condition,y = slope, colour = condition, fill = condition),
  #             position = position_nudge_center(x = .3,
  #                                              y = 0,
  #                                              center_x = 2,
  #                                              center_y = -10,
  #                                              direction = "split",
  #                                              kept.origin = c("original", "none")),trim = FALSE, alpha = .2)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Signed bias") +
  # facet_grid(. ~ type) +
  # annotate("text", x = 1.5, y = 2,  label = "***", size = 4) +
  # annotate("segment", x = 1, xend = 2, y = 1.9, yend = 1.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")
f3

# Unsigned
f4 =  ggplot(results_tblp, aes(x = condition,y = MBiasUnSigned, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=MBiasUnSigned, ymin=MBiasUnSigned-SDBiasSigned, ymax=MBiasUnSigned+SDBiasSigned),
                  position = position_dodgenudge(direction = "split", width = 3.2), size = 1.2, shape = 0)+
  geom_line(position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_line(data = results_tbls, mapping = aes(x = condition,y = mBiasUnSigned, group = subject, colour = condition, fill = condition),alpha = 0.6)+
  geom_point(data = results_tbls, mapping = aes(x = condition,y = mBiasUnSigned, colour = condition, fill = condition), size = 2.4, alpha = 1)+
  # geom_violin(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x = condition,y = slope, colour = condition, fill = condition),
  #             position = position_nudge_center(x = .3,
  #                                              y = 0,
  #                                              center_x = 2,
  #                                              direction = "split",
  #                                              kept.origin = c("original", "none")),trim = FALSE, alpha = .2)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  # geom_abline(slope = 0,
  #             intercept = 0,
  #             alpha = 0.5,
  #             linetype = "dashed") +
  labs(x = "Condition",
       y = "Unsigned bias") +
  ylim(c(0,1.11))+
  # facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 1.1,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 1.05, yend = 1.05, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")
f4

Figure1 = ggarrange(f1,
                    f2+rremove("x.text")+rremove("x.title"),
                    f3+rremove("x.text")+rremove("x.title"),
                    f4+rremove("x.text")+rremove("x.title"),
                    # ncol = 3, nrow = 1,labels = c("B", "C", "D"),
                    # common.legend = FALSE, legend="none", align = "h")+theme(plot.margin = unit(c(-10,0,-50,0), 'cm')),
                    heights = c(1, 1),
                    ncol = 2, nrow = 2,labels = c("A", "B","C","D"),
                    common.legend = TRUE, legend="top", align = "hv")
Figure1

Figure1 = ggarrange(f1,
                    f2,
                    f3,
                    f4,
                    # ncol = 3, nrow = 1,labels = c("B", "C", "D"),
                    # common.legend = FALSE, legend="none", align = "h")+theme(plot.margin = unit(c(-10,0,-50,0), 'cm')),
                    heights = c(1, 1),
                    ncol = 2, nrow = 2,labels = c("A", "B","C","D"),
                    common.legend = TRUE, legend="top", align = "hv")
Figure1
# 
# 
# 
# Figure1 = ggarrange(f1+theme(plot.margin = unit(c(0,1,-5,1), 'cm')),
#                     ggarrange(f2+rremove("x.text")+rremove("x.title"),
#                               f3+rremove("x.text")+rremove("x.title"),
#                               f4+rremove("x.text")+rremove("x.title"),
#                               ncol = 3, nrow = 1,labels = c("B", "C", "D"),
#                               common.legend = FALSE, legend="none", align = "h")+theme(plot.margin = unit(c(-10,0,-50,0), 'cm')),
#                     heights = c(.8, 1),
#                     ncol = 1, nrow = 2,labels = c("A", ""),
#                     common.legend = TRUE, legend="top", align = "h")
# Figure1
mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "FIG1. Experimento1 SENTADO SIN 5", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure1, width=18, height=15, units="cm", limitsize=FALSE, dpi=600)






# ROVED

m.Dist1 <-  lmer(perc_dist ~ target_distance*condition+(1+target_distance|subject)+(0+condition|subject),
                 data = filter(results_tbl,type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)


tabla.pob = filter(results_tbl,type == "ROVED") %>% group_by(target_distance,condition) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()


Final.Fixed<-effect(c("target_distance*condition"), m.Dist1,
                    xlevels=list(target_distance=c(2,2.9,4.2,6)))

# You have to convert the output to a dataframe
Final.Fixed<-as.data.frame(Final.Fixed)

Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =fit, group=condition))+
  geom_line(aes(color=condition), size=2)+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=condition),alpha=.2)+
  geom_point(data = tabla.pob, aes(x = target_distance, y =Mperc_dist,
                                   group = condition,
                                   color = condition),size = 5)+
  scale_x_continuous(name="Distance source (cm)", limits = c(0.9,7)) +
  scale_y_continuous(name="Perceived distance (cm)",   limits = c(0.9,7)) +
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  theme_bw()+
  theme(text=element_text(face="bold", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = c(.2, .82))
Final.Fixed.Plot
f5 = Final.Fixed.Plot
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ALL standing ROVED", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Final.Fixed.Plot, width=10, height=10, units="cm", limitsize=FALSE, dpi=600)



# m.Dist1 <-  lme(perc_dist ~ target_distance*condition, random = ~target_distance|subject,
#                 method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
#                 data = filter(results_tbl,type == "ROVED"))
# 
# anova(m.Dist1)
# eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
#                   list(a = round(m.Dist1$coefficients$fixed[[2]],digits = 2),
#                        b = round(m.Dist1$coefficients$fixed[[1]], digits = 2)))
# eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
#                   list(a = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], digits = 2),
#                        b = round(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], digits = 2)))
# eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
#                   list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))
# 
# 
# tabla.pob = filter(results_tbl,type == "ROVED") %>% group_by(target_distance,condition) %>%
#   summarise(Mperc_dist  = mean(perc_dist),
#             SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
#   ungroup()
# 
# f5 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = condition, color  = condition)) + 
#   geom_pointrange(aes(x = target_distance, y = Mperc_dist, ymin = Mperc_dist-SDperc_dist, ymax = Mperc_dist+SDperc_dist),size = .9,alpha = 1, 
#                   position = position_jitterdodge(jitter.width = 0,
#                                                   jitter.height = 0,
#                                                   dodge.width = 0 ))+
#   geom_abline(intercept = 0, slope = 1, linetype=2) +
#   scale_colour_manual(values = cbPalette) + 
#   scale_fill_manual(values = cbPalette) + 
#   geom_abline(slope =m.Dist1$coefficients$fixed[[2]], 
#               intercept =m.Dist1$coefficients$fixed[[1]], 
#               alpha = 0.5,
#               size = 1.2,
#               color = "#000000") +
#   geom_abline(slope =m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], 
#               intercept =m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], 
#               alpha = 0.5,
#               size = 1.2,
#               color = "#E69F00") +
#   geom_text(x = 1.1, y = 6.6, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 3.5, color = "#000000")+
#   geom_text(x = 1.1, y = 6.1, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 3.5, color = "#E69F00")+
#   #geom_text(x = 0.2, y = 6, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
#   scale_x_continuous(name="Source distance (m)", limits = c(1,7)) +
#   scale_y_continuous(name="Perceived distance (m)",   limits = c(1,7)) +
#   ggtitle("Experiment 2")+
#   theme_pubr(base_size = 12, margin = TRUE)+
#   theme(legend.position = "top",
#         legend.title = element_blank())
# 
# f5

#Slope ROVED


results_tblp <- filter(results_tbl,type == "ROVED") %>% 
  group_by(condition) %>%
  summarise(mslope  = mean(slope,na.rm=TRUE),
            SDslope  = sd(slope,na.rm=TRUE)/sqrt(length(slope)),
            mintercepto  = mean(intercepto,na.rm=TRUE),
            SDintercepto  = sd(intercepto,na.rm=TRUE),)  %>%
  ungroup()

f6 =  ggplot(results_tblp, aes(x = condition,y = mslope,colour = condition)) +
  geom_pointrange(aes(x=condition, y=mslope, ymin=mslope-SDslope, ymax=mslope+SDslope),
                  position = position_dodgenudge(direction = "split", width = 3.2), size = 1.2, shape = 0)+
  geom_line(position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_line(data = filter(results_tbl,type == "ROVED"), mapping = aes(x = condition,y = slope, group = subject, colour = condition, fill = condition),alpha = 0.6)+
  geom_point(data = filter(results_tbl,type == "ROVED"), mapping = aes(x = condition,y = slope, colour = condition, fill = condition), size = 2.4,alpha = 1)+
  
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Slope with LM") +
  # facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 1.2,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 1.1, yend = 1.1, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")
f6
#Bias signed

results_tbls <- filter(results_tbl,type == "ROVED") %>% 
  group_by(condition,subject) %>%
  summarise(mBiasSigned  = mean(rel_bias_signed,na.rm=TRUE),
            SdBiasSigned  = sd(rel_bias_signed,na.rm=TRUE)/sqrt(length(rel_bias_signed)),
            mBiasUnSigned  = mean(rel_bias_unsigned,na.rm=TRUE),
            SdBiasUnSigned  = sd(rel_bias_unsigned,na.rm=TRUE)/sqrt(length(rel_bias_unsigned)))  %>%
  ungroup()

results_tblp <- results_tbls %>% 
  group_by(condition) %>%
  summarise(MBiasSigned  = mean(mBiasSigned,na.rm=TRUE),
            SDBiasSigned  = sd(mBiasSigned,na.rm=TRUE)/sqrt(length(mBiasSigned)),
            MBiasUnSigned  = mean(mBiasUnSigned,na.rm=TRUE),
            SDBiasUnSigned  = sd(mBiasUnSigned,na.rm=TRUE)/sqrt(length(mBiasUnSigned)))  %>%
  ungroup()

f7 =  ggplot(results_tblp, aes(x = condition,y = MBiasSigned, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=MBiasSigned, ymin=MBiasSigned-SDBiasSigned, ymax=MBiasSigned+SDBiasSigned),
                  position = position_dodgenudge(direction = "split", width = 3.2), size = 1.2, shape = 0)+
  geom_line(position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_line(data = results_tbls, mapping = aes(x = condition,y = mBiasSigned, group = subject, colour = condition, fill = condition),alpha = 0.6)+
  geom_point(data = results_tbls, mapping = aes(x = condition,y = mBiasSigned, colour = condition, fill = condition), size = 2.4, alpha = 1)+
  
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Signed bias") +
  # facet_grid(. ~ type) +
  # annotate("text", x = 1.5, y = 2,  label = "***", size = 4) +
  # annotate("segment", x = 1, xend = 2, y = 1.9, yend = 1.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")
f7

# Unsigned
f8 =  ggplot(results_tblp, aes(x = condition,y = MBiasUnSigned, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=MBiasUnSigned, ymin=MBiasUnSigned-SDBiasSigned, ymax=MBiasUnSigned+SDBiasSigned),
                  position = position_dodgenudge(direction = "split", width = 3.2), size = 1.2, shape = 0)+
  geom_line(position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_line(data = results_tbls, mapping = aes(x = condition,y = mBiasUnSigned, group = subject, colour = condition, fill = condition),alpha = 0.6)+
  geom_point(data = results_tbls, mapping = aes(x = condition,y = mBiasUnSigned, colour = condition, fill = condition), size = 2.4, alpha = 1)+
  
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  # geom_abline(slope = 0,
  #             intercept = 0,
  #             alpha = 0.5,
  #             linetype = "dashed") +
  labs(x = "Condicion",
       y = "Unsigned bias") +
  ylim(c(0,1.11))+
  # facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 1,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = .95, yend = .95, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")
f8

Figure2 = ggarrange(f5,
                    f6+rremove("x.text")+rremove("x.title"),
                    f7+rremove("x.text")+rremove("x.title"),
                    f8+rremove("x.text")+rremove("x.title"),
                    # ncol = 3, nrow = 1,labels = c("B", "C", "D"),
                    # common.legend = FALSE, legend="none", align = "h")+theme(plot.margin = unit(c(-10,0,-50,0), 'cm')),
                    heights = c(1, 1),
                    ncol = 2, nrow = 2,labels = c("A", "B","C","D"),
                    common.legend = TRUE, legend="top", align = "hv")
Figure2

Figure2 = ggarrange(f5,
                    f6,
                    f7,
                    f8,
                    # ncol = 3, nrow = 1,labels = c("B", "C", "D"),
                    # common.legend = FALSE, legend="none", align = "h")+theme(plot.margin = unit(c(-10,0,-50,0), 'cm')),
                    heights = c(1, 1),
                    ncol = 2, nrow = 2,labels = c("A", "B","C","D"),
                    common.legend = TRUE, legend="top", align = "hv")
Figure2

mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "FIG2. Experimento2 SENTADO sin 5", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure2, width=18, height=15, units="cm", limitsize=FALSE, dpi=600)





# EXPERIMENTO 2 ----



m.Dist1 <-  lmer(perc_dist ~ target_distance*condition*location+(1+target_distance|subject)+(0+condition|subject),
                 data = filter(results_tbl,type == "NORMAL"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)


tabla.pob = filter(results_tbl,type == "NORMAL") %>% group_by(target_distance,condition,location) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()


Final.Fixed<-effect(c("target_distance*condition*location"), m.Dist1,
                    xlevels=list(target_distance=c(2,2.9,4.2,6)))

# You have to convert the output to a dataframe
Final.Fixed<-as.data.frame(Final.Fixed)

Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =fit, group=interaction(condition,location)))+
  geom_line(aes(color=condition,linetype = location), size=2)+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=condition),alpha=.2)+
  geom_point(data = tabla.pob, aes(x = target_distance, y =Mperc_dist,
                                   group = condition,
                                   color = condition),size = 5)+
  scale_x_continuous(name="Distance source (cm)", limits = c(0.9,7)) +
  scale_y_continuous(name="Perceived distance (cm)",   limits = c(0.9,7)) +
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  theme_bw()+
  ggtitle("NORMAL")+
  theme(text=element_text(face="bold", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = c(.2, .82))
Final.Fixed.Plot
f1 = Final.Fixed.Plot
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "NORMAL", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Final.Fixed.Plot, width=10, height=10, units="cm", limitsize=FALSE, dpi=600)




m.Dist1 <-  lmer(perc_dist ~ target_distance*location+(1+target_distance|subject)+(0+location|subject),
                 data = filter(results_tbl,condition == "Ear level",type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)
m.Dist1 <-  lmer(perc_dist ~ target_distance*location+(1+target_distance|subject)+(0+location|subject),
                 data = filter(results_tbl,condition == "Floor level",type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)
m.Dist1 <-  lmer(perc_dist ~ target_distance*condition+(1+target_distance|subject)+(0+condition|subject),
                 data = filter(results_tbl,location == "standing",type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)
m.Dist1 <-  lmer(perc_dist ~ target_distance*condition+(1+target_distance|subject)+(0+condition|subject),
                 data = filter(results_tbl,location == "sitting",type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)


m.Dist1 <-  lmer(perc_dist ~ target_distance*condition*location+(1+target_distance|subject)+(0+condition|subject),
                 data = filter(results_tbl,type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)

tabla.pob = filter(results_tbl,type == "ROVED") %>% group_by(target_distance,condition,location) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()


Final.Fixed<-effect(c("target_distance*condition*location"), m.Dist1,
                    xlevels=list(target_distance=c(2,2.9,4.2,6)))

# You have to convert the output to a dataframe
Final.Fixed<-as.data.frame(Final.Fixed)

Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =fit, group=interaction(condition,location)))+
  geom_line(aes(color=condition,linetype = location), size=2)+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=condition),alpha=.2)+
  geom_point(data = tabla.pob, aes(x = target_distance, y =Mperc_dist,
                                   group = condition,
                                   color = condition),size = 5)+
  scale_x_continuous(name="Distance source (cm)", limits = c(0.9,7)) +
  scale_y_continuous(name="Perceived distance (cm)",   limits = c(0.9,7)) +
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  theme_bw()+
  # ggtitle("ROVED")+
  # facet_grid(.~location)+
  # facet_grid(.~condition)+
  theme(text=element_text(face="bold", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = "top")
Final.Fixed.Plot
f5 = Final.Fixed.Plot
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ROVED todas", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Final.Fixed.Plot, width=10, height=10, units="cm", limitsize=FALSE, dpi=600)


