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


m.Dist1 <-  lmer(perc_dist ~ target_distance*condition*type+(1+target_distance|subject)+(0+condition|subject),
                 data = filter(results_tbl,location == "standing"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
anova(m.Dist1)

# 
tabla.pob = filter(results_tbl,location == "standing") %>% group_by(target_distance,condition,type) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()


Final.Fixed<-effect(c("target_distance*condition*type"), m.Dist1,
                    xlevels=list(target_distance=c(2,2.9,4.2,6)))

# You have to convert the output to a dataframe
Final.Fixed<-as.data.frame(Final.Fixed)

Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =fit, group=interaction(condition,type)))+
  geom_line(aes(color=condition,linetype = type), size=2)+
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



