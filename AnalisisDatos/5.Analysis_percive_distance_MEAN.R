library(tidyverse)
library(lme4)
library(nlme)
library(sjPlot)
library(MuMIn)
# library(lmerTest)
# library(jtools)
# library(broom)
library(ggstatsplot)
# library(gmodels)
library(ggpubr)


rm(list=ls())
figures_folder = "figuras"
results_tbl <- read.csv("./DatosUnificados/Dresults_without_outliers_slope_and_intercepto_lin_log.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

#LINEAL ----
#NORMAL
m.Dist1 <-  lme(perc_dist ~ target_distance*condition, random = ~target_distance|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = filter(results_tbl,type == "NORMAL"))
extract_stats(ggcoefstats(m.Dist1))
anova(m.Dist1)


eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                  list(a = round(m.Dist1$coefficients$fixed[[2]],digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[1]], digits = 2)))
eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                  list(a = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], digits = 2)))
eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))

tabla.pob = filter(results_tbl,type == "NORMAL") %>% group_by(target_distance,condition) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()

f1 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = condition, color  = condition)) + 
  geom_pointrange(aes(x = target_distance, y = Mperc_dist, ymin = Mperc_dist-SDperc_dist, ymax = Mperc_dist+SDperc_dist),alpha = 1, 
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope =m.Dist1$coefficients$fixed[[2]], 
              intercept =m.Dist1$coefficients$fixed[[1]], 
              alpha = 0.5,
              color = "#000000") +
  geom_abline(slope =m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], 
              intercept =m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], 
              alpha = 0.5,
              color = "#E69F00") +
  geom_text(x = 0.2, y = 7.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
  geom_text(x = 0.2, y = 6.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
  geom_text(x = 0.2, y = 6.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f1
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "5. Lme Lineal-Normal", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

# ROVED
m.Dist1 <-  lme(perc_dist ~ target_distance*condition, random = ~target_distance|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = filter(results_tbl,type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
anova(m.Dist1)


eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                  list(a = round(m.Dist1$coefficients$fixed[[2]],digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[1]], digits = 2)))
eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                  list(a = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], digits = 2)))
eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))

tabla.pob = filter(results_tbl,type == "ROVED") %>% group_by(target_distance,condition) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()

f2 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = condition, color  = condition)) + 
  geom_pointrange(aes(x = target_distance, y = Mperc_dist, ymin = Mperc_dist-SDperc_dist, ymax = Mperc_dist+SDperc_dist),alpha = 1, 
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope =m.Dist1$coefficients$fixed[[2]], 
              intercept =m.Dist1$coefficients$fixed[[1]], 
              alpha = 0.5,
              color = "#000000") +
  geom_abline(slope =m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], 
              intercept =m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], 
              alpha = 0.5,
              color = "#E69F00") +
  geom_text(x = 0.2, y = 7.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
  geom_text(x = 0.2, y = 6.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
  geom_text(x = 0.2, y = 6.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f2
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "6. Lme Lineal-ROVED", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f2, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)


#LOG ----
#NORMAL
m.Dist1 <-  lme(log10(perc_dist) ~ log10(target_distance)*condition, random = ~log10(target_distance)|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = filter(results_tbl,type == "NORMAL"))
extract_stats(ggcoefstats(m.Dist1))
anova(m.Dist1)

eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                  list(a = round(10^(m.Dist1$coefficients$fixed[[1]]),digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[2]], digits = 2)))
eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                  list(a = round(10^(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]]), digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], digits = 2)))
# 
# eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
#                   list(a = round(m.Dist1$coefficients$fixed[[2]],digits = 2),
#                        b = round(m.Dist1$coefficients$fixed[[1]], digits = 2)))
# eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
#                   list(a = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], digits = 2),
#                        b = round(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], digits = 2)))
eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))

tabla.pob = filter(results_tbl,type == "NORMAL") %>% group_by(target_distance,condition) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()

x1 = seq(0,8,by=.01)
curves1 = data.frame(x2 = x1,
                     y1 = (10^(m.Dist1$coefficients$fixed[[1]]))*(x1^m.Dist1$coefficients$fixed[[2]]),
                     condition = "Ear level")
curves2 = data.frame(x2 = x1,
                     y1 = 10^(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]])*(x1^(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]])),
                     condition = "Floor level")
curve = merge(x = curves1, y = curves2, all = TRUE)

f1 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = condition, color  = condition)) + 
  geom_pointrange(aes(x = target_distance, y = Mperc_dist, ymin = Mperc_dist-SDperc_dist, ymax = Mperc_dist+SDperc_dist),alpha = 1, 
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  geom_line(data = curve, mapping = aes(x = x2, y = y1, colour = condition))+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  # geom_abline(slope =m.Dist1$coefficients$fixed[[2]], 
  #             intercept =m.Dist1$coefficients$fixed[[1]], 
  #             alpha = 0.5,
  #             color = "#000000") +
  # geom_abline(slope =m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], 
  #             intercept =m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], 
  #             alpha = 0.5,
  #             color = "#E69F00") +
  geom_text(x = 0.2, y = 7.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
  geom_text(x = 0.2, y = 6.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
  geom_text(x = 0.2, y = 6.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f1
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "7. Lme Log-Normal", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

# ROVED
m.Dist1 <-  lme(log10(perc_dist) ~ log10(target_distance)*condition, random = ~log10(target_distance)|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = filter(results_tbl,type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
anova(m.Dist1)

eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                  list(a = round(10^(m.Dist1$coefficients$fixed[[1]]),digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[2]], digits = 2)))
eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                  list(a = round(10^(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]]), digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], digits = 2)))
# 
# eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
#                   list(a = round(m.Dist1$coefficients$fixed[[2]],digits = 2),
#                        b = round(m.Dist1$coefficients$fixed[[1]], digits = 2)))
# eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
#                   list(a = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], digits = 2),
#                        b = round(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], digits = 2)))
eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))

tabla.pob = filter(results_tbl,type == "ROVED") %>% group_by(target_distance,condition) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()

x1 = seq(0,8,by=.01)
curves1 = data.frame(x2 = x1,
                     y1 = (10^(m.Dist1$coefficients$fixed[[1]]))*(x1^m.Dist1$coefficients$fixed[[2]]),
                     condition = "Ear level")
curves2 = data.frame(x2 = x1,
                     y1 = 10^(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]])*(x1^(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]])),
                     condition = "Floor level")
curve = merge(x = curves1, y = curves2, all = TRUE)

f4 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = condition, color  = condition)) + 
  geom_pointrange(aes(x = target_distance, y = Mperc_dist, ymin = Mperc_dist-SDperc_dist, ymax = Mperc_dist+SDperc_dist),alpha = 1, 
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  geom_line(data = curve, mapping = aes(x = x2, y = y1, colour = condition))+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  # geom_abline(slope =m.Dist1$coefficients$fixed[[2]], 
  #             intercept =m.Dist1$coefficients$fixed[[1]], 
  #             alpha = 0.5,
  #             color = "#000000") +
  # geom_abline(slope =m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], 
  #             intercept =m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], 
  #             alpha = 0.5,
  #             color = "#E69F00") +
  geom_text(x = 0.2, y = 7.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
  geom_text(x = 0.2, y = 6.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
  geom_text(x = 0.2, y = 6.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f4
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "8. Lme Log-ROVED", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f4, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)
