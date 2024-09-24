library(tidyverse)
# library(nlme)
library(effects)



library(lme4)
# library(nlme)
library(sjPlot)
library(MuMIn)
library(lmerTest)
library(jtools)
library(gdtools)
library(broom)
library(ggstatsplot)
library(modelsummary)
# library(gmodels)
library(ggpubr)
# install.packages("stargazer")
# library(stargazer)
# library(nlme)
# install.packages("sjPlot")
library(flextable)
library(sjPlot)
# install.packages("webshot")
library(webshot)
library(officer)



rm(list=ls())
figures_folder = "figuras"
results_tbl <- read.csv("./DatosUnificados/Dresults_without_outliers_slope_and_intercepto_lin_log.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

#LINEAL ----
#NORMAL

m.Dist1 <-  lme(perc_dist ~ target_distance*condition, random = ~target_distance|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = filter(results_tbl,type == "NORMAL", location == "sitting"))
extract_stats(ggcoefstats(m.Dist1))
anova(m.Dist1)
anov = anova(m.Dist1)


m.Dist1 <-  lmer(perc_dist ~ target_distance*condition+(1+target_distance|subject)+(0+condition|subject),
                 data = filter(results_tbl,type == "NORMAL"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)

anova(m.Dist1)
anov1 = anova(m.Dist1)

# m.Dist1 <-  lmer(perc_dist ~ target_distance*condition+(target_distance|subject),
#                 data = filter(results_tbl,type == "NORMAL"))
# extract_stats(ggcoefstats(m.Dist1))
# r.squaredGLMM(m.Dist1)
# 
# anova(m.Dist1)
# anov2 = anova(m.Dist1)

###Tabla estimadores ----

p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}
anov1$Predictors = c("Target distance","Condition","Target distance:Condition")
anov1 = data.frame(anov1)
anov1 <- flextable(anov1,col_keys = c("Predictors","NumDF","DenDF", "F.value","Pr..F.")) %>%
hline_top(border = fp_border(color="black", width = .5), part = "all")%>%
hline_bottom(border = fp_border(color="black", width = .5))%>%
  width(j = 1, width = 5, unit = "cm")%>%
  align(align = "center", part = "all")%>%
  align(j = 1, align = "left", part = "all")%>%
  colformat_double(digits = 1, na_str = "N/A")%>%
  set_formatter(values = list("Pr..F." = p_val_format) )%>% 
  font(fontname = "+font-family: Arial;")%>%
font(fontname = "+font-family: Arial;", part = "header")%>%

fontsize(size = 10.5, part = "header")%>% 
fontsize(size = 10.5)%>%
bold(j = "Pr..F.", bold = TRUE)%>%
italic(italic = TRUE, part = "header")%>%
line_spacing(space = 1, part = "body")%>%
line_spacing(space = .5, part = "header")
anov1
  

save_as_image(anov1,"Anov_PAD_POB_NORMAL_LIN.png")
###Tabla anova -----
#tab_model(m.Dist1,file="plot.html")
tab_model(m.Dist1, wrap.labels = 80,
              auto.label = FALSE, show.stat = TRUE, string.p = "p.value", string.ci = "CI 95%", dv.labels = "Perceived distance",
              show.intercept = TRUE, show.aic = FALSE, show.zeroinf = TRUE, show.re.var = FALSE, show.reflvl = TRUE,
              CSS = list(css.table = '+font-family: Arial;'),
              pred.labels = c("Intercept","Target distance", "Condition (Floor level)","Target distance * Condition (Floor level)"),
              file = "plot.html")


webshot("plot.html","Summary_PAD_POB_NORMAL_LIN.png", vwidth = 600, vheight = 100)


###Grafico ----

# Final.Fixed<-effect(c("target_distance*condition"), m.Dist1)
# Final.Fixed<-as.data.frame(Final.Fixed)
# 
# HappyData = filter(results_tbl,type == "NORMAL")
# HappyData$Model.5.fitted<-predict(m.Dist1)
# FittedlmPlot5 <-ggplot()+
#   facet_grid(subject ~ Social, labeller=label_both)+
#   geom_line(data = HappyData, aes(x = target_distance, y =Model.5.fitted))+
#   geom_point(data = HappyData, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
#   #  coord_cartesian(ylim = c(.03,.074))+ 
#   xlab("Time Step")+ylab("Happiness")
# FittedlmPlot5


m.Dist1 <-  lme(perc_dist ~ target_distance*condition, random = ~target_distance|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = filter(results_tbl,type == "NORMAL"))
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
m.Dist1 <-  lmer(perc_dist ~ target_distance*condition+(target_distance|subject),
                 data = filter(results_tbl,type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)

anova(m.Dist1)
anov1 = anova(m.Dist1)

p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}
anov1$Predictors = c("Target distance","Condition","Target distance:Condition")
anov1 = data.frame(anov1)
anov1 <- flextable(anov1,col_keys = c("Predictors","NumDF","DenDF", "F.value","Pr..F.")) %>%
  hline_top(border = fp_border(color="black", width = .5), part = "all")%>%
  hline_bottom(border = fp_border(color="black", width = .5))%>%
  width(j = 1, width = 5, unit = "cm")%>%
  align(align = "center", part = "all")%>%
  align(j = 1, align = "left", part = "all")%>%
  colformat_double(digits = 1, na_str = "N/A")%>%
  set_formatter(values = list("Pr..F." = p_val_format) )%>% 
  font(fontname = "+font-family: Arial;")%>%
  font(fontname = "+font-family: Arial;", part = "header")%>%
  
  fontsize(size = 10.5, part = "header")%>% 
  fontsize(size = 10.5)%>%
  bold(j = "Pr..F.", bold = TRUE)%>%
  italic(italic = TRUE, part = "header")%>%
  line_spacing(space = 1, part = "body")%>%
  line_spacing(space = .5, part = "header")
anov1


save_as_image(anov1,"Anov_PAD_POB_ROVED_LIN.png")

tab_model(m.Dist1,file="plot.html")
tab_model(m.Dist1, wrap.labels = 80,
          auto.label = FALSE, show.stat = TRUE, string.p = "p.value", string.ci = "CI 95%", dv.labels = "Perceived distance",
          show.intercept = TRUE, show.aic = FALSE, show.zeroinf = TRUE, show.re.var = FALSE, show.reflvl = TRUE,
          CSS = list(css.table = '+font-family: Arial;'),
          pred.labels = c("Intercept","Target distance", "Condition (Floor level)","Target distance * Condition (Floor level)"),
          file = "plot.html")


webshot("plot.html","Summary_PAD_POB_ROVED_LIN.png", vwidth = 600, vheight = 100)

m.Dist1 <-  lme(perc_dist ~ target_distance*condition, random = ~target_distance|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = filter(results_tbl,type == "ROVED"))

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

m.Dist1 <-  lmer(log10(perc_dist) ~ log10(target_distance)*condition+(log10(target_distance)|subject),
                 data = filter(results_tbl,type == "NORMAL"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)

anova(m.Dist1)
anov1 = anova(m.Dist1)


p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}
anov1$Predictors = c("Target distance","Condition","Target distance:Condition")
anov1 = data.frame(anov1)
anov1 <- flextable(anov1,col_keys = c("Predictors","NumDF","DenDF", "F.value","Pr..F.")) %>%
  hline_top(border = fp_border(color="black", width = .5), part = "all")%>%
  hline_bottom(border = fp_border(color="black", width = .5))%>%
  width(j = 1, width = 5, unit = "cm")%>%
  align(align = "center", part = "all")%>%
  align(j = 1, align = "left", part = "all")%>%
  colformat_double(digits = 1, na_str = "N/A")%>%
  set_formatter(values = list("Pr..F." = p_val_format) )%>% 
  font(fontname = "+font-family: Arial;")%>%
  font(fontname = "+font-family: Arial;", part = "header")%>%
  
  fontsize(size = 10.5, part = "header")%>% 
  fontsize(size = 10.5)%>%
  bold(j = "Pr..F.", bold = TRUE)%>%
  italic(italic = TRUE, part = "header")%>%
  line_spacing(space = 1, part = "body")%>%
  line_spacing(space = .5, part = "header")
anov1


save_as_image(anov1,"Anov_PAD_POB_NORMAL_LOG.png")

# tab_model(m.Dist1,file="plot.html")
tab_model(m.Dist1, wrap.labels = 80,
          auto.label = FALSE, show.stat = TRUE, string.p = "p.value", string.ci = "CI 95%", dv.labels = "Perceived distance",
          show.intercept = TRUE, show.aic = FALSE, show.zeroinf = TRUE, show.re.var = FALSE, show.reflvl = TRUE,
          CSS = list(css.table = '+font-family: Arial;'),
          pred.labels = c("Intercept","Target distance", "Condition (Floor level)","Target distance * Condition (Floor level)"),
          file = "plot.html")


webshot("plot.html","Summary_PAD_POB_NORMAL_LOG.png", vwidth = 600, vheight = 100)





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

m.Dist1 <-  lmer(log10(perc_dist) ~ log10(target_distance)*condition+(log10(target_distance)|subject),
                 data = filter(results_tbl,type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)

anova(m.Dist1)
anov1 = anova(m.Dist1)


p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}
anov1$Predictors = c("Target distance","Condition","Target distance:Condition")
anov1 = data.frame(anov1)
anov1 <- flextable(anov1,col_keys = c("Predictors","NumDF","DenDF", "F.value","Pr..F.")) %>%
  hline_top(border = fp_border(color="black", width = .5), part = "all")%>%
  hline_bottom(border = fp_border(color="black", width = .5))%>%
  width(j = 1, width = 5, unit = "cm")%>%
  align(align = "center", part = "all")%>%
  align(j = 1, align = "left", part = "all")%>%
  colformat_double(digits = 1, na_str = "N/A")%>%
  set_formatter(values = list("Pr..F." = p_val_format) )%>% 
  font(fontname = "+font-family: Arial;")%>%
  font(fontname = "+font-family: Arial;", part = "header")%>%
  
  fontsize(size = 10.5, part = "header")%>% 
  fontsize(size = 10.5)%>%
  bold(j = "Pr..F.", bold = TRUE)%>%
  italic(italic = TRUE, part = "header")%>%
  line_spacing(space = 1, part = "body")%>%
  line_spacing(space = .5, part = "header")
anov1


save_as_image(anov1,"Anov_PAD_POB_ROVEDL_LOG.png")

# tab_model(m.Dist1,file="plot.html")
tab_model(m.Dist1, wrap.labels = 80,
          auto.label = FALSE, show.stat = TRUE, string.p = "p.value", string.ci = "CI 95%", dv.labels = "Perceived distance",
          show.intercept = TRUE, show.aic = FALSE, show.zeroinf = TRUE, show.re.var = FALSE, show.reflvl = TRUE,
          CSS = list(css.table = '+font-family: Arial;'),
          pred.labels = c("Intercept","Target distance", "Condition (Floor level)","Target distance * Condition (Floor level)"),
          file = "plot.html")


webshot("plot.html","Summary_PAD_POB_ROVED_LOG.png", vwidth = 600, vheight = 100)









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

#### debug -----
# ext_stats = extract_stats(ggcoefstats(m.Dist1))
# ext_stats_f = flextable(ext_stats$tidy_data) %>%
#   set_header_labels(values = list(
#                       term = "Term",
#                       estimate = "Estimate",
#                       std.error = "Std.error",
#                       conf.level = "Conf.level",
#                       conf.low = "Conf.low",
#                       conf.high = "Conf.high",
#                       statistic = "Statistic",
#                       df.error = "Df.error",
#                       p.value = "p.value",
#                       effect = "Effect",
#                       group = "Group",
#                       conf.method = "Conf.method",
#                       expression = "Expression"
#                     )
#   )%>%
#  
#   fontsize(size = 22)%>%
#   fontsize(size = 24, part = "header")%>%
#   width(j = 1, width = 12.6, unit = "cm")%>%
#   align(align ='center', part = 'all') %>% 
#   align(j = 1, align ='left', part = 'all')%>%
#   colformat_num(j = c("estimate","std.error","conf.low","conf.high",
#                     "statistic","p.value"), digits = 2,  big.mark = " ",
#                 na_str = "N/A")
# ext_stats_f = delete_columns(ext_stats_f, j = c("group","conf.method","expression"))
# ext_stats_f = autofit(ext_stats_f)
# # ext_stats = theme_booktabs(ext_stats)
# save_as_image(ext_stats_f,"stats_PAD_POB.png")
# 
# 
# ext_stats_re <- flextable(ext_stats$glance_data)%>%
#   set_header_labels(values = list(
#     R2_conditional = "R2 Conditional",
#     R2_marginal = "R2 Marginal"
#   )
#   )%>%
#   fontsize(size = 8)%>%
#   fontsize(size = 10, part = "header")%>%
#   align(align ='center', part = 'all')
# ext_stats_re = delete_columns(ext_stats_f, j = "expression")
# ext_stats_re = autofit(ext_stats_re)
# save_as_image(ext_stats_re,"stats_re_PAD_POB.png")