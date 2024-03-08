library(tidyverse)
library(lme4)
# library(nlme)
library(dplyr)
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

# install.packages("sjPlot")
library(flextable)
library(sjPlot)
# install.packages("webshot")
library(webshot)
library(officer)
library(reshape)


rm(list=ls())
figures_folder = "figuras"
results_tbl <- read.csv("./DatosUnificados/Dresults_without_outliers_slope_and_intercepto_lin_log.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")



results_tblp <- results_tbl %>% 
  group_by(condition, type) %>%
  summarise(mslope  = mean(slope,na.rm=TRUE),
            SDslope  = sd(slope,na.rm=TRUE),
            mintercepto  = mean(intercepto,na.rm=TRUE),
            SDintercepto  = sd(intercepto,na.rm=TRUE),)  %>%
  ungroup()

# geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
#   geom_point(aes(x = as.numeric(Tiempo)-.15, y = LeqAS, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+

f3 =  ggplot(results_tblp, aes(x = condition,y = mslope, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=mslope, ymin=mslope-SDslope, ymax=mslope+SDslope),
                  position = position_dodgenudge(direction = "split", width = 3), size = 1.2)+
  geom_line(aes(group = type),position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_point(data = results_tbl, mapping = aes(x = condition,y = slope, colour = condition, fill = condition), alpha = .8)+
  geom_line(data = results_tbl, mapping = aes(x = condition,y = slope, group = subject, colour = condition, fill = condition),alpha = 0.3)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Slope with LM") +
  facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 2,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 1.9, yend = 1.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0,0.2,0,1), 'lines'))

f3
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "9. Lm Slope", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f3, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

t.test(filter(results_tbl, 
              condition=="Ear level" & type=="NORMAL")$slope,
       filter(results_tbl, 
              condition=="Floor level" & type=="NORMAL")$slope, 
       paired = TRUE)


aaa <- filter(results_tbl,type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mslope  = mean(slope,na.rm=TRUE),
            SDslope  = sd(slope,na.rm=TRUE),
            mintercepto  = mean(intercepto,na.rm=TRUE),
            SDintercepto  = sd(intercepto,na.rm=TRUE),)  %>%
  ungroup()
m.RelativBias <- lm(mslope ~ condition, 
                    data = aaa)
extract_stats(ggcoefstats(m.RelativBias))
anova(m.RelativBias)

# Intercepto for condition --------

f4 =  ggplot(results_tblp, aes(x = condition,y = mintercepto, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=mintercepto, ymin=mintercepto-SDintercepto, ymax=mintercepto+SDintercepto),
                      position = position_dodgenudge(direction = "split", width = 3), size = 1.2)+
  geom_line(aes(group = type),position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_point(data = results_tbl, mapping = aes(x = condition,y = intercepto, colour = condition, fill = condition), alpha = .8)+
  geom_line(data = results_tbl, mapping = aes(x = condition,y = intercepto, group = subject, colour = condition, fill = condition),alpha = 0.3)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  # stat_summary(fun.data = "mean_se",
  #              geom = "pointrange",
  #              alpha = 1,
  #              size = 1,
  #              # position = position_dodge(width = 1)
  #              position = position_jitterdodge(jitter.width = 0.6,
  #                                              jitter.height = 0,
  #                                              dodge.width = 0 )) +
  # stat_summary(fun.data = "mean_se",
  #              geom = "linerange",
  #              size=2,
#              position = position_dodge(width = 1)) +
labs(x = "Condition", 
     y = "Intercepto with LM") +
  facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 8,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 7.9, yend = 7.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0,0.2,0,1), 'lines'))

f4
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "10. Lm Intercepto", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f4, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)


t.test(filter(results_tbl, 
              condition=="Ear level" & type=="ROVED")$slope,
       filter(results_tbl, 
              condition=="Floor level" & type=="ROVED")$slope, 
       paired = TRUE)


# Analysis Range ----

a = results_tbl[results_tbl$target_distance == 6,]
b = results_tbl[results_tbl$target_distance == 2,]




a1 <- filter(results_tbl, target_distance == 6| target_distance == 2)%>%
  group_by(subject,condition, type, target_distance) %>%
  summarise(perc_dist = perc_dist)%>%
  ungroup()

a6 = a1[a1$target_distance == 6,]
a6 = rename(a6, c(perc_dist="perc_dist6"))
a6 = rename(a6, c(target_distance = "target_distance6"))


a2 = a1[a1$target_distance == 2,]
a2 = rename(a2, c(perc_dist="perc_dist2"))
a2 = rename(a2, c(target_distance = "target_distance2"))
results_tblrange  = merge(x = a2, y = a6, all.x = TRUE)
results_tblrange$range = results_tblrange$perc_dist6 -  results_tblrange$perc_dist2 

results_tblp <- results_tblrange %>% 
  group_by(condition, type) %>%
  summarise(mslope  = mean(range,na.rm=TRUE),
            SDslope  = sd(range,na.rm=TRUE)/sqrt(length(range)))  %>%
  ungroup()

f5 =  ggplot(results_tblp, aes(x = condition,y = mslope, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=mslope, ymin=mslope-SDslope, ymax=mslope+SDslope),
                  position = position_dodgenudge(direction = "split", width = 3), size = 1.2, alpha=.5,shape = 0)+
  geom_line(aes(group = type),position = position_dodgenudge(direction = "split", width = 3),size = 1.2)+
  geom_point(data = results_tblrange, mapping = aes(x = condition,y = range, colour = condition, fill = condition), alpha = .8)+
  geom_line(data = results_tblrange, mapping = aes(x = condition,y = range, group = subject, colour = condition, fill = condition),alpha = 0.3)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Range rate") +
  facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 7,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 6.9, yend = 6.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")

f5
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "11.Range", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f5, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

# NORMAL
m.RangeRate <- lm(range ~ condition, 
                    data = filter(results_tblrange, type == "NORMAL"))
extract_stats(ggcoefstats(m.RangeRate))
anov = anova(m.RangeRate)
anov


p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}
anov$Predictors = c("Condition","Residuals")
anov = data.frame(anov)
anov <- flextable(anov,col_keys = c("Predictors","Df", "Sum.Sq", "Mean.Sq", "F.value","Pr..F.")) %>%
  hline_top(border = fp_border(color="black", width = .5), part = "all")%>%
  hline_bottom(border = fp_border(color="black", width = .5))%>%
  width(j = 1, width = 3, unit = "cm")%>%
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

anov


save_as_image(anov,"anov_RangeRate_POB.png")

tab_model(m.RangeRate,file="plot.html")
tab_model(m.RangeRate, wrap.labels = 80,
          auto.label = FALSE, show.stat = TRUE, string.p = "p.value", string.ci = "CI 95%", dv.labels = "Range rate",
          show.intercept = TRUE, show.aic = FALSE, show.zeroinf = TRUE, show.re.var = FALSE, show.reflvl = TRUE,
          CSS = list(css.table = '+font-family: Arial;'),
          pred.labels = c("Intercept","Condition (Floor level)"),
          file = "plotweee.html")


webshot("plotweee.html","plotqweqweqwe.png", vwidth = 600, vheight = 100)


t.test(filter(results_tblrange, 
              condition=="Ear level" & type=="NORMAL")$range,
       filter(results_tblrange, 
              condition=="Floor level" & type=="NORMAL")$range, 
       paired = TRUE)

t.test(filter(results_tblrange, 
              condition=="Ear level" & type=="ROVED")$range,
       filter(results_tblrange, 
              condition=="Floor level" & type=="ROVED")$range, 
       paired = TRUE)


#ROVED

m.RangeRate <- lm(range ~ condition, 
                  data = filter(results_tblrange, type == "ROVED"))
extract_stats(ggcoefstats(m.RangeRate))
anov = anova(m.RangeRate)
anov


p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}
anov$Predictors = c("Condition","Residuals")
anov = data.frame(anov)
anov <- flextable(anov,col_keys = c("Predictors","Df", "Sum.Sq", "Mean.Sq", "F.value","Pr..F.")) %>%
  hline_top(border = fp_border(color="black", width = .5), part = "all")%>%
  hline_bottom(border = fp_border(color="black", width = .5))%>%
  width(j = 1, width = 3, unit = "cm")%>%
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

anov


save_as_image(anov,"anov_RangeRate_POB_ROVED.png")

tab_model(m.RangeRate,file="plot.html")
tab_model(m.RangeRate, wrap.labels = 80,
          auto.label = FALSE, show.stat = TRUE, string.p = "p.value", string.ci = "CI 95%", dv.labels = "Range rate",
          show.intercept = TRUE, show.aic = FALSE, show.zeroinf = TRUE, show.re.var = FALSE, show.reflvl = TRUE,
          CSS = list(css.table = '+font-family: Arial;'),
          pred.labels = c("Intercept","Condition (Floor level)"),
          file = "plotweee.html")


webshot("plotweee.html","plotROVED.png", vwidth = 600, vheight = 100)


t.test(filter(results_tblrange, 
              condition=="Ear level" & type=="NORMAL")$range,
       filter(results_tblrange, 
              condition=="Floor level" & type=="NORMAL")$range, 
       paired = TRUE)

t.test(filter(results_tblrange, 
              condition=="Ear level" & type=="ROVED")$range,
       filter(results_tblrange, 
              condition=="Floor level" & type=="ROVED")$range, 
       paired = TRUE)







Figure3 = ggarrange(f3 + rremove("xlab")+ rremove("x.text")+theme(plot.margin = unit(c(-.3,0.1,-3,0.1), 'lines')),
                    f4+theme(plot.margin = unit(c(-.3,0.1,0,0.1), 'lines')),
                    ncol = 1, nrow = 2,
                    common.legend = TRUE, legend="top", align = "hv")
Figure3
mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "12. Combinate Fig Slop Intercept", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure3, width=12, height=12, units="cm", limitsize=FALSE, dpi=600)
