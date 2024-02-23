library(tidyverse)
library(lme4)
library(nlme)
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
library(nlme)
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
  # scale_y_log10(name="Standard deviation (m)\n +/- SEM Intra-subject") +
  scale_y_log10(name="Standard deviation (m)\n +/- SEM Intra-subject", breaks=c(0,0.5,1,1.5,2), 
                labels=c(0,0.5,1,1.5,2), minor_breaks=NULL, limits = c(-1.1,2)) +
  scale_x_log10(name="Distance source (m)",  breaks=c(2,2.9,4.2,6), labels=c(2,2.9,4.2,6), minor_breaks=NULL, limits = c(1.9,6.1)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f3
mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "12.Intrasujetos Desvio", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f3, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

m.Dist1 <-  lme(perc_dist_sd ~ target_distance*condition, random = ~target_distance|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = filter(results_tbl,type == "ROVED"))
extract_stats(ggcoefstats(m.Dist1))
anova(m.Dist1)
anov = anova(m.Dist1)

p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}
anov$Predictors = c("Intercept","Target distance","Condition","Target distance:Condition")
anov = data.frame(anov)
anov <- flextable(anov,col_keys = c("Predictors", "numDF", "denDF", "F.value","p.value")) %>%
hline_top(border = fp_border(color="black", width = .5), part = "all")%>%
hline_bottom(border = fp_border(color="black", width = .5))%>%
  width(j = 1, width = 5, unit = "cm")%>%
  align(align = "center", part = "all")%>%
  align(j = 1, align = "left", part = "all")%>%
  colformat_double(digits = 1, na_str = "N/A")%>%
  set_formatter(values = list("p.value" = p_val_format) )%>% 
  font(fontname = "+font-family: Arial;")%>%
font(fontname = "+font-family: Arial;", part = "header")%>%

fontsize(size = 10.5, part = "header")%>% 
fontsize(size = 10.5)%>%
bold(j = "p.value", bold = TRUE)%>%
italic(italic = TRUE, part = "header")%>%
line_spacing(space = 1, part = "body")%>%
line_spacing(space = .5, part = "header")
anov
  

save_as_image(anov,"anov_PAD_POB.png")

tab_model(m.Dist1,file="plot.html")
tab_model(m.Dist1, wrap.labels = 80,
              auto.label = FALSE, show.stat = TRUE, string.p = "p.value", string.ci = "CI 95%", dv.labels = "Perceived distance",
              show.intercept = TRUE, show.aic = FALSE, show.zeroinf = TRUE, show.re.var = FALSE, show.reflvl = TRUE,
              CSS = list(css.table = '+font-family: Arial;'),
              pred.labels = c("Intercept","Target distance", "Condition (Floor level)","Target distance * Condition (Floor level)"),
              file = "plot.html")


webshot("plot.html","plot.png", vwidth = 600, vheight = 100)


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
