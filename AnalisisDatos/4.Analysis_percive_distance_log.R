library(tidyverse)
library(ggpubr)


rm(list=ls())
figures_folder = "figuras"
results_tbl <- read.csv("./DatosUnificados/Dresults_without_outliers_slope_and_intercepto.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

#NORMAL ----
results_tbl$slopeLog = 0
results_tbl$interceptoLog = 0
fig_normal = list()
for (i in 1:length(levels(results_tbl$subject))) {
  print(i)
  sub = levels(results_tbl$subject)[i]
  print(sub)
  m.pend = lm(log10(perc_dist) ~ log10(target_distance)*condition,
              data = filter(results_tbl,type == "NORMAL", subject == sub))
  
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$slopeLog = m.pend$coefficients[[2]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$slopeLog = m.pend$coefficients[[2]]+m.pend$coefficients[[4]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$interceptoLog = m.pend$coefficients[[1]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$interceptoLog = m.pend$coefficients[[1]]+m.pend$coefficients[[3]]
  
  cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
  
  
  eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                    list(a = round(m.pend$coefficients[[2]],digits = 2),
                         b = round(10^(m.pend$coefficients[[1]]), digits = 2)))
  eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                    list(a = round(m.pend$coefficients[[2]]+m.pend$coefficients[[4]], digits = 2),
                         b = round(10^(m.pend$coefficients[[1]]+m.pend$coefficients[[3]]), digits = 2)))
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
    geom_function(fun = function(x) (10^(m.pend$coefficients[[1]]))*(x^m.pend$coefficients[[2]]))+
    # curve(m.pend$coefficients[[1]]*exp(m.pend$coefficients[[2]]*x),0,8, col = "#000000")+
    # curve((m.pend$coefficients[[1]]+m.pend$coefficients[[3]])*exp((.pend$coefficients[[2]]+m.pend$coefficients[[4]])*x),0,8, col = "#E69F00")+
    # # geom_abline(slope = m.pend$coefficients[[2]], 
    #             intercept = m.pend$coefficients[[1]], 
    #             alpha = 0.5,
    #             color = "#000000") +
    # geom_abline(slope = m.pend$coefficients[[2]]+m.pend$coefficients[[4]], 
    #             intercept = m.pend$coefficients[[1]]+m.pend$coefficients[[3]], 
    #             alpha = 0.5,
    #             color = "#E69F00") +
    geom_text(x = 0.2, y = 7.6, label = sub, hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#999999")+
    geom_text(x = 0.2, y = 7.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
    geom_text(x = 0.2, y = 6.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
    geom_text(x = 0.2, y = 6.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
    scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  fig_normal[[i]] = fig1
  
}

Figure1 = ggarrange(fig_normal[[1]],fig_normal[[2]],fig_normal[[3]],fig_normal[[4]],fig_normal[[5]],fig_normal[[6]],fig_normal[[7]],fig_normal[[8]],fig_normal[[9]],
                    fig_normal[[10]],fig_normal[[11]],fig_normal[[12]],fig_normal[[13]],fig_normal[[14]],fig_normal[[15]],fig_normal[[16]],fig_normal[[17]],fig_normal[[18]],
                    fig_normal[[19]],fig_normal[[20]],
                    ncol = 5, nrow = 5,
                    common.legend = TRUE, legend="top", align = "hv")

mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "3. Lm for subject NORMAL LOG", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure1, width=50, height=50, units="cm", limitsize=FALSE, dpi=600)

# ROVED ----
fig_roved = list()
for (i in 1:length(levels(results_tbl$subject))) {
  print(i)
  sub = levels(results_tbl$subject)[i]
  print(sub)
  m.pend = lm(perc_dist ~ target_distance*condition,
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
    geom_text(x = 0.2, y = 7.6, label = sub, hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#999999")+
    geom_text(x = 0.2, y = 7.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
    geom_text(x = 0.2, y = 6.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
    geom_text(x = 0.2, y = 6.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
    scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
    theme_pubr(base_size = 12, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  fig_roved[[i]] = fig2
  
}

Figure2 = ggarrange(fig_roved[[1]],fig_roved[[2]],fig_roved[[3]],fig_roved[[4]],fig_roved[[5]],fig_roved[[6]],fig_roved[[7]],fig_roved[[8]],fig_roved[[9]],
                    fig_roved[[10]],fig_roved[[11]],fig_roved[[12]],fig_roved[[13]],fig_roved[[14]],fig_roved[[15]],fig_roved[[16]],fig_roved[[17]],fig_roved[[18]],
                    fig_roved[[19]],fig_roved[[20]],
                    ncol = 5, nrow = 5,
                    common.legend = TRUE, legend="top", align = "hv")
mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "2. Lm for subject ROVED", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure2, width=50, height=50, units="cm", limitsize=FALSE, dpi=600)

# Write_csv with SLOPE AND INTERCEPTO
write_csv(results_tbl, "./DatosUnificados/Dresults_without_outliers_slope_and_intercepto.csv")
