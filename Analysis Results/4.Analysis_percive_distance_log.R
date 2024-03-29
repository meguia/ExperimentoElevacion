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
  # i = 2
  sub = levels(results_tbl$subject)[i]
  print(sub)
  m.pend = lm(log10(perc_dist) ~ log10(target_distance)*condition,
              data = filter(results_tbl,type == "NORMAL", subject == sub))
  
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$slopeLog = m.pend$coefficients[[2]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$slopeLog = m.pend$coefficients[[2]]+m.pend$coefficients[[4]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$interceptoLog = m.pend$coefficients[[1]]
  results_tbl[(results_tbl$type == "NORMAL" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$interceptoLog = m.pend$coefficients[[1]]+m.pend$coefficients[[3]]
  
  cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
  
  # x1 = c(0:8)
  x1 = seq(0,8,by=.01)
  curves1 = data.frame(x2 = x1,
             y1 = (10^(m.pend$coefficients[[1]]))*(x1^m.pend$coefficients[[2]]),
             condition = "Ear level")
  curves2 = data.frame(x2 = x1,
                      y1 = 10^(m.pend$coefficients[[1]]+m.pend$coefficients[[3]])*(x1^(m.pend$coefficients[[2]]+m.pend$coefficients[[4]])),
                      condition = "Floor level")
  curve = merge(x = curves1, y = curves2, all = TRUE)
  # bearlevel = function(x) (10^(m.pend$coefficients[[1]]))*(x^m.pend$coefficients[[2]])
  # bfloorlevel = function(x) (10^(m.pend$coefficients[[1]]+m.pend$coefficients[[3]]))*(x^(m.pend$coefficients[[2]]+m.pend$coefficients[[4]]))
  # 
  eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                    list(a = round(10^(m.pend$coefficients[[1]]),digits = 2),
                         b = round(m.pend$coefficients[[2]], digits = 2)))
  eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                    list(a = round(10^(m.pend$coefficients[[1]]+m.pend$coefficients[[3]]), digits = 2),
                         b = round(m.pend$coefficients[[2]]+m.pend$coefficients[[4]], digits = 2)))
  eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                    list(b = round(summary(m.pend)$r.squared, digits = 2)))
  
  fig1 = ggplot(filter(results_tbl,type == "NORMAL", subject == sub), 
                aes(x = target_distance, y = perc_dist,
                    colour = condition, fill = condition, group = condition))+
    geom_pointrange(aes(x = target_distance, y = perc_dist, ymin = perc_dist-perc_dist_sem, ymax = perc_dist+perc_dist_sem),alpha = 0.4, 
                    position = position_jitterdodge(jitter.width = .1,
                                                    jitter.height = 0,
                                                    dodge.width = .1 ))+
    geom_line(data = curve, mapping = aes(x = x2, y = y1, colour = condition))+
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    geom_abline(slope = 1, 
                intercept = 0, 
                alpha = 0.5, 
                linetype = "dashed") +
    # # geom_function(fun = earlevel, colour = "#000000")+
    # geom_function(fun = bfloorlevel, colour = "#E69F00")+
    # stat_function(fun = bearlevel, colour = "#000000")+

    geom_text(x = 0.2, y = 7.6, label = sub, hjust = 0, nudge_x =  0, parse = TRUE, size =  2.6, color = "#999999")+
    geom_text(x = 0.2, y = 6.6, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 2.6, color = "#000000")+
    geom_text(x = 0.2, y = 5.6, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 2.6, color = "#E69F00")+
    geom_text(x = 0.2, y = 4.6, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 2.6, color = "#009E73")+
    scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
    theme_pubr(base_size = 10, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  fig1
  
  fig_normal[[i]] = fig1
  graphics.off()
  earlevel = 0
  rm("earlevel", "floorlevel")
}

Figure1 = ggarrange(fig_normal[[1]],fig_normal[[2]],fig_normal[[3]],fig_normal[[4]],fig_normal[[5]],fig_normal[[6]],fig_normal[[7]],fig_normal[[8]],fig_normal[[9]],
                    fig_normal[[10]],fig_normal[[11]],fig_normal[[12]],fig_normal[[13]],fig_normal[[14]],fig_normal[[15]],fig_normal[[16]],fig_normal[[17]],fig_normal[[18]],
                    fig_normal[[19]],fig_normal[[20]],
                    ncol = 4, nrow = 6,
                    common.legend = TRUE, legend="top", align = "hv")

mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "3. Lm for subject NORMAL LOG", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure1, width=27, height=35, units="cm", limitsize=FALSE, dpi=600)

# ROVED ----
fig_ROVED = list()
for (i in 1:length(levels(results_tbl$subject))) {
  print(i)
  # i = 2
  sub = levels(results_tbl$subject)[i]
  print(sub)
  m.pend = lm(log10(perc_dist) ~ log10(target_distance)*condition,
              data = filter(results_tbl,type == "ROVED", subject == sub))
  
  results_tbl[(results_tbl$type == "ROVED" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$slopeLog = m.pend$coefficients[[2]]
  results_tbl[(results_tbl$type == "ROVED" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$slopeLog = m.pend$coefficients[[2]]+m.pend$coefficients[[4]]
  results_tbl[(results_tbl$type == "ROVED" & results_tbl$subject == sub &results_tbl$condition == "Ear level"),]$interceptoLog = m.pend$coefficients[[1]]
  results_tbl[(results_tbl$type == "ROVED" & results_tbl$subject == sub &results_tbl$condition == "Floor level"),]$interceptoLog = m.pend$coefficients[[1]]+m.pend$coefficients[[3]]
  
  cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
  
  # x1 = c(0:8)
  x1 = seq(0,8,by=.01)
  curves1 = data.frame(x2 = x1,
                       y1 = (10^(m.pend$coefficients[[1]]))*(x1^m.pend$coefficients[[2]]),
                       condition = "Ear level")
  curves2 = data.frame(x2 = x1,
                       y1 = 10^(m.pend$coefficients[[1]]+m.pend$coefficients[[3]])*(x1^(m.pend$coefficients[[2]]+m.pend$coefficients[[4]])),
                       condition = "Floor level")
  curve = merge(x = curves1, y = curves2, all = TRUE)
  # bearlevel = function(x) (10^(m.pend$coefficients[[1]]))*(x^m.pend$coefficients[[2]])
  # bfloorlevel = function(x) (10^(m.pend$coefficients[[1]]+m.pend$coefficients[[3]]))*(x^(m.pend$coefficients[[2]]+m.pend$coefficients[[4]]))
  # 
  eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                    list(a = round(10^(m.pend$coefficients[[1]]),digits = 2),
                         b = round(m.pend$coefficients[[2]], digits = 2)))
  eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)^italic((b)), 
                    list(a = round(10^(m.pend$coefficients[[1]]+m.pend$coefficients[[3]]), digits = 2),
                         b = round(m.pend$coefficients[[2]]+m.pend$coefficients[[4]], digits = 2)))
  eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                    list(b = round(summary(m.pend)$r.squared, digits = 2)))
  
  fig1 = ggplot(filter(results_tbl,type == "ROVED", subject == sub), 
                aes(x = target_distance, y = perc_dist,
                    colour = condition, fill = condition, group = condition))+
    geom_pointrange(aes(x = target_distance, y = perc_dist, ymin = perc_dist-perc_dist_sem, ymax = perc_dist+perc_dist_sem),alpha = 0.4, 
                    position = position_jitterdodge(jitter.width = .1,
                                                    jitter.height = 0,
                                                    dodge.width = .1 ))+
    geom_line(data = curve, mapping = aes(x = x2, y = y1, colour = condition))+
    scale_colour_manual(values = cbPalette) + 
    scale_fill_manual(values = cbPalette) + 
    geom_abline(slope = 1, 
                intercept = 0, 
                alpha = 0.5, 
                linetype = "dashed") +
    # # geom_function(fun = earlevel, colour = "#000000")+
    # geom_function(fun = bfloorlevel, colour = "#E69F00")+
    # stat_function(fun = bearlevel, colour = "#000000")+
    
    geom_text(x = 0.2, y = 7.6, label = sub, hjust = 0, nudge_x =  0, parse = TRUE, size = 2.6, color = "#999999")+
    geom_text(x = 0.2, y = 6.6, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 2.6, color = "#000000")+
    geom_text(x = 0.2, y = 5.6, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 2.6, color = "#E69F00")+
    geom_text(x = 0.2, y = 4.6, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 2.6, color = "#009E73")+
    scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
    scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
    theme_pubr(base_size = 10, margin = TRUE)+
    theme(legend.position = "top",
          legend.title = element_blank())
  fig1
  
  fig_ROVED[[i]] = fig1
  graphics.off()
  earlevel = 0
  rm("earlevel", "floorlevel")
}

Figure2 = ggarrange(fig_ROVED[[1]],fig_ROVED[[2]],fig_ROVED[[3]],fig_ROVED[[4]],fig_ROVED[[5]],fig_ROVED[[6]],fig_ROVED[[7]],fig_ROVED[[8]],fig_ROVED[[9]],
                    fig_ROVED[[10]],fig_ROVED[[11]],fig_ROVED[[12]],fig_ROVED[[13]],fig_ROVED[[14]],fig_ROVED[[15]],fig_ROVED[[16]],fig_ROVED[[17]],fig_ROVED[[18]],
                    fig_ROVED[[19]],fig_ROVED[[20]],
                    ncol = 4, nrow = 6,
                    common.legend = TRUE, legend="top", align = "hv")

mi_nombre_de_archivo = paste("figuras", .Platform$file.sep, "4. Lm for subject ROVED LOG", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Figure2, width=27, height=35, units="cm", limitsize=FALSE, dpi=600)

# Write_csv with SLOPE AND INTERCEPTO
write_csv(results_tbl, "./DatosUnificados/Dresults_without_outliers_slope_and_intercepto_lin_log.csv")
