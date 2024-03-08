library(tidyverse)
library(ggpubr)
library(janitor)

rm(list=ls())
figures_folder = "figuras"

#Data entry -----
tabla.raw <- read.csv("./DatosUnificados/datacrudafinal2.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
tabla.raw$abs_bias <-  tabla.raw$percived_distance - tabla.raw$target_distance
tabla.raw$signed_bias <- (tabla.raw$percived_distance - tabla.raw$target_distance) / tabla.raw$target_distance
tabla.raw$unsigned_bias <- abs(tabla.raw$signed_bias)
idx = tabla.raw$subject == "13" | tabla.raw$percived_distance == 0.05
tabla.raw[idx,]$percived_distance = 0.5


f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

results_tbl <- tibble(aggregate(cbind(percived_distance,signed_bias,unsigned_bias,abs_bias) ~ subject*block*condition*target_distance*type,
                                data = tabla.raw,
                                FUN  = f_promedio,na.action = NULL))

# Graf datos crudos
# cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")
# f.all = ggplot(results_tbl, aes(x = target_distance, y = percived_distance[,"mean"], colour = condition, fill = condition))+
#   geom_point(alpha = 0.4, 
#              position = position_jitterdodge(jitter.width = .1,
#                                              jitter.height = 0,
#                                              dodge.width = .1 )) +
#   geom_point(data = tabla.raw, aes(x=target_distance, y = percived_distance, color = condition),
#              alpha = 0.4, size = .6,
#              position = position_jitterdodge(jitter.width = .1,
#                                              jitter.height = 0,
#                                              dodge.width = .1 )) +
#   scale_colour_manual(values = cbPalette) + 
#   scale_fill_manual(values = cbPalette) + 
#   
#   geom_abline(slope = 1, 
#               intercept = 0, 
#               alpha = 0.5, 
#               linetype = "dashed") +
#   # stat_summary(fun.data = "mean_se", 
#   #              geom = "pointrange", 
#   #              alpha = .4, 
#   #              position = position_dodge(width = 1)) +
#   # stat_summary(fun.data = "mean_se", 
#   #              geom = "linerange",  
#   #              size=2, 
#   #              position = position_dodge(width = 1)) + 
#   #geom_text(x = .5, y = .9, label = as.character(as.expression(eq1)), parse = TRUE, size = 4, color = "#000000")+
#   #geom_text(x = .5, y = .76, label = as.character(as.expression(eq2)), parse = TRUE, size = 4, color = "#E69F00")+
#   scale_x_continuous(name="Distance source (m)", limits = c(-1,20)) +
#   scale_y_continuous(name="Perceived distance (m)",   limits = c(-1,20)) +
#   facet_grid(type ~ subject) +
#   theme_pubr(base_size = 12, margin = TRUE)+
#   theme(legend.position = "top",
#         legend.title = element_blank())
# f.all
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Fig1 All data", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=f.all, width=90, height=10, units="cm", limitsize=FALSE, dpi=600)


results_tbl %>%
  # clean_names() %>%
  mutate(subject = factor(subject),
         condition = factor(condition),
         type = factor(type),
         block = factor(block),
         
         perc_dist_sd = percived_distance[,"sd"],
         perc_dist_sem = percived_distance[,"sem"],
         perc_dist_var = percived_distance[,"var"],
         perc_dist_n = percived_distance[,"n"],
         perc_dist = percived_distance[,"mean"],
         
         rel_bias_signed_sd = signed_bias[,"sd"],
         rel_bias_signed_sem = signed_bias[,"sem"],
         rel_bias_signed_var = signed_bias[,"var"],
         rel_bias_signed_n = signed_bias[,"n"],
         rel_bias_signed = signed_bias[,"mean"],

         rel_bias_unsigned_sd = unsigned_bias[,"sd"],
         rel_bias_unsigned_sem = unsigned_bias[,"sem"],
         rel_bias_unsigned_var = unsigned_bias[,"var"],
         rel_bias_unsigned_n = unsigned_bias[,"n"],
         rel_bias_unsigned = unsigned_bias[,"mean"],

         abs_bias_sd = abs_bias[,"sd"],
         abs_bias_sem = abs_bias[,"sem"],
         abs_bias_var = abs_bias[,"var"],
         abs_bias_n = abs_bias[,"n"],
         abs_bias = abs_bias[,"mean"]) %>%
  
select(-c(percived_distance,signed_bias,unsigned_bias,abs_bias)) %>%
  
write_csv("./DatosUnificados/Dresults.csv")


