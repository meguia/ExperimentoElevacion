library(tidyverse)
library(ggpubr)
library(janitor)

rm(list=ls())
figures_folder = "figuras"

#Data entry -----
rm(list=ls())
figures_folder = "figuras"
data_folder         = "./DatosUnificados/Datos nuevos sentados"
cat('/* Importando datos desde la carpeta \'', data_folder, '\':\n', sep = '')

c <- 0
for (archivo in dir(data_folder, pattern = '*.csv')) {
  
  ruta_completa <- paste(data_folder, .Platform$file.sep, archivo, sep = '')
  cat('- Cargando archivo: \'', ruta_completa, '\'\n', sep = '')
  tabla_temp <- read.csv(ruta_completa, header = TRUE, sep = ',', stringsAsFactors = TRUE)
  
  if (c == 0)
    tabla.raw <- tabla_temp
  else
    tabla.raw <- rbind(tabla.raw, tabla_temp)
  
  c <- c + 1
}

rm("c", "tabla_temp", "archivo", "ruta_completa")
tabla.raw <- tabla.raw%>%
  mutate(type = case_when(
    condicion == 0 ~ "NORMAL",
    condicion == 2 ~ "ROVED"
  ))
tabla.raw <- tabla.raw%>%
  mutate(condition = case_when(
    altura == 0 ~ "Floor level",
    altura == 1 ~ "Ear level"
  ))
tabla.raw <- tabla.raw%>%
  mutate(subject = case_when(
    nsub == 1 ~ "T001",
    nsub == 2 ~ "T002",
    nsub == 3 ~ "T003",
    nsub == 4 ~ "T004",
    nsub == 5 ~ "T005",
    nsub == 6 ~ "T006",
    nsub == 7 ~ "T007",
    nsub == 8 ~ "T008",
    nsub == 9 ~ "T009",
    nsub == 10 ~ "T010",
    nsub == 11 ~ "T011",
    nsub == 12 ~ "T012",
    nsub == 13 ~ "T013",
    nsub == 14 ~ "T014",
    nsub == 15 ~ "T015",
    nsub == 16 ~ "T016",
    nsub == 17 ~ "T017",
    nsub == 18 ~ "T018",
    nsub == 19 ~ "T019",
    nsub == 20 ~ "T020",
    nsub == 21 ~ "T021"
  ))

tabla.raw = tabla.raw %>% rename(block = bloque)   # renombro columnas
tabla.raw = tabla.raw %>% rename(trial = itrial)   # renombro columnas
tabla.raw = tabla.raw %>% rename(percived_distance = respuesta)   # renombro columnas
tabla.raw = tabla.raw %>% rename(target_distance = distancia)   # renombro columnas
tabla.raw <- tabla.raw[ , !(names(tabla.raw) %in% c("nsub","altura","condicion"))]
tabla.raw$location = "sitting"


tabla.raw2 <- read.csv("./DatosUnificados/datacrudafinal2.csv", header = TRUE, sep = ';', stringsAsFactors = TRUE)
tabla.raw2$location = "standing"
tabla.raw = merge(x = tabla.raw, y = tabla.raw2, all = TRUE)
rm("tabla.raw2")

idx = tabla.raw$subject == "S013" & tabla.raw$percived_distance == 0.05
tabla.raw[idx,]$percived_distance = 0.5
tabla.raw$abs_bias <-  tabla.raw$percived_distance - tabla.raw$target_distance
tabla.raw$signed_bias <- (tabla.raw$percived_distance - tabla.raw$target_distance) / tabla.raw$target_distance
tabla.raw$unsigned_bias <- abs(tabla.raw$signed_bias)
tabla.raw$log_bias = log10(tabla.raw$percived_distance / tabla.raw$target_distance)
tabla.raw$log_bias_un = abs(tabla.raw$log_bias)


tabla.raw$antilog =  10^tabla.raw$log_bias - 1
tabla.raw$absantilog = abs(tabla.raw$antilog)
tabla.raw$antilog_un =  10^tabla.raw$log_bias_un



tabla.raw$antilog_un_p <- ifelse(
  tabla.raw$antilog_un >= 1,
  (tabla.raw$antilog_un - 1),
  (1/tabla.raw$antilog_un - 1)
)


# idx = tabla.raw$subject == "S013" & tabla.raw$percived_distance == 0.05
# tabla.raw[idx,]$percived_distance = 0.5


f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

results_tbl <- tibble(aggregate(cbind(percived_distance,signed_bias,unsigned_bias,abs_bias,log_bias,log_bias_un,antilog,absantilog,antilog_un,antilog_un_p) ~ subject*block*condition*target_distance*type*location,
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
       location = factor(location),

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

       log_bias_sd = log_bias[,"sd"],
       log_bias_sem = log_bias[,"sem"],
       log_bias_var = log_bias[,"var"],
       log_bias_n = log_bias[,"n"],
       log_bias_mean = log_bias[,"mean"],

       log_bias_sd_un = log_bias_un[,"sd"],
       log_bias_sem_un = log_bias_un[,"sem"],
       log_bias_var_un = log_bias_un[,"var"],
       log_bias_n_un = log_bias_un[,"n"],
       log_bias_un_mean = log_bias_un[,"mean"],
       
       antilog_bias_sd = antilog[,"sd"],
       antilog_bias_sem = antilog[,"sem"],
       antilog_bias_var = antilog[,"var"],
       antilog_bias_n = antilog[,"n"],
       antilog_bias_mean = antilog[,"mean"],

       absantilog_bias_sd = absantilog[,"sd"],
       absantilog_bias_sem = absantilog[,"sem"],
       absantilog_bias_var = absantilog[,"var"],
       absantilog_bias_n = absantilog[,"n"],
       absantilog_bias_mean = absantilog[,"mean"],

       antilog_bias_sd_un = antilog_un[,"sd"],
       antilog_bias_sem_un = antilog_un[,"sem"],
       antilog_bias_var_un = antilog_un[,"var"],
       antilog_bias_n_un = antilog_un[,"n"],
       antilog_bias_un_mean = antilog_un[,"mean"],

       antilog_bias_sd_un_p = antilog_un_p[,"sd"],
       antilog_bias_sem_un_p = antilog_un_p[,"sem"],
       antilog_bias_var_un_p = antilog_un_p[,"var"],
       antilog_bias_n_un_p = antilog_un_p[,"n"],
       antilog_bias_un_p_mean = antilog_un_p[,"mean"],

       abs_bias_sd = abs_bias[,"sd"],
       abs_bias_sem = abs_bias[,"sem"],
       abs_bias_var = abs_bias[,"var"],
       abs_bias_n = abs_bias[,"n"],
       abs_bias_mean = abs_bias[,"mean"]) %>%
  
  
select(-c(percived_distance,signed_bias,unsigned_bias,abs_bias,log_bias,log_bias_un,
          antilog,absantilog,antilog_un,antilog_un_p)) %>%
  
  write_csv("./DatosUnificados/Dresults.csv")

results_tbl2 <- read.csv("./DatosUnificados/Dresults.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)




