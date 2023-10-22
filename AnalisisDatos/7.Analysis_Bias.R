library(tidyverse)
library(lme4)
library(nlme)
library(sjPlot)
library(MuMIn)
library(ggstatsplot)
library(ggpubr)
library(ggpp)


rm(list=ls())
figures_folder = "figuras"
results_tbl <- read.csv("./DatosUnificados/Dresults_without_outliers_slope_and_intercepto_lin_log.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

results_tbls <- results_tbl %>% 
  group_by(condition, type,subject) %>%
  summarise(mBiasSigned  = mean(rel_bias_signed,na.rm=TRUE),
            SdBiasSigned  = sd(rel_bias_signed,na.rm=TRUE)/sqrt(length(rel_bias_signed)),
            mBiasUnSigned  = mean(rel_bias_unsigned,na.rm=TRUE),
            SdBiasUnSigned  = sd(rel_bias_unsigned,na.rm=TRUE)/sqrt(length(rel_bias_unsigned)))  %>%
  ungroup()

results_tblp <- results_tbls %>% 
  group_by(condition, type) %>%
  summarise(MBiasSigned  = mean(mBiasSigned,na.rm=TRUE),
            SDBiasSigned  = sd(mBiasSigned,na.rm=TRUE)/sqrt(length(mBiasSigned)),
            MBiasUnSigned  = mean(mBiasUnSigned,na.rm=TRUE),
            SDBiasUnSigned  = sd(mBiasUnSigned,na.rm=TRUE)/sqrt(length(mBiasUnSigned)))  %>%
  ungroup()
# geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
#   geom_point(aes(x = as.numeric(Tiempo)-.15, y = LeqAS, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+

f6 =  ggplot(results_tblp, aes(x = condition,y = MBiasSigned, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=MBiasSigned, ymin=MBiasSigned-SDBiasSigned, ymax=MBiasSigned+SDBiasSigned),
                  position = position_dodgenudge(direction = "split", width = 3), size = 1.2)+
  geom_line(aes(group = type),position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_point(data = results_tbls, mapping = aes(x = condition,y = mBiasSigned, colour = condition, fill = condition), alpha = .8)+
  geom_line(data = results_tbls, mapping = aes(x = condition,y = mBiasSigned, group = subject, colour = condition, fill = condition),alpha = 0.3)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Relative signed \nbias [%]") +
  facet_grid(. ~ type) +
  # annotate("text", x = 1.5, y = 2,  label = "***", size = 4) +
  # annotate("segment", x = 1, xend = 2, y = 1.9, yend = 1.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")

f6
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "13. Bias signed", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f6, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

t.test(filter(results_tbls, 
              condition=="Ear level" & type=="NORMAL")$mBiasSigned,
       filter(results_tbls, 
              condition=="Floor level" & type=="NORMAL")$mBiasSigned, 
       paired = TRUE)

t.test(filter(results_tbls, 
              condition=="Ear level" & type=="ROVED")$mBiasSigned,
       filter(results_tbls, 
              condition=="Floor level" & type=="ROVED")$mBiasSigned, 
       paired = TRUE)


aaa <- filter(results_tbl,type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias_signed)) %>%
  ungroup()
m.RelativBias <- lm(mSesgoRel ~ condition, 
                    data = aaa)
extract_stats(ggcoefstats(m.RelativBias))
anova(m.RelativBias)

aaa <- filter(results_tbl,type == "ROVED") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias_signed)) %>%
  ungroup()
m.RelativBias <- lm(mSesgoRel ~ condition, 
                    data = aaa)
extract_stats(ggcoefstats(m.RelativBias))
anova(m.RelativBias)

# Unsigned
f7 =  ggplot(results_tblp, aes(x = condition,y = MBiasUnSigned, colour = condition, fill = condition)) +
  geom_pointrange(aes(x=condition, y=MBiasUnSigned, ymin=MBiasUnSigned-SDBiasSigned, ymax=MBiasUnSigned+SDBiasSigned),
                  position = position_dodgenudge(direction = "split", width = 3), size = 1.2)+
  geom_line(aes(group = type),position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  geom_point(data = results_tbls, mapping = aes(x = condition,y = mBiasUnSigned, colour = condition, fill = condition), alpha = .8)+
  geom_line(data = results_tbls, mapping = aes(x = condition,y = mBiasUnSigned, group = subject, colour = condition, fill = condition),alpha = 0.3)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition",
       y = "Relative unsigned \nbias [%]") +
  ylim(c(-.1,1.6))+
  # scale_x_continuous(name="Condition", limits = c(-.5,1.5)) +
  # scale_y_continuous(name="Relative signed \nbias [%]") +
  facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 1.6,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 1.5, yend = 1.5, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")

f7
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "14. Bias unsigned", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f7, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

t.test(filter(results_tbls, 
              condition=="Ear level" & type=="NORMAL")$mBiasUnSigned,
       filter(results_tbls, 
              condition=="Floor level" & type=="NORMAL")$mBiasUnSigned, 
       paired = TRUE)

t.test(filter(results_tbls, 
              condition=="Ear level" & type=="ROVED")$mBiasUnSigned,
       filter(results_tbls, 
              condition=="Floor level" & type=="ROVED")$mBiasUnSigned, 
       paired = TRUE)


aaa <- filter(results_tbl,type == "NORMAL") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias_unsigned)) %>%
  ungroup()
m.RelativBias <- lm(mSesgoRel ~ condition, 
                    data = aaa)
extract_stats(ggcoefstats(m.RelativBias))
anova(m.RelativBias)

aaa <- filter(results_tbl,type == "ROVED") %>% 
  group_by(subject,condition) %>%
  summarise(mSesgoRel  = mean(rel_bias_unsigned)) %>%
  ungroup()
m.RelativBias <- lm(mSesgoRel ~ condition, 
                    data = aaa)
extract_stats(ggcoefstats(m.RelativBias))
anova(m.RelativBias)
