#************************************
# FFR_stat.R
# 心内科合作项目
# 分析FFR方法和金标准的差异性
# By Peiyao
# Date: 06/15/2017
#************************************
# clear all 
rm(list = ls())
# Add requried library
library(readxl)
library(dplyr)
library(ggplot2)
library(pROC)
library(BlandAltmanLeh)
# Load Data
FFR.raw.data <- read_xlsx('../data/FFR2.xlsx')
# Clear all NA rows
FFR.data.tmp <- FFR.raw.data[!is.na(FFR.raw.data$FFR),]
# We strip the FFR column into two column: 
#     1.vessel_position
#     2.FFR
FFR.data <- FFR.data.tmp %>%
  mutate(vessel = regmatches(FFR, regexpr("[[:alpha:]]+", FFR)),
         FFR_score = as.numeric(regmatches(FFR, regexpr("\\d+.\\d+", FFR)))) %>%
  select(-FFR) %>%
  mutate(diagnosis = factor(FFR_score > 0.8))
# Make ROC use FFR_score------------
FFR.CTA.roc <- roc(FFR.data$diagnosis,
                   FFR.data$FFRCTA
                   )
QCA.roc <- roc(FFR.data$diagnosis,
               FFR.data$QCA)
visual.roc <- roc(FFR.data$diagnosis,
                  FFR.data$visual)
# plot ROC curve
FFR.CTA <- data.frame(group='CTA-FFR',specificities = rev(1 - FFR.CTA.roc$specificities), sensitivities = rev(FFR.CTA.roc$sensitivities))
QCA <- data.frame(group='QCA',specificities = rev(1 - QCA.roc$specificities), sensitivities = rev(QCA.roc$sensitivities))
visual <- data.frame(group='CCTA',specificities = rev(1 - visual.roc$specificities), sensitivities = rev(visual.roc$sensitivities))
final.data <- rbind(FFR.CTA, QCA, visual)
f<-ggplot(final.data,aes(x = specificities, y = sensitivities, colour=group)) +
  geom_line(aes(linetype=group),size=1) + 
  geom_abline(intercept = 0, slope = 1) +
  xlab('1 - Specificity') + 
  ylab('Sensitivity') + 
  xlim(c(0,1)) +
  ylim(c(0,1)) + 
  theme(axis.title = element_text(family = 'Arial', size = 16, face = 'bold'),
        axis.text = element_text(family = 'Arial', size = 16, face = 'bold'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.text = element_text(size=22),
        legend.key = element_blank(),
        legend.key.size = unit(0.5, "in"),
        legend.title = element_blank()
        ) 
# Bland-Altman plot between FFR golden standard and FFR-CTA
ba.stat <- bland.altman.stats(FFR.data$FFR_score, FFR.data$FFRCTA,two=2)
ba <- data.frame(cbind(ba.stat$means,ba.stat$diffs))
colnames(ba) <- c('mean','diff')
g <- ggplot(ba, aes(mean,diff)) + 
  geom_point(fill='darkorange',colour='black',size=2.5, shape=21) +
  geom_hline(yintercept = -0.0766, colour='darkred',linetype=2) +
  geom_hline(yintercept = 0.0904, colour='darkred',linetype=2) + 
  geom_hline(yintercept = 0.00689, colour='orange',size=0.5) + 
  xlab('(FFR + CT-FFR) / 2') + 
  xlim(c(0.5,1)) + 
  scale_y_continuous(limits = c(-0.15, 0.15), breaks = c( -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) + 
  ylab('FFR - CT-FFR') + 
  theme(axis.text = element_text(family = 'Arial', size=16, face = 'bold'),
        axis.title = element_text(family='Arial', size = 16),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
library(ggExtra)
ggMarginal(g, type = 'histogram',bins=15)

