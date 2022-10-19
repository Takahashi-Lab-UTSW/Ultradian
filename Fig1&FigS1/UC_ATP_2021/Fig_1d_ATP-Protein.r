#ATP-protein
library(dplyr)
library(tidyr)
# library(readxl)
library(ggplot2)
library(scales)
scaleFUN <- function(x) sprintf("%.1f", x)  # digits

#remove outliers by boxplot in colname
remove.outlier <- function(df, cols=names(dplyr::select(df, where(is.numeric)))){
  df <- as.data.frame(df)
  idx <- c()
  for (c in cols){
    outliers <- boxplot.stats(df[,c])$out  #outliers in column c; default Quntile+-1.5*IQR
    idx <- c(idx, which(df[,c] %in% outliers))  #rownumber for outliers in column c
  }
  if(length(idx)==0) df else df[-unique(idx),]
}

path <- getwd()
setwd(path)

expt <- c('20210819', '20211117')

pts <- read.csv('PTs.csv')  #read in peak-trough 

res <- {} #statistics results
res0 <- {}
d0n <- {}  #normalized outlier-removed MeanATProt data

for (e in expt){
  #peak/trough
  pt <- pts %>% select(Sample, contains(e)) %>% 
    `colnames<-`(c('Sample', 'PT')) %>% 
    filter(PT!='NA') %>%
    mutate(Sample=paste0('UC', e, '_', Sample))
  
  #protein data (ug/ml; 10ul)
  dp <- read.csv(paste0(path, '/ProteinConcentration_', e, '.csv'), row.names = 1) %>% 
    select(Sample, Concentration) %>%
    group_by(Sample) %>%
    summarise(MeanProt=mean(Concentration, na.rm=T),
              SD=sd(Concentration, na.rm=T))
  if (e!='20211117') dp <- dp %>% mutate(Sample=paste0('UC', e, '_', Sample))
  
  #ATP data (uM; 45ul)
  da <- read.csv(paste0(path, '/ATP_Concentration_', e, '.csv'), row.names = 1) %>% 
    select(Sample, ATP) %>% 
    mutate(Sample=paste0('UC', e, '_', Sample)) %>%
    left_join(pt, by='Sample')
  
  #ATProt: ATP normalized to protein (umol/mg)
  dn <- da %>% left_join(dp, by="Sample") %>% mutate(ATProt=ATP*45/(MeanProt*10)) %>% select(-c('MeanProt', 'SD'))

  #average ATProt & ATP
  d <- dn %>% 
    group_by(Sample) %>%
    summarise(MeanATProt=mean(ATProt), SdATProt=sd(ATProt),
              MeanATP =mean(ATP), SdATP=sd(ATP)) %>%
    left_join(pt, by='Sample')

  #t-test pval between PT -----------
  pATP <- t.test(MeanATP~PT, d)$p.value  
  pATProt <- t.test(MeanATProt~PT, d)$p.value  
  
  #statistics results: mean, sd, pval -----------
  res[[e]] <- c(mean_P=mean(d%>%filter(PT=='P')%>%pull(MeanATProt)), 
                sd_P=sd(d%>%filter(PT=='P')%>%pull(MeanATProt)), 
                mean_T=mean(d%>%filter(PT=='T')%>%pull(MeanATProt)), 
                sd_T=sd(d%>%filter(PT=='T')%>%pull(MeanATProt)), 
                pVal=pATProt) 
  
  #data for plot
  d.p <- d %>% mutate(PT=ifelse(PT=='P', 'Peak', 'Trough'))
  
  #Violin plot ATProt between PT
  plt <- ggplot(d.p, aes(x=PT, y=MeanATProt, fill=PT))+
    geom_violin(trim=F, size=0.3)+
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
    stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="red", alpha=1, size=0.1)+
    theme_classic() +       # Removes gridlines & background
    theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
          axis.text.x = element_text(face = "plain", color = "black", size = 6, angle = 45, hjust = 1),
          axis.text.y = element_text(face = "plain", color = "black", size = 5),
          axis.line = element_line(colour = "black", size = 0.1),
          axis.ticks = element_line(colour = "black", size = 0.1),
          axis.ticks.length = unit(1, "pt"),
          plot.margin = margin(2, 1, 1, 1, "pt"),
          # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
          legend.position = "none",
          axis.title.x=element_blank())+
    ylab('ATP (mmol/g)')+
    annotate('text', x=2, y=max(d$MeanATProt)*1.2, label=paste0('p=', round(pATProt, 4)), size=2)
  ggsave(paste0('pVio_ATProt_', e, '.pdf'), plot=plt, width=1., height=1)
  
  #Boxplot ATProt between PT
  plt <- ggplot(d.p, aes(x=PT, y=MeanATProt))+
    geom_boxplot(width=0.7, size=0.3)+
    theme_classic() +       # Removes gridlines & background
    theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
          axis.text.x = element_text(face = "plain", color = "black", size = 6, angle = 45, hjust = 1), 
          axis.text.y = element_text(face = "plain", color = "black", size = 5),
          axis.line = element_line(colour = "black", size = 0.1),
          axis.ticks = element_line(colour = "black", size = 0.1),
          axis.ticks.length = unit(1, "pt"),
          plot.margin = margin(1,1,1,1, "pt"),
          # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
          legend.position = "none",
          axis.title.x=element_blank())+
    ylab('ATP (mmol/g)')+
    annotate('text', x=2, y=max(d$MeanATProt), label=paste0('p=', round(pATProt, 4)), size=2)
  ggsave(paste0('pBox_ATProt_', e, '.pdf'), plot=plt, width=1., height=1)
  

  #remove outliers ============
  d0 <- d %>% group_by(PT) %>% remove.outlier(c('MeanATProt'))
  
  #t-test pval between PT -----------
  pATP0 <- t.test(MeanATP~PT, d0)$p.value  
  pATProt0 <- t.test(MeanATProt~PT, d0)$p.value  
  
  #statistics results: mean, sd, pval
  res0[[e]] <- c(mean_P=mean(d0%>%filter(PT=='P')%>%pull(MeanATProt)), 
                 sd_P=sd(d0%>%filter(PT=='P')%>%pull(MeanATProt)), 
                 mean_T=mean(d0%>%filter(PT=='T')%>%pull(MeanATProt)), 
                 sd_T=sd(d0%>%filter(PT=='T')%>%pull(MeanATProt)), 
                 pVal=pATProt0) 

  #Normalize outlier-removed MeanATProt =========
  d0n[[e]] <- d0 %>% transmute(nATProt=MeanATProt/mean(MeanATProt), PT=PT)
  
  #data for plot
  d0.p <- d0 %>% mutate(PT=ifelse(PT=='P', 'Peak', 'Trough'))
  
  #Violin plot ATProt between PT
  plt <- ggplot(d0.p, aes(x=PT, y=MeanATProt, fill=PT))+
    geom_violin(trim=F, size=0.3)+
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
    stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="red", alpha=1, size=0.1)+
    theme_classic() +       # Removes gridlines & background
    theme(axis.title = element_text(face = "plain", color = "black", size = 6, hjust=1), # theme text size in pt
          axis.text.x = element_text(face = "plain", color = "black", size = 6, angle = 45, hjust = 1),
          axis.text.y = element_text(face = "plain", color = "black", size = 5),
          axis.line = element_line(colour = "black", size = 0.1),
          axis.ticks = element_line(colour = "black", size = 0.1),
          axis.ticks.length = unit(1, "pt"),
          plot.margin = margin(2, 1, 1, 1, "pt"),
          # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
          legend.position = "none",
          axis.title.x=element_blank())+
    ylab('ATP (mmol/g)')+
    # annotate('text', x=2, y=max(d0$MeanATProt)*1.2, label=paste0('p=', round(pATProt0, 3)), size=2)
    annotate('text', x=2, y=max(d0$MeanATProt), label=ifelse(pATProt0<0.05, '*', ''), size=3)
  ggsave(paste0('pVio_ATProt_', e, '_ro.pdf'), plot=plt, width=.8, height=.75)

  #Boxplot ATProt between PT
  plt <- ggplot(d0.p, aes(x=PT, y=MeanATProt))+
    geom_boxplot(width=0.7, size=0.3)+
    theme_classic() +       # Removes gridlines & background
    theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
          axis.text.y = element_text(face = "plain", color = "black", size = 5), 
          axis.text.x = element_text(face = "plain", color = "black", size = 6, angle = 45, hjust = 1), 
          axis.line = element_line(colour = "black", size = 0.1),
          axis.ticks = element_line(colour = "black", size = 0.1),
          axis.ticks.length = unit(1, "pt"),
          plot.margin = margin(1,1,1,1, "pt"),
          # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
          legend.position = "none",
          axis.title.x=element_blank())+
    ylab('ATP (mmol/g)')+
    annotate('text', x=2, y=max(d0$MeanATProt), label=paste0('p=', round(pATProt0, 4)), size=2)
  ggsave(paste0('pBox_ATProt_', e, '_ro.pdf'), plot=plt, width=1., height=1)
}


#combine normalized MeanATProt data from all experiments
dat <- do.call(rbind, d0n) 

#t-test pval between PT for combined data -----------
pATProt0n <- t.test(nATProt~PT, dat)$p.value  
res0n <- c(mean(dat$nATProt), sd(dat$nATProt), pATProt0n)

#statistics results: mean, sd, pval
res0n <- c(mean_P=mean(dat%>%filter(PT=='P')%>%pull(nATProt)), 
           sd_P=sd(dat%>%filter(PT=='P')%>%pull(nATProt)), 
           mean_T=mean(dat%>%filter(PT=='T')%>%pull(nATProt)), 
           sd_T=sd(dat%>%filter(PT=='T')%>%pull(nATProt)), 
           pVal=pATProt0n) 

#statistical results
do.call(rbind, res) %>% rbind(do.call(rbind, res0)) %>% rbind(res0n) %>% 
  # `colnames<-`(c('Mean', 'SD', 'pVal')) %>% 
  round(digits=4) %>% as.data.frame() %>%
  tibble::rownames_to_column(var='Expt') %>%
  mutate(Data=c('All', 'All', 'Outliar removed', 'Outliar removed', 'Combined')) %>%
  write.csv('Statistic_Results_ATProt.csv')

#dat for plot
dat.p <- dat %>% mutate(PT=ifelse(PT=='P', 'Peak', 'Trough'))

#Boxplot combined data: ATProt between PT
pbox <- ggplot(dat.p, aes(x=PT, y=nATProt))+
  geom_boxplot(width=0.7, size=0.3)+
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
        axis.text.y = element_text(face = "plain", color = "black", size = 5), 
        axis.text.x = element_text(face = "plain", color = "black", size = 6, angle = 45, hjust = 1), 
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        plot.margin = margin(1,1,1,1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        legend.position = "none",
        axis.title.x=element_blank())+
  ylab('Normalized ATP')+
  annotate('text', x=2, y=max(dat$nATProt), label=paste0('p=', round(pATProt0n, 4)), size=2)
ggsave(paste0('pBox_ATProt_combined_ron.pdf'), plot=pbox, width=1., height=1)

#violin plot combined data: ATProt between PT
pvio <- ggplot(dat.p, aes(x=PT, y=nATProt, fill=PT))+
  geom_violin(trim=F, size=0.3)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="red", alpha=1, size=0.1)+
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6, hjust=1), # theme text size in pt
        axis.text.x = element_text(face = "plain", color = "black", size = 6, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "plain", color = "black", size = 5),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        plot.margin = margin(2, 1, 1, 1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        legend.position = "none",
        axis.title.x=element_blank())+
  ylab('Normalized ATP')+
  annotate('text', x=2, y=max(dat$nATProt)*1.1, label='**', size=3)
  # annotate('text', x=2, y=max(dat$nATProt)*1.2, label=paste0('p=', round(pATProt0n, 4)), size=2)
ggsave(paste0('pVio_ATProt_combined_ron.pdf'), plot=pvio, width=.8, height=.75)

