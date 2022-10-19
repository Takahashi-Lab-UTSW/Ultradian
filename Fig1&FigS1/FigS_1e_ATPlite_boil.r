#ATPlite assay: boiled vs nonboiled samples
library(dplyr)
library(tidyr)
# library(readxl)
library(ggplot2)
library(scales)
scaleFUN <- function(x) sprintf("%.1f", x)  # digits

path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig1/"
setwd(path)


#read plate config
cf <- list.files(pattern='boil1') %>% grep('.csv', ., value=T) %>% grep('PlateConfig', ., value=T)
dcf <- read.csv(cf, header=T) %>% `colnames<-`(c('Row', 1:12)) %>%
  pivot_longer(cols=-1, names_to='Col', values_to='Sample') %>%
  na.omit
dcf$Col <- as.integer(dcf$Col)


#read lum data
file <- list.files(pattern='boil1') %>% grep('ATPlite',.,value=T)
d <- read.table(file, sep=',', fill=T, skip=25, nrows=8, header=F, stringsAsFactors = F)[,1:12] %>%
  `colnames<-`(1:12) %>% cbind(Row=LETTERS[1:8],.) %>%
  pivot_longer(cols=-1, names_to='Col', values_to='Lum')
d$Col <- as.integer(d$Col)

df <- d %>% left_join(dcf, ., by=c('Row', 'Col')) 

dfv <- df %>% filter(grepl('V', Sample)) %>% select(Sample, Lum) %>% mutate(Lum=as.numeric(Lum))

df <- df %>% filter(!grepl('V', Sample)) %>%
  group_by(Sample) %>%
  summarise(Lum=mean(Lum)) %>%
  rbind(dfv) %>%
  mutate(Boil=case_when(
    grepl('N', Sample) ~ 'NonBoil',
    grepl('P|T', Sample) ~ 'Boiled',
    grepl('V', Sample) ~ 'Vehicle'
  ))

# df$Boil <- factor(levels=c('Vehicle', 'Boiled', 'NonBoil'))

#one-way ANOVA -------
boil_aov <- aov(Lum ~ Boil, data=df)
summary(boil_aov)

#plot  -----------
dp <- df %>% group_by(Boil) %>%
  summarise(Luminescence=mean(Lum), sd=sd(Lum), n=n())

#barplot
pb <- ggplot(dp, aes(x=factor(Boil, level=c('Vehicle', 'Boiled', 'NonBoil')), y=Luminescence)) +
  geom_bar(stat='identity', width=0.75)+
  geom_errorbar(aes(ymin=Luminescence-sd, ymax=Luminescence+sd), width=0.2)+
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        plot.margin = margin(3,1,1,1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=5)
        )+
  scale_y_continuous(expand=c(0.,0.))
ggsave(filename='p_ATP-Boil_b.pdf', plot=pb, useDingbats=F, width=.6, height=.75)

#dotplot
pd <- ggplot(df, aes(x=factor(Boil, level=c('Vehicle', 'Boiled', 'NonBoil')), y=Lum)) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth=30)+
  ylab('Luminescence')+
  stat_summary(fun.data='mean_sdl', fun.args = list(mult=1),
               geom="pointrange", color="red", alpha=0.5, size=.1)+
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.2),
        axis.ticks.length = unit(1, "pt"),
        plot.margin = margin(3,1,1,1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=5)
        )+
  scale_y_continuous(expand=c(0.,0.))
ggsave(filename='p_ATP-Boil_d.pdf', plot=pd, useDingbats=F, width=.6, height=.75)
