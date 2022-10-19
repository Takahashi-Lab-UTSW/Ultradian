# MIC analysis
# Raw data was preprocessed, first detrended by subtracting loess fitted line, then scaled by centering to 0 & scaling to sd; 
# Preprocessed data were used to calculate MIC (minerva package) with mic_strenth() function,
# using pvals from 1000 permutations with mictools() function.The returned results contain missing values.
# Thus minepy (in python 3.7.0) was used to do the analysis instead.
# The upper triangle of the matrix is stored by row (condensed matrix). If m is the number of variables, 
# then for i < j < m, the statistic between (row) i and j is stored in k = m*i - i*(i+1)/2 - i - 1 + j. 
# The length of the vectors is n = m*(m-1)/2

library(dplyr)
library(scales)  # scale()
library("reshape2")  # melt
library(lemon)
library(ggplot2)
# library(minerva)  # MIC ??? missing values

path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig4/MIC/"

# prep data for minepy analysis - subset, loess detrend -------------------

# non-confluent =============================================================================
# I:\MIC\DV_nonconf\20170428CGB-BT10m\All\, noncontact: -T.csv; subconfluent: -T1 & -T11.csv

offset<-0  #offset (hours) after Shock
dt.ncf<-10  #imaging interval (min)

setwd(paste0(path, 'ncf/'))

#subconfluent --------------
files = list.files(pattern="*Tracked-T*") 
ncf<-c()
for (i in files){
  df0<-read.csv(i)
  df1<-t(na.omit(t(df0)))   # remove columns that contains NA
  df2<-df1[,grepl(pattern = "T", colnames(df1))]  # select data for TMRM
  ncf<-cbind(ncf,df2)
}

colnames(ncf)<-1:ncol(ncf)
ncf<-ncf[145:nrow(ncf),]  # subset data

n.image.ncf<-length(files)  # number of images for ncf
n.cell.ncf<-ncol(ncf)   # number of cells for ncf

Hours.ncf <- (1:nrow(ncf)-1)/(60/dt.ncf)+offset				# hours

ncfl<-apply(ncf,2, function(x) loess(x~Hours.ncf, span=0.75)$fitted)  # loess fit for trend
ncft<-scale(ncf-ncfl)  # subtract trend then scale

# write.csv(ncf, paste0(path, "Data_ncf_raw.csv"))
write.csv(ncft, paste0(path, "Data_ncf_norm.csv"))

#noncontact -------------
files = list.files(pattern="*Tracked-T.csv") 
nct<-c()
for (i in files){
  df0<-read.csv(i)
  df1<-t(na.omit(t(df0)))   # remove columns that contains NA
  df2<-df1[,grepl(pattern = "T", colnames(df1))]  # select data for TMRM
  nct<-cbind(nct,df2)
}

colnames(nct)<-1:ncol(nct)
nct<-nct[145:nrow(nct),]  # subset data

n.image.nct<-length(files)  # number of images for ncf
n.cell.nct<-ncol(nct)   # number of cells for ncf

Hours.nct <- (1:nrow(nct)-1)/(60/dt.ncf)+offset				# hours

nctl<-apply(nct,2, function(x) loess(x~Hours.nct, span=0.75)$fitted)  # loess fit for trend
nctt<-scale(nct-nctl)  # subtract trend then scale

# write.csv(ncf, paste0(path, "Data_nct_raw.csv"))
write.csv(nctt, paste0(path, "Data_nct_norm.csv"))


# confluent ==================================================
# I:\MIC\DV_confl\DV20151229C3p-DT5m\...Tracked-D.csv

setwd(paste0(path, 'cf/'))

dt.cf<-5
offset<-0

files = list.files(pattern="*Tracked-D.csv")

cf<-c()
for (i in files){
  df0<-read.csv(i)
  df1<-t(na.omit(t(df0)))   # remove columns that contains NA
  df2<-df1[,grepl(pattern = "T", colnames(df1))]  # select data for TMRM
  cf<-cbind(cf,df2)
}

cf<-cf[300:600,]  # subset data

colnames(cf)<-1:ncol(cf)
n.image.cf<-length(files)  # number of images for cf
n.cell.cf<-ncol(cf)   # number of cells for cf

Hours.cf <- (1:nrow(cf)-1)/(60/dt.cf)+offset				# hours

cfl<-apply(cf,2, function(x) loess(x~Hours.cf, span=0.75)$fitted)  # loess fit for trend
cft<-scale(cf-cfl)  # subtract trend then scale

write.csv(cft, paste0(path, "Data_cf_norm.csv"))

setwd(path)

# # MIC analysis by minerva (missing values occationally) ==================================
# ticenull.ncf <- mictools(ncft, nperm=1000) # compute pval, package "minerva"
# ms.ncf <- mic_strength(ncft, pval=ticenull.ncf$pval, alpha=NULL, pval.col = c(6, 4,5)) # compute MICs, package "minerva"
# write.csv(ms.ncf, "MIC-subcf.csv")

# # minepy analysis by "MIC.py" in python3.7 (BioHPC), export MIC for ploting

# plots MIC ============================================================

files<-list.files(pattern="Mic_Data_")
dfs<-lapply(files, function(x) read.csv(x,row.names = 1))
names<-unlist(lapply(files,function(x) regmatches(x,regexec("Mic_Data_(.*?).csv",x))[[1]][2]))  # substring between two patterns
names(dfs)<-names

for (f in 1:length(dfs)){
  df<-dfs[[f]]
  n<-nrow(df)  # n = m*(m-1)/2
  m<-(1+sqrt(1+8*n))/2  # number of samples, by solving n = m*(m-1)/2
  
  #get pairing for i < j < m, the statistic between (row) i and j is stored in k = m*i - i*(i+1)/2 - i - 1 + j
  ijs<-c()
  for (j in 1:(m-1)){
    for (i in 1:(j)){
      ij<-c(i,j)
      ijs<-rbind(ijs,ij)
    }
  }
  
  df<-cbind(df,ijs)  # assign pairs to MICs
  colnames(df)<-c("MIC","I","J")
  
  # plot MIC heatmap ----------------------
  p <- ggplot(data = df, aes(x=I, y=J, fill=MIC)) + 
    geom_tile()+
    scale_fill_gradient2(low = "black", high = "red", mid = "white", midpoint = 0.5,
                         limit = c(0,1), space = "Lab", name="MIC") +
    theme_classic() +
    ylab("Cell index") +
    xlab("Cell index") +
    theme(
      axis.title.x=element_text(size=6, margin=margin(1,0,0,0)),
      axis.title.y=element_text(size=6, margin=margin(0,1,0,0)),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "pt"),
      legend.position = c(.82, .34),
      legend.title=element_text(size=6), 
      legend.text=element_text(size=5),
      legend.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=0.1)
    ) +
    coord_fixed()+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    guides(fill= guide_colorbar(barheight = unit(0.3,"npc"), barwidth = unit(0.02,"npc")))
  ggsave(filename=paste("p_MIC_", names[f],".pdf",sep=""), plot=p, useDingbats=F, width=1.1, height=1.1)
}


# plot 6 random time series for non-contact cells ===========================

sed<-13265
# sed<-5265

set.seed(sed)
plotname<-"p_Series_nct"

cell.n<-6
dat0<-cbind(Hours.nct,nctt[,sample(ncol(nctt),cell.n)])  # randomly pick 6 preprocessed data
colnames(dat0)<-c("Hours", paste("Cell", 1:cell.n, sep="_"))
dat1 <- melt(data.frame(dat0), id.vars="Hours", variable.name="Cell", value.name="Fluorescence")
dat1<-data.frame(dat1)

p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group=Cell)) +
  geom_line(linetype=1, size=0.2, colour= "blue" )+
  # facet_rep_grid(Cell~., scales="free")+
  facet_wrap(~Cell, scales="free_y", nrow=cell.n,
             strip.position='left', labeller=as_labeller(1:cell.n)
  )+
  coord_capped_cart(bottom='both', left='both', ylim=c(-max(abs(min(dat0[,-1])), abs(max(dat0[,-1]))),max(abs(min(dat0[,-1])), abs(max(dat0[,-1])))))+
  ylab('R.I. Single Cells') +
  xlab("Hours") +
  theme_classic() +       # Removes gridlines & background
  theme(axis.title.y = element_text(face = "plain", color = "black", size = 6, margin=margin(0,0,0,0)), # theme text size in pt
        axis.title.x = element_text(face = "plain", color = "black", size = 6),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "plain", color = "black", size = 5, vjust=0),
        axis.line = element_line(size=0.1),
        axis.ticks.x = element_line(colour = "black", size = 0.1),
        axis.ticks.length.x = unit(1, "pt"),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.spacing.y=unit(0.1, "line"),
        legend.position = "none",
        plot.margin = margin(1,1,1,1,"pt"),
        strip.background = element_blank(),
        strip.text.y.left = element_text(face="plain", color="black", size=5, angle=0, margin=margin(0,0,0,0)),
        # strip.placement = 'outside'
  )+
  scale_y_continuous(breaks=pretty_breaks(n = 1), expand=c(0.1,0.1))+ # set y-axis labels,labels=scaleFUN
  scale_x_continuous(limits=c(range(Hours.nct)),breaks = c(0,30,60,90))
assign(plotname,p)
ggsave(filename=paste(plotname,"_",sed, ".pdf",sep=""), plot=get(plotname), useDingbats=F, width=1.2, height=1.2)


#plot non-contact cells in one image/view ----------------
plotname<-"p_Series_nct"

fs<- list.files(pattern="*Tracked-T.csv")
for (f in fs){
  samp <- regmatches(f,regexec("BT10m01_(.*?)_Tracked",f))[[1]][2]  #extract sample IDs
  df0<-read.csv(f)
  df1<-t(na.omit(t(df0)))   # remove columns that contains NA
  ncf<-df1[,grepl(pattern = "T", colnames(df1))]  # select data for TMRM
  
  if (ncol(ncf)<4 || is.null(ncol(ncf))) next
  if (samp!=71) next #plot view71 only
  
  colnames(ncf)<-1:ncol(ncf)
  ncf<-ncf[145:nrow(ncf),]  # subset data
  Hours.ncf <- (1:nrow(ncf)-1)/(60/dt.ncf)+offset				# hours
  ncfl<-apply(ncf,2, function(x) loess(x~Hours.ncf, span=0.75)$fitted)  # loess fit for trend
  ncft<-scale(ncf-ncfl)  # subtract trend then scale
  cell.n<-ncol(ncft)
  
  dat0<-cbind(Hours.ncf,ncft)
  colnames(dat0)<-c("Hours", paste("Cell", 1:cell.n, sep="_"))
  dat1 <- melt(data.frame(dat0), id.vars="Hours", variable.name="Cell", value.name="Fluorescence")
  dat1<-data.frame(dat1)
  
  p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group=Cell)) +
    geom_line(linetype=1, size=0.2, colour= "blue" )+
    # facet_rep_grid(Cell~., scales="free")+
    facet_wrap(~Cell, scales="free_y", nrow=cell.n,
               strip.position='left', labeller=as_labeller(1:cell.n)
    )+
    coord_capped_cart(bottom='both', left='both', ylim=c(-max(abs(min(dat0[,-1])), abs(max(dat0[,-1]))),max(abs(min(dat0[,-1])), abs(max(dat0[,-1])))))+
    ylab('R.I. Single Cells') +
    xlab("Hours") +
    theme_classic() +       # Removes gridlines & background
    theme(axis.title.y = element_text(face = "plain", color = "black", size = 6, margin=margin(0,0,0,0)), # theme text size in pt
          axis.title.x = element_text(face = "plain", color = "black", size = 6),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "plain", color = "black", size = 5, vjust=0),
          axis.line = element_line(size=0.1),
          axis.ticks.x = element_line(colour = "black", size = 0.1),
          axis.ticks.length.x = unit(1, "pt"),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing.y=unit(0.1, "line"),
          legend.position = "none",
          plot.margin = margin(1,1,1,1,"pt"),
          strip.background = element_blank(),
          strip.text.y.left = element_text(face="plain", color="black", size=5, angle=0, margin=margin(0,0,0,0)),
          # strip.placement = 'outside'
    )+
    scale_y_continuous(breaks=pretty_breaks(n = 1), expand=c(0.1,0.1))+ # set y-axis labels,labels=scaleFUN
    scale_x_continuous(limits=c(range(Hours.ncf)),breaks = c(0,30,60,90))
  assign(plotname,p)
  ggsave(filename=paste0(path, plotname,"_view",samp, ".pdf"), plot=get(plotname), useDingbats=F, width=1.2, height=1.2)
}


# plot subconfluent cells in one image/view --------------------
setwd(paste0(path, 'ncf/'))
plotname<-"p_Series_ncf"

fs<- list.files(pattern="*Tracked-T11.csv")
for (f in fs){
  samp <- regmatches(f,regexec("BT10m01_(.*?)_Tracked",f))[[1]][2]  #extract sample IDs
  df0<-read.csv(f)
  df1<-t(na.omit(t(df0)))   # remove columns that contains NA
  ncf<-df1[,grepl(pattern = "T", colnames(df1))]  # select data for TMRM
  colnames(ncf)<-1:ncol(ncf)
  ncf<-ncf[145:nrow(ncf),]  # subset data
  Hours.ncf <- (1:nrow(ncf)-1)/(60/dt.ncf)+offset				# hours
  ncfl<-apply(ncf,2, function(x) loess(x~Hours.ncf, span=0.75)$fitted)  # loess fit for trend
  ncft<-scale(ncf-ncfl)  # subtract trend then scale
  cell.n<-ncol(ncft)

  #rearrange cells according to appearance of the 1st peak
  if (samp==67){
    ncft<-ncft[,c(7,4,5,3,2,1,6,8)]
  }else if (samp==63){
    ncft<-ncft[,c(4,2,5,3,7,6,1)]
  }else if (samp==64){
    ncft<-ncft[,c(5,4,3,1,2)]
  }else if (samp==27){
    ncft<-ncft[,c(6,2,1,4,3,5)]
  }else if (samp==28){
    ncft<-ncft[,c(4,2,6,3,1,5)]
  }

  dat0<-cbind(Hours.ncf,ncft)
  colnames(dat0)<-c("Hours", paste("Cell", 1:cell.n, sep="_"))
  dat1 <- melt(data.frame(dat0), id.vars="Hours", variable.name="Cell", value.name="Fluorescence")
  dat1<-data.frame(dat1)

  p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group=Cell)) +
    geom_line(linetype=1, size=0.2, colour= "blue" )+
    # facet_rep_grid(Cell~., scales="free")+
    facet_wrap(~Cell, scales="free_y", nrow=cell.n,
               strip.position='left', labeller=as_labeller(1:cell.n)
               )+
    coord_capped_cart(bottom='both', left='both', ylim=c(-max(abs(min(dat0[,-1])), abs(max(dat0[,-1]))),max(abs(min(dat0[,-1])), abs(max(dat0[,-1])))))+
    ylab('R.I. Single Cells') +
    xlab("Hours") +
    theme_classic() +       # Removes gridlines & background
    theme(axis.title.y = element_text(face = "plain", color = "black", size = 6, margin=margin(0,0,0,0)), # theme text size in pt
          axis.title.x = element_text(face = "plain", color = "black", size = 6),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "plain", color = "black", size = 5, vjust=0),
          axis.line = element_line(size=0.1),
          axis.ticks.x = element_line(colour = "black", size = 0.1),
          axis.ticks.length.x = unit(1, "pt"),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing.y=unit(0.1, "line"),
          legend.position = "none",
          plot.margin = margin(1,1,1,1,"pt"),
          strip.background = element_blank(),
          strip.text.y.left = element_text(face="plain", color="black", size=5, angle=0, margin=margin(0,0,0,0)),
          # strip.placement = 'outside'
    )+
    scale_y_continuous(breaks=pretty_breaks(n = 1), expand=c(0.1,0.1))+ # set y-axis labels,labels=scaleFUN
    scale_x_continuous(limits=c(range(Hours.ncf)),breaks = c(0,30,60,90))
  assign(plotname,p)
  ggsave(filename=paste0(path, plotname,"_view",samp, ".pdf"), plot=get(plotname), useDingbats=F, width=1.2, height=1.2)
}


# plot 6 random series confluent cells  ===============================================

sed<-871
set.seed(sed)
cell.n<-6
dat0<-cbind(Hours.cf,cft[,sample(ncol(cft),cell.n)])  # randomly pick 6 preprocessed data
colnames(dat0)<-c("Hours", paste("Cell", 1:cell.n, sep="_"))
dat1 <- melt(data.frame(dat0), id.vars="Hours", variable.name="Cell", value.name="Fluorescence")
dat1<-data.frame(dat1)

plotname<-"p_Series_cf"

p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group=Cell)) +
  geom_line(linetype=1, size=0.2, colour= "blue" )+
  # facet_rep_grid(Cell~., scales="free")+
  facet_wrap(~Cell, scales="free_y", nrow=cell.n,
             strip.position='left', labeller=as_labeller(1:cell.n)
  )+
  coord_capped_cart(bottom='both', left='both', ylim=c(-max(abs(min(dat0[,-1])), abs(max(dat0[,-1]))),max(abs(min(dat0[,-1])), abs(max(dat0[,-1])))))+
  ylab('R.I. Single Cells') +
  xlab("Hours") +
  theme_classic() +       # Removes gridlines & background
  theme(axis.title.y = element_text(face = "plain", color = "black", size = 6, margin=margin(0,0,0,0)), # theme text size in pt
        axis.title.x = element_text(face = "plain", color = "black", size = 6),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "plain", color = "black", size = 5, vjust=0),
        axis.line = element_line(size=0.1),
        axis.ticks.x = element_line(colour = "black", size = 0.1),
        axis.ticks.length.x = unit(1, "pt"),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.spacing.y=unit(0.1, "line"),
        legend.position = "none",
        plot.margin = margin(1,1,1,1,"pt"),
        strip.background = element_blank(),
        strip.text.y.left = element_text(face="plain", color="black", size=5, angle=0, margin=margin(0,0,0,0)),
        # strip.placement = 'outside'
  )+
  scale_y_continuous(breaks=pretty_breaks(n = 1), expand=c(0.1,0.1))+ # set y-axis labels,labels=scaleFUN
  scale_x_continuous(limits=c(range(Hours.cf)))
assign(plotname,p)
ggsave(filename=paste(plotname,"_",sed, ".pdf",sep=""), plot=get(plotname), useDingbats=F, width=1.2, height=1.2)

#plot confluent cells in one view/image ---------------

setwd(paste0(path, 'cf/'))
dt.cf<-5
offset<-0
plotname<-"p_Series_cf"

fs<- list.files(pattern="*Tracked-D.csv")
for (f in fs){
  samp <- regmatches(f,regexec("20151229C3p-DT01_(.*?)_Tracked",f))[[1]][2]  #extract sample IDs
  df0<-read.csv(f)
  df1<-t(na.omit(t(df0)))   # remove columns that contains NA
  cf<-df1[,grepl(pattern = "T", colnames(df1))]  # select data for TMRM
  
  if (ncol(cf)<4 || is.null(ncol(cf))) next
  # if (samp!=71) next #plot view71 only
  
  colnames(cf)<-1:ncol(cf)
  cf<-cf[300:600,]  # subset data
  Hours.cf <- (1:nrow(cf)-1)/(60/dt.cf)+offset				# hours
  cfl<-apply(cf,2, function(x) loess(x~Hours.cf, span=0.75)$fitted)  # loess fit for trend
  cft<-scale(cf-cfl)  # subtract trend then scale
  cell.n<-ncol(cft)
  
  dat0<-cbind(Hours.cf,cft)
  colnames(dat0)<-c("Hours", paste("Cell", 1:cell.n, sep="_"))
  dat1 <- melt(data.frame(dat0), id.vars="Hours", variable.name="Cell", value.name="Fluorescence")
  dat1<-data.frame(dat1)
  
  p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group=Cell)) +
    geom_line(linetype=1, size=0.2, colour= "blue" )+
    # facet_rep_grid(Cell~., scales="free")+
    facet_wrap(~Cell, scales="free_y", nrow=cell.n,
               strip.position='left', labeller=as_labeller(1:cell.n)
    )+
    coord_capped_cart(bottom='both', left='both', ylim=c(-max(abs(min(dat0[,-1])), abs(max(dat0[,-1]))),max(abs(min(dat0[,-1])), abs(max(dat0[,-1])))))+
    ylab('R.I. Single Cells') +
    xlab("Hours") +
    theme_classic() +       # Removes gridlines & background
    theme(axis.title.y = element_text(face = "plain", color = "black", size = 6, margin=margin(0,0,0,0)), # theme text size in pt
          axis.title.x = element_text(face = "plain", color = "black", size = 6),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "plain", color = "black", size = 5, vjust=0),
          axis.line = element_line(size=0.1),
          axis.ticks.x = element_line(colour = "black", size = 0.1),
          axis.ticks.length.x = unit(1, "pt"),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          panel.spacing.y=unit(0.1, "line"),
          legend.position = "none",
          plot.margin = margin(1,1,1,1,"pt"),
          strip.background = element_blank(),
          strip.text.y.left = element_text(face="plain", color="black", size=5, angle=0, margin=margin(0,0,0,0)),
          # strip.placement = 'outside'
    )+
    scale_y_continuous(breaks=pretty_breaks(n = 1), expand=c(0.1,0.1))+ # set y-axis labels,labels=scaleFUN
    scale_x_continuous(limits=c(range(Hours.cf)))
  assign(plotname,p)
  ggsave(filename=paste0(path, plotname,"_view",samp, ".pdf"), plot=get(plotname), useDingbats=F, width=1.2, height=1.2)
}










