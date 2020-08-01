# MIC analysis
# Raw data was preprocessed, first detrended by subtracting loess fitted line, then scaled by centering to 0 & scaling to sd; 
# Preprocessed data were used to calculate MIC (minerva package) with mic_strenth() function,
# using pvals from 1000 permutations with mictools() function.The returned results contain missing values.
# Thus minepy (in python 3.7.0) was used to do the analysis instead.
# The upper triangle of the matrix is stored by row (condensed matrix). If m is the number of variables, 
# then for i < j < m, the statistic between (row) i and j is stored in k = m*i - i*(i+1)/2 - i - 1 + j. 
# The length of the vectors is n = m*(m-1)/2

library(scales)  # scale()
library("reshape2")  # melt
library(lemon)
# library(minerva)  # MIC ??? missing values

# non-confluent =============================================================================
# path.ncf<-"I:/MIC/DV_subconf/20170405CGB-LQBT10m/"  # non-confl
# path.ncf<-"I:/MIC/DV_nonconf/20170428CGB-BT10m/MIC/"  # non-confl
# path.ncf<-"I:/MIC/DV_nonconf/20170428CGB-BT10m/All/"  # non-confl
# path.ncf<-"I:/MIC/DV_nonconf/20170428CGB-BT10m/Noncf/"
# setwd(path.ncf)

# prep data for minepy analysis - subset, loess detrend -------------------

offset<-0             ##### offset (hours) after Shock
dt.ncf<-10                ##### imaging interval (min)

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
# write.csv(ncf, "Data_ncf-all_20170428_raw.csv")
write.csv(ncf, "Data_ncf_20170428_raw.csv")

n.image.ncf<-length(files)  # number of images for ncf
n.cell.ncf<-ncol(ncf)   # number of cells for ncf

Hours.ncf <- (1:nrow(ncf)-1)/(60/dt.ncf)+offset				# hours

ncfl<-apply(ncf,2, function(x) loess(x~Hours.ncf, span=0.75)$fitted)  # loess fit for trend
ncft<-scale(ncf-ncfl)  # subtract trend then scale
# write.csv(ncft, "Data_ncf-all_20170428_norm.csv")
write.csv(ncft, "Data_ncf_20170428_norm.csv")


# confluent #########################################################################################
# prep data for minepy analysis - subset, loess detrend -------------------

# path.cf<-"I:/MIC/DV_confl/DV20151229C3p-DT5m/"  # confluent cells
# # path.cf<-"I:/MIC/DV_confl/DV20160119C3C1-DT10m/"  # confluent cells
# setwd(path.cf)
dt.cf<-5
# dt.cf<-10
offset<-0

files = list.files(pattern="*Tracked-D.csv")
# files = list.files(pattern="*Tracked-T.csv")
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

write.csv(cft, "Data_cft_20151229_norm.csv")

# # MIC analysis by minerva (missing values occationally) ==================================
# ticenull.ncf <- mictools(ncft, nperm=1000) # compute pval, package "minerva"
# ms.ncf <- mic_strength(ncft, pval=ticenull.ncf$pval, alpha=NULL, pval.col = c(6, 4,5)) # compute MICs, package "minerva"
# write.csv(ms.ncf, "MIC-subcf.csv")

# minepy analysis by "MIC.py" in python3.7 (BioHPC), export MIC for ploting

# plots ============================================================
# MIC plot -----------------------
library(ggplot2)

# path<-"I:/MIC/"
# setwd(path)

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
      axis.title=element_text(size=8),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.title=element_text(size=8), 
      legend.text=element_text(size=6),
      panel.border = element_rect(colour = "black", fill=NA, size=0.5)
    ) +
    coord_fixed()+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    guides(fill= guide_colorbar(barheight = unit(0.3,"npc"), barwidth = unit(0.02,"npc")))
  ggsave(filename=paste("p_MIC_", names[f],".pdf",sep=""), plot=p, useDingbats=F, width=4.5, height=4)
}


# plot all cells raw & trend -------------------
n<-n.cell.ncf
pdf(paste("ALL-cell-subcf_raw-trd.pdf",sep=""), width = 2*ceiling(sqrt(n)), height = 2*ceiling(sqrt(n)))
par(mfrow=c(ceiling(sqrt(n)),ceiling(sqrt(n))), mar=c(1.5,1.5,1,1.5), oma=c(3,3,0,3))
x<-Hours.ncf
for (i in 1:n){
  plot(x,ncf[,i], type="l", col="blue",xaxt="n", yaxt="n", xlab=NA, ylab=NA)
  axis(1, at=seq(12,max(x),by=12), mgp=c(2,0.2,0),tck=-0.015, cex.axis=1)
  axis(side = 2, mgp=c(2,0.2,0),tck=-0.015,cex.axis= 1)
  lines(x,ncfl[,i], col="red")
  par(new=F)
}
mtext("Hours", side=1, outer=TRUE, font=2, line=1)
mtext("TMRM", side=2, outer=TRUE, font=2, line=1)
dev.off()

# plot all cells preprocessed  -------------------
n<-n.cell.ncf
pdf(paste("ALL-cell-subcf_preprocessed.pdf",sep=""), width = 2*ceiling(sqrt(n)), height = 2*ceiling(sqrt(n)))
par(mfrow=c(ceiling(sqrt(n)),ceiling(sqrt(n))), mar=c(1.5,1.5,1,1.5), oma=c(3,3,0,3))
x<-Hours.ncf
for (i in 1:n){
  plot(x,ncft[,i], type="l", col="blue",xaxt="n", yaxt="n", xlab=NA, ylab=NA, ylim = range(ncft))
  axis(1, at=seq(12,max(x),by=12), mgp=c(2,0.2,0),tck=-0.015, cex.axis=1)
  axis(side = 2, mgp=c(2,0.2,0),tck=-0.015,cex.axis= 1)
  par(new=F)
}
mtext("Hours", side=1, outer=TRUE, font=2, line=1)
mtext("TMRM", side=2, outer=TRUE, font=2, line=1)
dev.off()

# plot 6 example series -------------------
sed<-13265
set.seed(sed)
cell.n<-6
dat0<-cbind(Hours.ncf,ncft[,sample(ncol(ncft),cell.n)])  # randomly pick 6 preprocessed data
# dat0<-cbind(Hours.ncf,ncf[,sample(ncol(ncf),cell.n)])  # randomly pick 6 raw data
colnames(dat0)<-c("Hours", paste("Cell", 1:cell.n, sep="_"))
dat1 <- melt(data.frame(dat0), id.vars="Hours", variable.name="Cell", value.name="Fluorescence")
dat1<-data.frame(dat1)

# labels for each facets
len <- length(levels(dat1$Cell))
vars <- data.frame(expand.grid(levels(dat1$Cell)))
colnames(vars) <- c("Cell")
fct_labs<-data.frame(x=0, y=max(dat0[,-1])-min(dat0[,-1]), vars, lbl=paste("Cell",1:cell.n, sep="_"))
scaleFUN <- function(x) sprintf("%.1f", x)  # digits

plotname<-"p_Series_subcf"

p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group=Cell)) + 
  geom_line(linetype=1, size=0.1, colour= "blue" )+  
  facet_rep_grid(Cell~., scales="free")+
  coord_capped_cart(bottom='both', left='both', ylim=c(range(dat0[,-1])))+
  geom_text(aes(x,y,label=lbl, group=NULL), data=fct_labs,  parse = TRUE, hjust=0.1, vjust=2, size=1.5)+ 
  ylab("TMRM Fluorescence") +
  xlab("Hours") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title.y = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
        axis.title.x = element_text(face = "plain", color = "black", size = 6), 
        axis.text.y = element_text(face = "plain", color = "black", size = 4), 
        axis.text.x = element_text(face = "plain", color = "black", size = 4, vjust=0), 
        axis.line = element_line(size=0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        panel.border = element_blank(),
        panel.spacing.y=unit(0.2, "line"),
        legend.position = "none",
        plot.margin = margin(8,12,8,12,"pt"),
        strip.background = element_blank(),
        strip.text = element_blank()
  )+
  scale_y_continuous(breaks=pretty_breaks(n = 3), expand=c(0.2,0.2), labels=scaleFUN)+ # set y-axis labels
  scale_x_continuous(limits=c(range(Hours.ncf)),breaks = c(0,30,60,90))

assign(plotname,p)
ggsave(filename=paste(plotname,"_",sed, ".pdf",sep=""), plot=get(plotname), useDingbats=F, width=2, height=2)



# plot subconfluent cells in one image/view --------------------
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
  
  # labels for each facets
  len <- length(levels(dat1$Cell))
  vars <- data.frame(expand.grid(levels(dat1$Cell)))
  colnames(vars) <- c("Cell")
  # fct_labs<-data.frame(x=0, y=max(dat0[,-1])-min(dat0[,-1]), vars, lbl=paste("Cell",1:cell.n, sep="_"))
  fct_labs<-data.frame(x=0, y=2*max(abs(dat0[,-1])), vars, lbl=paste("Cell",1:cell.n, sep="_"))
  scaleFUN <- function(x) sprintf("%.1f", x)  # digits
  
  plotname<-"p_Series_subcf"
  
  p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group=Cell)) + 
    geom_line(linetype=1, size=0.1, colour= "blue" )+  
    facet_rep_grid(Cell~., scales="free")+
    coord_capped_cart(bottom='both', left='both', ylim=c(-max(abs(min(dat0[,-1])), abs(max(dat0[,-1]))),max(abs(min(dat0[,-1])), abs(max(dat0[,-1])))))+
    geom_text(aes(x,y,label=lbl, group=NULL), data=fct_labs,  parse = TRUE, hjust=0.1, vjust=2, size=1.5)+ 
    ylab("TMRM Fluorescence") +
    xlab("Hours") + 
    theme_classic() +       # Removes gridlines & background
    theme(axis.title.y = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
          axis.title.x = element_text(face = "plain", color = "black", size = 6), 
          axis.text.y = element_text(face = "plain", color = "black", size = 4), 
          axis.text.x = element_text(face = "plain", color = "black", size = 4, vjust=0), 
          axis.line = element_line(size=0.1),
          axis.ticks = element_line(colour = "black", size = 0.1),
          axis.ticks.length = unit(1, "pt"),
          # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
          panel.border = element_blank(),
          panel.spacing.y=unit(0.2, "line"),
          legend.position = "none",
          plot.margin = margin(8,12,8,12,"pt"),
          strip.background = element_blank(),
          strip.text = element_blank()
    )+
    scale_y_continuous(breaks=pretty_breaks(n = 3), expand=c(0.2,0.2),labels=scaleFUN)+ # set y-axis labels
    scale_x_continuous(limits=c(range(Hours.ncf)),breaks = c(0,30,60,90))
  
  assign(plotname,p)
  ggsave(filename=paste(plotname,"_view",samp, ".pdf",sep=""), plot=get(plotname), useDingbats=F, width=2, height=2)

}



# plot all confluent cells raw & trend ===============================================
n<-n.cell.cf
pdf(paste("ALL-cell-cf_raw-trd.pdf",sep=""), width = 2*ceiling(sqrt(n)), height = 2*ceiling(sqrt(n)))
par(mfrow=c(ceiling(sqrt(n)),ceiling(sqrt(n))), mar=c(1.5,1.5,1,1.5), oma=c(3,3,0,3))
x<-Hours.cf
for (i in 1:n){
  plot(x,cf[,i], type="l", col="blue",xaxt="n", yaxt="n", xlab=NA, ylab=NA)
  axis(1, at=seq(12,max(x),by=12), mgp=c(2,0.2,0),tck=-0.015, cex.axis=1)
  axis(side = 2, mgp=c(2,0.2,0),tck=-0.015,cex.axis= 1)
  lines(x,cfl[,i], col="red")
  par(new=F)
}
mtext("Hours", side=1, outer=TRUE, font=2, line=1)
mtext("TMRM", side=2, outer=TRUE, font=2, line=1)
dev.off()

# plot all cells preprocessed  -------------------
n<-n.cell.cf
pdf(paste("ALL-cell-cf_preprocessed.pdf",sep=""), width = 2*ceiling(sqrt(n)), height = 2*ceiling(sqrt(n)))
par(mfrow=c(ceiling(sqrt(n)),ceiling(sqrt(n))), mar=c(1.5,1.5,1,1.5), oma=c(3,3,0,3))
x<-Hours.cf
for (i in 1:n){
  plot(x,cft[,i], type="l", col="blue",xaxt="n", yaxt="n", xlab=NA, ylab=NA, ylim = range(cft))
  axis(1, at=seq(12,max(x),by=12), mgp=c(2,0.2,0),tck=-0.015, cex.axis=1)
  axis(side = 2, mgp=c(2,0.2,0),tck=-0.015,cex.axis= 1)
  par(new=F)
}
mtext("Hours", side=1, outer=TRUE, font=2, line=1)
mtext("TMRM", side=2, outer=TRUE, font=2, line=1)
dev.off()

# plot 6 example series -------------------

sed<-92754
set.seed(sed)
cell.n<-6
dat0<-cbind(Hours.cf,cft[,sample(ncol(cft),cell.n)])  # randomly pick 6 preprocessed data
# dat0<-cbind(Hours.cf,cf[,sample(ncol(cf),cell.n)])  # randomly pick 6 raw data
colnames(dat0)<-c("Hours", paste("Cell", 1:cell.n, sep="_"))
dat1 <- melt(data.frame(dat0), id.vars="Hours", variable.name="Cell", value.name="Fluorescence")
dat1<-data.frame(dat1)

# labels for each facets
len <- length(levels(dat1$Cell))
vars <- data.frame(expand.grid(levels(dat1$Cell)))
colnames(vars) <- c("Cell")
fct_labs<-data.frame(x=0, y=max(dat0[,-1])-min(dat0[,-1]), vars, lbl=paste("Cell",1:cell.n, sep="_"))
scaleFUN <- function(x) sprintf("%.1f", x)  # digits

plotname<-"p_Series_cf"

p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group=Cell)) + 
  geom_line(linetype=1, size=0.1, colour= "blue" )+  
  facet_rep_grid(Cell~., scales="free")+
  coord_capped_cart(bottom='both', left='both', ylim=c(range(dat0[,-1])))+
  geom_text(aes(x,y,label=lbl, group=NULL), data=fct_labs,  parse = TRUE, hjust=0.1, vjust=2, size=1.5)+ 
  ylab("TMRM Fluorescence") +
  xlab("Hours") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title.y = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
        axis.title.x = element_text(face = "plain", color = "black", size = 6), 
        axis.text.y = element_text(face = "plain", color = "black", size = 4), 
        axis.text.x = element_text(face = "plain", color = "black", size = 4, vjust=0), 
        axis.line = element_line(size=0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        panel.border = element_blank(),
        panel.spacing.y=unit(0.2, "line"),
        legend.position = "none",
        plot.margin = margin(8,12,8,12,"pt"),
        strip.background = element_blank(),
        strip.text = element_blank()
  )+
  scale_y_continuous(breaks=pretty_breaks(n = 3), expand=c(0.2,0.2), labels=scaleFUN)+ # set y-axis labels
  scale_x_continuous(limits=c(range(Hours.cf)))

assign(plotname,p)
ggsave(filename=paste(plotname,"_",sed, ".pdf",sep=""), plot=get(plotname), useDingbats=F, width=2, height=2)

