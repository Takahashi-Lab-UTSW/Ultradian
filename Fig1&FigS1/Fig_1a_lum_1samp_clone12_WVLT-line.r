path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig1/"

setwd(paste(path))

# folders = list.dirs('.', full.names=FALSE, recursive=FALSE)
# read csv data exported from Lumicycle Analysis
samp.clone<-"WT/Per2::Luc"  ############################################### provide sampe name
file<-"012811H p3-2 clone12_Raw.csv" ################################## data file to read
plotname<-"p_clone12"   ############################################# plot name
dats<-list()
nrows<-c()
# files = list.files(pattern="*.csv")
n<-length(samp.clone)
clrs<-c("blue", "green", "red", "orange","violet","cyan") # 6 colours
c<-clrs[1:n]

# read raw data exported from Lumicycle Analysis
for (i in 1:n){
  # samp.clone[i]<-substr(files[i], 1, nchar(files[i])-4)	##### sample name;
  df<-read.csv(file, skip=3, col.names = c("Date","Time",	"Days",	"CountsPerSec",	"Baseline")) 
  lum<-df[,"CountsPerSec"]
  day<-df$Days-df$Days[1]   # normalized time in days
  
  # detrend lum by subtracting the loess line, then shift back
  dat<-cbind(day,lum)
  loess_fit <- loess(lum ~ day, data.frame(dat))
  lum<-lum-predict(loess_fit)+mean(predict(loess_fit))
  
  dats[[i]]<-cbind(day,lum)
  nrows<-c(nrows,nrow(df))
}

# put data in one dataframe and subset
nr<-min(nrows)   # number of rows to plot
dat<-data.frame()
for (i in 1:n){
  if (i==1){
    dat<-dats[[i]][1:nr,]
  }else{
    dat<-cbind(dat,dats[[i]][1:nr,2])
  }
}
colnames(dat)<-c("Days",samp.clone)

# subset data
dat<-dat[dat[,1]<=8,]

# normalize data to mean of the 1st day
if (n>1){
  dat0<-sweep(dat[,-1], 2, colMeans(dat[dat[,1]<1,-1]), FUN="/") #normalize to mean of the 1st day
  dat0<-cbind(dat[,1],dat0)
  colnames(dat0)<-c("Days",samp.clone)
}else{
  lum.norm<-dat[,2]/mean(dat[,2])
  dat0<-cbind(dat[,1],lum.norm)
  colnames(dat0)<-c("Days",samp.clone)
}

# make long form data
library("reshape2")
dat0<-data.frame(dat0)   # wide form data
dat1 <- melt(dat0, id.vars="Days", variable.name="Sample", value.name="Luminescence")

#plot
library(ggplot2)
library(scales)
# library(cowplot)
# library(magick)
# library(grid)

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

pl <- ggplot(data=dat1, aes(x=Days, y=Luminescence, group = Sample)) +  
  geom_line(linetype=1, color="blue", size=0.2)+
  ylab("Luminescence") +
  xlab("Days") + 
  ggtitle(samp.clone)+
  theme_classic() +       # Removes gridlines & background
  theme(plot.title = element_text(hjust = 0.5, size=7, margin=margin(0,0,2,0)),
        axis.title = element_text(face = "plain", color = "black", size = 6), 
        axis.title.x=element_blank(),
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        axis.text.x = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(1, 1, 1, 1, "pt"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=5),
        legend.key.height = unit(0.01,"line"),
        legend.key.width = unit(0.2, "line"),
        legend.spacing.x = unit(0.2, "line")
        # legend.spacing.y = unit(0.005, "line")
        )+
  # scale_color_manual(values=c)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels
  
# output pdf
ggsave(filename=paste0(plotname,"-line.pdf"), plot=pl, useDingbats=F, width=1.5, height=.6) 


# Wavelet analysis ====================
Hours<-(1:nrow(dat0)-1)/6   ##############
x<-dat0[,2]
my.data = data.frame(x = x)
row.names(my.data)<-Hours
my.w = analyze.wavelet(my.data, "x",
                       loess.span = 0,
                       dt = 1/6,        # Time unit = Hour
                       dj = 1/250,      # y-axis resolution
                       lowerPeriod = 1,
                       upperPeriod = 48,
                       make.pval = T, 
                       n.sim = 100)

# plot wavelet as pdf
pdf(paste0(plotname,"_wt.pdf"), width = 3.2, height = 2)
par(mar=c(2.5,2.5,0.2,1), mgp=c(2,.8,0), cex.axis=.8)
pwt<- wt.image(my.w, n.levels = 250, # colors
               plot.coi = F, 
               plot.contour = F,
               plot.ridge = F,
               siglvl = 0.01,
               timelab = "Time (day)",
               periodlab = "Period (hour)",
               timetcl = -0.2,
               periodtcl = -0.2,
               spec.time.axis = list(at = (0:4)*24*6, labels = 0:4, las = 1, hadj = NA, padj = 0),
               spec.period.axis = list(at = c(2,8,24), labels = T, las = 1, hadj = 1, padj = NA),
               # lwd = 2, 
               lwd.axis = .1, 
               legend.params = list(width=0.5, shrink=0.7, mar=2, n.ticks=2, label.digits=0, lab.line=0.1))
dev.off()

# # plot wvlt & line (failed )
# library(ggplotify)
# pcow<-plot_grid(pl, as.ggplot(pwt), nrow = 2, rel_heights = c(1,1.2), align = 'v')
# save_plot(filename=paste0(plotname, 'WVLT-line.pdf'), pcow, base_width = 1.5, base_height = 1.2)

