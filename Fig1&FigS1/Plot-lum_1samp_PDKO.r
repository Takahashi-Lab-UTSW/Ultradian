path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig1/"

setwd(paste(path))

# folders = list.dirs('.', full.names=FALSE, recursive=FALSE)
# read csv data exported from Lumicycle Analysis
samp.pdko<-"PDKO/Per2-Luc"  ############################################### provide sampe name
file<-"2011-9-21_PDKO$fPL-10_Raw.csv" ################################## data file to read
plotname<-"p_PDKO"   ############################################# plot name
dats<-list()
nrows<-c()
# files = list.files(pattern="*.csv")
n<-length(samp.pdko)
clrs<-c("blue", "green", "red", "orange","violet","cyan") # 6 colours
c<-clrs[1:n]

# read raw data exported from Lumicycle Analysis
for (i in 1:n){
  # samp.pdko[i]<-substr(files[i], 1, nchar(files[i])-4)	##### sample name;
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
colnames(dat)<-c("Days",samp.pdko)

# subset data
dat<-dat[dat[,1]<=8,]

# normalize data to mean of the 1st day
if (n>1){
  dat0<-sweep(dat[,-1], 2, colMeans(dat[dat[,1]<1,-1]), FUN="/") #normalize to mean of the 1st day
  dat0<-cbind(dat[,1],dat0)
  colnames(dat0)<-c("Days",samp.pdko)
}else{
  lum.norm<-dat[,2]/mean(dat[,2])
  dat0<-cbind(dat[,1],lum.norm)
  colnames(dat0)<-c("Days",samp.pdko)
}

# make long form data
library("reshape2")
dat0<-data.frame(dat0)   # wide form data
dat1 <- melt(dat0, id.vars="Days", variable.name="Sample", value.name="Luminescence")

#plot
library(ggplot2)
library(scales)

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

p <- ggplot(data=dat1, aes(x=Days, y=Luminescence, group = Sample)) +  
  geom_line(linetype=1, color="blue", size=0.2)+
  ylab("Luminescence") +
  xlab("Days") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), 
        # axis.title.x = element_text(face = "plain", color = "black", size = 8), 
        axis.text = element_text(face = "plain", color = "black", size = 6), 
        # axis.text.x = element_text(face = "plain", color = "black", size = 8), 
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1.5, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        # plot.margin = margin(1, 1, 1, 1, "pt"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=5),
        legend.key.height = unit(0.01,"line"),
        legend.key.width = unit(0.2, "line"),
        legend.spacing.x = unit(0.2, "line"),
        # legend.spacing.y = unit(0.005, "line")
        )+
  # scale_color_manual(values=c)+
  scale_y_continuous(expand=c(0.1,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels
  

if (n==1){
  p<-p + theme(legend.position="none") + 
    annotate("text", label=samp.pdko, color="black", x=max(dat1$Days), y=max(dat1$Luminescence), hjust=1, vjust=0.75, size=2) # label/legend for single sample
}
assign(plotname,p)

# output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=2, height=1) 



