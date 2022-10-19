path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig4/CBX/"

setwd(paste(path))

# folders = list.dirs('.', full.names=FALSE, recursive=FALSE)
# read csv data exported from Lumicycle Analysis
samp.CBX<-c("Vehicle", "CBX")  ##################################### sample name in file names
sampname.cbx<-c("Glycyrrhizin","Carbenoxolone")  ## provide sampe name
plotname<-"p_CBX"   ############################################ plot name
dats<-list()
nrows<-c()
files = list.files(pattern="*.csv")
# files<-files[grepl("_CBX",files)]    ###############################
n<-length(samp.CBX)
# clrs<-c("blue", "green", "red", "orange","violet","cyan") # 6 colours
clrs<-c("blue", "red", "orange","violet","cyan")
c<-clrs[1:n]

# read raw data exported from Lumicycle Analysis
for (i in 1:n){
  df<-read.csv(files[grepl(paste("-",samp.CBX[i],sep=""),files)], skip=3, col.names = c("Date","Time",	"Days",	"CountsPerSec",	"Baseline")) 
  lum<-df[,"CountsPerSec"]
  day<-df$Days-df$Days[1]   # normalized time in days
  
  # # detrend lum by subtracting the loess line, then shift back ############################## detrend
  # dat<-cbind(day,lum)
  # loess_fit <- loess(lum ~ day, data.frame(dat))
  # lum<-lum-predict(loess_fit)+mean(predict(loess_fit))
  
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
colnames(dat)<-c("Days", sampname.cbx)

# subset data
dat<-dat[dat[,1]<=4,]

# normalize data to mean of the 1st day
if (n>1){
  dat0<-sweep(dat[,-1], 2, colMeans(dat[dat[,1]<1,-1]), FUN="/") #normalize to mean of the 1st day
  dat0<-cbind(dat[,1],dat0)
  colnames(dat0)<-c("Days", sampname.cbx)
}else{
  lum.norm<-dat[,2]/mean(dat[,2])
  dat0<-cbind(dat[,1],lum.norm)
  colnames(dat0)<-c("Days", sampname.cbx)
}

# make long form data
library("reshape2")
dat0<-data.frame(dat0)   # wide form data
dat1 <- melt(dat0, id.vars="Days", variable.name="Sample", value.name="Luminescence")
library(dplyr)
rg <- range(dat1$Luminescence)%>%diff

#plot
library(ggplot2)
library(scales)

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

p <- ggplot(data=dat1, aes(x=Days, y=Luminescence, group = Sample, colour = factor(Sample, labels=sampname.cbx))) +  
  geom_line(linetype=1,size=0.2)+
  ylab("Luminescence") +
  xlab("Days") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), 
        # axis.title.x = element_text(face = "plain", color = "black", size = 8), 
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        # axis.text.x = element_text(face = "plain", color = "black", size = 8), 
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1., "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(1, 1, 1, 1, "pt"),
        legend.position = c(0.28,0.95),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=5,margin = margin(t=.5,b=.5, unit = "pt")),
        legend.key.height = unit(0.01,"line"),
        legend.key.width = unit(0.2, "line"),
        legend.spacing.x = unit(0.1, "line"),
        # legend.spacing.y = unit(0.05, "line")
        )+
  scale_color_manual(values=c)+
  scale_y_continuous(expand=c(0.1,0.1), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels

# # Add text on a specific positions:
# p<- p + annotate("text", x = c(2,4.5), y = c(20,25), label = c("label 1", "label 2") , color="orange", size=5 , angle=45, fontface="bold")
# # Add rectangles
p<- p + annotate("rect", xmin=1.15, xmax=2.15, ymin=0.5 , ymax=0.5+rg/25, alpha=0.25, size=0, color="black", fill="black")
# # Add segments
# p<- p + annotate("segment", x = 1, xend = 3, y = 25, yend = 15, colour = "purple", size=3, alpha=0.6)
# Add arrow
# p<- p + annotate("segment", x = 1.21, xend = 1.21, y = 1.45, yend = 1.35, colour = "black", size=0.25, alpha=1, arrow=arrow(length=unit(0.05,"npc")))

if (n==1){
  p<-p + theme(legend.position="none") + 
    annotate("text", label=sampname.cbx, color="black", x=max(dat1$Days), y=max(dat1$Luminescence), hjust=1, vjust=0, size=2.5) # label/legend for single sample
}
assign(plotname,p)

# # output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=1.5, height=.75)



