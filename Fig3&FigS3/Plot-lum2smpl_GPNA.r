path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig3/"

setwd(paste(path))

# folders = list.dirs('.', full.names=FALSE, recursive=FALSE)
# read csv data exported from Lumicycle Analysis
samp.gpna<-c("Vehicle", "GPNA 1mM")  ##################################### provide sampe name
legends<-c("Vehicle", "GPNA")
plotname<-"p_GPNA"   ############################################ plot name
dats<-list()
nrows<-c()
files = list.files(pattern="*.csv")
files<-files[grepl("_GPNA",files)]    ###############################
n<-length(samp.gpna)
# clrs<-c("blue", "green", "red", "orange","violet","cyan") # 6 colours
clrs<-c("blue", "red", "orange","violet","cyan")
c<-clrs[1:n]

# read raw data exported from Lumicycle Analysis
for (i in 1:n){
  df<-read.csv(files[grepl(paste("-",samp.gpna[i],sep=""),files)], skip=3, col.names = c("Date","Time",	"Days",	"CountsPerSec",	"Baseline")) 
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
colnames(dat)<-c("Days",samp.gpna)

# subset data
dat<-dat[dat[,1]<=5,]

# normalize data to mean of the 1st day
if (n>1){
  dat0<-sweep(dat[,-1], 2, colMeans(dat[dat[,1]<1,-1]), FUN="/") #normalize to mean of the 1st day
  dat0<-cbind(dat[,1],dat0)
  colnames(dat0)<-c("Days",samp.gpna)
}else{
  lum.norm<-dat[,2]/mean(dat[,2])
  dat0<-cbind(dat[,1],lum.norm)
  colnames(dat0)<-c("Days",samp.gpna)
}

# make long form data
library("reshape2")
dat0<-data.frame(dat0)   # wide form data
dat1 <- melt(dat0, id.vars="Days", variable.name="Sample", value.name="Luminescence")

#plot
library(ggplot2)
library(scales)

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

p <- ggplot(data=dat1, aes(x=Days, y=Luminescence, group = Sample, colour = factor(Sample, labels=legends))) +  
  geom_line(linetype=1,size=0.2)+
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
        plot.margin = margin(8, 6, 6, 8, "pt"),
        legend.position = c(0.8,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=5,margin = margin(t=-0.5,b=-0.5, unit = "pt")),
        legend.key.height = unit(0.01,"line"),
        legend.key.width = unit(0.2, "line"),
        legend.spacing.x = unit(0.1, "line"),
        # legend.spacing.y = unit(0.05, "line")
        )+
  scale_color_manual(values=c)+
  scale_y_continuous(expand=c(0.1,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels

# # Add text on a specific positions:
# p<- p + annotate("text", x = c(2,4.5), y = c(20,25), label = c("label 1", "label 2") , color="orange", size=5 , angle=45, fontface="bold")
# # Add rectangles
p<- p + annotate("rect", xmin=1.3, xmax=2.6, ymin=0.65 , ymax=0.675, alpha=0.25, size=0, color="black", fill="black")
# # Add segments
# p<- p + annotate("segment", x = 1, xend = 3, y = 25, yend = 15, colour = "purple", size=3, alpha=0.6)
# Add arrow
# p<- p + annotate("segment", x = 1.21, xend = 1.21, y = 1.45, yend = 1.35, colour = "black", size=0.25, alpha=1, arrow=arrow(length=unit(0.05,"npc")))

if (n==1){
  p<-p + theme(legend.position="none") + 
    annotate("text", label=samp.gpna, color="black", x=max(dat1$Days), y=max(dat1$Luminescence), hjust=1, vjust=0.75, size=2.8) # label/legend for single sample
}
assign(plotname,p)

# # output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=2, height=1.5)



