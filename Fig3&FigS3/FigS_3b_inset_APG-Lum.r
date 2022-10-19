#normalize data to mean of first 6h
path <- "C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig3/"	# Set path ############
setwd(path)

library(dplyr)
library(ggplot2)
library(scales)
library("reshape2")
library(cowplot)

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

#fluo data
dat<-read.csv("20151206C3pAPG-norm.csv")
Hours<-dat[,1]
dat0<-dat[,-1]
dat0<-sweep(dat0, 2, colMeans(dat%>%filter(Hours<=6))[-1], FUN="/") #normalize to mean of first 6h
dat0<-cbind(Hours,dat0)

#plot
plotname<-"p_APGf" ##

# make long form data
dat0<-data.frame(dat0)   # wide form data
n<-ncol(dat0)-1  # number of samples
clrs<-c("blue", "green", "red", "orange","violet","cyan") # 6 colours
c<-clrs[1:n]
dat1 <- melt(dat0, id.vars="Hours", variable.name="Sensor", value.name="Fluorescence")
p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group = Sensor)) +  
  geom_line(linetype=1, aes(color=Sensor), size=0.2)+
  labs(x = NULL, y = NULL)+
  theme_nothing() + #(cowplot) Removes gridlines & background
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.background = element_rect(fill = "white", color = NA)
  )+
  scale_color_manual(values=c)+
  scale_x_continuous(expand=c(0.,0))+
  scale_y_continuous(expand=c(0.,0), limits=c(0., 1.2)) # set y-axis labels

if (n==1){
  p<-p + theme(legend.position="none") + 
    annotate("text", label=samples[i], color="black", x=max(dat1$Hours), y=max(dat1$Fluorescence), hjust=1, vjust=0.75, size=2.8) # label/legend for single Sensor
}
assign(plotname,p)

# output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=.75, height=.75) 


# plot Lum
samp.APG<-c("Lum")  ##################################### provide sampe name
plotname<-"p_APGl"   ############################################ plot name
dats<-list()
nrows<-c()
files = list.files(pattern="*APG_Lum.csv")

n<-length(samp.APG)
# clrs<-c("blue", "orange","green", "red", "violet","cyan") # 6 colours
clrs<-c("black","blue", "red", "green","violet","cyan")
c<-clrs[1:n]

# read raw data exported from Lumicycle Analysis
for (i in 1:n){
  df<-read.csv(files[grepl(paste("_",samp.APG[i],sep=""),files, fixed = TRUE)], skip=3, col.names = c("Date","Time",	"Days",	"CountsPerSec",	"Baseline")) 
  lum<-df[,"CountsPerSec"]
  day<-df$Days-df$Days[1]   # normalized time in days
  
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
colnames(dat)<-c("Days",samp.APG)

# normalize data to mean of the 1st 6h
if (n>1){
  dat0<-sweep(dat[,-1], 2, colMeans(dat[dat[,1]<0.25,-1]), FUN="/") #normalize to mean of the 1st 6h
  dat0<-cbind(dat[,1],dat0)
  colnames(dat0)<-c("Days",samp.APG)
}else{
  lum.norm<-dat[,2]/mean(dat[dat[,1]<0.25,2])  # normalize to mean of the 1st 6h
  dat0<-cbind(dat[,1],lum.norm)
  colnames(dat0)<-c("Days",samp.APG)
}

# make long form data
dat0<-data.frame(dat0) %>% filter(Lum>=0.4)   # wide form data
dat1 <- melt(dat0, id.vars="Days", variable.name="Sample", value.name="Luminescence")

#plot

p <- ggplot(data=dat1, aes(x=Days, y=Luminescence, group = Sample, colour = factor(Sample, labels=samp.APG))) +  
  geom_line(linetype=1,size=0.2)+
  ylab("R.I.") +
  xlab("Days") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), 
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1., "pt"),
        plot.margin = margin(1, 1, 1, 1, "pt"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA)
  )+
  scale_color_manual(values=c)+
  scale_x_continuous(expand=c(0.035,0.035))+
  scale_y_continuous(limits=c(0.,1.2),expand=c(0,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels

assign(plotname,p)

#output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=1.5, height=.75)


# plot inset

p<- ggdraw() +
  draw_plot(p_APGl, 0, 0, 1, 1) +
  draw_plot(p_APGf, 0.308, 0.32, 0.446, .66) +
  annotate("text", x = c(.55), y = c(.92), label = c("APG Fluorescence"), size=1.8)
  
  # draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 0.92), size = 15)


# save_plot("Fig1.pdf", pcow, base_height = NULL, base_aspect_ratio = 1.618, base_width = 6)
assign("p_APG",p)
save_plot("p_APG.pdf", p, base_height = .75, base_width = 1.5)

