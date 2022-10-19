path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig1/"
setwd(paste(path))

# folders = list.dirs('.', full.names=FALSE, recursive=FALSE)
plotname<-"p_tempF"   ################################################## plot name

# read csv data exported from Lumicycle Analysis
samp.fct<-c("30c","36c","39c")  # vector of sample names
dats<-list()
nrows<-c()
files = list.files(pattern="*.csv")
n<-length(samp.fct)
clrs<-c("blue", "green", "red", "orange","violet","cyan") # 6 colours
c<-clrs[1:n]

# read raw data exported from Lumicycle Analysis
for (i in 1:n){
  df<-read.csv(files[grepl(samp.fct[i],files)], skip=3, col.names = c("Date","Time",	"Days",	"CountsPerSec",	"Baseline"))
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
colnames(dat)<-c("Days",samp.fct)

# subset data
dat<-dat[dat[,1]<=8,]

# normalize data to mean of the 1st day
if (n>1){
  dat0<-sweep(dat[,-1], 2, colMeans(dat[dat[,1]<1,-1]), FUN="/") #normalize to mean of the 1st day
  dat0<-data.frame(cbind(dat[,1],dat0))
  colnames(dat0)<-c("Days",samp.fct)
}else{
  lum.norm<-dat[,2]/mean(dat[,2])
  dat0<-data.frame(cbind(dat[,1],lum.norm))
  colnames(dat0)<-c("Days",samp.fct)
}

# make long form data
library("reshape2")
dat0<-data.frame(dat0)   # wide form data
dat1 <- melt(dat0, id.vars="Days", variable.name="Samples", value.name="Luminescence")
dat1<-data.frame(dat1)

# labels for each facets
len <- length(levels(dat1$Samples))
vars <- data.frame(expand.grid(levels(dat1$Samples)))
colnames(vars) <- c("Samples")
fct_labs<-data.frame(x=0, y=apply(dat0[,-1],2,max)+0.05, vars, lbl=c(as.character(expression('30'*degree*'C')),
                                                                as.character(expression('36'*degree*'C')),
                                                                as.character(expression('39'*degree*'C'))))
scaleFUN <- function(x) sprintf("%.1f", x)  # digits


#plot
library(ggplot2)
library(scales)
library(lemon)

# p + facet_rep_grid(drv ~ cyl) + coord_capped_cart(bottom='both', left='both') +
#   theme_bw() + theme(panel.border=element_blank(), axis.line=element_line())

p <- ggplot(data=dat1, aes(x=Days, y=Luminescence, group=Samples)) + 
  geom_line(linetype=1, size=0.1)+
  facet_rep_grid(Samples~., scales="free")+
  # facet_wrap(~Samples, ncol=1,  scales="free")+
  coord_capped_cart(bottom='both', left='both')+
  
  geom_text(aes(x,y,label=lbl, group=NULL), data=fct_labs,  parse = TRUE, hjust=0.2, vjust=1, size=2)+ 
  # geom_text size = 1 (mm) = 1/0.35 points
  
  scale_y_continuous(breaks=pretty_breaks(n = 3), labels=scaleFUN)+ # set y-axis labels
  
  ylab("Luminescence") +
  xlab("Days") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
        axis.text.y = element_text(face = "plain", color = "black", size = 5), 
        axis.text.x = element_text(face = "plain", color = "black", size = 5, vjust=0), 
        axis.line = element_line(size=0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        panel.spacing.y=unit(-0.2, "line"),
        plot.margin = margin(1,1,1,1, "pt"),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank()
        )

  # scale_color_manual(values=c)
assign(plotname,p)

# output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=1.5, height=1.75)


