
path <- "C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig3/"	# Set path ############
dt <- 15   # Input, interval=15min ##########################
offs <- 0  # Input offset in Hours ###########################
setwd(path)


dat<-read.csv("20170929C3pCLIH-MQAE-TMRM15m_7.csv")
dat<-sweep(dat, 2, colMeans(dat), FUN="/") #normalize to mean of each column
dat$Hours <- (1:nrow(dat)-1)/(60/dt)+offs				# offset hours after Shock


#plot
library(ggplot2)
library(scales)
library("reshape2")
scaleFUN <- function(x) sprintf("%.1f", x)  # digits

# make long form data
  dat0<-data.frame(dat)   # wide form data
  n<-ncol(dat0)-1  # number of sensors
  clrs<-c("blue", "red", "orange","violet","cyan") # 6 colours
  # clrs<-c("green", "red", "orange","violet","cyan")
  c<-clrs[1:n]
  dat1 <- melt(dat0, id.vars="Hours", variable.name="Sensor", value.name="Fluorescence")
  plotname<-"p_MQAE"   ###########################################################################
  p <- ggplot(data=dat1, aes(x=Hours, y=Fluorescence, group = Sensor)) +  
    geom_line(linetype=1, aes(color=Sensor), size=0.2)+
    ylab("R.I.") +
    xlab("Hours") + 
    theme_classic() +       # Removes gridlines & background
    theme(axis.title = element_text(face = "plain", color = "black", size = 6), 
          axis.text = element_text(face = "plain", color = "black", size = 5), 
          axis.line = element_line(colour = "black", size = 0.1),
          axis.ticks = element_line(colour = "black", size = 0.1),
          axis.ticks.length = unit(1., "pt"),
          # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
          plot.margin = margin(1, 1, 1, 1, "pt"),
          legend.position = c(0.15,0.85),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(margin = margin(b=.25, t=.25, unit="line"), size=4.5),
          legend.key.height = unit(0.1,"line"),
          legend.key.width = unit(0.2, "line"),
          legend.spacing.x = unit(0.2, "line"),
          # legend.spacing.y = unit(0.05, "line")
    )+
    guides(fill = guide_legend(byrow = TRUE))+
    scale_color_manual(values=c)+
    scale_x_continuous(breaks=seq(0,48,12))+
    scale_y_continuous(expand=c(0.1,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels
  
  # # Add text on a specific positions:
  # p + annotate("text", x = c(2,4.5), y = c(20,25), label = c("label 1", "label 2") , color="orange", size=5 , angle=45, fontface="bold")
  # # Add rectangles
  # p + annotate("rect", xmin=c(2,4), xmax=c(3,5), ymin=c(20,10) , ymax=c(30,20), alpha=0.2, color="blue", fill="blue")
  # # Add segments
  # p + annotate("segment", x = 1, xend = 3, y = 25, yend = 15, colour = "purple", size=3, alpha=0.6)
  # # Add arrow
  # p + annotate("segment", x = 2, xend = 4, y = 15, yend = 25, colour = "pink", size=3, alpha=0.6, arrow=arrow())
  
  if (n==1){
    p<-p + theme(legend.position="none") + 
      annotate("text", label=samples[i], color="black", x=max(dat1$Hours), y=max(dat1$Fluorescence), hjust=1, vjust=0.75, size=2.8) # label/legend for single Sensor
  }
  assign(plotname,p)
  
  # output pdf
  ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=1.5, height=.75) 
  





