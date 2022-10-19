path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig1/"

setwd(paste(path))
dt <- 10   # Input, interval=15min ##########################
offs <- 0  # Input offset in Hours ###########################

samp.LT<-c("LUM", "TMRM")  ##################################### provide sampe name
plotname<-"p_LTcrop"   ########################################## plot name

#clrs<-c("blue", "green", "red", "orange","violet","cyan") # 6 colours
clrs<-c("blue", "red", "orange","violet","cyan") # 5 colours
c<-clrs[1:length(samp.LT)]

# files = list.files(pattern="*.csv")
dat<-read.csv("20160229C3pCGB-T10m_LV_crops.csv") ############################### file to read
dat<-data.matrix(dat[,-1])
dat<-dat[,grepl("crop1",colnames(dat))]  # select columns for cell1 ###########################
dat0<-sweep(dat, 2, colMeans(dat), FUN="/") #normalize to mean of each column 
# dat0<-data.frame(dat0)
Hours <- (1:nrow(dat0)-1)/(60/dt)+offs				# offset hours after Shock
dat0<-cbind(Hours,dat0)

# # subset data
# dat<-dat[dat[,1]<=8,]

# make long form data
library("reshape2")
dat0<-data.frame(dat0)   # wide form data
dat1 <- melt(dat0, id.vars="Hours", variable.name="Sample", value.name="R.I.")

#plot
library(ggplot2)
library(scales)

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

p <- ggplot(data=dat1, aes(x=Hours, y=R.I., group = Sample, colour = factor(Sample, labels=samp.LT))) +  
  geom_line(linetype=1,size=0.25, alpha=0.75)+
  ylab("R.I.") +
  xlab("Hours") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), 
        # axis.title.x = element_text(face = "plain", color = "black", size = 8), 
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        # axis.text.x = element_text(face = "plain", color = "black", size = 8), 
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(1, 1, 1, 1, "pt"),
        legend.position = c(0.25,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=5),
        legend.key.height = unit(0.01,"line"),
        legend.key.width = unit(0.2, "line"),
        legend.spacing.x = unit(0.1, "line")
        # legend.spacing.y = unit(0.005, "line")
        )+
  guides(colour = guide_legend(nrow = 1))+
  scale_color_manual(values=c)+
  scale_x_continuous(breaks=seq(0,120,12))+
  scale_y_continuous(expand=c(0.1,0.1), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels
  
assign(plotname,p)

# output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=1.5, height=.75)



