path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig1/"

library(ggplot2)
library(scales)
library(WaveletComp)
library("reshape2")

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

setwd(paste(path))
dt <- 10   # Input, interval=15min ##########################
offs <- 0  # Input offset in Hours ###########################

samp.LT<-c("LUM", "TMRM")  ##################################### provide sampe name
plotname<-"p_LTcrop1"   ########################################## plot name

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

# Cross-Wavelet analysis
x<-dat0[,2] # Lum data
y<-dat0[,3] # TMRM data
my.data = data.frame(x = x, y=y)
row.names(my.data)<-Hours
my.wc = analyze.coherency(my.data, my.pair = c("x", "y"),
                       loess.span = 0,
                       dt = dt/60,        # Time unit = Hour
                       dj = 1/250,      # y-axis resolution
                       lowerPeriod = 1,
                       upperPeriod = 48,
                       make.pval = T, 
                       n.sim = 100)

# plot cross-wavelet as pdf
pdf("p_LTcrop1_wc.pdf", width = 4, height = 2)
par(mar=c(2.5,3,0.5,1), mgp=c(2.5,1,0) )

p_LTcrop1_wc<-wc.image(my.wc, n.levels = 250, # colors
                       plot.coi = F,
                       plot.contour = F,
                       # plot.ridge = F,
                       # siglvl.contour = 0.05,
                       # siglvl.arrow = 0.000001,
                       p=0,
                       lvl=3.5,
                       timelab = "Time (hour)",
                       periodlab = "Period (hour)",
                       # timetck = 0.02,
                       timetcl = -0.2,
                       # periodtck = 0.02,
                       periodtcl = -0.2,
                       spec.time.axis = list(at = seq(0,240*6,12*6), labels = seq(0,240,12)),
                       # spec.time.axis = list(at = (0:7)*24*6, labels = 0:7),
                       lwd = 2,
                       lwd.axis = 0.5,
                       legend.params = list(
                         # lab = "Cross-Wavelet power",
                         width=0.5, shrink=0.5, mar=2, n.ticks=2, label.digits=0, lab.line=0.5) )
dev.off()

# # plot phase diff
# pdf("p_LTcrop1_phs.pdf", width = 4, height = 2)
# par(mar=c(2.5,3,0.5,1), mgp=c(2.5,1,0) )
# wc.phasediff.image(my.wc, which.contour = "wc", n.levels = 250, siglvl = 0.01,
#                    legend.params = list(lab = "phase difference levels"))
# dev.off()

# # plot phase diff
# pdf("p_LTcrop1_phsd.pdf", width = 4, height = 2)
# par(mar=c(2.5,3,0.5,1), mgp=c(2.5,1,0) )
# wc.sel.phases(my.wc, sel.period = 14, only.sig = T, siglvl = 0.05,
#               which.sig = "wc",
#               timelab = "Time (hour)",
#               # timetck = 0.02,
#               timetcl = -0.2,
#               # phasetck = 0.02,
#               phasetcl = -0.2,
#               spec.time.axis = list(at = seq(0,240*6,12*6), labels = seq(0,240,12)),
#               # spec.time.axis = list(at = (0:7)*24*6, labels = 0:7),
#               lwd = 1,
#               lwd.axis = 0.5,
#               legend.coords = "topright", legend.horiz = F)
# dev.off()
# 


# make long form data
library("reshape2")
dat0<-data.frame(dat0)   # wide form data
dat1 <- melt(dat0, id.vars="Hours", variable.name="Sample", value.name="R.I.")

#plot
p <- ggplot(data=dat1, aes(x=Hours, y=R.I., group = Sample, colour = factor(Sample, labels=samp.LT))) +  
  geom_line(linetype=1,size=0.2)+
  ylab("R.I.") +
  xlab("Hours") + 
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
        legend.position = c(0.7,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=5),
        legend.key.height = unit(0.01,"line"),
        legend.key.width = unit(0.2, "line"),
        legend.spacing.x = unit(0.1, "line"),
        # legend.spacing.y = unit(0.005, "line")
        )+
  scale_color_manual(values=c)+
  scale_x_continuous(breaks=seq(0,120,12))+
  scale_y_continuous(limits=c(0.7,1.3),expand=c(0.1,0.1), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels
  
# # Add text on a specific positions:
# p + annotate("text", x = c(2,4.5), y = c(20,25), label = c("label 1", "label 2") , color="orange", size=5 , angle=45, fontface="bold")
# # Add rectangles
# p + annotate("rect", xmin=c(2,4), xmax=c(3,5), ymin=c(20,10) , ymax=c(30,20), alpha=0.2, color="blue", fill="blue")
# # Add segments
# p + annotate("segment", x = 1, xend = 3, y = 25, yend = 15, colour = "purple", size=3, alpha=0.6)
# # Add arrow
# p + annotate("segment", x = 2, xend = 4, y = 15, yend = 25, colour = "pink", size=3, alpha=0.6, arrow=arrow())
  
  
if (length(samp.LT)==1){
  p<-p + theme(legend.position="none") + 
    annotate("text", label=samp.LT, color="black", x=max(dat1$Days), y=max(dat1$Luminescence), hjust=1, vjust=0.75, size=2.8) # label/legend for single sample
}
assign(plotname,p)

# output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=2, height=1)



