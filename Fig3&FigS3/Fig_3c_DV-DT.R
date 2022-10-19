
library(dplyr)
library(ggplot2)
library(scales)
library("reshape2")


path <- "C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig3/"	# Set path ############
dt <- 10   # Input, interval=15min ##########################
offs <- 0  # Input offset in Hours ###########################
cnm <- c("DiBAC","TMRM")  # Channel/sensor names #########################
setwd(path)

#read data --------
df <- read.csv('20160119C3C1-DT01_3_combine_corrected_normalized.csv')[,-1] #%>% 
# df1 <- sweep(df, 2, colMeans(df), FUN="/") #normalize to mean of each column
# df1$Hours <- (1:nrow(df1)-1)/(60/dt)+offs				# offset hours after Shock


#plot -----------
scaleFUN <- function(x) sprintf("%.1f", x)  # digits

file <- 'p_DT10m'

# make long form data
dat0<-df   # wide form data
n<-ncol(dat0)-1  # number of sensors
clrs<-c("blue", "red", "green", "orange","violet","cyan") # 6 colours
c<-clrs[1:n]
dat1 <- melt(dat0, id.vars="Hours", variable.name="Sensor", value.name="Intensity")

p <- ggplot(data=dat1, aes(x=Hours, y=Intensity, group = Sensor)) +  
  geom_line(linetype=1, aes(color=Sensor), size=0.2, alpha=0.75)+
  ylab("R.I.") +
  xlab("Hours") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), 
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(1, 1, 1, 1, "pt"),
        legend.position = c(0.3, 1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=4.5, margin=margin(l=.25, r=.25, unit='line')),
        legend.key.height = unit(0.01,"line"),
        legend.key.width = unit(0.2, "line"),
        legend.spacing.x = unit(0.1, "line"),
        # legend.spacing.y = unit(0.005, "line")
  )+
  guides(colour = guide_legend(nrow = 1))+
  scale_color_manual(values=c)+
  scale_y_continuous(expand=c(0.1,0.5), breaks=pretty_breaks(n = 3), labels=scaleFUN) # set y-axis labels

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
    annotate("text", label=samples[i], color="black", x=max(dat1$Hours), y=max(dat1$Intensity), hjust=1, vjust=0.75, size=2.8) # label/legend for single Sensor
}

# output pdf
ggsave(filename=paste(file,"pdf",sep="."), plot=p, useDingbats=F, width=1.5, height=.75) 


# 
# #plot 2 y axes ------------
# df$Hours <- (1:nrow(df)-1)/(60/dt)+offs				# offset hours after Shock
# 
# scaleFUN <- function(x) sprintf("%.0f", x)  # digits
# 
# d.range <- diff(range(df$Lum))/diff(range(df$pHrodo))
# 
# 
# plt <- ggplot()+
#   geom_line(data=df, aes(x=Hours, y=(pHrodo-min(pHrodo))*d.range+min(Lum)), linetype=1,size=0.2, color = "red", alpha=0.75)+
#   geom_line(data=df, aes(x=Hours, y=Lum), stat = "identity", linetype=1, size=0.2, color = "blue", alpha=0.75)+
#   xlab("Hours") + 
#   scale_y_continuous(name = "Luminescence",
#                      expand = c(0.1,0.1),
#                      labels=scaleFUN,
#                      breaks=pretty_breaks(n = 4),
#                      sec.axis = sec_axis(~(.-min(df$Lum))/d.range+min(df$pHrodo), name = "pHrodo")) +
#   theme_classic() +       # Removes gridlines & background
#   theme(
#     axis.title.y = element_text(color = "blue"),
#     axis.title.y.right = element_text(color = "red"),
#     axis.title = element_text(face = "plain", color = "black", size = 5),
#     axis.text = element_text(face = "plain", color = "black", size = 4), 
#     axis.line = element_blank(),
#     axis.ticks = element_line(colour = "black", size = 0.1),
#     axis.ticks.length = unit(1, "pt"),
#     plot.margin = margin(0, 0, 0, 0, "pt"),
#     panel.border = element_rect(colour = "black", fill=NA, size=0.1)
#   )
# 
# ggsave(filename=paste(file,"pdf",sep="_2y."), plot=plt, useDingbats=F, width=1.5, height=.75) 

