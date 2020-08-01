path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig3/"
setwd(path)

dt <- 5   # sec, Input, interval ##########################
offs <- 0  # Input offset in Hours ###########################

plotname<-"p_TMRM-KG"
clrs<-c("black","blue", "red", "violet")

dat<-read.csv("20161031TMRM_DmKG-FCCP-OligA_corrected.csv")
samp.TMKG<-colnames(dat)
dat0<-sweep(dat, 2, colMeans(dat[1:18,]), FUN="/") #normalize to mean of the 1st 18
dat0$Mins <- (1:nrow(dat0)-1)/(60/dt)+offs				# time

# make long form data
library("reshape2")
dat0<-data.frame(dat0)   # wide form data
dat1 <- melt(dat0, id.vars="Mins", variable.name="Drug", value.name="R.I.")

#plot
library(ggplot2)
library(scales)

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

p <- ggplot(data=dat1, aes(x=Mins, y=R.I., group = Drug, colour = factor(Drug, labels=samp.TMKG))) +  
  geom_line(linetype=1,size=0.2)+
  # ylab("Luminescence") +
  # xlab("Days") + 
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
        legend.position = c(0.25,0.9),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size=5,margin = margin(t=-0.5,b=-0.5, unit = "pt")),
        legend.key.height = unit(0.01,"line"),
        legend.key.width = unit(0.2, "line"),
        legend.spacing.x = unit(0.1, "line"),
        # legend.spacing.y = unit(0.05, "line")
  )+
  scale_color_manual(values=clrs)+
  scale_y_continuous(expand=c(0.1,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels

# # Add text on a specific positions:
# p<- p + annotate("text", x = c(2,4.5), y = c(20,25), label = c("label 1", "label 2") , color="orange", size=5 , angle=45, fontface="bold")
# # Add rectangles
# p<- p + annotate("rect", xmin=1.2, xmax=4, ymin=0.1 , ymax=0.15, alpha=0.25, size=0, color="black", fill="black")
# # Add segments
# p<- p + annotate("segment", x = 1, xend = 3, y = 25, yend = 15, colour = "purple", size=3, alpha=0.6)
# Add arrow
p<- p + annotate("segment", x = 1.5, xend = 1.5, y = 1.18, yend = 1.1, colour = "black", size=0.25, alpha=1, arrow=arrow(length=unit(0.05,"npc")))

# if (n==1){
#   p<-p + theme(legend.position="none") + 
#     annotate("text", label=samp.2DG, color="black", x=max(dat1$Days), y=max(dat1$Luminescence), hjust=1, vjust=0.75, size=2.8) # label/legend for single sample
# }
assign(plotname,p)

# # output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=1.5, height=1.5)
