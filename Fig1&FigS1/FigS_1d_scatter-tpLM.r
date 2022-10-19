path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig1/"
setwd(paste(path))
scaleFUN_X <- function(x) sprintf("%.0f", x)  # digits
scaleFUN_Y <- function(x) sprintf("%.1f", x)  # digits

df<-read.csv("Temp-KaiSQ.csv" )   ####################### data file to read
plotname<-"p_tempS"                   ################# plot name

# make long form data
library("reshape2")
dat1<- melt(df, id.vars="Temp", value.name="Per")



# linear fit
fit<-lm(Temp~Per, dat1)



#plot
library(ggplot2)
library(scales)

p <- ggplot(data=data.frame(dat1), aes(x=Temp, y=Per),inherit.aes = FALSE) + 
  geom_point(shape=1)+geom_smooth(method=lm,size=0.5)+
  # geom_text size = 1 (mm) = 1/0.35 points
  
  scale_x_continuous(breaks=pretty_breaks(n = 4), labels=scaleFUN_X)+ # set y-axis labels
  scale_y_continuous(breaks=pretty_breaks(n = 4), labels=scaleFUN_Y)+ # set y-axis labels
  
  xlab("Temperature") +
  ylab("Period (h)") + 
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
        axis.text = element_text(face = "plain", color = "black", size = 6), 
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1.5, "pt"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        legend.position = "none"
        )+
  geom_text(x=36, y=7, label=expression("Q"[10]*"=2.11"),size=2) # subscript; ^{2} superscript

# output pdf
assign(plotname,p)

# output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=2, height=2) 


