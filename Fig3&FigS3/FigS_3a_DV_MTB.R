
path <- "C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig3/"	# Set path ############
dt <- 5   # Input, interval=15min ##########################
offs <- 0  # Input offset in Hours ###########################
cnm <- c("MitG","TMRM", "BeRST")  # Channel/sensor names #########################
setwd(path)

#plot
setwd(path)
library(ggplot2)
library(scales)
library("reshape2")
scaleFUN <- function(x) sprintf("%.1f", x)  # digits

#read data --------
df <- read.csv('CWTpCLIH-MgTB01_4_combine_normalized.csv')[,-1] %>% 
  filter(Hours>=offs) %>% #subset data
  mutate(Hours=Hours-offs) #%>% #reset time
# df1 <- sweep(df %>% select(-Hours), 2, colMeans(df%>%filter(Hours<10)%>%select(-Hours)), FUN="/") #normalize to mean of each column
# df <- cbind(df1, Hours=df$Hours)


#plot -----------
scaleFUN <- function(x) sprintf("%.1f", x)  # digits

file <- 'p_MTB'

# make long form data
dat0<-df   # wide form data
n<-ncol(dat0)-1  # number of sensors
clrs<-c("blue", "green", "red", "orange","violet","cyan") # 6 colours
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
        legend.position = c(0.4, 1),
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
  scale_y_continuous(expand=c(0.1,0.), limits=c(.6, 1.4), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels

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
assign(file,p)

# output pdf
ggsave(filename=paste(file,"pdf",sep="."), plot=p, useDingbats=F, width=1.5, height=.75) 




