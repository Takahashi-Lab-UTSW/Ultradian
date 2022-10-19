path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig4/shCxQPCR/"

setwd(paste(path))
library("reshape2")
library(ggplot2)
library(scales)

plotname<-"p_shCxQPCR"

df<-read.csv("shCx_QPCR.csv")

dat1 <- melt(df, id.vars="Sample", variable.name="Cx", value.name="mRNA")

#plot
p <- ggplot(data=dat1, aes(x=Sample, y=mRNA, fill=Cx)) +  
  geom_bar(stat="identity", color="black", size=0.1, position=position_dodge()) +
  # geom_errorbar(aes(ymin=ATP, ymax=ATP+std), width=.2, size=0.1, position=position_dodge(.9))+
  
  ylab("Relative mRNA") +
  # xlab("Days") +
  theme_classic() +       # Removes gridlines & background
  theme(
    axis.title = element_text(face = "plain", color = "black", size = 6),
    axis.title.x = element_blank(),
    axis.text.x = element_text(face = "plain", color = "black", size = 5, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "plain", color = "black", size = 5),
    axis.line = element_blank(),
    axis.line.y = element_line(colour = "black", size = 0.1),
    axis.ticks = element_line(colour = "black", size = 0.1),
    axis.ticks.length = unit(1., "pt"),
    axis.ticks.x = element_blank(),
    # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
    plot.margin = margin(3, 1, 1, 1, "pt"),
    legend.position = c(0.85,0.9),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size=5,margin = margin(t=-0.1,b=-0.1, unit = "pt")),
    legend.key.height = unit(0.01,"line"),
    legend.key.width = unit(0.2, "line"),
    legend.spacing.x = unit(0.1, "line"),
    legend.spacing.y = unit(0.05, "line")
  )+
  
  scale_y_continuous(limits = c(0, 1), expand = c(0,0))+
  # scale_x_discrete(limits=df$samp)+
  scale_fill_manual(values=c("blue","red"))
# scale_y_continuous(expand=c(0.1,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels

# # Add text on a specific positions:
# p<- p + annotate("text", x = 2, y = 2.5, label = "***" , color="black", size=2)

# p<- p + annotate("text", x = c(2,4.5), y = c(20,25), label = c("label 1", "label 2") , color="orange", size=5 , angle=45, fontface="bold")
# # Add rectangles
# p<- p + annotate("rect", xmin=0.93, xmax=2.75, ymin=0.325 , ymax=0.35, alpha=0.25, size=0, color="black", fill="black")
# # Add segments
# p<- p + annotate("segment", x = 1, xend = 3, y = 25, yend = 15, colour = "purple", size=3, alpha=0.6)
# Add arrow
# p<- p + annotate("segment", x = 1.21, xend = 1.21, y = 1.45, yend = 1.35, colour = "black", size=0.25, alpha=1, arrow=arrow(length=unit(0.05,"npc")))

# if (n==1){
#   p<-p + theme(legend.position="none") + 
#     annotate("text", label=samp.DmKG, color="black", x=max(dat1$Days), y=max(dat1$Luminescence), hjust=1, vjust=0.75, size=2.8) # label/legend for single sample
# }
assign(plotname,p)

# # output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=1., height=.9)

