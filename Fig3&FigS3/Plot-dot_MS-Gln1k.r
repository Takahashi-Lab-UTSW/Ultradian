path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig3/"

setwd(paste(path))

plotname<-"p_MS-Gln_dot"

df<-read.csv("20140905FormicAcid_Gln_raw1k.csv")

#plot
library(ggplot2)

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

p <- ggplot(data=df, aes(x=factor(samp, levels=unique(samp)), y=glutamine, fill=samp)) +  
  geom_dotplot(binaxis="y", stackdir="center") +
  stat_summary(fun.data=data_summary, color="red", size=0.1)+
  
  ylab("Relative Gln") +
  # xlab("Days") +
  theme_classic() +       # Removes gridlines & background
  theme(
          axis.title = element_text(face = "plain", color = "black", size = 6),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "plain", color = "black", size = 6, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "plain", color = "black", size = 6),
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1.5, "pt"),
        # axis.ticks.x = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(8, 6, 6, 8, "pt"),
        legend.position = "none"
        # legend.title = element_blank(),
        # legend.background = element_blank(),
        # legend.text = element_text(size=5,margin = margin(t=-0.5,b=-0.5, unit = "pt")),
        # legend.key.height = unit(0.01,"line"),
        # legend.key.width = unit(0.2, "line"),
        # legend.spacing.x = unit(0.1, "line"),
        # legend.spacing.y = unit(0.05, "line")
        )+

  # scale_y_continuous(limits = c(0.5, 1.2),expand = c(0,0))+
  # scale_x_discrete(limits=df$samp)+
  scale_fill_manual(values=c("black","white"))
    # scale_y_continuous(expand=c(0.1,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels

# # Add text on a specific positions:
p<- p + annotate("text", x = 2, y = 1.05, label = "***" , color="black", size=2)

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
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=1, height=1)



