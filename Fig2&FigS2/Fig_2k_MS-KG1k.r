path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig2/"

setwd(paste(path))

plotname<-"p_MS-KG"

df<-read.csv("20141030TBA_KG_raw1k.csv")

#plot
library(ggplot2)

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#Violin plot 
p <- ggplot(df, aes(x=factor(samp, levels=unique(samp)), y=KG, fill=samp)) + 
  geom_violin(trim=F, size=0.3)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="red", alpha=1, size=0.1)+
  theme_classic() +       # Removes gridlines & background
  theme(axis.title = element_text(face = "plain", color = "black", size = 6), # theme text size in pt
        axis.text.x = element_text(face = "plain", color = "black", size = 6, angle = 45, hjust = 1),
        axis.text.y = element_text(face = "plain", color = "black", size = 5),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        plot.margin = margin(2, 1, 1, 1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        legend.position = "none",
        axis.title.x=element_blank())+
  ylab("KG") +
  scale_y_continuous(limits=c(0.9,1.5), expand = c(0.,0.))+
  annotate("text", x = 2, y = 1.45, label = "*" , color="black", size=3)
assign(plotname,p)
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=.8, height=1)

