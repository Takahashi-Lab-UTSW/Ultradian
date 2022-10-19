path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig2/ATPase/"

setwd(paste(path))

plotname<-"p_ATPsyn-KG-C3p_Ki"

df<-read.csv("20170501ATPsyn-KG_C3pCLIH.csv")
df<-df[,-1]
df<-df[-1,]

# # fit for Ki based on noncompetitive inhibition: Vm.app=Vm/(1+I/Ki)
# y<-rowMeans(df[,-1])
# x<-df[,1]
# mod <- nls(y ~ 1/(1+x/Ki), start=list(Ki=10))
# Ki<-as.numeric(coef(mod)) 


# make long form data
library("reshape2")
dat<- melt(df, id.vars="KGmM", value.name="Vm")

scaleFUN <- function(x) sprintf("%.1f", x)  # digits

#plot
library(ggplot2)


p <- ggplot(data=dat, aes(x=log10(KGmM), y=Vm)) +  
  geom_point(shape=1, size=1) +
  # geom_smooth(size=0.5)+
  # stat_smooth(method = "lm", formula = y ~ 1/(1+x/17.3), size = 1)+
  geom_smooth(method = "nls",
              se = FALSE,
              size = 0.5,
              method.args = list(formula = y ~ Vmax / (1 + 10^x/Ki), 
                                 start = list(Vmax = 1, Ki = 17.3)))+
  
  ylab("ATP synthase activity") +
  xlab("Log(KG) mM") +
  theme_classic() +       # Removes gridlines & background
  theme(
          axis.title = element_text(face = "plain", color = "black", size = 6),
        # axis.title.x = element_blank(),
        axis.text = element_text(face = "plain", color = "black", size = 5),
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1., "pt"),
        # axis.ticks.x = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(2, 1, 1, 1, "pt"),
        legend.position = "none"
        # legend.title = element_blank(),
        # legend.background = element_blank(),
        # legend.text = element_text(size=5,margin = margin(t=-0.5,b=-0.5, unit = "pt")),
        # legend.key.height = unit(0.01,"line"),
        # legend.key.width = unit(0.2, "line"),
        # legend.spacing.x = unit(0.1, "line"),
        # legend.spacing.y = unit(0.05, "line")
        )+

  scale_x_continuous(expand = c(0.1,0.1), breaks=pretty_breaks(n = 4), labels=scaleFUN)
  # scale_x_discrete(limits=df$samp)+
  # scale_fill_manual(values=c("black","white"))
    # scale_y_continuous(expand=c(0.1,0), breaks=pretty_breaks(n = 4), labels=scaleFUN) # set y-axis labels

# # Add text on a specific positions:
p<- p + annotate("text", x = 0, y = 0, label = expression('K'[i]*' = 17.3 mM') , color="black", size=2)

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


