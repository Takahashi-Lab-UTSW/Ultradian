path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig1/"
setwd(paste(path))
scaleFUN_X <- function(x) sprintf("%.0f", x)  # digits
scaleFUN_Y <- function(x) sprintf("%.1f", x)  # digits

df<-read.csv("Temp-KaiSQ.csv" )   ####################### data file to read
plotname<-"p_tempS"                   ################# plot name

# make long form data
library("reshape2")
dat1<- melt(df, id.vars="Temp", value.name="Per")

frq<-cbind(dat1$Temp, 1/dat1$Per) # frequency = 1/Per (/hour)
colnames(frq)<-c("Temp","Frq")
frq<-data.frame(frq)

# Q10 temperature coefficient
library(respirometry)
Q10f<-Q10(R_vec = frq$Frq, T_vec = frq$Temp) # Q10 calculated from frq


curve_x = data.frame(T_vec = seq(29, 40, by = 0.1))
best_fit = Q10(R_vec = dat1$Per, T_vec = dat1$Temp,model=T)$model
curve_y = predict(best_fit, newdata = curve_x)
curv<-cbind(curve_x$T_vec, curve_y)
colnames(curv)<-c("x",'y')

# lines(curve_x$T_vec, curve_y)

#plot
library(ggplot2)
library(scales)

p <- ggplot() + 
  geom_point(data=data.frame(dat1), aes(x=Temp, y=Per),inherit.aes = FALSE,shape=1)+
  geom_line(data=data.frame(curv), aes(x=x,y=y), color="red", size=0.75)+
  # geom_smooth(method=lm,size=0.5)+
  # geom_text size = 1 (mm) = 1/0.35 points
  
  scale_x_continuous(expand=c(0.1,0.1), breaks=pretty_breaks(n = 4), labels=scaleFUN_X)+ # set y-axis labels
  scale_y_continuous(expand=c(0.1,0.1), breaks=pretty_breaks(n = 4), labels=scaleFUN_Y)+ # set y-axis labels
  
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
  # geom_text(x=36, y=7, label=expression("Q"[10]*"=2.23"),size=2) # subscript; ^{2} superscript
annotate("text", label=expression("Q"[10]*"=2.23"), color="black", x=40, y=7.5, hjust=1, vjust=0.75, size=2.5) # label/legend for single sample

# output pdf
assign(plotname,p)

# output pdf
ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=2, height=2) 


