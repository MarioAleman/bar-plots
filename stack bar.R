setwd("C:/Users/Mario Aleman/OneDrive/Desktop/PROYECTOS_R/QUALITATIVE ANALYSIS")

data <- read.csv(file="qualitative analysis_quantitative-section.csv", header=TRUE, sep=",", dec=".")

#install packages
install.packages("hrbrthemes", "Rtools", dependencies = TRUE)
install.packages("ggthemes", dependencies = TRUE)
install.packages("patchwork", dependencies = TRUE)

# library
library(ggplot2)
library (ggthemes)
library (scales)
library (dplyr)
library(viridis)
library(hrbrthemes)
library (RColorBrewer)
library(patchwork)
library(gridExtra)
library(ggpubr)

#headings******Replace your heading to group your plot
  #Theme	
  #Coding	
  #NCDs	
  #Code frequency

# Stacked
  p1 <- ggplot(data, aes(fill=Coding, y=Code_frequency, x=NCDs)) +
  geom_bar(position="stack", stat="identity")+
  labs(title ="Absolute coding frequency")+
  labs(x ="NCD's", y = "Coding frequency")+
  scale_fill_viridis(discrete = T)+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 14))+
  theme(title = element_text(face = "bold", color = "black", size = 18))+
  theme(axis.text.y = element_text(color = "black", size = 14, face = "plain"))+
  scale_x_discrete(limit = c("Obesity", "Diabetes", "Cancer", "Other"))+
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=14))+ #change legend text font size
  theme(axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75)) 

  #Visualize plot
  p1 + plot_annotation(tag_levels = "A")
  

    # Fill
   p2<-  ggplot(data, aes(fill=Coding, y=Code_frequency, x=NCDs)) +
    geom_bar(position="fill", stat="identity")+
    scale_y_continuous(labels = scales::percent)+
    labs(title ="Relative abundance of coding frequency")+
    labs(x ="NCD's", y = "Percentage (%)")+
      scale_fill_viridis(discrete = T)+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 14))+
      theme(title = element_text(face = "bold", color = "black", size = 18))+
      theme(axis.text.y = element_text(color = "black", size = 14, face = "plain"))+
      scale_x_discrete(limit = c("Obesity", "Diabetes", "Cancer", "Other"))+
      theme(legend.key.size = unit(1, 'cm'), #change legend key size
            legend.key.height = unit(1, 'cm'), #change legend key height
            legend.key.width = unit(1, 'cm'), #change legend key width
            legend.title = element_text(size=16), #change legend title font size
            legend.text = element_text(size=14))+ #change legend text font size
      theme(axis.title.y = element_text(vjust = +3),
            axis.title.x = element_text(vjust = -0.75)) 
   
   #Visualize plot
    p2 + plot_annotation(tag_levels = "B")
   
    
 #****************************************************   
    
# show plots side-by-side with patchwork package
    p1<- p1+theme(legend.position ="none")
    p2 <-p2+theme(legend.position ="none")
    combined <- p1+p2
    combined + plot_annotation(
      title = "Combined Plot")+
      #subtitle = "This is a subtitle that describes more information about the plot",
      #caption = "This is a caption")+
      plot_annotation(tag_levels = "A")+
      theme(legend.position ="none")
    
    
#*****************************
#Facet_wrap
#*****************************************************
  
# dodge
 p4 <- ggplot(data, aes(fill=Coding, y=Code_frequency, x=NCDs)) +
    geom_bar(position="dodge", stat="identity")+
    labs(title ="Relative abundance of coding frequency")+
    labs(x ="NCD's", y = "Percentage (%)")+
    scale_fill_viridis(discrete = T)+
    theme(axis.text.x = element_text(face = "bold", color = "black", size = 14))+
    theme(title = element_text(face = "bold", color = "black", size = 18))+
    theme(axis.text.y = element_text(color = "black", size = 14, face = "plain"))+
    scale_x_discrete(limit = c("Obesity", "Diabetes", "Cancer", "Other"))+
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=14))+ #change legend text font size
    theme(axis.title.y = element_text(vjust = +3),
          axis.title.x = element_text(vjust = -0.75))+ 
    facet_wrap(~Theme) 

 p4 + plot_annotation(tag_levels = "C")
  
# Stacked
  p5 <- ggplot(data, aes(fill=Coding, y=Code_frequency, x=NCDs)) +
    geom_bar(position="stack", stat="identity")+
    labs(title ="Absolute coding frequence per theme")+
    labs(x ="NCD's", y = "Frequency")+
    scale_fill_viridis(discrete = T)+
    theme(axis.text.x = element_text(face = "bold", color = "black", size = 14))+
    theme(title = element_text(face = "bold", color = "black", size = 18))+
    theme(axis.text.y = element_text(color = "black", size = 14, face = "plain"))+
    scale_x_discrete(limit = c("Obesity", "Diabetes", "Cancer", "Other"))+
   # theme(legend.position="bottom")+
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=14))+ #change legend text font size
    theme(axis.title.y = element_text(vjust = +3),
          axis.title.x = element_text(vjust = -0.75))+ 
    facet_wrap(~Theme) 
  
p5 + plot_annotation(tag_levels = "c")
  
 
  
# Fill
  p6 <- ggplot(data, aes(fill=Coding, y=Code_frequency, x=NCDs)) +
    geom_bar(position="fill", stat="identity")+
    scale_y_continuous(labels = scales::percent)+
    labs(title ="Relative abundance of coding frequency")+
    labs(x ="NCD's", y = "Percentage (%)")+
    scale_fill_viridis(discrete = T)+
    theme(axis.text.x = element_text(face = "bold", color = "black", size = 14, angle = 90))+
    theme(title = element_text(face = "bold", color = "black", size = 18))+
    theme(axis.text.y = element_text(color = "black", size = 14, face = "plain"))+
    scale_x_discrete(limit = c("Obesity", "Diabetes", "Cancer", "Other"))+
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=14))+ #change legend text font size
    theme(axis.title.y = element_text(vjust = +3),
          axis.title.x = element_text(vjust = -0.75))+ 
    facet_wrap(~Theme) 
  
  p6 + plot_annotation(tag_levels = "A")

#visualize plot  
p6
  
  
  p5<- p5+theme(legend.position ="none")
  ggarrange(ggarrange(p1, p2, ncol = 2, labels = c("A", "B")), 
            ggarrange(p5, labels = "C"), 
            nrow = 2)
         #   common.legend = TRUE)

  