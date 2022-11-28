
setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
library(dplyr) #加载dplyr包
library(ggpmisc) #加载ggpmisc包
library(readxl)

dtest <- read_xlsx(  "CLSER仪器数据测一次.xlsx",
                     sheet = "测试一次仪器数据",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)

#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")
#Figure1


p <- ggscatter(dtest, x = "second-chin-CL", y = "chinSER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                           Chin",
               xlab = "The second-time CSL (a.u.)" ,#"??"
               ylab = "One-time SER (a.u./min)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray"),
               #conf.int = TRUE,# Add confidence interval
               #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               #cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"),
               #cor.coef.size = 5
)+stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           #label.x.npc = 'left',
           #label.y.npc = 'top',
           label.x = 0, label.y = 6
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)
ggsave("Chin.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/第二次CSL与一次SER")


#figure


p <- ggscatter(dtest, x = "second-rightcheek-CL", y = "rightcheekSER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                 Right cheek",
               xlab = "The second-time CSL (a.u.)" ,#"??"
               ylab = "One-time SER (a.u./min)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray"),
               #conf.int = TRUE,# Add confidence interval
               #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               #cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"),
               #cor.coef.size = 5
)+stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~` ,`~")),
           r.accuracy = 0.0001,
           p.accuracy = 0.001,
           #label.x.npc = 'left',
           #label.y.npc = 'top',
           label.x = -8, label.y = 8
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)

ggsave("Rightcheek.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/第二次CSL与一次SER")





#figureleftcheek

p <- ggscatter(dtest, x = "second-leftcheek-CL", y = "leftcheekSER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Left cheek",
               xlab = "The second-time CSL (a.u.)" ,#"??"
               ylab = "One-time SER (a.u./min)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray"),
               #conf.int = TRUE,# Add confidence interval
               #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               #cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"),
               #cor.coef.size = 5
)+stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           #label.x.npc = 'left',
           #label.y.npc = 'top',
           label.x = -10, label.y = 7
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Leftctheek.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/第二次CSL与一次SER")


#figure nose

p <- ggscatter(dtest, x = "second-nose-CL", y = "noseSER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                    Nose",
               xlab = "The second-time CSL (a.u.)" ,#"??"
               ylab = "One-time SER (a.u./min)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray"),
               #conf.int = TRUE,# Add confidence interval
               #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               #cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"),
               #cor.coef.size = 5
)+stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.0001,
           p.accuracy = 0.001,
           #label.x.npc = 'left',
           #label.y.npc = 'top',
           label.x = 8, label.y = 9
           , position = "identity")
p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Nose.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/第二次CSL与一次SER")



#figure nose

p <- ggscatter(dtest, x = "second-forehead-CL", y = "foreheadSER",
               combine = FALSE,
               color = "black",
               shape = 19,
               size =2,
               title ="                     Forehead",
               xlab = "The second-time CSL (a.u.)" ,#"??"
               ylab = "One-time SER (a.u./min)" ,#"??"
               add = "reg.line", conf.int = TRUE,    
               add.params = list(fill = "lightgray"),
               #conf.int = TRUE,# Add confidence interval
               #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
               #cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"),
               #cor.coef.size = 5
)+stat_cor(method = "spearman", 
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           r.accuracy = 0.001,
           p.accuracy = 0.001,
           #label.x.npc = 'left',
           #label.y.npc = 'top',
           label.x = 5, label.y = 8
           , position = "identity")


p+theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        
)


ggsave("Forehead.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/第二次CSL与一次SER")


