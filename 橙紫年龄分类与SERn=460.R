

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

etest <- read_xlsx(  "CS-2011-66 受试者信息3.xlsx",
                     sheet = "Sheet1",
                     range = NULL,
                     col_names = TRUE,
                     col_types = NULL,
                     na = ""
                     
)

gtest <- read_xls(  "临床评估.xls",
                    sheet = "Sheet1",
                    range = NULL,
                    col_names = TRUE,
                    col_types = NULL,
                    na = ""
                    
)



ftest <- merge(dtest,etest,by = "编号")


htest <- merge(ftest,gtest,by = "编号")
htest<- subset(htest,age<=35)

m<-"male"
f<-"female"

htest$sex <- gsub("男", "male", htest$sex, fixed = TRUE)
htest$sex <- gsub("女", "female", htest$sex, fixed = TRUE)

mtest <- htest[htest$sex=="male",]

m1test <- mtest[mtest$skintype==1,]

m2test <- mtest[mtest$skintype==2,]

wtest <- htest[htest$sex=="female",]

w1test <- wtest[wtest$skintype==1,]

w2test <- wtest[wtest$skintype==2,]

twotest <- htest[htest$skintype==2,]
onetest <- htest[htest$skintype==1,]


#ctest <- openxlsx::read.xlsx("测量三次删减成第一次总.xlsx", sheet="Sheet1", rowNames=T,na.strings = ".")


cols <- c("male" = "green", "female" = "red")
sha <- c("male" = "19", "female" = "17")
shapes <- c("male" = 6, "female" = 4)

htest$sex <- factor(htest$sex )



#figure1 forehead
p <- ggplot(data = onetest,aes(x=age,y=foreheadSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = foreheadSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("female", "male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m1test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w1test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="         Non-oily skin (dry or neutral)", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("female", "male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("Foreheadnonoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")



p <- ggplot(data = twotest,aes(x=age,y=foreheadSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = foreheadSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("female", "male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m2test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w2test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="         Non-oily skin (dry or neutral)", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("female", "male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("Foreheadoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")






#figure2 cheek

p <- ggplot(data = onetest,aes(x=age,y=leftcheekSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = leftcheekSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("Female", "Male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m1test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w1test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="         Non-oily skin (dry or neutral)", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("Female", "Male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("leftcheeknonoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")



#figure2 cheek ooly

p <- ggplot(data = twotest,aes(x=age,y=leftcheekSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = leftcheekSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("Female", "Male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m2test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w2test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="                   Oily skin", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("Female", "Male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("leftcheekoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")

#figure3 rightcheek



p <- ggplot(data = onetest,aes(x=age,y=rightcheekSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = rightcheekSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("Female", "Male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m1test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w1test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="         Non-oily skin (dry or neutral)", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("Female", "Male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("Rightcheeknonoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")

#figure2 cheek ooly

p <- ggplot(data = twotest,aes(x=age,y=rightcheekSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = rightcheekSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("Female", "Male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m2test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w2test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="                   Oily skin", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("Female", "Male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("Rightcheekoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")




#chin



p <- ggplot(data = onetest,aes(x=age,y=chinSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = chinSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("Female", "Male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m1test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w1test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="         Non-oily skin (dry or neutral)", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("Female", "Male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("Chinnonoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")

#figure2 cheek ooly

p <- ggplot(data = twotest,aes(x=age,y=chinSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = chinSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("Female", "Male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m2test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w2test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="                   Oily skin", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("Female", "Male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("Chinoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")


#nose


p <- ggplot(data = onetest,aes(x=age,y=noseSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = noseSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("Female", "Male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m1test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w1test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="         Non-oily skin (dry or neutral)", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("Female", "Male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("Nosenonoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")

#figure2 cheek ooly

p <- ggplot(data = twotest,aes(x=age,y=noseSER,colour=sex,shape=sex))+
  geom_point(aes(x = age,y = noseSER), size=3)+
  #scale_colour_manual(values = cols)+
  scale_colour_manual(values = c("male" = "orange", "female" = "purple"),name  ="Sex",
                      breaks=c("female", "male"),
                      labels=c("Female", "Male"))+
  #scale_colour_manual(values = c("green",  "red"))+
  #scale_shape_manual(values = sha)+
  geom_smooth(data = m2test,method="loess",formula = y ~ x ,color="orange",linetype=6,level=.00)+
  geom_smooth(data = w2test,method="loess",formula = y ~ x ,color="purple",linetype=2,level=.00)+
  labs(title ="                   Oily skin", x="Age (year)", y="One-time SER (a.u./min)",color="Sex")+
  #scale_fill_discrete(name = "Title")
  #p$labels$fill <-"New Legend Title"
  
  #scale_colour_discrete(name  ="Sex",
  #breaks=c("female", "male"),
  #labels=c("female", "male")) +
  scale_shape_discrete(name  ="Sex",
                       breaks=c("female", "male"),
                       labels=c("Female", "Male"))+
  
  theme(text=element_text(size=15,  family="sans", face = "bold")#times new roma#sans/宋体#mono/雅黑  
        ,axis.line = element_line(colour = "black")
        ,panel.grid.major=element_blank()
        ,panel.grid.minor=element_blank()
        ,panel.background = element_blank()
        ,axis.ticks.length.y = unit(.15, "cm"),
        legend.key = element_blank (),
        legend.position = "bottom",
  )

p

ggsave("Noseoily.tiff",device = "tiff",dpi = 300,path = "C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx/性别与年龄")









