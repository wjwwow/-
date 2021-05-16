library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
options(scipen=200)
#数据准备
setwd("C://data")
ag=read.csv("database.csv")
#图形的基础设置
# 修改全局：theme_replace
# 修改背景边框：plot.background = element_rect()
# 修改网格内线条：panel.grid=element_blank()
# 修改坐标轴边框：panel.border = element_rect(fill=NA)
# 修改标题：plot.title = element_text(hjust = 0.5)
theme_replace(plot.background = element_blank(),
              panel.border = element_blank(),
              panel.grid=element_blank(),
plot.title = element_text(hjust = 0.5))

#对数据按年分组，构建数据框
summarise(group_by(ag,年份=ag$Year),数量=n()) %>%
ggplot(aes(x=年份,y=数量))+
  geom_point(colour="red")+
  geom_line(colour="blue")+
  labs(x="年份(年)",y="凶杀案数量(件)",title="洛杉矶凶杀案曲线图")+
  theme(panel.border=element_rect(fill=NA))

#不同地区犯罪分子的平均年龄
  ag[which(ag$City %in% c("Los Angeles","New York","Cook","Wayne","Harris","Philadelphia")),] %>%
  group_by(地区=City) %>% summarise(平均年龄=mean(Victim.Age)) %>%
ggplot(aes(x=地区,y=平均年龄))+
  geom_bar(stat= "identity",fill = 'pink', width = 0.7,colour="grey")+
  labs(x="地区",y="平均年龄(岁)",title="犯罪分子的平均年龄差异柱状图")+
  geom_text(aes(label=平均年龄,),size = 3,vjust=-2)

#每年罪犯平均年龄的变化趋势
  
v_p<-ag %>% group_by(年份=Year) %>% summarise(平均年龄=mean(Victim.Age)) %>%
  ggplot(aes(x=年份,y=平均年龄))+
  geom_point(colour="blue")+
  geom_line(colour="red")+
  labs(x="年份(年)",y="罪犯平均年龄(岁)",title="罪犯平均年龄变化")+
  theme(panel.border=element_rect(fill=NA))

#每年受害者平均年龄的变化趋势
p_p<-ag %>% group_by(年份=Year) %>% summarise(平均年龄=mean(Perpetrator.Age)) %>%
  ggplot(aes(x=年份,y=平均年龄))+
  geom_point(colour="blue")+
  geom_line(colour="green")+
  labs(x="年份(年)",y="受害者平均年龄(岁)",title="受害者平均年龄变化")+
  theme(panel.border=element_rect(fill=NA))

#合拼图片
plot_grid(v_p, p_p,nrow = 1,labels =  LETTERS[1:2])

#两天线在同一图片
#修改图例位置：legend.position  legend.justification
#修改图例图标的大小：legend.key.size
#修改图例标题的位置： legend.title=element_text
#修改图例图标的背景：legend.key = element_blank()
#修改图例图标的距离：legend.key.height =unit(0.8, "cm")
#修改图例的背景：legend.background = element_blank()
#修改图例图标里面形状的大小：shape = guide_legend(override.aes = list(size = 0.5)
# 图例颜色：scale_colour_manual
# 图例形状：scale_shape_manual
# 图例线形：scale_linetype_manual
summarise(group_by(ag,年份=ag$Year),罪犯平均年龄=mean(Victim.Age),受害者平均年龄=mean(Perpetrator.Age)) %>%
ggplot(aes(x=年份)) + 
  geom_point(aes(y=罪犯平均年龄,shape = "罪犯"),size=2,color="#000000" ) + 
  geom_line(aes(y=罪犯平均年龄, colour="罪犯",linetype="罪犯"),size=0.5,) + 
  geom_point(aes(y=受害者平均年龄,shape = "受害者"),size=2,color="#000000",) + 
  geom_line(aes(y=受害者平均年龄, colour="受害者",linetype="受害者"),size=0.5) +
  scale_shape_manual("图例",values = c("罪犯"=3, "受害者"=5))+
  scale_colour_manual("图例",values = c("罪犯" = "red","受害者"="blue"))+
  scale_linetype_manual("图例", values=c("罪犯"=5,"受害者"=1)) +
  labs(x="年份(年)",y="平均年龄(岁)",title="美国犯罪VS受害者平均年龄",fill="name")+
  theme(panel.border=element_rect(fill=NA),legend.position = c(0.1, 1),
        legend.justification = c(0.1, 1),legend.key.size = unit(3, 'cm'),
        legend.title=element_text(hjust = 0.3),legend.key = element_blank(),
        legend.key.height =unit(0.8, "cm"),legend.background = element_blank() )+
  guides(shape = guide_legend(override.aes = list(size = 1 ) ))

#把美国的凶杀数据按照月份进行汇总，绘图展示每个月份的犯罪数量的差异，
#将英文月份转换为数字：match(c("March","May"), month.name)
#修改X轴的值：  scale_x_continuous(breaks=, labels =)
summarise(group_by(ag,月份=ag$Month),凶杀案数量=n()) %>%
  ggplot(aes(x=match(月份,month.name),y=as.numeric(凶杀案数量)))+
  geom_point(color="red",shape=1,size=3)+geom_line(color="blue",linetype=5)+
  labs(x="月份(月)",y="凶杀案数量(件)",title="1980~2014年美国1~12月凶杀案数量")+
  theme(panel.border=element_rect(fill=NA))+
  scale_x_continuous(breaks=c(1:12), labels =c(1:12))+
  geom_text(aes(label=凶杀案数量),size = 3,vjust=-1)

#把美国的凶杀数据按照凶手性别和月份进行汇总
#修改 X 轴刻度文字的大小、位置、角度：theme(axis.text.x = element_text(size = 15vjust = 0.5, hjust = 0.5, angle = 45))
m_n=ag[-which(ag$Victim.Sex=="Unknown"),] %>% group_by(月份=Month) %>% summarise(数量=n())
m_n2=rbind(m_n,m_n)
m_n3=m_n2[order(m_n2$月份),]
m_s_n=ag[-which(ag$Victim.Sex=="Unknown"),] %>% group_by(月份=Month,凶手性别=Victim.Sex) %>% 
  summarise(数量=n())
  ggplot(m_s_n,aes(x=月份,y=m_s_n$数量/m_n3$数量*100,fill=凶手性别))+
  geom_bar(stat = "identity",width = 0.8,position ="stack")+
  theme(axis.text.x = element_text(size = 8, vjust = 0.5, hjust = 0.5, angle = 45))+
  labs(x="月份(月)",y="比例",title="每月不同性别凶手的犯罪比例")
  
ag[-which(ag$Perpetrator.Sex=="Unknown"),] %>% group_by(月份=Month,受害人性别=Perpetrator.Sex) %>% 
    summarise(数量=n()) %>%
  ggplot(aes(x=月份,y=数量,fill=受害人性别))+
  geom_bar(stat = "identity",width = 0.8,position ="stack")+
  theme(axis.text.x = element_text(size = 8, vjust = 0.5, hjust = 0.5, angle = 45))+
  labs(x="月份(月)",y="频数",title="每月不同性别受害人的遇害频数")
  
  

#请绘图展示男性凶手和女性凶手，与被害人的社会关系？
ag[-which(ag$Victim.Sex=="Unknown"),] %>% group_by(与被害人的关系=Relationship,凶手性别=Victim.Sex) %>% 
  summarise(数量=n())  %>%
  ggplot(aes(x=与被害人的关系,y=数量,fill=凶手性别))+
  geom_bar(stat = "identity",width = 0.8,position ="stack")+
  theme(axis.text.x = element_text(size = 8, vjust = 0.5, hjust = 0.5, angle = 45))+
  labs(x="月份(月)",y="频数",title="男性凶手和女性凶手与被害人的社会关系")+
  theme(panel.border=element_rect(fill=NA),legend.position = c(0.1, 1),
        legend.justification = c(0.15, 1),legend.key.size = unit(1, 'cm'),
        legend.title=element_text(hjust = 0.1),legend.key = element_blank(),
        legend.key.height =unit(0.8, "cm"),legend.background = element_blank() )+
  guides(shape = guide_legend(override.aes = list(size = 1 ) ))

#请绘图展示男性被害人和女性被害人，与凶手的社会关系 ？
ag[-which(ag$Perpetrator.Sex=="Unknown"),] %>% group_by(与凶手的关系=Relationship,被害人性别=Perpetrator.Sex) %>% 
  summarise(数量=n())  %>%
  ggplot(aes(x=与凶手的关系,y=数量,fill=被害人性别))+
  geom_bar(stat = "identity",width = 0.8,position ="stack")+
  theme(axis.text.x = element_text(size = 8, vjust = 0.5, hjust = 0.5, angle = 45))+
  labs(x="月份(月)",y="频数",title="男性被害人和女性被害人与凶手的社会关系")+
  theme(panel.border=element_rect(fill=NA),legend.position = c(0.1, 1),
        legend.justification = c(0.15, 1),legend.key.size = unit(1, 'cm'),
        legend.title=element_text(hjust = 0.1),legend.key = element_blank(),
        legend.key.height =unit(0.8, "cm"),legend.background = element_blank() )+
  guides(shape = guide_legend(override.aes = list(size = 1 ) ))

#请绘图展示凶手性别、被害人性别、所用凶器的关系统计。
subset(ag,Perpetrator.Sex!="Unknown"&Victim.Sex!="Unknown") %>% group_by(凶器=Weapon,被害人性别=Perpetrator.Sex,凶手性别=Victim.Sex) %>% 
  summarise(数量=n())  %>% unite(性别,被害人性别,凶手性别,sep="(凶)-(被害)") %>%
  ggplot(aes(x=凶器,y=数量,fill=性别))+
  geom_bar(stat = "identity",width = 0.8,position ="stack")+
  theme(axis.text.x = element_text(size = 8, vjust = 0.5, hjust = 0.5, angle = 45))+
  labs(x="凶器",y="频数",title="凶手性别、被害人性别、所用凶器的关系统计")+
  theme(panel.border=element_rect(fill=NA),legend.position = c(0.1, 1),
        legend.justification = c(0.15, 1),legend.key.size = unit(1, 'cm'),
        legend.title=element_text(hjust = 0.1),legend.key = element_blank(),
        legend.key.height =unit(0.8, "cm"),legend.background = element_blank() )+
  guides(shape = guide_legend(override.aes = list(size = 1 ) ))
