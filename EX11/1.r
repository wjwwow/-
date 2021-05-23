library("ggplot2")
library("dplyr")
library("tidyr")
library("do")
library("lubridate")
setwd("C:\\data")
data<-read.csv("data.csv")
#取一到四节的投篮数据
data<-data %>% subset(period %in% c(1:4))
#将每节分成三个时间段
data$time<-3-data$minutes_remaining%/%4
data$period<-as.factor(data$period)
data$playoffs<-as.factor(data$playoffs)

theme_replace(plot.title=element_text(hjust = 0.5),
              plot.background=element_rect(),
              panel.grid=element_blank(),
              panel.border=element_rect(fill=NA))
#投篮次数
zs<-data %>% drop_na(shot_made_flag) %>% group_by(time,period) %>% summarise(count=n()) 
  ggplot(zs,aes(x=time,y=count,fill=period))+
           geom_bar(stat = "identity",position = "dodge")+
  labs(x="时间段",y="投篮次数",title="投篮次数图",fill="节数")
#投篮命中率
mz<-data %>% drop_na(shot_made_flag) %>% subset(shot_made_flag==1) %>%
  group_by(time,period) %>%summarise(count=n()) 
mz$count=mz$count/zs$count
ggplot(mz,aes(x=time,y=count,fill=period))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="时间段",y="投篮命中率",title="投篮命中率图",fill="节数")
#投篮的类别
lb<-data %>% drop_na(shot_made_flag) %>% 
  group_by(time,period,shot_type) %>%summarise(count=n()) %>%
  unite("t_p", time, period) 
ggplot(lb,aes(x=t_p,y=count,fill=shot_type))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="时间段_节数",y="投篮次数",title="投篮类别次数图",fill="类别")
#投篮类别下的命中率
lbmz<-data %>% drop_na(shot_made_flag) %>% subset(shot_made_flag==1) %>%
  group_by(time,period,shot_type) %>%summarise(count=n()) %>%
  unite("t_p", time, period) 
lbmz$count=lbmz$count/lb$count
ggplot(lbmz,aes(x=t_p,y=count,fill=shot_type))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="时间段_节数",y="投篮命中率",title="投篮类别命中率图",fill="类别")
#左侧投篮、右侧投篮、中间位置投篮的次数
data[data=="Left Side Center(LC)"]="Left Side(L)"
data[data=="Right Side Center(RC)"]="Right Side(R)"
data[data=="Back Court(BC)"]="Center(C)"
wz<-data %>% drop_na(shot_made_flag) %>% 
  group_by(time,period,shot_zone_area) %>%summarise(count=n()) %>%
  unite("t_p", time, period) 
ggplot(wz,aes(x=t_p,y=count,fill=shot_zone_area))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="时间段_节数",y="投篮次数",title="投篮位置次数图",fill="位置")
#左侧投篮、右侧投篮、中间位置投篮的命中率
wzmz<-data %>% drop_na(shot_made_flag) %>% subset(shot_made_flag==1) %>%
  group_by(time,period,shot_zone_area) %>%summarise(count=n()) %>%
  unite("t_p", time, period) 
wzmz$count=wzmz$count/wz$count
ggplot(wzmz,aes(x=t_p,y=count,fill=shot_zone_area))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="时间段_节数",y="投篮命中率",title="投篮位置命中率图",fill="位置")
#把投篮距离分为近距离、中距离和远距离，分别判断投篮的次数差异
new_data<-data %>%
  Replace(from=c('24\\+ ft.',"Back Court Shot"),to="远距离") %>%
Replace(from=c("16-24 ft.","8-16 ft."),to="中距离") %>%
  Replace(from="Less Than 8 ft.",to="近距离")
jl<-new_data %>% drop_na(shot_made_flag) %>% 
  group_by(time,period,shot_zone_range) %>%summarise(count=n()) %>%
  unite("t_p", time, period) 
ggplot(jl,aes(x=t_p,y=count,fill=shot_zone_range))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="时间段_节数",y="投篮次数",title="投篮距离次数图",fill="距离")
#命中率
jlmz<-new_data %>% drop_na(shot_made_flag) %>% subset(shot_made_flag==1) %>%
  group_by(time,period,shot_zone_range) %>%summarise(count=n()) %>%
  unite("t_p", time, period) 
jlmz$count=jlmz$count/jl$count
ggplot(jlmz,aes(x=t_p,y=count,fill=shot_zone_range))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="时间段_节数",y="投篮命中率",title="投篮距离命中率图",fill="距离")
#主客场表现的差异

#请分析科比在每节的最后2分钟投篮次数与命中率的差异
cs<-data %>% subset(minutes_remaining<=2) %>%
  group_by(period) %>%summarise(count=n())  
ggplot(cs,aes(x=period,y=count))+geom_bar(stat = "identity",fill = 'pink',width = 0.7)+
  labs(x="节数",y="投篮次数",title="最后投篮次数图")+
  geom_text(aes(label=count,),size = 3,vjust=-1)

csmz<-data %>% subset(minutes_remaining<=2) %>% subset(shot_made_flag==1) %>%
  group_by(period) %>%summarise(count=n())  
csmz$count=csmz$count/cs$count  
ggplot(csmz,aes(x=period,y=count))+geom_bar(stat = "identity",fill = 'pink',width = 0.7)+
  labs(x="节数",y="投篮命中率",title="最后投篮命中率图")+
  geom_text(aes(label=count,),size = 3,vjust=-1)

#越是有水平的球员，在关键比赛中发挥越是稳定。请比较科比在
#常规赛和季后赛得分情况和命中率对比。以及常规赛和季后赛的
#最后2分钟投篮次数与命中率的差异
bs<-data %>% group_by(period,playoffs) %>%summarise(count=n()) 
  ggplot(bs,aes(x=period,y=count,fill=playoffs))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="节数",y="投篮次数",title="投篮次数图",fill="是否季后赛")
bsmz<-data %>% subset(shot_made_flag==1) %>% 
  group_by(period,playoffs) %>%summarise(count=n())
bsmz$count=bsmz$count/bs$count  
ggplot(bsmz,aes(x=period,y=count,fill=playoffs))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="节数",y="投篮命中率",title="投篮命中率图",fill="是否季后赛")

zh<-data %>% subset(minutes_remaining<=2) %>%
  group_by(period,playoffs) %>%summarise(count=n())  
ggplot(zh,aes(x=period,y=count,fill=playoffs))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="节数",y="投篮次数",title="最后投篮次数图",fill="是否季后赛")
zhmz<-data %>% subset(minutes_remaining<=2) %>%  subset(shot_made_flag==1) %>%
  group_by(period,playoffs) %>%summarise(count=n())  
zhmz$count=zhmz$count/zh$count  
ggplot(zhmz,aes(x=period,y=count,fill=playoffs))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="节数",y="投篮命中率",title="最后投篮命中率图",fill="是否季后赛")

#分析科比在联盟中打球以来
#• 每年投篮的变化趋势（次数，投篮区域，投篮类别，扣篮次数等）
data$year=year(data$game_date)
y_cs<-data %>% group_by(year) %>%summarise(count=n())  
  ggplot(y_cs,aes(x=year,y=count))+
  geom_bar(stat = "identity",fill="pink",width = 0.7)+
  labs(x="年份",y="投篮次数",title="年份投篮图")

new_data$year=year(new_data$game_date)
jl<-new_data %>% drop_na(shot_made_flag) %>% 
  group_by(year,shot_zone_range) %>%summarise(count=n()) 
ggplot(jl,aes(x=year,y=count,fill=shot_zone_range))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="年份",y="投篮次数",title="投篮距离次数图",fill="距离")

lb<-data %>% drop_na(shot_made_flag) %>% 
  group_by(year,shot_type) %>%summarise(count=n())
ggplot(lb,aes(x=year,y=count,fill=shot_type))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="年份",y="投篮次数",title="投篮类别次数图",fill="类别")

data %>% drop_na(shot_made_flag) %>% subset(combined_shot_type=="Dunk") %>%
  group_by(year) %>%summarise(count=n()) %>%
  ggplot(aes(x=year,y=count))+
  geom_bar(stat = "identity",fill="pink",width = 0.7)+
  labs(x="年份",y="投篮次数",title="扣篮次数图")

y_mz<-data %>% subset(shot_made_flag==1) %>% 
  group_by(year) %>%summarise(count=n())
y_mz$count=y_mz$count/y_cs$count
ggplot(y_mz,aes(x=year,y=count))+
  geom_bar(stat = "identity",fill="pink",width = 0.7)+
  labs(x="年份",y="投篮命中率",title="扣篮命中率图")