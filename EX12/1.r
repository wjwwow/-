library("ggplot2")
library("dplyr")
library("tidyr")
library("cowplot")
library("ggthemes")
library("nnet")
setwd("C:\\data")
data=read.csv("titanic_data.csv")
data$Survived=as.character(data$Survived)
data$Pclass=as.character(data$Pclass)
data$Fare=as.numeric(data$Fare)


#男性乘客、女性乘客的比例/男性乘客、女性乘客生还的比例
data %>% group_by(Survived,Sex) %>% summarise(count=n()) %>%
  ggplot(aes(x=Sex,y=count,fill=Survived))+
  geom_bar(stat="identity",position = "fill")+
  labs(x="性别",y="比例",fill="是否存活",title="男女比例和存活比例")+
  theme_wsj()+scale_fill_wsj("rgby", "",breaks=c("0","1"),labels=c("未存活","存活"))+
  theme(plot.title = element_text(hjust = 0.5))
#舱位与生还的关系
data %>% group_by(Pclass,Survived) %>% summarise(count=n()) %>%
ggplot(aes(x=Pclass,y=count,fill=Survived))+
  geom_bar(stat="identity",position = "fill")+
  labs(x="舱位",y="比例",fill="是否存活",title="不同舱位的存活比例")+
  theme_wsj()+scale_fill_wsj("rgby", "",breaks=c("0","1"),labels=c("未存活","存活"))+
  theme(plot.title = element_text(hjust = 0.5))
#船票价格与生还的关系
data$Fare=data$Fare %>% cut(breaks=quantile(data$Fare),labels = c("低","较低","较高","高"),ordered_result = T)
data[is.na(data$Fare),]$Fare="低"
data %>% group_by(Fare=Fare,Survived) %>% summarise(count=n()) %>%
  ggplot(aes(x=Fare,y=count,fill=Survived))+
  geom_bar(stat="identity",position = "fill")+
  labs(x="票价",y="比例",fill="是否存活",title="不同票价的存活比例")+
  theme_wsj()+scale_fill_wsj("rgby", "",breaks=c("0","1"),labels=c("未存活","存活"))+
  theme(plot.title = element_text(hjust = 0.5))
 
#同行人数与生还的关系  
data$peer=data$SibSp+data$Parch
data %>% subset(peer<8) %>% group_by(peer,Survived) %>% summarise(count=n()) %>%
  ggplot(aes(x=peer,y=count,fill=Survived))+
  geom_bar(stat="identity",position = "fill")+
  labs(x="同行人数",y="比例",fill="是否存活",title="不同同行人数的存活比例")+
  theme_wsj()+scale_fill_wsj("rgby", "",breaks=c("0","1"),labels=c("未存活","存活"))+
  theme(plot.title = element_text(hjust = 0.5))
#船票价格与年龄、同行人数的关系

data %>% subset(peer<8) %>% group_by(peer,Fare)%>% summarise(count=n())%>%
  ggplot(aes(x=peer,y=count,fill=Fare))+
  geom_bar(stat="identity",position = "fill")+
  labs(x="同行人数",y="比例",fill="船票价格",title="不同同行人数的船票价格比例")+
  theme_wsj()+scale_fill_wsj("rgby", "")+
  theme(plot.title = element_text(hjust = 0.5))

a_data=data %>% drop_na(Age)
a_data$Age=cut(a_data$Age,breaks = quantile(a_data$Age),labels=c("幼年","青年","中年","老年"),ordered_result = T)
a_data[is.na(a_data$Age),]$Age="幼年"
a_data %>% group_by(Age,Fare)%>% summarise(count=n())%>%
  ggplot(aes(x=Age,y=count,fill=Fare))+
  geom_bar(stat="identity",position = "fill")+
  labs(x="年龄",y="比例",fill="船票价格",title="不同年龄的船票价格比例")+
  theme_wsj()+scale_fill_wsj("rgby", "")+
  theme(plot.title = element_text(hjust = 0.5))
 #问题二 
#绘图
data %>% drop_na(Age) %>%  mutate(cl = ifelse(Age <18, "未成年", "成年")) %>%
  group_by(Sex,cl,Survived) %>% summarise(n=n()) %>%
  ggplot(aes(y=n,fill=Survived))+
  geom_bar(aes(x=Sex),stat="identity",position = 'fill')+
  geom_bar(aes(x=cl),stat="identity",position = 'fill')+
  labs(x="年龄和性别",y="数量",fill="是否存活",title="是否小孩和女士优先")+
  theme_wsj()+scale_fill_wsj("rgby", "",breaks=c("0","1"),labels=c("未存活","存活"))+
  scale_x_discrete(breaks=c("female","male","未成年", "成年"),labels=c("女性","男性","未成年", "成年"))+
  theme(plot.title = element_text(hjust = 0.5))
#回归
t_d=data %>% drop_na(Age) %>%  mutate(cl = ifelse(Age <18, "未成年", "成年")) %>%
  group_by(Sex,cl,Survived) 
t_d$Survived=factor(t_d$Survived,levels = c(0,1),labels = c("未存活","存活"))
mx=glm(Survived~cl+Sex,family=binomial(),data=t_d)
summary(mx)

#问题三
new_data=data %>% drop_na()
new_data$Survived=factor(new_data$Survived,levels = c(0,1),labels = c("未存活","存活"))
n_mx=glm(Survived~peer+Fare+Embarked+Pclass+Age+Sex,family=binomial(),data=new_data)
summary(n_mx)
n_mx2=glm(Survived~peer+Pclass+Age+Sex,family=binomial(),data=new_data)
summary(n_mx2)

#年龄和性别与社会地位的关系
p_d=data %>% drop_na(Age) %>% group_by(Age,Sex,Pclass) 
p_d$Pclass=factor(p_d$Pclass,levels = c(1,2,3),labels = c("高","中","低"))
p_mx= multinom(Pclass~Age+Sex,data=p_d)
z = summary(p_mx)$coefficients/summary(p_mx)$standard.errors
p = (1-pnorm(abs(z),0,1))*2
p

a_data  %>% group_by(Age,Pclass)%>%summarise(n=n()) %>%
  ggplot(aes(x=Age,y=n,fill=Pclass))+
  geom_bar(stat="identity",position = "fill")+
  labs(x="年龄",y="比例",fill="社会地位",title="不同年龄的社会地位比例")+
  theme_wsj()+scale_fill_wsj("rgby", "",breaks=c(1,2,3),labels=c("高","中","低"))+
  theme(plot.title = element_text(hjust = 0.5))

data %>% group_by(Sex,Pclass)%>%summarise(n=n()) %>% 
  ggplot(aes(x=Sex,y=n,fill=Pclass))+
  geom_bar(stat="identity",position = "fill")+
  labs(x="性别",y="比例",fill="社会地位",title="不同性别的社会地位比例")+
  theme_wsj()+scale_fill_wsj("rgby", "",breaks=c(1,2,3),labels=c("高","中","低"))+
  theme(plot.title = element_text(hjust = 0.5))
