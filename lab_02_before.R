# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK

# lab 2

# загружаем пакеты
library("memisc") # две и более регрессий в одной табличке
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тестирование гипотез в линейных моделях
library("sjPlot") # графики
library("sgof")
library("ggplot2") # графики
library("foreign") # загрузка данных в разных форматах
library("car")
library("hexbin") # графики
library("rlms") # загрузка данных в формате rlms (spss)
library(reshape2) # манипуляции с данными
library(rms) 
library(memisc) # Таблицы (mtable)
library(xtable) # Таблицы (xtable)
library(texreg) # Таблицы (texreg)
library(tseries) # Jarque-Bera test
library(ResourceSelection) # Hosmer–Lemeshow test
library(lmtest) # LR-test
library(stats) # LR-test
library(aod) # Wald Test
library(BaylorEdPsych) # McFadden
library(mfx) # подсчет предельных эффектов
library(foreign) # чтение файлов в некоторых форматах 
library(texreg) # обработка результатов регрессии
library(ggplot2)
library(vcd)
library(ggplot2) # Графики
library(ggthemes) # Темы графиков
library(GGally) # Графики +
library(ggfortify) # 
library(cluster)
library(plm)


m<-readRDS("deep.rds") #мой набор данных

DEEPISHE<-read.spss("deep.sav")  #набор сав

deep$Birth<-DEEPISHE$Q92 # из сава в мой набор можно перекинуть так

deep<-cbind(deep, DEEPISHE$нас.пункт) #или так, но тогда название не изменится
summary(m$Alcohol)

names (deep) # все столбцы из моего набора

table(deep$City)

barplot(summary(na.omit(deep$Sex)), main="Количество мужчин и женщин в опросе",
        xlab="Пол", ylab="Количество",
        col=c("blue", "red")) # столбчатая диаграмма

barplot(summary(na.omit(deep$AgeFac)), main="Возраст респондентов",
        xlab="Возраст", ylab="Количество") #столбчатая диаграмма

barplot(summary(na.omit(m$Medic)), xlab="Отношение к алкоголю", ylab="Количество",
        col=c("#B0E0E6", "#87CEEB", "#87CEFF","#7EC0EE", "#6CA6CD")) #столбчатая диаграмма

barplot(summary(na.omit(m$AgeFac)), main="Возраст респондентов",
        xlab="Возраст", ylab="Количество") #столбчатая диаграмма

pie(summary(na.omit(m$Sex)), main="Количество мужчин и женщин в опросе",
    xlab="Пол", ylab="Количество",
    col=c("blue", "red"))

mosaic(data = deep, ~ Health+AgeFac+Sex, shade = TRUE,main="Оценка здоровья") #мозаичный график

deep$Birth<-mChoice(deep$Birth1, deep$Birth2) # объединяла столбцы чтоб сделать единый вопрос

deep$Birth2 <- as.character(deep$Birth2) # переименование столбцов1
deep$Birth2[deep$Birth2 == "да"] <- 0 #переименование столбцов2
deep$Birth1 <- as.factor(deep$Birth1) #переименование столбцов3
summary(Females) # проверка переимнования

deep$Visits<- ordered(deep$Visits, levels=c("каждую неделю", "1-2 раза/мес", "5-6 раз/год", 
                                            "3-4 раз/год", "1-2 раз/год", "не обращался")) # уровни по порядку (не по алфавиту), но тогда в таблицах криво показывает названия

counts <- table(deep$Sex, deep$AgeFac) # набор для объединенной толбчатой диагр возраст-пол
barplot(counts, col=c("#6495ED", "#FF1493"), axisnames=TRUE,  beside=TRUE) # сама диаграмма

dotchart(deep)

Females<-m[m$Sex == "женский"]

summary(m$Incom[m$Health == "Удовл"])
barplot(summary(na.omit(m$Health[m$Proph == "нет"])))
summary(m$Proph)

saveRDS(deep, "deep.rds") # после добавления каждого столбика сохраняю набор

aaaa<-read_excel("vybr.xlsx")
aaaa<-aaaa[1:75,]
aaaat<-t(aaaa)
AAAA<-melt(aaaat)


all_disease <- read_excel("all_disease.xlsx", col_names = TRUE)
all_vybr <- read_excel("все выбросы для евьюс1.xlsx")
all_disease1 <-plm.data(all_disease, 75)
all_vybr1 <- plm.data(all_vybr, 75)
vyb<-all_disease[all_disease$year == "2008", "s89"]
write.csv(vyb, "s89.csv")

ggplot(all_disease1, aes(year, s56-s64, group=Clust)) + geom_point()



Cluster <- kmeans((all_disease1[all_disease1$year == "2008",17]), 8, nstart = 20)
Cluster$cluster

clusters <- hclust(dist(all_disease[all_disease$year == "2008",17]))
plot(clusters)
clusters

clusterCut <- cutree(clusters, 8)
clust_t<-table(clusterCut,  na.omit(all_disease1$r2))
clust_t<-data.frame(clust_t)
clust<-(clust_t$clusterCut[clust_t$Freq == 1])
clust

clust1<-rep(clust_t$clusterCut[clust_t$Freq == 1],each = 13)


ggplot(all_disease1, aes(year, s56-s64, color = r1)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut)# +
  scale_color_manual(values = c('black', 'red', 'green'))
  
  coplot(s56-s64 ~ year|r1, type="l", data=all_disease]) # Lines
  coplot(s56-s64 ~ year|r1, type="b", data=all_disease) # Points and lines

source("https://raw.github.com/talgalili/R-code-snippets/master/clustergram.r")
  
  data(iris)
  set.seed(250)
  par(cex.lab = 1.5, cex.main = 1.2)
  Data <- scale(all_disease[,13]) # notice I am scaling the vectors)
  clustergram(Data, k.range = 2:8, line.width = 0.004) # notice how I am using line.width.  Play with it on your problem, according to the scale of Y.
  
  
    
write.csv(AAAA, "vybrosy.csv", sep="\t")
