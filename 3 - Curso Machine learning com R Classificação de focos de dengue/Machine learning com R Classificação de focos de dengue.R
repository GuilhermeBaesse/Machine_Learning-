library(DAAG)
library(lattice)

names(dengue)
summary(dengue)
data(dengue)

mb<-glm(NoYes~humid, data=dengue, family=binomial)
summary(mb)

# p<.05 a variável é significativa 

library(ggplot2)
ggplot(dengue, aes(x=humid, y=NoYes)) + geom_point() + geom_smooth(method="glm",method.args=list(family='binomial'))


hm<-data.frame(humid=23)
predict(mb, hm, type='response')

mb2 <-glm(NoYes~humid+temp, data=dengue, family=binomial)
summary(mb2)

hm2<- data.frame(humid=23,temp=34)

predict(mb2, hm2, type='response')

xyplot(temp~humid, data=dengue, groups=NoYes)

set.seed(123)
library(caret) #para modelo
dengue2<-na.omit(dengue)

dataindex<-createDataPartition(dengue2$NoYes,p=.7, list=FALSE)
denguetreino<-dengue2[dataindex,] #separando treino
dengueteste<-dengue2[-dataindex,] #separando teste

maiscomum<-sum(denguetreino$NoYes==1)/(dim(denguetreino)[1])
maiscomum

if(maiscomum >= 0.5){
  print("classeMaisComum <- 1")
  classeMaisComum <-1
}else{
  print("classeMaisComum <- 0")
  classeMaisComum <- 0
}

taxadeacerto<-sum(dengueteste$NoYes==classeMaisComum)/(dim(dengueteste)[1])
taxadeacerto

library(rattle)
modelo<-train(as.factor(NoYes)~humid, data=denguetreino, method="rpart")
fancyRpartPlot(modelo$finalModel)

modelo2<-train(as.factor(NoYes)~temp, data=denguetreino, method="rpart")
fancyRpartPlot(modelo2$finalModel)

modelo3<-train(as.factor(NoYes)~trees, data=denguetreino, method="rpart")
fancyRpartPlot(modelo3$finalModel)

# modelo de classificação

modeloml1<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, metodh="glm")

modeloml2<-train(as.factor(NoYes)~humid+temp+trees, data=denguetreino, metodh="glm")
