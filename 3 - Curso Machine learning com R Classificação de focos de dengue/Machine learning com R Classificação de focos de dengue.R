library(DAAG)

#data(dengue)
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
