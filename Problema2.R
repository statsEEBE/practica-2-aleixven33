#Codigo para problema 2
mis_dades <- iris
mis_dades
dim(mis_dades)
names(mis_dades)

mis_dades$Petal.Length
mean(mis_dades$Petal.Length)
sd(mis_dades$Petal.Length)
hist(mis_dades$Petal.Length)

x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length
plot(x, y)

m <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2) #mean = mitjana
m
b <- mean(y)-m*mean(x)
b

m*1.5+b

mod <- lm(y~x)
summary(mod)

#en todos los valores de x
predict(mod,data.frame(x=x))
ypredict<- predict(mod,data.frame(x=x))
plot(x,y,col='red',pch=16)
lines(x,ypredict,col='black')

#coeficiente de determinación
rsq <- sum((ypredict-mean(y))^2)/sum((y-mean(y))^2)
rsq

summary(mod)
+
