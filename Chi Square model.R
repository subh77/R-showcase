install.packages("ISLR")
install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)
?Carseats
View(Carseats)

rm(list=ls())

summary(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes")

## we use the data.frame() function to merge High with
View(Carsets)
Carseats <- data.frame(Carseats , High)

## We now use the tree() function to
### Sales. The syntax of the tree()

tree.carseats = tree(High~ .-Sales, Carseats)
tree.carseats

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty = 0)

tree.carseats

set.seed(2)
train=sample (1: nrow(Carseats), 320)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats =tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type = "class")
table(tree.pred, High.test)

(86+57) /200
(40 + 26) / 80

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)

cv.carseats$size
cv.carseats$dev

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

prune.carseats=prune.misclass(tree.carseats, best = 9)
par(mfrow=c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty=0)

tree.pred=predict(prune.carseats,Carseats.test, type = "class")
table(tree.pred, High.test)