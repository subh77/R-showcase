# Fitting Regression Trees

library (MASS)
set.seed(1)
?Boston
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)
ncol(Boston)

plot(tree.boston)
tree.boston
text(tree.boston, pretty = 0)

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

prune.boston = prune.tree(tree.boston, best = 7)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat = predict(tree.boston, newdata = Boston [-train, ])
boston.test = Boston [-train, "medv"]
mean((yhat - boston.test)^2)
sqrt(mean((yhat - boston.test)^2))

?Boston

