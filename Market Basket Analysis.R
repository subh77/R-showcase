rm(list=ls())
# Load the libraries
#install.packages("arules")
library(arules)
#library(arulesViz)
#install.packages("datasets")
library(datasets)
library(arulesViz)

# Load the data set
data(Groceries)
?Groceries
## Lets explore the data before we make any rules:

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

## We will always have to pass the minimum required support and confidence.
## We set the minimum support to 0.001
## We set the minimum confidence of 0.8
## We then show the top 5 rules

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, 
                                             conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])

## output
## lhs                        rhs             support      confidence  lift
## 1 {liquor,red/blush wine} => {bottled beer}  0.0019       0.90        11.2
## 2 {curd,cereals}          => {whole milk}    0.0010       0.91        3.6
## 3 {yogurt,cereals}        => {whole milk}    0.0017       0.81        3.2
## 4 {butter,jam}            => {whole milk}    0.0010       0.83        3.3
## 5 {soups,bottled beer}    => {whole milk}    0.0011       0.92        3.6

## if someone buys yogurt and cereals, they are 81% likely to buy whole milk too.

## The number of rules generated: 410
## The distribution of rules by length: Most rules are 4 items long
## The summary of quality measures: interesting to see ranges of support, lift, and 
## confidence.
## The information on the data mined: total data mined, and minimum parameters.

summary(rules)

# set of 410 rules
# 
# rule length distribution (lhs + rhs): sizes
# 3   4   5   6 
# 29 229 140  12 

# summary of quality measures:
#   support     conf.           lift     
# Min.   :0.00102     Min.   :0.80    Min.   : 3.1  
# 1st Qu.:0.00102     1st Qu.:0.83    1st Qu.: 3.3  
# Median :0.00122     Median :0.85    Median : 3.6  
# Mean   :0.00125     Mean   :0.87    Mean   : 4.0  
# 3rd Qu.:0.00132     3rd Qu.:0.91    3rd Qu.: 4.3  
# Max.   :0.00315     Max.   :1.00    Max.   :11.2  
# 
# mining info:
#   data      n      support   confidence
# Groceries      9835   0.001     0.8

rules<-sort(rules, by="confidence", decreasing=TRUE)
options(digits=2)
inspect(rules[1:5])

## we will want the most relevant rules first. Lets say we wanted to have the most 
## likely rules. We can easily sort by confidence by executing the following code.

# lhs                                            rhs            support  conf.  lift
# 1 {rice,sugar}                                => {whole milk}   0.0012   1      3.9
# 2 {canned fish,hygiene articles}              => {whole milk}   0.0011   1      3.9
# 3 {root vegetables,butter,rice}               => {whole milk}   0.0010   1      3.9
# 4 {root vegetables,whipped/sour cream,flour}  => {whole milk}   0.0017   1      3.9
# 5 {butter,soft cheese,domestic eggs}          => {whole milk}   0.0010   1      3.9

## Rule 4 is perhaps excessively long. Lets say you wanted more concise rules. 
## That is also easy to do by adding a "maxlen" parameter to your apriori function:

rules <- apriori(Groceries, 
                 parameter = list(supp = 0.001, 
                                  conf = 0.8,maxlen=3))

options(digits=2)
inspect(rules[1:5])

## Sometimes, rules will repeat. Redundancy indicates that one item might be a given.
## Alternatively, you can remove redundant rules generated.

rules <- apriori(Groceries, parameter = list(supp = 0.001, 
                                             conf = 0.8))


subset.matrix <- is.subset(rules, rules)
class(subset.matrix)
subset.matrix[lower.tri(subset.matrix, diag=T)] = 0
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]

# rules<-rules.pruned

rules
rules.pruned


options(digits=2)
inspect(rules[1:5])

# we wanted to target items to generate rules. There are two types of targets we 
# might be interested in that are illustrated with an example of "whole milk":
# 1. What are customers likely to buy before buying whole milk
# 2. What are customers likely to buy if they purchase whole milk?
# This essentially means we want to set either the Left Hand Side and Right Hand 
# Side.

rules<-apriori(data=Groceries, parameter=list(supp=0.001,
                                              conf = 0.08),
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))

# rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
#                appearance = list(default="lhs",rhs="whole milk")
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# lhs                                              rhs          supp.   conf.  lift
# 1 {rice,sugar}                                 => {whole milk}  0.0012   1     3.9
# 2 {canned fish,hygiene articles}               => {whole milk}  0.0011   1     3.9
# 3 {root vegetables,butter,rice}                => {whole milk}  0.0010   1     3.9
# 4 {root vegetables,whipped/sour cream,flour}   => {whole milk}  0.0017   1     3.9
# 5 {butter,soft cheese, domestic eggs}          => {whole milk}  0.0010   1     3.9

# Likewise, we can set the left hand side to be "whole milk" and find its antecedents.
# Note the following:
# We set the confidence to 0.15 since we get no rules with 0.8
# We set a minimum length of 2 to avoid empty left hand side items

rules<-apriori(data=Groceries, 
               parameter=list(supp=0.001,conf = 0.15,minlen=2),
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))

# rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2),
#                appearance = list(default="rhs",lhs="whole milk"))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

# lhs             rhs                support confidence lift
# 1 {whole milk} => {other vegetables}   0.075       0.29  1.5
# 2 {whole milk} => {rolls/buns}         0.057       0.22  1.2
# 3 {whole milk} => {yogurt}             0.056       0.22  1.6
# 4 {whole milk} => {root vegetables}    0.049       0.19  1.8
# 5 {whole milk} => {tropical fruit}     0.042       0.17  1.6
# 6 {whole milk} => {soda}               0.040       0.16  0.9

plot(rules, method = "graph", interactive = TRUE, shading = NA)