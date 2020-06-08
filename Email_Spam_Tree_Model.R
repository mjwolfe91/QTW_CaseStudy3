library(rpart)

email.df = data.frame(emailDFrp)

set.seed(567)
n = nrow(email.df)
train = sort(sample(1:n, floor(n/2)))

email.train = email.df[train,]
email.test = email.df[-train,]

email.rp = rpart(isSpam ~ . ,                         
                data = email.df,                       
                subset = train,                       
                method = "class",                     
                parms = list(split = "information"),  
                maxsurrogate = 0,                     
                cp = 0,                               
                minsplit = 5,                         
                minbucket = 2)

summary(email.rp)
plot(email.rp, 
     uniform=TRUE,
     compress=TRUE,
     margin = .2)
text(email.rp, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE)


colours = apply(matrix(email.df[,"isSpam"]), 
                 1,
                 function(x){if (x=="Setosa") 
                   1 else 
                     if (x == "Versicolor")
                       2 else 3}
)

colours <-  c("red", "green3", "blue")[colours]

plot(iris.df[train,"PetalWidth"],iris.df[train,"PetalLength"], 
     col = colours[train], main = "Recursive partitoning regions")
lines(x=c(0,2.5), y = c(2.45,2.45), lty = 1)
lines(x=c(1.65,1.65), y = c(2.45,7), lty = 2)
lines(x=c(0,1.65), y = c(4.96,4.95), lty = 3)


pred.rp = predict(email.rp,
                   newdata = email.df[-train,],
                   type = "class")
pred.rp

predict(email.rp, 
        newdata = email.df[-train,],
        type = "prob")

predict(email.rp, 
        newdata = email.df[-train,],
        type = "vector")

predict(email.rp, 
        newdata = email.df[-train,],
        type = "matrix")


table(email.df$isSpam[-train], pred.rp)

printcp(email.rp)
plotcp(email.rp)

email.rp <- prune(email.rp, cp = 0.1)

plot(email.rp, 
     compress=TRUE,
     margin = .2)
text(email.rp, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE)
