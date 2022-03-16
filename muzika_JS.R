data <- read.csv("abcChurn.csv", stringsAsFactors = FALSE)
summary(data)
sum(is.na(data))
#nema nedostajucih numericke 
# brisem kolone X jer je samo redni broj opservacije, 
#i id jer se ne mogu izvuci bitni podaci
data$X <- NULL
data$id<- NULL
data$pol <- factor(data$pol, levels = c("male","female"))
#muskarci 1, zene 2


#preabcivanje int promentljivih vezanih za datum u char i u datum
data$datumLogovanja <- as.character(data$datumLogovanja)
data$datumLogovanja <- as.Date(data$datumLogovanja, format="%Y%m%d")

data$datum_registrovanja <- as.character(data$datum_registrovanja)
data$datum_registrovanja <- as.Date(data$datum_registrovanja, format="%Y%m%d")

data$datumTransakcije <- as.character(data$datumTransakcije)
data$datumTransakcije <- as.Date(data$datumTransakcije, format="%Y%m%d")

data$datumIsticanjaClanstva <- as.character(data$datumIsticanjaClanstva)
data$datumIsticanjaClanstva <- as.Date(data$datumIsticanjaClanstva, format="%Y%m%d")

length(which(data$grad == " " |data$grad == "" | data$grad == "Na" | 
               data$grad == "Missing"))
length(which(is.na(data$grad)))
#nema praznog stringa za grad

unique(data$grad)
table(data$grad)



evropa <- c("Beograd", "Budmipesta", "Ljubljana", "London", "Monako", "Oslo",
            "Prag", "Rim", "San Marino", "Talin")

neevropa <- c("Doha", "Havana","Karakas", "Male", "Nju Delhi", "Peking", 
              "Rio de Zaneiro", "Sangaj", "Seul", "Singapur", "Tokio")

data$grad[ data$grad %in% evropa ] <- "Evropa"
data$grad[ data$grad %in% neevropa ] <- "VanEvrope"

length(which(data$grad == " " |data$grad == "" | data$grad == "Na" | 
               data$grad == "Missing"))
length(which(is.na(data$grad)))




data$grad <- factor(data$grad, levels = c("Evropa", "VanEvrope")) #### PROBLEM
#evropa 1 , van 2

sum(is.na(data$grad))


sum(is.na(data$pol))
table(data$pol)
bezpola <- which(is.na(data$pol))
data1 <- data[-bezpola,] 
sum(is.na(data1))
#

sum(is.na(data1))

summary(data1)
table(data1$metod_placanja) # Bilo u 53 redu koda, prebaceno

library(ggplot2)
ggplot(data=data1) +
    geom_bar(mapping = aes(x = pol, fill = grad), position="fill")

sum(ifelse(test = data1$cena_izabranog_plana == data1$uplaceno,
       yes = 0,
       no = 1))
which(data1$cena_izabranog_plana != data1$uplaceno)
#preklapanje kolone cena izabranog plana i uplaceno je 772 red, mozemo jednu obrisati 
data1$uplaceno <- NULL
table(data1$cena_izabranog_plana)




data1$god <- cut(data1$godine, breaks = c(0,23, 27, 33, Inf),
                    labels = c("jako mladi", "mladi","mladji odrasli", "odrasli"))
novi <- data1
data1$godine <- NULL

library(ggplot2)
ggplot(data=novi) +
  geom_bar(mapping = aes(x = god, fill = pol), position="fill")


library(corrplot)

nenumericke <- c(1,9,10,12,17,18,20)
m <- cor(data1[,-nenumericke])
corrplot.mixed(m, tl.cex=0.75, number.cex=0.75)
summary(data1$godine)

summary(data1$num50)
ggplot(data1 , aes(x = ukupno_vreme)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Broj korisnika") + 
  xlab("Godine korisnika") +  scale_x_continuous(breaks = seq(0,10,1)) 



table(data$godine)
summary(data$godine)


table(data$metod_placanja)
table(data1$clansto)
#clanstvo i izabran plan visoko kore 0.96 
data1$cena_izabranog_plana <- NULL
nenume <- c(1,9,10,12,16,17,19)
a <- cor(data1[,-nenume])
corrplot.mixed(a, tl.cex=0.75, number.cex=0.75)
corrplot.mixed(a, tl.cex=0.75, number.cex=0.75)


ggplot(data1)+geom_density(aes(x=num_25, color = 'yellow')) +
  geom_density(aes(x=num50, color = 'red')) +
  geom_density(aes(x=num_75, color = 'blue'))+
  geom_density(aes(x=num_75, color = 'green'))
summary(data1$num50)
boxplot(data1$num_25)
length(boxplot.stats(data1$num_25)$out)


#broj pesama koje se preslusao
data1$pesmeukupno <- data1$num_25 + data1$num50 + data1$num_75 +data1$num_985 + data1$num_100
sum(is.na(data1$pesmeukupno))
data1$num_25 <-NULL
data1$num50 <-NULL
data1$num_75 <-NULL
data1$num_985 <-NULL
data1$num_100<-NULL
length(boxplot.stats(data1$pesmeukupno)$out)



data1$datumreg<- difftime(data1$datumIsticanjaClanstva ,data1$datum_registrovanja , units = c("days"))
data1$datumreg <-as.numeric(data1$datumreg)



df<-data1

# razlika izmedju dana logovanja, refist, placanja u osnosu na registraciju
df$datumLogovanja<-NULL
df$datum_registrovanja<-NULL
df$datumIsticanjaClanstva<-NULL
df$datumTransakcije<-NULL

df$izlazna <- as.factor(df$churn)
df$churn<-NULL

table(data$churn)

df$metod_placanja <- as.factor(df$metod_placanja)
df$metod_registrovanja <- as.factor(df$metod_registrovanja)
df$autoProduzetak<-as.factor(df$autoProduzetak)



#STABLA

library(caret)

set.seed(4)
train.indices <- createDataPartition(df$izlazna, # the variable defining the class
                                     p = .80,            # the proportion of observations in the training set
                                     list = FALSE)       # do not return the result as a list (which is the default)
train.data <- df[train.indices,]
test.data <- df[-train.indices,]

library(rpart)
library(e1071)

numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by = 0.0025)) 

set.seed(4)

dt.cv <- train(x = train.data[,-14], 
               y = train.data$izlazna, 
               method = "rpart", 
               trControl = numFolds, 
               tuneGrid = cpGrid)
dt.cv

optimal_cp <- dt.cv$bestTune$cp
set.seed(4)
tree3 <- rpart(izlazna ~ ., data = train.data, method = "class",
               control = rpart.control(cp = optimal_cp))

library(rpart.plot)
s<-rpart.plot(tree3)
s



compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

tree3.pred <- predict(tree3, newdata = test.data, type = "class")
tree3.cm <- table(true = test.data$izlazna, predicted = tree3.pred)
tree3.cm
tree3.eval <- compute.eval.metrics(tree3.cm)
tree3.eval
library(rpart.plot)
rpart.plot(tree3)

table(df$izlazna)

#DATUMI , PROVERITI

####### KMEANS ########

km <-df
#AUTJALERI

library(DescTools)

boxplot(km$num_unq)

km$num_unq <- Winsorize(km$num_unq, 
                       probs = c(0,0.93)) 
boxplot(km$num_unq)



boxplot(km$ukupno_vreme)

km$ukupno_vreme <- Winsorize(km$ukupno_vreme, 
                        probs = c(0,0.91)) 
boxplot(km$ukupno_vreme)



boxplot(km$datumreg)
#nema aut




boxplot(km$clansto)

km$clansto <- Winsorize(km$clansto, 
                             probs = c(0.05,0.95)) 

boxplot(km$clansto)

summary(km$clansto)
km$clansto <- NULL
#PREVISE AUTL , SVI BUDU POSLE 30



km$grad<-as.numeric(km$grad)
km$pol <- as.numeric(km$pol)
km$metod_placanja<-NULL
table(km$metod_registrovanja)
summary(km$metod_registrovanja)
km$metod_registrovanja<-NULL
km$izlazna<-NULL
km$godin <- novi$godine
km$god<-NULL
km$metod_registrovanja <-NULL
km$autoProduzetak <-as.numeric(km$autoProduzetak)
#apply(X= km[,], MARGIN = 2, FUN = function(x)length(boxplot.stats(x)$out))

#plot(km$datumtra)
#summary(km$datumtra)
#boxplot(km$autoProduzetak)
#table(df$clansto)
#km$clansto <- df$clansto



normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}



# normalize all numeric variables from the retail.data dataset
normalizovaniKm<- as.data.frame(apply(km[,], 2, normalize.feature))

library(corrplot)
corr.matrix <- cor(normalizovaniKm)
corrplot.mixed(corr.matrix, tl.cex=0.75, number.cex=0.75)

normalizovaniKm$ukupno_vreme <-NULL
normalizovaniKm$num_unq <-NULL
normalizovaniKm$devet <-NULL

# provaj i sa datreg


eval.metrics <- NULL
eval.metrics <- data.frame()

# run kmeans for all K values in the range 2:8
for (k in 2:8) {
  set.seed(4)
  km.res <- kmeans(x=normalizovaniKm, centers=k, iter.max=10, nstart = 50)
  
  # combine cluster number and the error measure, write to df
  eval.metrics <- rbind(eval.metrics, 
                        c(k, km.res$tot.withinss, km.res$betweenss/km.res$totss)) 
}

# assign more meaningful column names
names(eval.metrics) <- c("k", "tot.within.ss", "ratio")

# print the evaluation metrics
eval.metrics
library(ggplot2)
# plot the line chart for K values vs. tot.within.ss 
ggplot(data=eval.metrics, aes(x=k, y=tot.within.ss)) + 
  geom_line() +
  geom_point() +
  labs(x = "\nK (cluster number)", 
       y = "Total Within Cluster Sum of Squares\n",
       title = "Reduction in error for different values of K\n") + 
  theme_bw() +
  scale_x_continuous(breaks=seq(from=0, to=8, by=1))
# 3 klastera
km1 <- kmeans(x=normalizovaniKm, centers=4, iter.max=10, nstart = 50)
km1$cluster
km1

install.packages("factoextra")
library(factoextra)
fviz_cluster(km1, data = normalizovaniKm, pallete = c("blue","red","pink"),geom = "point", eclipse.type = "convex", ggtheme = theme_bw())



#analiza prezivljavanja
install.packages(c("survival", "survminer"))

library(survival)
library(survminer)

prez <- df
prez$datumreg <-prez$datumreg/365 


df.ap <- data.frame(time = prez$datumreg,
                    event = prez$izlazna ==1, 0, 1,
                    group = prez$god)
model <- survfit(Surv(time,event)~group, data = df.ap)

ggsurvplot(model, pval = TRUE,conf.int = TRUE, risk.table = TRUE,
           risk.table.col = "strata", linetype = "strata",
           surv.median.line = "hv",
           ggtheme = theme_bw(), palette = c("#E7B800, #2E9FDF"))

