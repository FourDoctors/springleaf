## ----dataload------------------------------------------------------------
prjpath <- "~/work/learn/competitions/kaggle/springleaf/"
datapath <- paste(prjpath, "data/", sep="")
analpath <- paste(prjpath, "analyses/", sep="")
rcodepath <- paste(analpath, "Rcode/", sep="")
#if the current directory is already at analpath 
#it gives error, check ways to remove that 
setwd(analpath)
train <- read.csv(paste(datapath, 'train_labeled.csv', sep=""),
                  stringsAsFactor=FALSE)
test <- read.csv(paste(datapath, 'test_labeled.csv', sep=""),
                 stringsAsFactor=FALSE)
install.packages("simsalapar")
##-----
#Mahalabonis distance 
#Load only the numerical values 
#Using as reference 
#http://www.r-bloggers.com/mahalanobis-distance-with-r-exercice/
##-----
#Choose the ones with numeric columns 
train_numeric_only = train[,grep("^numeric", names(train), value=TRUE)]
#means 
x <- train_numeric_only
mean <-colMeans(x)
#covariance matrix
Sx <- cov(x)
D2 <- mahalanobis(x, mean, Sx)

#plot as histogram mahalanobis distance 