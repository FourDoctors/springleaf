load("kld_num_train_df.Rdata")
df<-kld_num_train_df
 ##Select informative numeric variables.Informative=high entropy per value or high kld
df<-df[!((df$entropy/log(df$distinct)*log(2))<0.05&df$kld<0.05),]

##Classify as factor numeric variables with less than 10 distinct values. I add another 
##column to df called df$datatype

df$datatype[df$distinct<10]<-'factor'
df$datatype[is.na(df$datatype)]<-'numeric'
