rm(list=ls())
library(rio)
# GPA Data n=50
gpa50=import("Module 3 Data Sets.xlsx",
             sheet="GPAs 50 Stacked")
colnames(gpa50)=tolower(make.names(colnames(gpa50)))
attach(gpa50)
t.test(gpas50,mu=2.1,alternative = c("two.sided"))
results=t.test(gpas50,mu=2.1,alternative = c("two.sided"))
results
names(results)
results$p.value
results$conf.int
results$conf.int[1]
results$conf.int[2]
# IQ Data, Independent Sampling
iq=import("Module 3 Data Sets.xlsx",sheet="IQ")
colnames(iq)=tolower(make.names(colnames(iq)))
attach(iq)
results=t.test(age.25,age.60,mu=0,
               alternative = c("two.sided"))
results
boxplot(iq$age.25,iq$age.60,col="red",main="IQ Boxplot")
boxplot(iq$age.25,iq$age.60,notch=TRUE,col="red",
        main="IQ Notched Boxplot")
# Rat Pups Data, Paired Comparison
rats=import("Module 3 Data Sets.xlsx",sheet="Rat Pups")
colnames(rats)=tolower(make.names(colnames(rats)))
attach(rats)
results=t.test(male,female,mu=0,
               alternative=c("two.sided"),paired=TRUE)
results
boxplot(male,female,notch=TRUE,col="red",
        main="Rat Pups Boxplot")
# Grocery Data
grocery=import("Module 3 Data Sets.xlsx",
               sheet="Grocery")
colnames(grocery)=tolower(make.names(colnames(grocery)))
names(grocery)
str(grocery)
levels(grocery$division)
grocery$division=as.factor(grocery$division)
str(grocery)
levels(grocery$division)
fairview=subset(grocery,
                division=="Fairview")
summerfield=subset(grocery,division=="Summerfield")
set.seed(99)
my.fairview=fairview[sample(1:nrow(fairview),18,
                            replace=FALSE),]
my.summerfield=summerfield[sample(1:nrow(summerfield),15,
                                  replace=FALSE),]
t.test(my.fairview$customer.penetration,mu=.2,
       alternative=c("greater"))
t.test(my.fairview$customer.penetration,mu=.25,
       alternative=c("greater"))
t.test(my.summerfield$deli.sales,my.fairview$deli.sales,
       mu=0,alternative=c("two.sided"))
boxplot(my.summerfield$deli.sales,my.fairview$deli.sales,        col=c("red","blue"),names=c("Summerfield","Fairview"),
        main="Summerfield and Fairview Divisions, Deli Sales")
boxplot(my.summerfield$deli.sales,my.fairview$deli.sales,
        col=c("red","blue"),names=c("Summerfield","Fairview"),
        main="Summerfield and Fairview Divisions, Deli Sales",
        notch = TRUE)
