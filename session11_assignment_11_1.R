#session11_assignment_11.1

#1. Use the given link and locate the bank marketing dataset. Data Set Link 
#Perform the below operations: 

getwd()
p="C:/Users/Swapna/Documents"
setwd(p)

library(readr)

bank<-read_csv("C:/Users/Swapna/Documents/R files test/bank-additional.zip")
bankmark<- read_delim("C:/Users/Swapna/Documents/R files test/bank-additional-full.zip", ";",escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)
View(bankmark)



bankma<-read.csv("C:/Users/Swapna/Documents/R files test/bank-additional-full.csv", sep = ";", stringsAsFactors = TRUE)
View(bankma)

str(bankma)
head(bankma)

#a. Create a visual for representing missing values in the dataset. 

library(mice)
is.na(bankma)
md.pattern(bankma)

library(Amelia)
missmap(bankma,main="Missing Data - Bank ", col=c("red","grey"),legend=FALSE)

library(VIM)
bankma[bankma=="unknown"] <- NA #convert unknown as NA to find missing values
missvalueplot<-aggr(bankma,col=c('navyblue','red'), numbers=TRUE,sortVars=TRUE, labels=names(bankma), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#b. Show a distribution of clients based on a Job. 

count<-table(bankma$job)
barplot(count,col="orange",legend=rownames(count), main="job distribution")

par(oma=c(2,0,0,0))  #so labels are not cut off   
barplot(table(bankma$job),ylab = "Frequency", main = "Distribution of clients based on job",border="yellow", col="green",las=2)


plot(head(rnorm(bankma$job)))

table(bankma$job)
plot(table(bankma$job))
hist(table(bankma$job), col = "red")

#c. Check whether is there any relation between Job and Marital Status? 


table(bankma$job, bankma$marital)
with(bankma,chisq.test(job,marital)) # p value is less than 0.5, so no relationship between job and marital



#d. Check whether is there any association between Job and Education? 
cor(table(bankma$job, bankma$education), method = "pearson")
cor.test(table(bankma$job), table(bankma$education), method = "pearson")

with(bankma,prop.table(table(job,education)))

with(bankma,chisq.test(job,marital))


#Pearson's Chi-squared test

data:  job and marital
X-squared = 4197.5, df = 33, p-value < 2.2e-16