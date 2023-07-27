####### R vector fill the other way round!!!!!!!!!!!!!!!!!


library("psych")
library("rmatio")

options(max.print=10000)

m <- read.mat("C:\\Users\\dshafaie\\Desktop\\Q2\\Fard90.mat")

m    # size= 20 * 1481586

a <- matrix(0,ncol=5,nrow=2)
typeof(m)
dim(m)
dim(a)
length(a)
dim(m$Fard90)
length(m$Fard90)
as.data.frame(lapply(m, dim))


ProbMat <- matrix(0,ncol=6,nrow=9)
#ProbMat[1,][5] = 5
#ProbMat
RowGirlsBoys <- matrix(0,ncol=2,nrow=9)

agesALL<-c()

##my test
#for(i in 1:29631720)
#{
#if(!is.na(m$Fard90[i]) & m$Fard90[i] == 4 )
#{
#if(!is.na(m$Fard90[i+1481586]) & m$Fard90[i+1481586] == 1)
#{
#print(i)
#}}}



 
#for(i in (23705376+1):26668548)##wrong columns selected

for(i in (25186962+1):26668548)  ## sweeping column 18 only
{
if(!is.na(m$Fard90[i]) & !is.na(m$Fard90[i+1481586]) & ((m$Fard90[i]+m$Fard90[i+1481586]) != 0) )
{
if(m$Fard90[i]+m$Fard90[i+1481586] <= 8)
{
ProbMat[m$Fard90[i]+m$Fard90[i+1481586],][6] = ProbMat[m$Fard90[i]+m$Fard90[i+1481586],][6] +1

if(!is.na(m$Fard90[i-16297446]) & m$Fard90[i-16297446] == 1) #Literacy
{ProbMat[m$Fard90[i]+m$Fard90[i+1481586],][4] = ProbMat[m$Fard90[i]+m$Fard90[i+1481586],][4] +1}

if(!is.na(m$Fard90[i-20742204])) #age
{ProbMat[m$Fard90[i]+m$Fard90[i+1481586],][3] = ProbMat[m$Fard90[i]+m$Fard90[i+1481586],][3]+ m$Fard90[i-20742204]}


RowGirlsBoys[m$Fard90[i]+m$Fard90[i+1481586],][1]=RowGirlsBoys[m$Fard90[i]+m$Fard90[i+1481586],][1] + m$Fard90[i] #Girls
RowGirlsBoys[m$Fard90[i]+m$Fard90[i+1481586],][2]=RowGirlsBoys[m$Fard90[i]+m$Fard90[i+1481586],][2] + m$Fard90[i+1481586] #Boys



}


ProbMat[9,][6] = ProbMat[9,][6] +1

if(!is.na(m$Fard90[i-16297446]) & m$Fard90[i-16297446] == 1) #Literacy
{ProbMat[9,][4] = ProbMat[9,][4] +1}

if(!is.na(m$Fard90[i-20742204])) #age
{ProbMat[9,][3] = ProbMat[9,][3]+ m$Fard90[i-20742204]
}#agesALL<-c(agesALL,m$Fard90[i-20742204])}


RowGirlsBoys[9,][1]=RowGirlsBoys[9,][1]+m$Fard90[i] #Girls
RowGirlsBoys[9,][2]=RowGirlsBoys[9,][2]+m$Fard90[i+1481586] #Boys



}}


RowGirlsBoys

for(i in 1: 9)
{ProbMat[i,][3] = ProbMat[i,][3]/ProbMat[i,][6]
ProbMat[i,][4] = ProbMat[i,][4]/ProbMat[i,][6]
ProbMat[i,][5] = ProbMat[i,][6]/ProbMat[9,][6]
ProbMat[i,][1]=RowGirlsBoys[i,][1]/RowGirlsBoys[i,][2]}

#ProbMat[9,][2] <- median(agesALL)

ProbMat


ChildNo <-c(1,2,3,4,5,6,7,8)


n <- as.data.frame(m$Fard90[])


x<-c(1,2,3)
y<-c(7,23,55)

n[,4]
n[,13]

#m <- (n,na.rm=TRUE)

m <- na.omit(n)
m[,13]

cor.test((m[,18]+m[,19]), m[,13], alternative = c("two.sided"))
cor.test((m[,18]+m[,19]), m[,4], alternative = c("two.sided"))

var.test((m[,18]+m[,19]), m[,4], alternative = c("two.sided"))

var.test( lm(m[,11] ~ (m[,18]+m[,19])),  lm(m[,5] ~ (m[,18]+m[,19])))

lm(m[,11] ~ (m[,18]+m[,19]) )
lm(m[,5] ~ (m[,18]+m[,19]) )

