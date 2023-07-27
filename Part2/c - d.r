####### R vector fill the other way round!!!!!!!!!!!!!!!!!


library("psych")
library("rmatio")

options(max.print=10000)

m <- read.mat("C:\\Users\\dshafaie\\Desktop\\Q2\\Fard90.mat")

#m    # size= 20 * 1481586

a <- matrix(0,ncol=5,nrow=2)



ProbMat <- matrix(0,ncol=6,nrow=9)

RowGirlsBoys <- matrix(0,ncol=2,nrow=9)

agesALL<-c()





 
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
}

RowGirlsBoys[9,][1]=RowGirlsBoys[9,][1]+m$Fard90[i] #Girls
RowGirlsBoys[9,][2]=RowGirlsBoys[9,][2]+m$Fard90[i+1481586] #Boys



}}




for(i in 1: 9)
{ProbMat[i,][3] = ProbMat[i,][3]/ProbMat[i,][6]
ProbMat[i,][4] = ProbMat[i,][4]/ProbMat[i,][6]
ProbMat[i,][5] = ProbMat[i,][6]/ProbMat[9,][6]
ProbMat[i,][1]=RowGirlsBoys[i,][1]/RowGirlsBoys[i,][2]}

#ProbMat[9,][2] <- median(agesALL)






n <- as.data.frame(m)


#ProbMat


Literacy <- c()
for (i in 1:8){
Literacy <- c(Literacy,ProbMat[i,4])}


ChildNo <-c(1,2,3,4,5,6,7,8)
fit1 <- lm (ChildNo ~ Literacy)


o <- n[,18]+n[,19]

Literacy <- n[,7]
fit2 <- lm ( o ~ Literacy)


fit3 <- lm ( o ~ Literacy + n[,10])



residuals_vec1<- residuals(fit1)
residuals_vec2<- residuals(fit2)
residuals_vec3<- residuals(fit3)

sd(residuals_vec1)
sd(residuals_vec2)
sd(residuals_vec3)

summary(cor(n[,5], n[,11], method = c("pearson"), use="pairwise.complete.obs"))#, "kendall", "spearman")))



