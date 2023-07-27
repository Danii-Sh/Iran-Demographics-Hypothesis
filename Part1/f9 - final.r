library("psych")
library("rmatio")
library("car")

m <- read.mat("C:\\Users\\hamze\\Desktop\\Final\\Q1\\6Subjects.mat")

m$RR$fMRI[[1]][[2]][1]
m$RR$CL

mat1 <-matrix(0,ncol=8,nrow=348) # 8*348 : region 1 all voxels to all images - reset
mat2 <-matrix(0,ncol=8,nrow=58) # 8*58 :  region 2 all voxels to all images - reset
mat3 <-matrix(0,ncol=8,nrow=138) # 8*138 :  region 3 all voxels to all images - reset

rest1 <-matrix(0,ncol=1,nrow=348)
rest2 <-matrix(0,ncol=1,nrow=58)
rest3 <-matrix(0,ncol=1,nrow=138)

for (i in 1:348){
mat1[i,][1] <- m$RR$fMRI[[1]][[1]][i*9-8]
mat1[i,][2] <- m$RR$fMRI[[1]][[1]][i*9-7]
mat1[i,][3] <- m$RR$fMRI[[1]][[1]][i*9-6]
mat1[i,][4] <- m$RR$fMRI[[1]][[1]][i*9-5]
mat1[i,][5] <- m$RR$fMRI[[1]][[1]][i*9-4]
rest1[i,] <- m$RR$fMRI[[1]][[1]][i*9-3]
mat1[i,][6] <- m$RR$fMRI[[1]][[1]][i*9-2]
mat1[i,][7] <- m$RR$fMRI[[1]][[1]][i*9-1]
mat1[i,][8] <- m$RR$fMRI[[1]][[1]][i*9-0]
}
for (i in 1:58){
mat2[i,][1] <- m$RR$fMRI[[1]][[2]][i*9-8]
mat2[i,][2] <- m$RR$fMRI[[1]][[2]][i*9-7]
mat2[i,][3] <- m$RR$fMRI[[1]][[2]][i*9-6]
mat2[i,][4] <- m$RR$fMRI[[1]][[2]][i*9-5]
mat2[i,][5] <- m$RR$fMRI[[1]][[2]][i*9-4]
rest2[i,] <- m$RR$fMRI[[1]][[2]][i*9-3]
mat2[i,][6] <- m$RR$fMRI[[1]][[2]][i*9-2]
mat2[i,][7] <- m$RR$fMRI[[1]][[2]][i*9-1]
mat2[i,][8] <- m$RR$fMRI[[1]][[2]][i*9-0]
}
for (i in 1:138){
mat3[i,][1] <- m$RR$fMRI[[1]][[3]][i*9-8]
mat3[i,][2] <- m$RR$fMRI[[1]][[3]][i*9-7]
mat3[i,][3] <- m$RR$fMRI[[1]][[3]][i*9-6]
mat3[i,][4] <- m$RR$fMRI[[1]][[3]][i*9-5]
mat3[i,][5] <- m$RR$fMRI[[1]][[3]][i*9-4]
rest3[i,] <- m$RR$fMRI[[1]][[3]][i*9-3]
mat3[i,][6] <- m$RR$fMRI[[1]][[3]][i*9-2]
mat3[i,][7] <- m$RR$fMRI[[1]][[3]][i*9-1]
mat3[i,][8] <- m$RR$fMRI[[1]][[3]][i*9-0]
}
#mat2

#w <- c("a","b","c")
#x<- c(1,2,3)
#x<- c(1,2,3,4,5,6,7,8)

(t.test(mat3[,1],rest3))$p.value
a <- t.test(mat3[,1],rest3)
a$p.value


#qqPlot(mat1[4,],mat3[4,])

h <- as.data.frame(mat3)
g <- as.data.frame(mat1)
g[,4]
h[,4]

#qqnorm(mat1[,4],mat3[,4],ylim=c(0,1), xlab="wer")

plot(sort(mat1[,4]),sort(quantile(mat3[,4],probs=ppoints(mat1[,4]))),xlab="Face")

ks.test(mat1[,4], mat3[,4])

dev.new()

#qqnorm(mat1[,5],mat3[,5],xlab="House")
plot(sort(mat1[,5]),sort(quantile(mat3[,5],probs=ppoints(mat1[,5]))),xlab="House")
ks.test(mat1[,5], mat3[,5])
dev.new()

qqnorm(mat1[,4], distribution="norm",xlab="Ventral Temporal to norm")

dev.new()

qqnorm(mat3[,4], distribution="norm",xlab="House To norm")


