library("psych")
library("rmatio")
library("R.basic")

m <- read.mat("C:\\Users\\hamze\\Desktop\\Final\\Q1\\6Subjects.mat")

m$RR$fMRI[[1]][[2]][1]
m$RR$CL

mat1 <-matrix(0,ncol=8,nrow=348) # 8*348 : region 1 all voxels to all images - rest
mat2 <-matrix(0,ncol=8,nrow=58) # 8*58 :  region 2 all voxels to all images - rest
mat3 <-matrix(0,ncol=8,nrow=138) # 8*138 :  region 3 all voxels to all images - rest

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



mat1zscore <-(mat1-mean(rest1))/sd(rest1)
mat2zscore <-(mat2-mean(rest2))/sd(rest2)
mat3zscore <-(mat3-mean(rest3))/sd(rest3)



error.bars(mat1zscore ,ylab='ventral temporal')#,bar=TRUE)
dev.new()
error.bars(mat2zscore ,ylab='Face')#,bar=TRUE)
dev.new()
error.bars(mat3zscore ,ylab='House')#,bar=TRUE)
