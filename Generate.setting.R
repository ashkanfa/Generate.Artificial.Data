#rm(list=ls())

library("glmnet")
library("ggplot2")
library("coefplot")
library("rje")
library("rtkore")
library("Rcpp")
###################################################################################################
##################################################################################################

#txt <-" 1- First Setting\n
#n=100\n
#p=10 \n 
#true.p=0.25\n
#Correlation structure \n 
#Within tru med \n
#btw tru and spurious med \n 
#magnitude of the effect (coefficients) = [0.4,0.6]"
#setwd("C:/Users/RandKID/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases")
#dir.create("1")
#setwd("C:/Users/RandKID/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases/1")
#writeLines(txt,"setting.txt")

############################################################
## Predictor Setting

#p=10
#n=100
#true.p = 0.3
#cor1="med"
#cor2="med"
#cor3="med"
#min.coef=0.4
#max.coef=0.6
############################################################


#case<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
  #          min.coef=0.4,max.coef=0.6,cor1=c("med","high"),cor2="high", cor3="med")
#case2<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                   # min.coef=0.8,max.coef=1,cor1=c("med","high"),cor2="high", cor3="med")

#case3<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                    #min.coef=0.4,max.coef=0.6,cor1=c("med","high"),cor2="med", cor3="med")

#case4<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                   # min.coef=0.8,max.coef=1,cor1=c("med","high"),cor2="med", cor3="med")

#data<-NULL
#corr.vec<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)

#for (j in seq(nrow(case))){
 #   for (i in seq(100)){
#        set.seed(i)
#        p=case$p[j]
#        n=case$n[j]
#        true.p = case$true.p[j]
#        cor1=as.character(case$cor1[j])
 #       cor2=as.character(case$cor2[j])
 #       cor3=as.character(case$cor3[j])
 #       min.coef=case$min.coef
 #       max.coef=case$max.coef
        
#        corr.vec[,i]<-cor.vec.generate(p, true.p, cor1, cor2, cor3)
 #       data[[i]]=generate.data(n,p,true.p, corr.vec=corr.vec[,i], min.coef=min.coef, max.coef=max.coef, seed=i)
  #      write.csv(data[[i]][["data"]],file=paste0("data",i,".csv"),row.names = F)
#        write.csv(data[[i]][["true.model"]],file = paste0("data",i,".true.model.csv"),row.names=F)
 #       write.csv(data[[i]][["cor"]],file = paste0("data",i,".cor.csv"),row.names=F)
        
        
 #   }
#}


##############################################################################################################################################################
#zero set of simulations cor2="high" , min.coef=0.4,max.coef=0.6

sim.data<-NULL
big.data<-NULL
#corr.vec<-NULL
#corr.vec<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)

case<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                  min.coef=0.4,max.coef=0.6,cor1=c("low","med","high"),cor2="high", cor3="med")


for (j in seq(nrow(case))){
    #setting up simulation
    p=case$p[j]
    n=case$n[j]
    true.p = case$true.p[j]
    cor1=as.character(case$cor1[j])
    cor2=as.character(case$cor2[j])
    cor3=as.character(case$cor3[j])
    min.coef=case$min.coef[j]
    max.coef=case$max.coef[j]
    
    #creating directory for the setting
    #setwd("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases")
    #dir.create(paste0(j))
    #setwd(paste0("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases/",j))
    #write.csv(case[j,],file="setting.csv",row.names = F)
    
    corr.vec<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)
    sim.data<-NULL
    
    for (i in seq(100)){
        set.seed(i)
        #simulating the data
        corr.vec[,i]<-cor.vec.generate(p, true.p, cor1, cor2, cor3)
        sim.data[[i]]=generate.data(n,p,true.p, corr.vec=corr.vec[,i], min.coef=min.coef, max.coef=max.coef, seed=i)
        #write.csv(sim.data[[i]][["data"]],file=paste0("data",i,".csv"),row.names = F)
        #write.csv(sim.data[[i]][["true.model"]],file = paste0("data",i,".true.model.csv"),row.names=F)
        #write.csv(sim.data[[i]][["cor"]],file = paste0("data",i,".cor.csv"),row.names=F)
    }
    big.data[[j]]<-sim.data
    
}

##############################################################################################################################################################        
#first set of simulations cor2="high" , min.coef=0.8,max.coef=0.1

sim.data1<-NULL
big.data1<-NULL
#corr.vec<-NULL
#corr.vec1<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)

case1<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                  min.coef=0.8,max.coef=1,cor1=c("low","med","high"),cor2="high", cor3="med")


for (j in seq(nrow(case1))){
  #setting up simulation
  p=case1$p[j]
  n=case1$n[j]
  true.p = case1$true.p[j]
  cor1=as.character(case1$cor1[j])
  cor2=as.character(case1$cor2[j])
  cor3=as.character(case1$cor3[j])
  min.coef=case1$min.coef[j]
  max.coef=case1$max.coef[j]
  
  #creating directory for the setting
  #setwd("C:/Users/randkid/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases")
  #dir.create(paste0(j+nrow(case)))
  #setwd(paste0("C:/Users/randkid/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases/",j+nrow(case)))
 #write.csv(case1[j,],file="setting.csv",row.names = F)
  
  corr.vec1<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)
  sim.data1<-NULL
  
  for (i in seq(100)){
    set.seed(i)
    #simulating the data
    corr.vec1[,i]<-cor.vec.generate(p, true.p, cor1, cor2, cor3)
    sim.data1[[i]]=generate.data(n,p,true.p, corr.vec=corr.vec1[,i], min.coef=min.coef, max.coef=max.coef, seed=i)
    #write.csv(sim.data1[[i]][["data"]],file=paste0("data",i,".csv"),row.names = F)
    #write.csv(sim.data1[[i]][["true.model"]],file = paste0("data",i,".true.model.csv"),row.names=F)
    #write.csv(sim.data1[[i]][["cor"]],file = paste0("data",i,".cor.csv"),row.names=F)
  }
  big.data1[[j]]<-sim.data1
  
}

##############################################################################################################################################################        
#2nd set of simulations cor2="med" , min.coef=0.4,max.coef=0.6

sim.data2<-NULL
big.data2<-NULL
#corr.vec<-NULL
#corr.vec2<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)

case2<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                   min.coef=0.4,max.coef=0.6,cor1=c("low","med","high"),cor2="med", cor3="med")


for (j in seq(nrow(case2))){
  #setting up simulation
  p=case2$p[j]
  n=case2$n[j]
  true.p = case2$true.p[j]
  cor1=as.character(case2$cor1[j])
  cor2=as.character(case2$cor2[j])
  cor3=as.character(case2$cor3[j])
  min.coef=case2$min.coef[j]
  max.coef=case2$max.coef[j]
  
  #creating directory for the setting
  #setwd("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases")
  #dir.create(paste0(j+2*nrow(case)))
  #setwd(paste0("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases/",j+2*nrow(case)))
  #write.csv(case2[j,],file="setting.csv",row.names = F)
  
  corr.vec2<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)
  sim.data2<-NULL
  
  for (i in seq(100)){
    set.seed(i)
    #simulating the data
    corr.vec2[,i]<-cor.vec.generate(p, true.p, cor1, cor2, cor3)
    sim.data2[[i]]=generate.data(n,p,true.p, corr.vec=corr.vec2[,i], min.coef=min.coef, max.coef=max.coef, seed=i)
    #write.csv(sim.data2[[i]][["data"]],file=paste0("data",i,".csv"),row.names = F)
    #write.csv(sim.data2[[i]][["true.model"]],file = paste0("data",i,".true.model.csv"),row.names=F)
    #write.csv(sim.data2[[i]][["cor"]],file = paste0("data",i,".cor.csv"),row.names=F)
  }
  big.data2[[j]]<-sim.data2
  
}

##############################################################################################################################################################        
#3rd set of simulations cor2="med" , min.coef=0.8,max.coef=0.1

#sim.data3<-NULL
big.data3<-NULL
#corr.vec<-NULL

case3<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                   min.coef=0.8,max.coef=1,cor1=c("low","med","high"),cor2="med", cor3="med")


for (j in seq(nrow(case3))){
  #setting up simulation
  p=case3$p[j]
  n=case3$n[j]
  true.p = case3$true.p[j]
  cor1=as.character(case3$cor1[j])
  cor2=as.character(case3$cor2[j])
  cor3=as.character(case3$cor3[j])
  min.coef=case3$min.coef[j]
  max.coef=case3$max.coef[j]
  
  #creating directory for the setting
  #setwd("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases")
  #dir.create(paste0(j+3*nrow(case)))
  #setwd(paste0("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases/",j+3*nrow(case)))
  #write.csv(case3[j,],file="setting.csv",row.names = F)
  
  corr.vec3<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)
  sim.data3<-NULL
  
  for (i in seq(100)){
    set.seed(i)
    #simulating the data
    corr.vec3[,i]<-cor.vec.generate(p, true.p, cor1, cor2, cor3)
    sim.data3[[i]]=generate.data(n,p,true.p, corr.vec=corr.vec3[,i], min.coef=min.coef, max.coef=max.coef, seed=i)
    #write.csv(sim.data3[[i]][["data"]],file=paste0("data",i,".csv"),row.names = F)
    #write.csv(sim.data3[[i]][["true.model"]],file = paste0("data",i,".true.model.csv"),row.names=F)
    #write.csv(sim.data3[[i]][["cor"]],file = paste0("data",i,".cor.csv"),row.names=F)
  }
  big.data3[[j]]<-sim.data3
  
}

##############################################################################################################################################################        
#4th set of simulations cor2="low" , min.coef=0.4,max.coef=0.6

#sim.data4<-NULL
big.data4<-NULL
#corr.vec<-NULL
#corr.vec4<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)

case4<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                   min.coef=0.4,max.coef=0.6,cor1=c("low","med","high"),cor2="low", cor3="med")


for (j in seq(nrow(case4))){
  #setting up simulation
  p=case4$p[j]
  n=case4$n[j]
  true.p = case4$true.p[j]
  cor1=as.character(case4$cor1[j])
  cor2=as.character(case4$cor2[j])
  cor3=as.character(case4$cor3[j])
  min.coef=case4$min.coef[j]
  max.coef=case4$max.coef[j]
  
  #creating directory for the setting
  #setwd("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases")
  #dir.create(paste0(j+4*nrow(case)))
  #setwd(paste0("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases/",j+4*nrow(case)))
  #write.csv(case4[j,],file="setting.csv",row.names = F)
  
  corr.vec4<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)
  sim.data4<-NULL
  
  for (i in seq(100)){
    set.seed(i)
    #simulating the data
    corr.vec4[,i]<-cor.vec.generate(p, true.p, cor1, cor2, cor3)
    sim.data4[[i]]=generate.data(n,p,true.p, corr.vec=corr.vec4[,i], min.coef=min.coef, max.coef=max.coef, seed=i)
    #write.csv(sim.data4[[i]][["data"]],file=paste0("data",i,".csv"),row.names = F)
    #write.csv(sim.data4[[i]][["true.model"]],file = paste0("data",i,".true.model.csv"),row.names=F)
    #write.csv(sim.data4[[i]][["cor"]],file = paste0("data",i,".cor.csv"),row.names=F)
  }
  big.data4[[j]]<-sim.data4
  
}

##############################################################################################################################################################        
#5th set of simulations cor2="low" , min.coef=0.8,max.coef=1

#sim.data5<-NULL
big.data5<-NULL
#corr.vec<-NULL
#corr.vec5<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)

case5<-expand.grid(p = c(10,20), n = c(100,250,500), true.p=c(0.3,0.5,.7),
                   min.coef=0.8,max.coef=1,cor1=c("low","med","high"),cor2="low", cor3="med")


for (j in seq(nrow(case5))){
  #setting up simulation
  p=case5$p[j]
  n=case5$n[j]
  true.p = case5$true.p[j]
  cor1=as.character(case5$cor1[j])
  cor2=as.character(case5$cor2[j])
  cor3=as.character(case5$cor3[j])
  min.coef=case5$min.coef[j]
  max.coef=case5$max.coef[j]
  
  #creating directory for the setting
  #setwd("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases")
  #dir.create(paste0(j+5*nrow(case)))
  #setwd(paste0("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases/",j+5*nrow(case)))
  #write.csv(case5[j,],file="setting.csv",row.names = F)
  
  corr.vec5<-matrix(NA,nrow = p*(p-1)/2,ncol = 100)
  sim.data5<-NULL
  
  for (i in seq(100)){
    set.seed(i)
    #simulating the data
    corr.vec5[,i]<-cor.vec.generate(p, true.p, cor1, cor2, cor3)
    sim.data5[[i]]=generate.data(n,p,true.p, corr.vec=corr.vec5[,i], min.coef=min.coef, max.coef=max.coef, seed=i)
    #write.csv(sim.data5[[i]][["data"]],file=paste0("data",i,".csv"),row.names = F)
    #write.csv(sim.data5[[i]][["true.model"]],file = paste0("data",i,".true.model.csv"),row.names=F)
    #write.csv(sim.data5[[i]][["cor"]],file = paste0("data",i,".cor.csv"),row.names=F)
  }
  big.data5[[j]]<-sim.data5
  
}

setwd("C:/Users/axa8855/OneDrive/Research/First Paper/Generate.simulated.cases/Simulate.Cases")
write.csv(rbind(case,case1,case2,case3,case4),file="cases.csv")


no.true.p<-NULL
p<-NULL
case.all<-rbind(case,case1,case2,case3,case4,case5)
for (i in seq(324)){
    no.true.p[i]=case.all[i,"p"]*case.all[i,"true.p"]
    p<-case.all[,"p"]
}

big.data11<-append(big.data, big.data1, after = length(big.data))
big.data12<-append(big.data11, big.data2, after = length(big.data11))
big.data13<-append(big.data12, big.data3, after = length(big.data12))
big.data14<-append(big.data13, big.data4, after = length(big.data13))
big.data15<-append(big.data14, big.data5, after = length(big.data14))
big.data<-big.data15
