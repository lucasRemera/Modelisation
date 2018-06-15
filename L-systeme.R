readWord=function(mot,alpha=pi/2,delta=1,alphabet=c("X","A"),alpha0=0){
  dirt=alpha0
  dirc=alpha0
  mots=strsplit(mot,split = "")[[1]]
  xc=c(0,0)
  xt=c(0,0)
  segm=matrix(c(xc,xc,mots[1]),ncol=5)
  colnames(segm)=c("x0","y0","x1","y1","lettre")
  for(i in mots){
    if(i=="["){
      #print("store")
      # xc=xt
      # dirc=dirt
      xc=c(xt,xc)
      dirc=c(dirt,dirc)
      
    }  #aussi conserver l'angle
    if(i=="]"){
      #print("restore")
      # xt=xc
      # dirt=dirc
      xt=xc[1:2]
      xc=xc[-c(1:2)]
      dirt=dirc[1]
      dirc=dirc[-1]
    } 
    if(i=="+") dirt=dirt+alpha
    if(i=="-") dirt=dirt-alpha
    if(i %in%alphabet){
      xtt=xt+delta*c(cos(dirt),sin(dirt))
      segm=rbind(segm,c(xt,xtt,i))
      xt=xtt
    } 
    
  }
  return(segm)
}

# mot="aiuzei(0.2)e(1.2)jfi"
# ss=strsplit(s,split = )
# 
# 
# readWordParam=function(mot,alpha=pi/2,delta=1,alphabet=c("X","A"),alpha0=0){
#   mmot=""
#   deltac=delta
#   dirt=alpha0
#   dirc=alpha0
#   mots=strsplit(mot,split = "")[[1]]
#   if("("%in%mots){
#     debp=which(mots=="(")
#     finp=which(mots==")")
#     substr(mot,debp,finp)
#     cdelta=as.numeric(sapply(1:length(debp), function(ii) substr(mot,debp[ii]+1,finp[ii]-1)))
#     #print(cdelta)
#   }
# 
#   xc=c(0,0)
#   xt=c(0,0)
#   segm=matrix(c(xc,xc,mots[1]),ncol=5)
#   colnames(segm)=c("x0","y0","x1","y1","lettre")
#   for(i in mots){
#     if(i=="("){
#       deltac=cdelta[1]
#       cdelta=cdelta[-1]
#     } 
#     
#     if(i=="["){
#       #print("store")
#       # xc=xt
#       # dirc=dirt
#       xc=c(xt,xc)
#       dirc=c(dirt,dirc)
#       
#     }  #aussi conserver l'angle
#     if(i=="]"){
#       #print("restore")
#       # xt=xc
#       # dirt=dirc
#       xt=xc[1:2]
#       xc=xc[-c(1:2)]
#       dirt=dirc[1]
#       dirc=dirc[-1]
#     } 
#     if(i=="+") dirt=dirt+alpha
#     if(i=="-") dirt=dirt-alpha
#     if(i %in%alphabet){
#       #print(mmot)
#       #print(xt)
#       xtt=xt+deltac*c(cos(dirt),sin(dirt))
#       #print(xtt)
#       segm=rbind(segm,c(xt,xtt,i))
#       xt=xtt
#       if(mmot==")") deltac=delta
#     } 
#     mmot=i
#     
#   }
#   return(segm)
# }


readWordRandom=function(mot,alpha=c(pi/2,pi),delta=c(1,2),alphabet=c("X","A"),alpha0=0){
  dirt=alpha0
  dirc=alpha0
  mots=strsplit(mot,split = "")[[1]]
  xc=c(0,0)
  xt=c(0,0)
  segm=matrix(c(xc,xc,mots[1]),ncol=5)
  colnames(segm)=c("x0","y0","x1","y1","lettre")
  for(i in mots){
    if(i=="["){
      #print("store")
      # xc=xt
      # dirc=dirt
      xc=c(xt,xc)
      dirc=c(dirt,dirc)
      
    }  #aussi conserver l'angle
    if(i=="]"){
      #print("restore")
      # xt=xc
      # dirt=dirc
      xt=xc[1:2]
      xc=xc[-c(1:2)]
      dirt=dirc[1]
      dirc=dirc[-1]
    } 
    if(i=="+") dirt=dirt+runif(1,alpha[1],alpha[2])
    if(i=="-") dirt=dirt-runif(1,alpha[1],alpha[2])
    if(i %in%alphabet){
      xtt=xt+runif(1,delta[1],delta[2])*c(cos(dirt),sin(dirt))
      segm=rbind(segm,c(xt,xtt,i))
      xt=xtt
    } 
    
  }
  return(segm)
}


generateWord=function(w0,alphabet=c("X","A"),regle=list(X="X",A="A"),n=1){
  word=w0
  for(i in 1:n){
    ws=strsplit(word,split = "")[[1]]
    wt=""
    for(s in ws){
      if(s %in%alphabet){ wt=paste0(wt,regle[[s]])}
      else wt=paste0(wt,s)
      
    }
    word=wt
  }
  return(word)
}

# generateWordParam=function(w0,alphabet=c("X","A"),regle=list(X="X",A="A"),n=1){
#   word=w0
#   for(i in 1:n){
#     ws=strsplit(word,split = "")[[1]]
#     wt=""
#     for(s in ws){
#       if(s %in%alphabet){ wt=paste0(wt,regle[[s]])}
#       else wt=paste0(wt,s)
#       
#     }
#     word=wt
#   }
#   return(word)
# }

generateWordRandom=function(w0,alphabet=c("X","A"),regle=list(X=c(X=0.8,A=0.2),A=c(A=0.8,X=0.2)),n=1){
  word=w0
  for(i in 1:n){
    ws=strsplit(word,split = "")[[1]]
    wt=""
    for(s in ws){
      if(s %in%alphabet){
        prob=regle[[s]]
        motif=names(regle[[s]])
        
        wt=paste0(wt,sample(motif,1,prob=prob))}
      else wt=paste0(wt,s)
      
    }
    word=wt
  }
  return(word)
}

#test=readWord("A--A--A",alpha = pi/3,alphabet = "A")

library(ggplot2)
#ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))


#Koch=generateWord("A",alphabet = "A",regle = list(A="A+A-A-A+A"),n=6)
#test=readWord(Koch,alphabet = "A")

Koch=generateWord("A--A--A",alphabet = "A",regle = list(A="A+A--A+A"),n=6)
test=readWord(Koch,alphabet = "A",alpha=pi/3)

ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))


plante=generateWord("X",regle=list(X="A[+X]A[-X]+X",A="AA"),n=6)
test=readWord(plante,20*pi/180,alphabet = c("A","X"))

plante=generateWord("X",regle=list(X="A+[[X]-X]-A[-AX]+X",A="AA"),n=6)
test=readWord(plante,25*pi/180,alphabet = c("A","X"),alpha0 = pi/3)
#test=readWordRandom(plante,c(20,30)*pi/180,alphabet = c("A","X"),alpha0 = pi/3)
ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))


plante=generateWordRandom("X",regle=list(X=c("AA[+X]A[-X]+X"=0.5,"A+[[X]-X]-A[-AX]+X"=0.5),A=c("AA"=1)),n=6)
test=readWordRandom(plante,c(20,30)*pi/180,alphabet = c("A","X"),alpha0 = pi/3)
ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))


plante=generateWordRandom("X",regle=list(X=c("A[-X][+X]+X"=0.5,"A"=0.2,"X"=0.3),A=c("AA"=0.3,"A"=0.5,"X"=0.2)),n=10)
#plante=generateWordRandom("X",regle=list(X=c("A[-X][+X]"=1),A=c("AA"=0,"A"=1)),n=6)
#plante=generateWord("X",regle=list(X="A[-X][+X]",A="A"),n=6)
test=readWord(plante,20*pi/180,alphabet = c("A","X"))
ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))

plante=generateWordRandom("X",regle=list(X=c("-X+D[-X][+X]+X"=0.5,"D"=0.2,"X"=0.3),M=c("MM"=0.3,"M"=0.7),D=c("X"=0.3,"D"=0.5,"M"=0.2)),n=10,alphabet = c("X","D","M"))
test=readWord(plante,20*pi/180,alphabet = c("M","X","D"))
ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))


plante=generateWordRandom("X",regle=list(X=c("D+[[X]-X]-D[-DX]+X"=0.6,"D"=0.2,"DD"=0.1,"X"=0.1),M=c("MM"=0.4,"M"=0.6),D=c("D[++M]"=0.4,"DD"=0.2,"D"=0.3,"M"=0.1)),n=8,alphabet = c("X","D","M"))
test=readWord(plante,25*pi/180,alphabet = c("D","X","M"),alpha0 = pi/2)
ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))


plante=generateWordRandom("X",regle=list(X=c("D[+X]D[-X]+X"=.35,"D[-X]D[-X]+X"=.35,"D"=.3),M=c("MM"=0.4,"M"=0.6),D=c("D[++X]"=0.1,"DD"=0.5,"D"=0.3,"D[--X]"=0.1)),n=8,alphabet = c("X","D","M"))
test=readWord(plante,25*pi/180,alphabet = c("D","X","A"),alpha0 = pi/2)
ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))

prele=generateWord("X",regle=list(X="AY[+D][-G]X",Y="Y[+D][-G]",A="AA",D="+DM",G="-GM",M="M"),n=8,alphabet = c("X","M","D","G","A","Y"))
test=readWord(prele,10*pi/180,alpha0 = pi/2,alphabet = c("M","X","D","G","A","Y"))

ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))+labs(x="",y="",col="")

fougere=generateWord("X",regle=list(X="A[-X]A[+X]X",A="MA",M="M"),n=8)
test=readWord(fougere,pi/4,alphabet = c("A","X","M"),alpha0 = pi/2)
ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))


test=readWordParam("(0.3)A--A--(0.2)A",alphabet = "A",alpha=pi/3)

ggplot()+geom_segment(aes(x=as.numeric(test[,1]),y=as.numeric(test[,2]),xend=as.numeric(test[,3]),yend=as.numeric(test[,4]),col=test[,5]))


dragonL=generateWord("FX",alphabet = c("F","X","Y"),regle=list("F"="F","X"="X+YF+","Y"="-FX-Y"),n=15)
dragonw=readWord(dragonL,alphabet = c("F"))
ggplot()+geom_segment(aes(x=as.numeric(dragonw[,1]),y=as.numeric(dragonw[,2]),xend=as.numeric(dragonw[,3]),yend=as.numeric(dragonw[,4]),col=dragonw[,5]))+labs(x="",y="",col="")
