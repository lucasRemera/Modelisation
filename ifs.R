# YY=runif(100000)
# 
# XX=1/(1-YY)
# plot(density(XX))
# plot(ecdf(XX),xlim=c(0.9,5))
# lines(seq(1,5,by=0.001),1-1/seq(1,5,by=0.001),col="green")
# 
# YYY=replicate(100,{runif(100000)})
# sYYY=apply(YYY,1,sum)
# plot(density(sYYY))
# plot(ecdf(sYYY))
# 
# XXX=1/(1-YYY)
# sXXX=apply(XXX,1,sum)
# plot(ecdf(sXXX),xlim=c(1,1000))

f1=function(x) x/2
f2=function(x) x/2+0.5
f3=function(x) x/2+0.5+1i*sqrt(3)/4

fern1=function(z) 0.16i*Im(z)
fern2=function(z) (0.85-0.04i)*Re(z)+(0.04+0.85i)*Im(z)+1.6i
fern3=function(z) (0.2+0.23i)*Re(z)+(-0.26+0.22i)*Im(z)+1.6i
fern4=function(z) (-0.15+0.26i)*Re(z)+(0.28+0.24i)*Im(z)+0.44i

levyh=function(z){0.5*z*(1+1i)}
levyl=function(z){0.5*((1-1i)*z+1+1i)}
levyv=function(z){
  zz=levyl(z)
  return(sin(Re(zz))+1i*sin(Im(zz)))
}
dragon=function(z){
  0.5*(1-1i)*z
}

t1=function(z) 0.5*(sin(Re(z))+1i*cos(Im(z)))
t2=function(z) 0.5*(cos(Re(z))-1i*sin(Im(z)))
t3=function(z) 0.5*(sin(Im(z))+1i*cos(Re(z)))
t4=function(z) 0.5*(cos(Im(z))-1i*sin(Re(z)))

tt1=function(z) 0.8*(z-(1+1i))*exp(1i*pi/3)
tt2=function(z) 0.5*(z-(1-1i))*exp(1i*3*pi/4)
tt3=function(z) 0.8*(z+(1+1i))*exp(-1i*pi/3)

IFS=function(z,f,n=1){
  zz=unlist(lapply(f, function(ff) ff(z)))
  if(n==1) return(zz)
  else return(IFS(zz,f,n-1))
}


IFSrandom=function(z,f,p=rep(1,length(f))/length(f),n=1){
  for(i in 1:n){
    ff=f[[sample(1:length(p),1,prob = p)]]
    z=c( ff(z[1]) ,z)
  }
  return(z)
}


# IFS=function(z,n){
#   #if(n==0) return(z)
#   zz=c(z/2,z/2+0.5,z/2+0.5+1i*sqrt(3)/4)
#   if(n==1) return(zz)
#   else return(IFS(zz,n-1))
#   #else return(unlist(lapply(z, function(zz) IFS(zz,n-1) )))
# }

zf=IFS(0,c(fern1,fern2,fern3,fern4),10) #the fern
zf=IFS(0,c(dragon,levyl),15) #the dragon curve

ggplot()+geom_point(aes(x=Re(zf),y=Im(zf)),col="darkgreen",size=0.001)+coord_fixed(ratio=1)

# zf=IFSrandom(0,c(dragon,levyl),n=100000)
# 
# zf=IFS(0,c(tt1,tt2,tt3),10)
# 
# du1=function(z) 0.5*exp(-3i*pi/4)*z
# du2=function(z) 0.5*exp(3i*pi/4)*(z-1)+1
# du3=function(z) 0.5*exp(1i*pi/2)*(z-1i)+1i
# du4=function(z) 0.5*Re(z)
# du5=function(z) 0.5*Im(z)
# du6=function(z) 0.5*Im(z)+0.5*Re(z)
# zf=IFS(0,c(du1,du2,du3,du6),8)
# zf0=IFS(0,c(du1,du2),12)
# 
# ddu1=function(z) 0.25*z**2+0.25+0.5*1i
# ddu2=function(z) 0.5*z+i/4
# zf=IFS(1i,c(ddu1,ddu2),15)
# 
# dddu1=function(z) 0.85*exp(-3i*pi/4)*z
# dddu2=function(z) 0.85*exp(3i*pi/4)*(z-1)+1
# zf=IFS(0,c(dddu1,dddu2),15)
# ggplot()+geom_point(aes(x=Re(zf),y=Im(zf)),col="darkgreen",size=0.001)+coord_fixed(ratio=1)
# 
# 
# 
# transF=function(z,k=1,theta=0,z0=0,ztrans=0) k*exp(1i*theta)*(z-z0)+z0+ztrans
# ff1=function(z) transF(z,0.5,-2*pi/3,1,0)
# ff2=function(z) transF(z,0.7,2*pi/3,0)
# zf=IFS(0,c(ff1,ff2),15)
# ggplot()+geom_point(aes(x=Re(zf),y=Im(zf)),col="darkgreen",size=0.001)+coord_fixed(ratio=1)
# 
# 
# ff1=function(z) transF(z,1/3,pi,0)
# ff2=function(z) transF(z,1/2,0,1)
# #ff3=function(z) transF(z,2/3,-pi,1i)
# #zf=IFS(0,c(ff1,ff2,ff3),10)
# ggplot()+geom_point(aes(x=Re(zf),y=Im(zf)),col="darkgreen",size=0.001)+coord_fixed(ratio=1)
# 
# ff1=function(z) transF(z,exp(-Mod(z)),pi/2,-1,0)
# ff2=function(z) transF(z,exp(-Mod(z)),-pi/2,1,0)
# ff3=function(z) transF(z,exp(-1/Mod(z)),-pi,1i,0)
# zf=IFS(0,c(ff1,ff2,ff3),10)
# ggplot()+geom_point(aes(x=Re(zf),y=Im(zf)),col="darkgreen",size=0.001)+coord_fixed(ratio=1)

