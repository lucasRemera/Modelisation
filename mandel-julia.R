mandelbrodt=function(c0,z0=0i,n=100,d=2){
  x=Re(c0)
  y=Im(c0)
  p=sqrt((x-0.25)**2+y**2)
  if(z0==0i & d==2){
    if(x<(p-2*p**2+0.25)) return(0)
    if(((x+1)**2+y**2)<(1/16)) return(0)
  }
  for(i in 1:n){
    if(Mod(z0)>2) return(n-i+1)
    else z0=z0**d+c0
  }
  return(0)
}

xx=seq(-2,1,by=0.005)
yy=seq(-1,1,by=0.005)
xy=expand.grid(xx,yy)
cc=xy[,1]+xy[,2]*1i

mandel=sapply(cc,mandelbrodt)
library(ggplot2)
ggplot()+geom_raster(aes(x=Re(cc),y=Im(cc),fill=(mandel)))+coord_fixed(ratio=1)+guides(fill=FALSE)

xx=seq(-1.1,1.1,by=0.005)
yy=seq(-1,1.2,by=0.005)
xy=expand.grid(xx,yy)
cc=xy[,1]+xy[,2]*1i
julia=sapply(cc,function(ccc) mandelbrodt(-0.122+0.744i,ccc,d=2,n=50))
ggplot()+geom_raster(aes(x=Re(cc),y=Im(cc),fill=(julia)))+coord_fixed(ratio=1)+guides(fill=FALSE)
