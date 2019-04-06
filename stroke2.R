im = matrix(0,16,16)
len=5
theta = pi*0.2
xs=2
ys=2
x=xs+cos(theta)*c(0:len)
y=ys+sin(theta)*c(0:len)
im(x,y)=256

d1=scan("data/digits/Zl1d.dat",nlines=1000,n=256000)
d1=matrix(d1,256,1000)

z = matrix(d1[,2],16,16)
image(c(1:16),c(1:16),z[,16:1],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))

len=15
theta = pi*0.66
x=c(1:16)
y=c(1:16)

stroke = function(x1,y1,theta,len,width, wind_size=c(1:16)){
  # X stuff
  x = wind_size - x1
  ux = x * cos(theta)
  vx = -x * sin(theta)

  
  # Y stuff
  y = wind_size - y1
  uy = y * sin(theta)
  vy = y * cos(theta)
  
  # Build points
  u = matrix(ux,16,16) + t(matrix(uy,16,16))
  v = matrix(vx,16,16) + t(matrix(vy,16,16))
  
  
  # Fill length/width (based on gaussian)
  region = (v > 0 & v < len)
  return(exp(-(u^2)/width) * region)
  
}




stroke(0, 0, 0, 8, 2)
s1=stroke(14,14,0.55*pi,15,2)
s2=stroke(13,13,1.0*pi,15,3)

image(c(1:16),c(1:16),256*s1,col=gray(c(0:256)/256))
image(c(1:16),c(1:16),256*s2,col=gray(c(0:256)/256))

image(c(1:16),c(1:16),256*exp(-u*u/2.0)*m,col=gray(c(0:256)/256))

theta1=0
theta2=0
x1=0
y1=0
x2=0
y2=0
chi=0
len1=0
width1=0
len2=0
width2=0

dist = function(a,b)
{
sum((a-b)^2)
}


imfit = function(z)
{
  theta1=0
  theta2=0
  x1=0
  y1=0
  x2=0
  y2=0
  chi=0
  len1=0
  width1=0
  len2=0
  width2=0
  
  theta1[1] = 0.55*pi 
  theta2[1] = 0.8*pi
  x1[1] = 15 
  y1[1] = 15 
  x2[1] = 15 
  y2[1] = 15 
  len1[1] = 15
  width1[1] = 2.0
  len2[1] = 15 
  width2[1] = 2.0
  
  imin=1
  chimin=10000000
  
  for(i in c(1:100000))
  {
    theta1[i] = theta1[imin] + 0.5*(runif(1)-0.5)
    theta2[i] = max(theta2[imin] + 0.5*(runif(1)-0.5),theta1[i]+0.05*pi)
    x1[i] = min(17,x1[imin] + 5.0*(runif(1)-0.5))
    y1[i] = min(17,y1[imin] + 5.0*(runif(1)-0.5))
    x2[i] = min(17,x2[imin] + 5.0*(runif(1)-0.5))
    y2[i] = min(17,y2[imin] + 5.0*(runif(1)-0.5))
    len1[i] = min(max(2.0,len1[imin] + 5*(runif(1)-0.5)),17.0)
    width1[i] = min(max(width1[imin] + 7.0*(runif(1)-0.5),0.5),5.0)
    len2[i] = min(max(2.0,len2[imin] + 5*(runif(1)-0.5)),17.0)
    width2[i] = min(max(width2[imin] + 7.0*(runif(1)-0.5),0.5),5.0)
    
    s1=stroke(x1[i],y1[i],theta1[i],len1[i],width1[i])
    s2=stroke(x2[i],y2[i],theta2[i],len2[i],width2[i])
    im=s1+s2
    im[im > 1] = 1
    chi[i] = dist(256*im,z[,16:1])
    
    if(chi[i] < chimin)
    {
      chimin = chi[i]
      imin =i
    }
  }#end of for loop
  
  c(x1[imin],y1[imin],theta1[imin],len1[imin],width1[imin],x2[imin],y2[imin],theta2[imin],len2[imin],width2[imin],chimin)
}




min(chi)
k=which.min(chi)

s1=stroke(x1[k],y1[k],theta1[k],len1[k],width1[k])
s2=stroke(x2[k],y2[k],theta2[k],len2[k],width2[k])
#s1=stroke(16.3,15.1,0.6*pi,16.0,2.0)
#s2=stroke(13,13.5,0.95*pi,15,6.0)
s1=stroke(res[1],res[2],res[3],res[4],res[5])
s2=stroke(res[6],res[7],res[8],res[9],res[10])

im=s1+s2
im[im > 1] = 1
dist(256*im,z[,16:1])

image(c(1:16),c(1:16),256*im,col=gray(c(0:256)/256))

for(i in c(0:1000))
{
theta = i*2*pi/1000
s=stroke(10,10,theta)
image(c(1:16),c(1:16),256*s,col=gray(c(0:256)/256))
#readline()
}

