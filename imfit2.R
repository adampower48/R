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


