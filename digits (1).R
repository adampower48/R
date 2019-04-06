d0=scan("Zl0d.dat",nlines=1000,n=256000)
d1=scan("Zl1d.dat",nlines=1000,n=256000)
d2=scan("Zl2d.dat",nlines=1000,n=256000)
d3=scan("Zl3d.dat",nlines=1000,n=256000)
d4=scan("Zl4d.dat",nlines=1000,n=256000)
d5=scan("Zl5d.dat",nlines=1000,n=256000)
d6=scan("Zl6d.dat",nlines=1000,n=256000)
d7=scan("Zl7d.dat",nlines=1000,n=256000)
d8=scan("Zl8d.dat",nlines=1000,n=256000)
d9=scan("Zl9d.dat",nlines=1000,n=256000)


d=c(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9)


d=matrix(d,256,10000)
d=t(d)
c=var(d)
e=eigen(c)
plot(e$values)
p=d %*% e$vectors[,1:3]
plot(p[,1],p[,2])
group=identify(p[,1],p[,2],n=5)

par(mfrow=c(5,6))
for(i in group2)
{
z = matrix(d[i,],16,16)
image(c(1:16),c(1:16),z[16:1,],col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
readline()
}






width = 1
ii = c(0:15)
ii[9:16] = 17 - c(9:16)
ii = ii^2
ex = exp(-ii/width)
gau = ex %*% t(ex)
image(c(1:16),c(1:16),256*gau,col=gray(c(0:256)/256))

x2=d
x=d
for(i in c(1:10000))
{
z = matrix(as.numeric(x[i,]),16,16)
ft=fft(z)
ft = ft *gau
z2=fft(ft,inverse=T)
#image(c(1:16),c(1:16),abs(z2),col=gray(c(0:256)/256))
x2[i,]=matrix(abs(z2),1,256)
}

c=var(x2)

for(i in c(1001:2000))
{
z = matrix(as.numeric(x2[i,]),16,16)
image(c(1:16),c(1:16),z,col=gray(c(0:256)/256),xlab="",ylab="",mar=c(1,1,1,1))
readline()
}


b=t(p)
b=cbind(b,c(3000,0,0),c(-3000,0,0))
b=cbind(b,c(0,3000,0),c(0,-3000,0))
b=cbind(b,c(0,0,3000),c(0,0,-3000))

r=b

theta=2*pi/100
rot=matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2,2)
rev=matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),2,2)

colors=c(rep("red",1000),rep("blue",1000),rep("green",1000),rep("yellow",1000),rep("chocolate",1000),rep("cyan",1000),rep("magenta",1000),rep("brown",1000),rep("blueviolet",1000),rep("black",1000))

ch="0"
while(ch != "q")
{

pos = locator(1);
if(pos$x > 0.0 & pos$y < 0.0) b[c(1,3),]=rot %*% b[c(1,3),]
else if(pos$x < 0.0 & pos$y < 0.0) b[c(1,3),]=rev %*% b[c(1,3),]

if(pos$x > 0.0 & pos$y > 0.0) b[c(1,2),]=rot %*% b[c(1,2),]
else if(pos$x < 0.0 & pos$y > 0.0) b[c(1,2),]=rev %*% b[c(1,2),]


r[1,]=4000*b[1,]/(5000-b[3,])
r[2,]=4000*b[2,]/(5000-b[3,])

plot.new()
plot.window(xlim=c(-3000,3000),ylim=c(-3000,3000))

lines(r[1,10001:10002],r[2,10001:10002],col='darkturquoise')
lines(r[1,10003:10004],r[2,10003:10004],col='deeppink')
lines(r[1,10005:10006],r[2,10005:10006])

zorder = order(b[3,])
points(r[1,zorder],r[2,zorder],col=colors[zorder])

}

index = c(1:10000)
group1 = index[(p[,1] > -750) & (p[,2] > -200 & p[,2] < 200) & (p[,3] > -200 & p[,3] < 200)]
group2 = index[(p[,1] < -2200) & (p[,2] > -200 & p[,2] < 200) & (p[,3] > -200 & p[,3] < 200)]

dist = function(a,b)
{
sum((a-b)^2)
}
