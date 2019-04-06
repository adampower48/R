x=rnorm(1000)
var(x)
y=rnorm(1000)
z=cbind(x,y)
head(z)
dim(z)
var(z)
c=var(z)
eigen(c)
y1=2*x+y
x1=2*x-y
plot(x1,y1)
c=var(z)
c
z1=cbind(x1,y1)
c=var(z1)
c
eigen(c)
abline(0,1)
abline(0,-1)
results = read.table("results.txt",header=T)
head(results)
var(results[,2;5])
var(results[,2:5],na.rm=T)
c=var(results[,2:5],na.rm=T)
eigen(c)
u1=c$vectors[,1]
e=eigen(c)
u1=e$vectors[,1]
u1
t(u1) %*% u1
attach(results)
r=cbind(arch1,prog1,arch2,prog2)
class(r)
head(r)
p = r %*% e$vectors
head(p)
p = results[,2:5] %*% e$vectors
plot(p[,1],p[,2])
group = identify(p[,1],p[,2],n=5)
results[group,]

