draw<-function(){
	x11()
	plot(c(0,0),xlim=c(0,1),ylim=c(0,1),col='white',main="please click c(0,0) first then c(1,1) then the points")
	data<-data.frame(x=0,y=0)
	new<-F
	p1<-c(0.1475410,0.1743666)
	p2<-c(0.9090909,0.8569300)
	color<<-'black'
	pen<<-1
	cex<<-5
	record<<-matrix(0,1e+3,1e+3)
	flag<<-matrix(0,1e+3,1e+3)
	add<-function(a,px,py){
		point<-function(x,y){
			temp<-matrix(c(runif(30)),ncol=3)
			temp[,1]<-sin(temp[,1]*2*pi)*0.02*temp[,2]+px-0.01
			temp[,2]<-sin(temp[,3]*2*pi)*0.02*temp[,2]+py-0.01
			points(temp,pch=16,cex=0.8*cex/10,col=color)
		}
		px<-round((px-p1[1])/(p2[1]-p1[1]),3)
		py<-round((py-p1[2])/(p2[2]-p1[2]),3)
		if(px<1 & px>0 & py<1 & py>0){
			record[px*(1e+3),py*(1e+3)]<-ifelse(color=='e',0,1)
			if(pen==1)points(px,py,pch=16,col=color,cex=cex/10)
			if(pen==2)point(px,py)
			##if(pen==3)paint(px,py)
			flag<<-matrix(0,1e+3,1e+3)
		}
		return(NULL)
	}
	painting<-function(a,px,py){
		paint<-function(x,y){
			if(!(x<=1e-3 | y<=1e-3 | x>1 | y>1) & record[x*(1e+3),y*(1e+3)]==record[px*(1e+3),py*(1e+3)] & flag[x*(1e+3),y*(1e+3)]==0){
				##points(x,y,pch=15,cex=0.1,col=color)
				flag[x*(1e+3),y*(1e+3)]<<-1
				if(round(x,1)>(1e-3) & round(y,1)>(1e-3) & round(x,1)<=1-(1e-3) & round(y,1)<=1-(1e-3))if(flag[x*(1e+3),y*(1e+3)+1]==0)paint(x,y+(1e-3))
				if(round(x,1)>(1e-3) & round(y,1)>(1e-3) & round(x,1)<=1-(1e-3) & round(y,1)<=1-(1e-3))if(flag[x*(1e+3)+1,y*(1e+3)]==0)paint(x+(1e-3),y)
				if(round(x,1)>(1e-3) & round(y,1)>(1e-3) & round(x,1)<=1-(1e-3) & round(y,1)<=1-(1e-3))if(flag[x*(1e+3),y*(1e+3)-1]==0)paint(x,y-(1e-3))
				if(round(x,1)>(1e-3) & round(y,1)>(1e-3) & round(x,1)<=1-(1e-3) & round(y,1)<=1-(1e-3))if(flag[x*(1e+3)-1,y*(1e+3)]==0)paint(x-(1e-3),y)
			}
		}
		x<-(px-p1[1])/(p2[1]-p1[1])
		y<-(py-p1[2])/(p2[2]-p1[2])
		x<-round(x,1)
		y<-round(y,1)
		if(!(x<0 | y<0 | x>1 | y>1) & record[x*(1e+3),y*(1e+3)]==record[px*(1e+3),py*(1e+3)] & flag[x*(1e+3),y*(1e+3)]==0)paint(x,y)
		return(NULL)
	}
	col<-function(K){
		K<-tolower(K)
		if(K=='r'){color<<-'red'}
		if(K=='b'){color<<-'black'}
		if(K=='g'){color<<-'green'}
		if(K=='p'){color<<-'purple'}
		if(K=='u'){color<<-'blue'}
		if(K=='y'){color<<-'yellow'}
		if(K=='o'){color<<-'orange'}
		if(K=='e'){color<<-'white'}
		if(K=='q'){pen<<-1}
		if(K=='w'){pen<<-2}
		if(K=='a'){pen<<-3}
		if(K%in%as.character(1:9)){cex<<-as.numeric(K)}
		points(0,0,col='white')
	}
	getGraphicsEvent(prompt="create.reg",onMouseMove=add,onKeybd=col)
}