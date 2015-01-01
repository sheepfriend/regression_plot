draw<-function(){
	x11()
	plot(c(0,0),xlim=c(0,1),ylim=c(0,1),col='white',main="the weather is fine",xlab="q-pen  w-brush  r-line  t-circle  y-rect  u-eraser (filling not support)",ylab='color')
	data<-data.frame(x=0,y=0)
	new<-F
	p1<-c(0.1475410,0.1743666)
	p2<-c(0.9090909,0.8569300)
	color<-c(1,1)
	pre.col<-color
	pen<-1
	cex<-5
	prex<-0
	prey<-0
	lin<-F
	cir<-F
	rec<-F
	tag<-100
	col.vector<-rainbow(100)
	for(i in 1:100){col.vector[i]<-substring(col.vector[i],1,7)}
	grey.vector<-99:0
	points(rep(0,100),100:1/100/1.2,col=paste("grey",grey.vector,sep=""),pch=15)
	points(rep(0.05,tag),1:tag/tag/1.2,col=col.vector,pch=15)
	points(c(0.07,0.02),c(pre.col[1]/tag,pre.col[2]/100)/1.2,pch=18,cex=1.2,col='white')
	points(c(0.07,0.02),c(color[1]/tag,color[2]/100)/1.2,pch=18,cex=1.2,col='grey20')
	points(0.05,0.94,pch=15,col=col.vector[color[1]],cex=3)
	points(0,0.94,pch=15,col='black',cex=3)
	abline(v=0.1)
	add1<-function(a,px,py){
		change.col<-function(){
			if(color[2]/100>=0.5 & color[1]>=0){col<-mixcolor((color[2]/100-0.5)*2,hex2RGB(col.vector[color[1]]),RGB(1,1,1))}
			else if(color[1]>=0){col<-mixcolor((0.5-color[2]/100)*2,hex2RGB(col.vector[color[1]]),RGB(0,0,0))}
			else if(color[1]==-1){col<-mixcolor(color[2]/100,RGB(0,0,0),RGB(1,1,1))}
			col<-col@coords
			return(rgb(col[1],col[2],col[3]))
		}
		show.col<-function(){
			points(c(0.07,0.02),c(pre.col[1]/tag,pre.col[2]/100)/1.2,pch=18,cex=1.2,col='white')
			points(c(0.07,0.02),c(color[1]/tag,color[2]/100)/1.2,pch=18,cex=1.2,col='grey20')
			points(0.05,0.94,pch=15,col=change.col(),cex=3)
			pre.col<<-color
		}
		choose.col<-function(x,y){
			if(abs(x)<0.01 & y>0 & y<1/1.2){color[2]<<-round(y*120)}
			else if(abs(x-0.05)<0.01 & y>0 & y<1/1.2){color[1]<<-round(y*1.2*100)}
			else if(abs(x)<0.03 & y>1/1.2){color[1]<<--1}
			show.col()
		}
		if(pen!=4){lin<<-F}
		if(pen!=5){cir<<-F}
		point<-function(x,y){
			temp<-matrix(c(runif(3*cex*10)),ncol=3)
			temp[,1]<-sin(temp[,1]*2*pi)*0.02*temp[,2]+px-0.01
			temp[,2]<-sin(temp[,3]*2*pi)*0.02*temp[,2]*cex/5+py-0.01
			points(temp,pch=16,cex=0.4,col=change.col())
		}
		line<-function(x,y){
			if(lin==F){lin<<-T}
			else{
				lines(c(x,prex),c(y,prey),type='l',lwd=5*cex/10,col=change.col())
				lin<<-F
			}
			prex<<-x
			prey<<-y
		}
		circle<-function(x,y){
			if(cir==F){cir<<-T}
			else{
				temp<-(1:1000)/1000
				ra<-sqrt((x-prex)^2+(y-prey)^2)
				points(prex+cos(temp*2*pi)*ra,prey+sin(temp*2*pi)*ra,cex=0.8*cex/10,col=change.col())
				cir<<-F
			}		
			prex<<-x
			prey<<-y	
		}
		rect<-function(x,y){
			if(rec==F){rec<<-T}
			else{
				lines(c(x,prex),c(prey,prey),type='l',lwd=5*cex/10,col=change.col())
				lines(c(x,x),c(y,prey),type='l',lwd=5*cex/10,col=change.col())
				lines(c(prex,prex),c(y,prey),type='l',lwd=5*cex/10,col=change.col())
				lines(c(x,prex),c(y,y),type='l',lwd=5*cex/10,col=change.col())
				rec<<-F
			}
			prex<<-x
			prey<<-y	
		}
		px<-round((px-p1[1])/(p2[1]-p1[1]),3)
		py<-round((py-p1[2])/(p2[2]-p1[2]),3)
		if(px<1 & px>-0.05 & py<1 & py>0){
			if(px<0.13){choose.col(px,py)}
			else{
				if(pen==1)points(px,py,pch=16,col=col.vector[color[1]],cex=cex/10)
				if(pen==2)point(px,py)
				if(pen==3)painting(a,px,py)
				if(pen==4)line(px,py)
				if(pen==5)circle(px,py)
				if(pen==6)rect(px,py)
				if(pen==7)points(px,py,pch=16,col='white',cex=cex)	
			}
		}
		return(NULL)
	}
	add2<-function(a,px,py){
		change.col<-function(){
			if(color[2]/100>=0.5 & color[1]>=0){col<-mixcolor((color[2]/100-0.5)*2,hex2RGB(col.vector[color[1]]),RGB(1,1,1))}
			else if(color[1]>=0){col<-mixcolor((0.5-color[2]/100)*2,hex2RGB(col.vector[color[1]]),RGB(0,0,0))}
			else if(color[1]==-1){col<-mixcolor(color[2]/100,RGB(0,0,0),RGB(1,1,1))}
			col<-col@coords
			return(rgb(col[1],col[2],col[3]))
		}
		show.col<-function(){
			points(c(0.07,0.02),c(pre.col[1]/tag,pre.col[2]/100)/1.2,pch=18,cex=1.2,col='white')
			points(c(0.07,0.02),c(color[1]/tag,color[2]/100)/1.2,pch=18,cex=1.2,col='grey20')
			points(0.05,0.94,pch=15,col=change.col(),cex=3)
			pre.col<<-color
		}
		choose.col<-function(x,y){
			if(abs(x)<0.01 & y>0 & y<1/1.2){color[2]<<-round(y*120)}
			else if(abs(x-0.05)<0.01 & y>0 & y<1/1.2){color[1]<<-round(y*1.2*100)}
			else if(abs(x)<0.03 & y>1/1.2){color[1]<<--1}
			show.col()
		}
		if(pen!=4 & pen!=1){lin<<-F}
		if(pen!=5){cir<<-F}
		point<-function(x,y){
			temp<-matrix(c(runif(3*cex*10)),ncol=3)
			temp[,1]<-sin(temp[,1]*2*pi)*0.02*temp[,2]+px-0.01
			temp[,2]<-sin(temp[,3]*2*pi)*0.02*temp[,2]*cex/5+py-0.01
			points(temp,pch=16,cex=0.4,col=change.col())
		}
		line<-function(x,y){
			if(lin==F){lin<<-T}
			else{
				lines(c(x,prex),c(y,prey),type='l',lwd=5*cex/10,col=change.col())
				lin<<-F
			}
			prex<<-x
			prey<<-y
		}
		circle<-function(x,y){
			if(cir==F){cir<<-T}
			else{
				temp<-(1:1000)/1000
				ra<-sqrt((x-prex)^2+(y-prey)^2)
				points(prex+cos(temp*2*pi)*ra,prey+sin(temp*2*pi)*ra,cex=0.8*cex/10,col=change.col())
				cir<<-F
			}		
			prex<<-x
			prey<<-y	
		}
		rect<-function(x,y){
			if(rec==F){rec<<-T}
			else{
				lines(c(x,prex),c(prey,prey),type='l',lwd=5*cex/10,col=change.col())
				lines(c(x,x),c(y,prey),type='l',lwd=5*cex/10,col=change.col())
				lines(c(prex,prex),c(y,prey),type='l',lwd=5*cex/10,col=change.col())
				lines(c(x,prex),c(y,y),type='l',lwd=5*cex/10,col=change.col())
				rec<<-F
			}
			prex<<-x
			prey<<-y	
		}
		linep<-function(x,y){
			if(lin==F){lin<<-T}
			else{lines(c(x,prex),c(y,prey),type='l',lwd=5*cex/10,col=change.col())}
			prex<<-x
			prey<<-y
		}
		px<-round((px-p1[1])/(p2[1]-p1[1]),3)
		py<-round((py-p1[2])/(p2[2]-p1[2]),3)
		if(px<1 & px>-0.05 & py<1 & py>0){
			if(px<0.13){choose.col(px,py)}
			else{
				if(pen==1)linep(px,py)
				if(pen==2)point(px,py)
				if(pen==3)painting(a,px,py)
				if(pen==4)line(px,py)
				if(pen==5)circle(px,py)
				if(pen==6)rect(px,py)
				if(pen==7)points(px,py,pch=16,col='white',cex=cex)	
			}
		}
		return(NULL)
	}
	painting<-function(a,px,py){
		paint<-function(x,y){
			if(!(x<=1e-3 | y<=1e-3 | x>1 | y>1)){
				if(record[x*(1e+3),y*(1e+3)]==record[px*(1e+3),py*(1e+3)] & flag[x*(1e+3),y*(1e+3)]==0){
					##points(x,y,pch=15,cex=0.1,col=color)
					flag[x*(1e+3),y*(1e+3)]<<-1
					if(round(x,1)>(1e-3) & round(y,1)>(1e-3) & round(x,1)<=1-(1e-3) & round(y,1)<=1-(1e-3))if(flag[x*(1e+3),y*(1e+3)+1]==0)paint(x,y+(1e-3))
					if(round(x,1)>(1e-3) & round(y,1)>(1e-3) & round(x,1)<=1-(1e-3) & round(y,1)<=1-(1e-3))if(flag[x*(1e+3)+1,y*(1e+3)]==0)paint(x+(1e-3),y)
					if(round(x,1)>(1e-3) & round(y,1)>(1e-3) & round(x,1)<=1-(1e-3) & round(y,1)<=1-(1e-3))if(flag[x*(1e+3),y*(1e+3)-1]==0)paint(x,y-(1e-3))
					if(round(x,1)>(1e-3) & round(y,1)>(1e-3) & round(x,1)<=1-(1e-3) & round(y,1)<=1-(1e-3))if(flag[x*(1e+3)-1,y*(1e+3)]==0)paint(x-(1e-3),y)
				}
			}
		}
		px<-(px-p1[1])/(p2[1]-p1[1])
		py<-(py-p1[2])/(p2[2]-p1[2])
		px<-round(px,1)
		py<-round(py,1)
		x<-px
		y<-py
		##if(!(x<0 | y<0 | x>1 | y>1) & record[x*(1e+3),y*(1e+3)]==record[px*(1e+3),py*(1e+3)] & flag[x*(1e+3),y*(1e+3)]==0){paint(x,y)}
		return(NULL)
	}
	col<-function(K){
		K<-tolower(K)
		if(K=='q'){pen<<-1}
		if(K=='w'){pen<<-2}
		if(K=='e'){pen<<-3}
		if(K=='r'){pen<<-4}
		if(K=='t'){pen<<-5}
		if(K=='y'){pen<<-6}
		if(K=='u'){pen<<-7}
		if(K%in%as.character(1:9)){cex<<-as.numeric(K)}
		points(0,0,col='white')
	}
	getGraphicsEvent(prompt="create.reg",onMouseMove=add2,onMouseDown=add1,onKeybd=col)
}
draw()