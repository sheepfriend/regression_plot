create.reg<-function(){
	x11()
	plot(c(0,0),xlim=c(0,1),ylim=c(0,1),col='white',main="please click c(0,0) first then c(1,1) then the points")
	points(c(0,1),c(0,1))
	data<-data.frame(x=0,y=0)
	new<-F
	p1<-c()
	p2<-c()
	temp<-0
	add<-function(a,px,py){
		if(temp==0){p1<<-c(px,py);temp<<-temp+1;points(0,0,col='white')}
		else if(temp==1){p2<<-c(px,py);temp<<-temp+1;points(1,1,col='white')}
		else{
			px<-(px-p1[1])/(p2[1]-p1[1])
			py<-(py-p1[2])/(p2[2]-p1[2])
			if(px<1 & px>0 & py<1 & py>0){
				data<<-rbind(data,c(px,py))
				points(px,py)
				if(new==F){data<-data[-1,];new=T}
				if(nrow(data)>1){
					reg<-lm(y~x,data=data)
					print(summary(reg))
					abline(reg)
				}
			}
		}
	}
	getGraphicsEvent(prompt="create.reg",onMouseDown=add)
}
plot.reg<-function(data){
	x11()
	data<-data.frame(x=data[,1],y=data[,2])
	p1<-c()
	p2<-c()
	temp<-0
	data[,1]<-(data[,1]-min(data[,1])+0.1)/(max(data[,1])-min(data[,1])+0.2)
	data[,2]<-(data[,2]-min(data[,2])+0.1)/(max(data[,2])-min(data[,2])+0.2)
	print(data)
	plot(data,xlim=c(0,1),ylim=c(0,1),main="please click c(0,0) first then c(1,1) then the points")
	points(c(0,1),c(0,1))
	reg<-lm(y~x,data=data)
	abline(reg)
	find<-function(a,px,py){
		if(temp==0){p1<<-c(px,py);temp<<-temp+1;points(0,0,col='white')}
		else if(temp==1){p2<<-c(px,py);temp<<-temp+1;points(1,1,col='white')}
		else if(temp==2){
			px<-(px-p1[1])/(p2[1]-p1[1])
			py<-(py-p1[2])/(p2[2]-p1[2])
			result<-which(abs(px-data[,1])<=0.01 & abs(py-data[,2])<=0.01)
			if(length(result)==1){
				points(data[result,1],data[result,2],col='white')
			}
			else if(length(result)>1){
				result<-result[1]
				points(data[result,1],data[result,2],col='white')
			}
			if(length(result)==1){
				data<-data[-result,]
				abline(reg,col='white')
				reg<<-lm(y~x,data=data)
				print(reg)
				abline(reg)
			}
		}	
	}
	getGraphicsEvent(prompt="plot.reg",onMouseDown=find)
}