#####################################
#
# A	ß¢º|Cgf[^Ìì¬
#
# 2024/4/7 ê¡Ô
#####################################

#ìÆtH_ÌÝè
setwd("D:\\03_src")

#ÇÝÝ
tmp<-read.csv(".\\03_result\\BaseRecord_Join.csv", fileEncoding="sjis")

#¾¡Ìº²ÆÌÎivjÆ½¢ÌÌª


kokudaka_col<-13#Î1ÌtB[h
ryobun_col<-12#Ì¼®ÌtB[h

#VID
city<-unique(tmp[,6])
l<-length(city)


out <- file(".\\03_result\\ß¢º|Cgf[^.csv", "w", encoding="SJIS")
writeLines("Ô,,ID,¼,SID,S¼,S,ºID,VID,º¼,æÝ,Îv,ÌªP,ÎP,ÌªQ,ÎQ,ÌªR,ÎR,ÌªS,ÎS,ÌªT,ÎT,ÌªU,ÎU,Longitude,Latitude",out)
for(i in 1:l){
 ss<-tmp[tmp[,6]==city[i],]
 kokudaka<-as.numeric(as.character(ss[,kokudaka_col]))
 meiji_city<-as.character(ss[,ryobun_col])

 t<-aggregate(kokudaka,by=list(meiji_city), FUN=sum)

#ÌªÀÑÖ¦
if(length(t[,1])==1){
li<-rep(0,2)
li[1]<-t[,1]
li[2]<-t[,2]
}else{
	#ÀÑÖ¦
	t<-t[order(-(t[,2])),]
	j<-length(t[,1])
	li<-rep(0,2*j)
	for(k in 1:j){
		li[k*2-1]<-t[k,1]
		li[k*2]<-t[k,2]
	}
}

#JEg
if(length(t[,1])==1){ai<-1}else{ai<-length(t[,1])}

tl<-c(i,ai,as.character(ss[1,c(2,7,3,8,4,5,6,9,10)]),sum(kokudaka),li,rep("",12-length(t[,1])*2),as.character(ss[1,c(15:16)]))
print(tl)
writeLines(t(tl),out, sep=",")
writeLines("",out)

}

close(out)
