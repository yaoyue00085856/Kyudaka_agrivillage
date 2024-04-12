#####################################
#
# ②	近世村ポイントデータの作成
#
# 2024/4/7 遙嶽
#####################################

#作業フォルダの設定
setwd("D:\\03_src")

#読み込み
tmp<-read.csv(".\\03_result\\BaseRecord_Join.csv", fileEncoding="sjis")

#明治の村ごとの石高（合計）と多い順の領分


kokudaka_col<-13#石高1のフィールド
ryobun_col<-12#旧領名整理のフィールド

#VID
city<-unique(tmp[,6])
l<-length(city)


out <- file(".\\03_result\\近世村ポイントデータ.csv", "w", encoding="SJIS")
writeLines("番号,相給,国ID,国名,郡ID,郡名,国郡,村ID,VID,村名,よみ,石高計,領分１,石高１,領分２,石高２,領分３,石高３,領分４,石高４,領分５,石高５,領分６,石高６,Longitude,Latitude",out)
for(i in 1:l){
 ss<-tmp[tmp[,6]==city[i],]
 kokudaka<-as.numeric(as.character(ss[,kokudaka_col]))
 meiji_city<-as.character(ss[,ryobun_col])

 t<-aggregate(kokudaka,by=list(meiji_city), FUN=sum)

#領分並び替え
if(length(t[,1])==1){
li<-rep(0,2)
li[1]<-t[,1]
li[2]<-t[,2]
}else{
	#並び替え
	t<-t[order(-(t[,2])),]
	j<-length(t[,1])
	li<-rep(0,2*j)
	for(k in 1:j){
		li[k*2-1]<-t[k,1]
		li[k*2]<-t[k,2]
	}
}

#相給カウント
if(length(t[,1])==1){ai<-1}else{ai<-length(t[,1])}

tl<-c(i,ai,as.character(ss[1,c(2,7,3,8,4,5,6,9,10)]),sum(kokudaka),li,rep("",12-length(t[,1])*2),as.character(ss[1,c(15:16)]))
print(tl)
writeLines(t(tl),out, sep=",")
writeLines("",out)

}

close(out)
