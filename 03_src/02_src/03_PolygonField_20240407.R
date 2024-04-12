#####################################
#
# ③	近世村ポリゴンデータの作成
#
# 2023/11/08 遙嶽
#####################################

#作業フォルダの設定
setwd("D:\\03_src")

#出力ファイル名
output<-".\\03_result\\近世村ポリゴンデータ.csv"
filename<-".\\03_result\\近世村ポイントデータ.csv"
VID<-read.csv(".\\01_org\\04_RekihakuRecord_VID_AID.csv")
tmp<-read.csv(filename)

#VIDをtmpに結合
UID<-rep("0",length(tmp[,1]))
for(i in 1:length(tmp[,1])){
 if(tmp[i,24]>0) UID[i]<-VID[VID[,2]==tmp[i,8],3]
}

#11,村名
#9,石高
#6,領分
#明治の村ごとの石高（合計）と多い順の領分

kokudaka_col<-9#石高1
ryobun_col<-7#旧領名

#統合コード
Key<-VID[,3]
city<-unique(Key)
l<-length(city)


#IDの開始番号
id<-1

out <- file(output, "w")
writeLines("番号,村数,国ID,国名,郡ID,郡名,国郡,村ID,AID,村名,よみ,石高計,領分１,石高１,領分２,石高２,領分３,石高３,領分４,石高４,領分５,石高５,領分６,石高６,領分７,石高７,領分８,石高８",out)

for(i in 1:l){
ss<-tmp[UID==city[i],]

#石高
meiji_city<-c(as.character(ss[,12]),as.character(ss[,14]),
as.character(ss[,16]),as.character(ss[,18]),
as.character(ss[,20]),as.character(ss[,22]))
kokudaka<-c(as.numeric(as.character(ss[,13])),
as.numeric(as.character(ss[,15])),
as.numeric(as.character(ss[,17])),
as.numeric(as.character(ss[,19])),
as.numeric(as.character(ss[,21])),
as.numeric(as.character(ss[,23])))


#領分と石高統合
t<-aggregate(kokudaka,by=list(meiji_city), FUN=sum)
#無名""の削除
t<-t[t[,1]!="",]

d<-length(ss[,1])
#村名の統合,村IDUID

if(d==1){#単一の場合
 vi<-as.character(ss[,9])
 viy<-as.character(ss[,10])
 sid<-as.character(ss[,7])
 uid<-as.character(ss[,8])
}else{
 vi<-as.character(ss[1,9])#名称の統合
 viy<-as.character(ss[1,10])#よみかたの統合
 for( j in 2:d){
  vi<-paste(vi,as.character(ss[j,9]),sep="・")
  viy<-paste(viy,as.character(ss[j,10]),sep="・")
  sid<-paste(as.character(ss[1,7]),"A",sep="")
#UIDに最小の番号を用いる（2021/12/19に変更なのでそれ以前のものは要修正）
  uid<-paste(sort(as.character(ss[,8]))[1],"A",sep="")
 }
}

#統合

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

li<-c(li,rep("",16-length(li)))

tl<-c(i,d,t(ss[1,2:6]),sid,uid,vi,viy,sum(ss[,11]),li)
#print(tl)
writeLines(t(tl),out, sep=",")
writeLines("",out)
}

close(out)
