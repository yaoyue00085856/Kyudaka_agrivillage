#####################################
#
# 01_データベースれきはくの整理
#
# 2024/7/7 遙嶽
#####################################

#①作業フォルダの設定
setwd("D:\\03_src")


#データベースれきはく
dBReki<-read.csv(".\\01_org\\01_Rekihaku.csv", fileEncoding="sjis")
#UID毎の緯度経度
UIDloc<-read.csv(".\\01_org\\03_VID_latlon.csv", fileEncoding="sjis")
#基本形式
BaseRecord<-read.csv(".\\01_org\\02_Base_Record_diff.csv", fileEncoding="sjis")

#出力ファイル名
out <- file(".\\03_result\\BaseRecord_Join.csv", "w", encoding="SJIS")
writeLines("No,RekiID,国ID,郡ID,国郡,村ID,VID,旧国名,旧郡名,村名,ふりがな,旧領名取調帳,旧領名整理,石高,備考,longitude,latitude",out)

for(i in 1:length(BaseRecord[,1])){
 #データベースれきはくの統合
 tmp<-BaseRecord[i,]
 tmp[1]<-i
 if(!is.na(tmp[15])){#れきはくと結合する場合
  tmpdB<-dBReki[dBReki[,1]==as.numeric(tmp[15]),]
  if(tmp[16]==0){#村名が更新されていなければ（Flag=0なら）コピー
　tmp[9]<-tmpdB[4]#村名
  tmp[10]<-tmpdB[5]#ふりがな
  }
  tmp[11]<-tmpdB[6]#取調帳
　if(tmp[17]==0){
    if(!is.na(as.numeric(tmpdB[9]))){
    tmp[13]<-as.numeric(tmpdB[9])#石高が更新されていなければ（Flag=0なら）コピー
   }else{tmp[13]<-0}
  }
　#文字列である場合は0とする
　if(is.na(tmp[13]))tmp[13]<-0

  if(sum(UIDloc[8]==as.character(tmp[6]))!=0){#位置がある場合にコピー
   ll<-UIDloc[UIDloc[8]==as.character(tmp[6]),9:10]
   }else{ll<-t(c(0,0))}

  #print(cbind(tmp[1:14],ll))
  writeLines(t(cbind(i,tmp[,15],tmp[2:14],ll)),out, sep=",")
  writeLines("",out)
 
 }else{#新規追加村
  ll<-UIDloc[UIDloc[8]==as.character(tmp[6]),9:10]
  if(is.na(tmp[13]))tmp[13]<-0
  print(cbind(tmp[1:14],ll))
  writeLines(t(cbind(i,0,tmp[2:14],ll)),out, sep=",")
  writeLines("",out)
 }
}
close(out)
