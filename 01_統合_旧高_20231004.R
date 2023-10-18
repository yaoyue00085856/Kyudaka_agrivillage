
#同郡同名があるため順番で統合

setwd("D:\\HistoryGIS\\03_Japan\\20210508_農業センサス領分作成\\0903_公開")

#UID毎の緯度経度
UIDloc<-read.csv("UID_latlon_230930.csv")
#データベースれきはく
dBReki<-read.csv("データベースれきはく_20230905.csv")
#基本形式
BaseRecord<-read.csv("Base_Record.csv")

out <- file("BaseRecord_Join.csv", "w")
writeLines("番号,国ID,郡ID,国郡,村ID,UID,旧国名,旧郡名,村名,ふりがな,旧領名取調帳,旧領名整理,石高,備考,longitude,latitude",out)

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
  writeLines(t(cbind(tmp[1:14],ll)),out, sep=",")
  writeLines("",out)
 
 }else{#新規追加村
  ll<-UIDloc[UIDloc[8]==as.character(tmp[6]),9:10]
  if(is.na(tmp[13]))tmp[13]<-0
  print(cbind(tmp[1:14],ll))
  writeLines(t(cbind(tmp[1:14],ll)),out, sep=",")
  writeLines("",out)
 }
}
close(out)
