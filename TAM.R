#����������� � ������� ���������� ��������� ����� �������

library(TAM)
setwd("C:/Users/�������/Downloads")#������������� ������� ����������
Data=read.csv('bts.csv',sep=';')#������ ���� � ��������, sep ������������� � ����������� �� �������
Data[32]=NULL#�������� ������� � ������ ������
Data[1]=NULL#�������� ������� � ���������
model_2pl=TAM::tam.mml.2pl(Data,irtmodel="2PL")#�������� ������
newparams=as.data.frame(cbind(model_2pl$item$AXsi_.Cat1,
                              model_2pl$item$B.Cat1.Dim1))#�������� �������� � ����������� ����� �������
oldparams=tryCatch({
  as.data.frame(read.csv('ab2pl.csv',sep=';'))
  },
                   warning=function(cond){
                     return(NULL)
                     },
                   error=function(cond){
                     return(NULL)
                     }
                   )#���������� ���������� � ����������� ������ �������, ���� ��� ���-NULL
d=tryCatch({
  rbind(oldparams,newparams)
  },
  warning=function(cond){
    return(newparams)
    },
  error=function(cond){
    return(newparams)
    })#���������������� ������
row.names(newparams)=NULL#����� ������� �� �������
write.table(newparams,'ab2pl.csv',sep=';')#������ �����