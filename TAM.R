#добавлением в датасет параметров параметры новых заданий

library(TAM)
setwd("C:/Users/Алексей/Downloads")#устанавливаем рабочую директорию
Data=read.csv('bts.csv',sep=';')#читаем файл с ответами, sep устанавливаем в зависимости от формата
Data[32]=NULL#удаление столбца с суммой баллов
Data[1]=NULL#удаление столбца с фамилиями
model_2pl=TAM::tam.mml.2pl(Data,irtmodel="2PL")#обучение модели
newparams=as.data.frame(cbind(model_2pl$AXsi[,2],
                              model_2pl$item$B.Cat1.Dim1))#создание датасета с параметрами новых заданий
oldparams=tryCatch({
  as.data.frame(read.csv('ab2pl10.csv',sep=';'))
  },
                   warning=function(cond){
                     return(NULL)
                     },
                   error=function(cond){
                     return(NULL)
                     }
                   )#считывание датафрейма с параметрами старых заданий, если его нет-NULL
d=tryCatch({
  rbind(oldparams,newparams)
  },
  warning=function(cond){
    return(newparams)
    },
  error=function(cond){
    return(newparams)
    })#конкатенирование таблиц
row.names(newparams)=NULL#новые индексы по порядку
write.table(newparams,'ab2pl10.csv',sep=';')#запись файла
