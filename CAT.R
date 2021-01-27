library(catR)
setwd("C:/Users/Алексей/Downloads")#устанавливаем рабочую директорию
Data=read.csv('ab2pl10.csv',sep=';')#читаем файл с параметрами, sep устанавливаем в зависимости от формата
m=c()#заданные вопросы
x=c()#полученные ответы
Data=cbind(Data,rep(0,times=30),rep(1,times=30))
bank=seq(1,30)#нужен read.csv с чтением банка
ans=seq(1,30)#нужен read.csv c чтением ответов
k=startItems(Data, seed=as.integer(runif(1,1,10000)), nrItems = 2)#рандомная выгрузка двух вопросов
for (var in bank[c(k$items)]) {#печать вопросов и прием ответом
  print(var)
  m=c(m,var)
  l=as.integer(readline())
  if (l==ans[var]){#правильный/неправильный ответ
    x=c(x,1)
  }else{
    x=c(x,0)
  }
}
p=Data[c(k$items),1:4]#параметры заданных вопросов

eap=thetaEst(p,x,method='EAP')#ожидаемая апостериорная оценка
bm=thetaEst(p,x,current.th = eap, method='BM')#максимальная апостериорная оценка


while (TRUE){
  bmse=semTheta(bm,p,x=x,method='BM')#ошибка среднего ма оценки, считаем для определения точности оценки
  stop <- list(rule = c("length", "precision"), thr = c(20, 0.2))#условие остановки теста
  if(checkStopRule(th = bm, it=p,se = bmse, N=length(m), stop = stop)$decision){#проверка стоп-условия
    break
  }
  j=nextItem(Data,x=x,theta=bm, out=m)#выгрузка следующего вопроса
  print(bank[c(j$item)])#печать вопроса
  m=c(m,c(j$item))
  l=as.integer(readline())
  if (l==ans[c(j$item)]){
    print
    x=c(x,1)
  }else{
    x=c(x,0)
  }
  p=rbind(p,Data[c(j$item),1:4])#парметры заданных вопросов

  
  bm=thetaEst(p,x,current.th=bm,method='BM')#максимальная апостериорная оценка
}
print(bm)#печать окончательной оценки
