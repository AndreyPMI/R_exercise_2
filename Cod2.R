




# загрузка пакетов
library('data.table')          # работаем с объектами "таблица данных"
library('moments')             # коэффициенты асимметрии и эксцесса 
library('lattice')
library('ggplot2')









fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'
if (!file.exists('./data')) {
   dir.create('./data')
}
if (!file.exists('./data/download.log')) {
   file.create('./data/download.log')
}
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
   download.file(fileURL,
                 './data/040510-Imp-RF-comtrade.csv')
}
# читаем данные из загруженного .csv во фрейм,
# если он ещё не существует
if (!exists('DT.import')) {
   DT.import <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv',
                                    stringsAsFactors = F))
}
str(DT.import)
x <- DT.import$Trade.Value.USD
y <- DT.import$Netweight.kg


####################################################################################################
fit <- lm(y ~ x)
summary(fit) 


# пробуем регрессию на логарифмах
y[y == 0] <- NA
fit.log <- lm(log(y) ~ log(x))
summary(fit.log) 
R.sq.log <- summary(fit.log)$r.squared 
# новый столбец, в котором будут заполнены пропуски
DT.import[, Netweight.kg.model := Netweight.kg]
# прогноз по модели на логарифмах сохраняем как вектор
y.model.log <- predict(fit.log,
                       newdata = data.frame(x = NAs))
# пересчитываем в исходные единицы измерения y
y.model <- exp(y.model.log)
# заполняем пропуски модельными значениями
DT.import[is.na(Netweight.kg.model),
          Netweight.kg.model := round(y.model, 0)]
# смотрим результат
DT.import[is.na(Netweight.kg), Netweight.kg.model , Trade.Value.USD]


#####################################
x <- DT.import[,Reporter]
sub_educ <- subset(DT.import, Reporter=c(Azerbaijan,Armenia,Kyrgyzstan))
#style="Color:red;"
#Далее другой способ реализации, он не доделан, но вполне рабочий (не пригодился)
library('dplyr') # функции манипуляции данными
#>
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#>
#> filter, lag
#> The following objects are masked from 'package:base':
#>
#> intersect, setdiff, setequal, union
library('data.table') # объекты "таблица данных"
#>,Armenia, Azerbaijan)
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#>
#> between, first, last
# DT.sng <- filter(DT.import,Reporter=="Kyrgyzstan"|Reporter=="Armenia"|Reporter=="Azerbaijan")
# str(DT.import)
# DT.1per <- filter(DT.sng,Period>=201001 & Period<=201408)
# table1 <- select(DT.1per,Period,Netweight.kg.model)
# table1 <- mutate(table1,fact=1)
# table1 <- data.frame(table1)
# DT.2per <- filter(DT.sng,Period>=201409 & Period<=201812)
# table2 <- select(DT.2per,Period,Netweight.kg.model)
# table2 <- mutate(table2,fact=2)
# table2 <- data.frame(table2)
# table3 <- rbind(table1,table2)
                 ############
# ящики с усами по месяцам
boxplot(table3$Netweight.kg.model~table3$fact,
        xlab = "Период",
        ylab = 'Масса поставок',col = "coral","red" )


fact <- DT.import$Period
head(fact)
str(fact)

DT.import$Period[500]
for (i in 1:length(fact)){
   if((DT.import$Period[i]>=201001) &(DT.import$Period[i]<=201408))
   {
      fact[i] <- 1
   }
   else if(DT.import$Period[i]>=201409 & DT.import$Period[i]<=201812)
   {
      fact[i] <- 2
   }
   else 
   {
      fact[i] <- 0
   }

}

head(fact)
DT.import[, fact := as.double(fact)]
##тут дописать второй цикл по введению фактора даты 
########################################
Rep <- DT.import$Reporter
head(Rep)
str(Rep)
Coun.1 <- "Kyrgyzstan"
Coun.2 <- "Armenia"
Coun.3 <- "Azerbaijan"
Coun.4 <- "Belarus"
Coun.5 <- "Russian Federation"

for (i in 1:length(Rep)){
   if(setequal(Rep[i],Coun.1)|setequal(Rep[i],Coun.3)|setequal(Rep[i],Coun.2) )
   {
      Rep[i] <- 1
   }
   else if(setequal(Rep[i],Coun.1)|setequal(Rep[i],Coun.4)|setequal(Rep[i],Coun.5) )
   {
      Rep[i] <- 2
   }
   else{
      Rep[i] <- 3
   }
}
DT.import[, Rep := as.double(Rep)]
#########################################
(?boxplot())
png('Pic-01.png', width = 500, height = 500)
boxplot(DT.import$Netweight.kg ~ as.factor(DT.import$Year), 
        boxwex = 0.3, at = 1:10 - 0.2,
        subset = Rep == 1, col = "red",
        xlab = 'Год', 
        ylab = 'Суммарная масса поставок', yaxs = "i")
boxplot(DT.import$Netweight.kg ~ as.factor(DT.import$Year), 
        add = TRUE,
        boxwex = 0.3, at = 1:10 - 0.2,
        subset = Rep == 2, col = "green")
boxplot(DT.import$Netweight.kg ~ as.factor(DT.import$Year), 
        add = TRUE,
        boxwex = 0.3, at = 1:10 - 0.2,
        subset = Rep == 3, col = "blue")
legend('topright', legend = c(' СНГ', 
                              ' Таможенный союз ','другие страны'),fill=(c("red","green","blue")))
dev.off()


#########################################
#########################################


#########################################
##################
DT.import[, SNG := factor(Rep, levels = c(1,2,3), 
                          labels = c(' СНГ без Белоруссии и Казахстана', 
                                     ' Таможенный союз России, Белоруссии, Казахстана','другие страны'))]
DT.import[, Data := factor(fact, levels = c(1,2), 
                          labels = c('с 2010 по август 2014 года', 
           'с сентября 2014 по декабрь 2018 года'))]
png('Pic-02.png', width = 500, height = 500)
bwplot( Netweight.kg ~  as.factor(Year)|SNG* Data , data = DT.import, 
        xlab = 'Год', 
        ylab = 'Суммарная масса поставок')
dev.off()

##########################
png('Pic-03.png', width = 500, height = 500)
DT.import2 <- DT.import
DT.import2 <- filter(DT.import2, fact==1|fact==2)
gp <- ggplot(data = DT.import2, 
             aes(x = as.factor(Year), 
                 y = Netweight.kg, 
                 color = SNG))
gp <- gp + geom_boxplot()



?facet_grid
gp <- gp + facet_grid( .~ Data)
gp <- gp + xlab('Год')
gp <- gp + ylab('Суммарная масса поставок')
gp
dev.off()

##############

