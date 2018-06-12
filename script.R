# ЧАСТЬ-1. готовим датафрейм. 
	# шаг-1. вчитываем таблицу с данными по геоморфологии. делаем из нее исходный датафрейм


MDepths <- read.csv("DepthTect.csv", header=TRUE, sep = ",")
	# шаг-2. чистим датафрейм от NA значений
MDFl <- na.omit(MDepths) 
row.has.na <- apply(MDFl, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
head(MDFl) # смотрим очищенный датафрейм. теперь с ним работаем. 

DFDT = melt(setDT(MDFl), measure = patterns("^profile", "^tectonics", "^tg"), value.name = c("depth", "tectonics", "trench_angle"))
	# на выходе имеем лист с 4 колонками (номера измерений 1:518, номера профилей 1:25, глубины, тектоника).
DFDT
	# в явном виде ему указываем: номера профилей - факторное значение, глубины - численное значение
DFDT$variable <- as.factor(DFDT$variable)
DFDT$depth<- as.numeric(DFDT$depth)
DFDT$trench_angle <- as.numeric(DFDT$trench_angle)


###############head(MDF) 

# ЧАСТЬ 2: рисуем график рассеяния максимальных глубин по профилям // Scatterplot
gi <- ggplot(MDF, aes(profile, igneous_volc)) + 
	geom_line() +   
	geom_smooth(method="lm", se=F) +   
labs(subtitle="Correlation: Maximal Depths vs Profiles", 
	y="Depths (Max)",  x="profiles",         
	title="Scatterplot with overlapping points", caption="Source: R, QGIS")
gi

angles <- ggplot(MDF, aes(profile, tg_angle, colour = tg_angle)) + 
	geom_point() +   
#	geom_smooth(method="lm", se=F) +   
labs(subtitle="Correlation: Angles (tg(A/H)) vs Profiles", 
	y="Angles",  x="profiles",         
	title="Scatterplot with overlapping points", caption="Source: R, QGIS")
angles

ab<- angles + brackets(0, 0.05, 5, 0.55, h = NULL,  ticks = 0.5, curvature = 0.5, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)


library(pBrackets)
grid.locator(unit="native") 
angles
grid.brackets(155, 155, 78,  155, lwd=1, col="red") 
grid.brackets(307, 155, 155, 155, lwd=1, col="red") 
grid.brackets(420, 155, 307, 155, lwd=1, col="red")
grid.brackets(420, 155, 593, 155, lwd=1, col="red")
grid.brackets(686, 155, 593, 155, lwd=1, col="red")

	
ab<- angles + grid.brackets(155, 155, 80, 155, lwd=1, col="red") 
grid.brackets(325, 155, 155, 155, lwd=1, col="red") 
	
pn <- ggplot(data= MDF, aes(x=MDF$profile)) + 
		geom_point(mapping = aes(x = MDF$profile, y = MDF$igneous_volc, colour = "igneous_volc")) + 	   		 
		geom_smooth(mapping = aes(x = MDF$profile, y = MDF$igneous_volc, colour = "si"), method = "loess", se = F, size=.3, linetype = "solid", span = 0.95, show.legend=TRUE) + 	
		geom_point(mapping = aes(x = MDF$profile, y = MDF$plate_maria, colour = "plate_maria")) + 
		geom_smooth(mapping = aes(x = MDF$profile, y = MDF$plate_maria, colour = "ma"),  method = "loess", se = F, size=.3, linetype = "solid", show.legend=TRUE, span = 0.9) +
		scale_color_manual(values = c("igneous_volc" = "purple", "plate_maria"="orange", "si" = "green", "ma" = "blue")) +
		scale_x_continuous() +
		scale_y_continuous("igneous_volc", sec.axis = sec_axis(~./1, name = "plate_maria", breaks = c(seq(0.0, 300, by = 50))))
pn 
		
		
# вариант с белой полосой в центре сине-красной палитры
mid<-mean(DFDT$depth) # вычисление среднего значения 
g2<- ggplot(DFDT, aes(x = variable, y = depth, color = depth)) + 
	geom_point() + 

	scale_y_continuous(breaks = c(seq(-10000, -0, by = 1000))) +	
#	scale_color_gradient(low="blue", high="red") + # можно сделать палитру сине-красной
	scale_color_gradient2(midpoint=mid, low="blue", mid="white",                      high="red", space ="Lab") +
labs(subtitle="Scope of Depth Extension by Profiles", 
	y = "Depth",  x ="profiles",         
	title="Scatterplot with overlapping points", caption="Source: R, QGIS")
g2

# вариант в синей гамме (дефолтный)
alldepths<- ggplot(DFDT, aes(x = variable, y = depth, color = depth)) + 
	geom_point() + 
	scale_y_continuous(breaks = c(seq(-10000, -0, by = 1000))) +	
labs(subtitle="Scope of Depth Extension by Profiles", 
	y = "Depth",  x ="profiles",         
	title="Scatterplot with overlapping points", caption="Source: R, QGIS")
alldepths


# ЧАСТЬ 3 2 графика вместе, 2 оси y
		# совмещаем 2 категории Y на одной оси X на общем графике
p<- ggplot(DFDT, mapping = aes(x = DFDT$variable)) + 
	geom_point(DFDT, mapping = aes(x = DFDT$variable, y = DFDT$depth), color = "blue") + 	
	geom_point(DFDT, mapping = aes(x = DFDT$variable, y = DFDT$trench_angle), color = "red") +
	scale_y_continuous("Depth", sec.axis = sec_axis(~./200000, name = "Angle")) 

p<- p + scale_y_continuous(sec.axis = dup_axis())
p<- p + scale_y_continuous("Depth", sec.axis = sec_axis(~./200000, name = "Angle"))
p


		# второй график


p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))

p <- p +scale_y_continuous(breaks = c(seq(-10000, -0, by = 1000))) +	
labs(subtitle="Scope of Depth Extension by Profiles", 
	y = "Depth",  x ="profiles",         
	title="Scatterplot with overlapping points", caption="Source: R, QGIS")
g2

#
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))

p<- ggplot(DFDT, aes(x = variable)) + 
	geom_point(aes(y = depth, color = 'blue')) + 
	geom_point(aes(y = trench_angle, color = 'red')) + 
	scale_y_continuous(breaks = c(seq(-10000, -0, by = 1000))) +
	
	scale_y_continuous(
			name = "Depth", sec.axis = sec_axis(~./5, 
			name = "Angle"))
	
labs(subtitle="Scope of Depth Extension by Profiles", 
	y = "Depth",  x ="profiles",         
	title="Scatterplot with overlapping points", caption="Source: R, QGIS")

