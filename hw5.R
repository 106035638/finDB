install.packages("ggplot2")
library(ggplot2)
head(diamonds)
#
set.seed(5)
diamonds.subset <- diamonds[sample(nrow(diamonds), 100), ]
#
qplot(diamonds$carat, diamonds$price)
#
qplot(carat, price, data = diamonds)
#
qplot(log(carat), log(price), data = diamonds)
#
qplot(carat, x * y * z, data = diamonds)
#
qplot(carat, price, data = diamonds.subset, color = color)
#
qplot(carat, price, data = diamonds.subset, shape = cut)
#
qplot(carat, price, data = diamonds, alpha = I(1/10))
#
qplot(carat, price, data = diamonds, alpha = I(1/100))
#
qplot(carat, price, data = diamonds.subset,
      geom = c("point", "smooth"))
#
qplot(carat, price, data = diamonds,
      geom = c("point", "smooth"))
#
qplot(color, price / carat, data = diamonds, geom = "boxplot")
#
qplot(color, price / carat, data = diamonds, geom = "jitter")
#
qplot(color, price / carat, data = diamonds,
      geom = "jitter", alpha = I(1 / 5))
#
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 50))
#
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 200))
#
qplot(carat, data = diamonds, geom = "histogram")
#
qplot(carat, data = diamonds, geom = "histogram",
      binwidth = 0.5, xlim = c(0, 3))
#
qplot(carat, data = diamonds, geom = "histogram",
      binwidth = 0.01, xlim = c(0, 3))
#
qplot(carat, data = diamonds, geom = "histogram",
      fill = color)
#
qplot(carat, data = diamonds, geom = "density")
#
qplot(carat, data = diamonds, geom = "density", adjust = 3)
#
qplot(carat, data = diamonds, geom = "density",
      color = color)
#
qplot(carat, ..density.., data = diamonds,
      geom = c("histogram", "density"))
#
qplot(color, data = diamonds, geom = "bar")
#
qplot(color, data = diamonds, geom = "bar", weight = carat) +
  ylab("carat")
#
qplot(date, unemploy / pop, data = economics, geom = "line")
#
qplot(date, uempmed, data = economics, geom = "line")
#
qplot(unemploy / pop, uempmed, data = economics,
      geom = c("point", "path"))
#
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics,
      geom = "path", colour = year(date))
#
qplot(carat, data = diamonds, facets = color ~ cut,
      geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
#
qplot(carat, data = diamonds, facets = color ~ .,
      geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
#
qplot(
  carat, price, data = diamonds.subset,
  xlab = "Price ($)", ylab = "Weight (carats)",
  main = "Price-weight relationship"
)
#
qplot(
  carat, price/carat, data = diamonds.subset,
  ylab = expression(frac(price,carat)),
  xlab = "Weight (carats)",
  main="Small diamonds",
  xlim = c(.2,1)
)
#
qplot(carat, price, data = diamonds.subset, log = "xy")
#
# 產生繪圖用的因子變數
mtcars$gear <- factor(mtcars$gear,levels=c(3, 4, 5),
                      labels=c("3gears", "4gears", "5gears"))
mtcars$am <- factor(mtcars$am,levels=c(0, 1),
                    labels=c("Automatic", "Manual"))
mtcars$cyl <- factor(mtcars$cyl,levels=c(4, 6, 8),
                     labels=c("4cyl", "6cyl", "8cyl"))

# 以 gear 分組畫出 mpg 密度函數圖
qplot(mpg, data = mtcars, geom = "density",
      fill = gear, alpha = I(.5),
      main="Distribution of Gas Milage",
      xlab="Miles Per Gallon",
      ylab="Density")
# 將資料以 gear 與 cylinder 分組，
# 畫出 mpg 與 hp 的散佈圖，
# 並且以資料點的顏色與形狀標示 am
qplot(hp, mpg, data = mtcars, shape = am, color = am,
      facets = gear~cyl, size = I(3),
      xlab = "Horsepower", ylab = "Miles per Gallon")
# 畫出箱形圖，
# 並且在上面用 jitter 資料點畫出實際資料的位置
qplot(gear, mpg, data = mtcars, geom = c("boxplot", "jitter"),
      fill = gear, main = "Mileage by Gear Number",
      xlab = "", ylab = "Miles per Gallon")


my.plot <- ggplot(diamonds, aes(carat, price, colour = cut))
#
my.plot <- my.plot + layer(
geom = "point",
stat = "identity",
position = "identity",
params = list(na.rm = FALSE)
)
#
my.plot
#
my.plot2 <- ggplot(diamonds, aes(x = carat))
my.plot2 <- my.plot2 + layer(
  geom = "bar",
  stat = "bin",
  position = "identity",
  params = list(
    fill = "steelblue",
    binwidth = 0.2,
    na.rm = FALSE
  )
)
my.plot2
#
my.plot3 <- ggplot(diamonds, aes(x = carat))
my.plot3 <- my.plot3 +
  geom_histogram(binwidth = 0.2, fill = "steelblue")
my.plot3
#
# 做法一
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) +
  geom_point()
# 做法二
qplot(sleep_rem / sleep_total, awake, data = msleep)
#
# 做法一
qplot(sleep_rem / sleep_total, awake, data = msleep) +
  geom_smooth()
# 做法二
qplot(sleep_rem / sleep_total, awake, data = msleep,
      geom = c("point", "smooth"))
# 做法三
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) +
  geom_point() + geom_smooth()
#
my.plot4 <- ggplot(msleep, aes(sleep_rem / sleep_total, awake))
summary(my.plot4)
#
my.plot4 <- my.plot4 + geom_point()
summary(my.plot4)
#
bestfit <- geom_smooth(method = "lm", se = F,
                       color = alpha("steelblue", 0.5), size = 2)
qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data = msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit
#
my.plot5 <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
my.plot5
#
mtcars.trans <- transform(mtcars, mpg = mpg ^ 2)
my.plot5 %+% mtcars.trans
#
aes(x = weight, y = height, color = age)
#
aes(weight, height, colour = sqrt(age))
#
my.plot6 <- ggplot(mtcars, aes(x = mpg, y = wt))
my.plot6 <- my.plot6 + geom_point()
summary(my.plot6)
#
my.plot7 <- ggplot(mtcars)
my.plot7 <- my.plot7 + aes(x = mpg, y = wt)
my.plot7 <- my.plot7 + geom_point()
summary(my.plot7)
#
my.plot7 + geom_point(aes(colour = factor(cyl)))
my.plot7 + geom_point(aes(y = disp))
#
aes(y = NULL)
#
my.plot8 <- ggplot(mtcars, aes(mpg, wt))
my.plot8 + geom_point(colour = "blue")
#
my.plot8 + geom_point(aes(colour = "blue"))
#
my.plot9 <- ggplot(Oxboys, aes(age, height)) +
  geom_line()
#
my.plot9 <- ggplot(Oxboys, aes(age, height, group = Subject)) +
  geom_line()
#
my.plot9 + geom_smooth(method="lm", se = F)
#
my.plot9 + geom_smooth(aes(group = 1), method="lm", size = 2, se = F)
#
boysbox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
#
boysbox + geom_line(aes(group = Subject), color = "blue")
#
my.df <- data.frame(x = 1:3, y = 1:3, z = 1:3)
qplot(x, y, data = my.df, color = factor(z), size = I(5)) +
  geom_line(size = 3, group = 1)
#
qplot(color, data = diamonds, geom = "bar",
      fill = cut)
#
ggplot(diamonds, aes(carat)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1)
#
qplot(carat, ..density.., data = diamonds,
      geom="histogram", binwidth = 0.1)
#
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar(position="dodge")
#
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar(position="fill")
#
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar(position="stack")
#
ggplot(diamonds, aes(clarity, group = cut)) +
  geom_line(aes(color = cut), position="identity", stat = "count")
#
set.seed(5)
diamonds.subset <- diamonds[sample(nrow(diamonds), 500), ]
ggplot(diamonds.subset, aes(clarity, cut)) +
  geom_point(aes(color = color), position="jitter")
#
my.plot <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
my.plot + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
#
my.plot + stat_bin(
  aes(size = ..density..), binwidth = 0.1,
  geom = "point", position="identity"
)
#
library(nlme)
model <- lme(height ~ age, data = Oxboys, random = ~ 1 + age | Subject)
age_grid <- seq(-1, 1, length = 10)
subjects <- unique(Oxboys$Subject)
preds <- expand.grid(age = age_grid, Subject = subjects)
preds$height <- predict(model, preds)
#
my.plot <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
my.plot + geom_line(data = preds, colour = "#3366FF", size= 0.4)
#
Oxboys$fitted <- predict(model)
Oxboys$resid <- with(Oxboys, fitted - height)
my.plot %+% Oxboys + aes(y = resid) + geom_smooth(aes(group=1))
#

