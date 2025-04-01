num <- c(1:12)
square<-num*num
plot(num, square, type="b")
plot(num, square, type="c")
plot(num, square, type="o")
plot(num, square, type="h")
plot(num, square, type="s")
plot(num, square, type="n")

opar <- par(no.readonly = TRUE)
par(lty = 2,
    pch = 17, 
    lwd = 2,
    cex = 1.5,
    col = "red",
    col.axis = "blue", 
    col.lab = "pink", 
    col.main = "yellow", 
    col.sub = "black", 
    Fg = "orange",
    Bg = "purple",
    family = "C",
    pin = c(2,3))
plot(num, square, type="b", pch=19, lty=2, col = "red")
plot(num, square, type="b", pch=23, lty=6, col = "blue", bg = "green")
par(opar)

month <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
month.f = month
month.o = ordered(month.f, levels = month)
plot(month.o, square, type="b", pch = 23, lty = 6, col = "blue", bg = "green")
plot(num, square, type="b", pch=23, lty = 6, col = "blue", bg = "green", las = 3)

n <- 10
mycolors = rainbow(n)
pie(rep(1,n), labels=mycolors, col = mycolors)
mygrays <- gray(0:n/n)
pie(rep(1,n), labels=mygrays, col = mygrays)

windowsFonts(
  A = windowsFont("Arial Black"),
  B = windowsFont("Bookman Old Style"),
  C = windowsFont("Comic Sans MS")
)

plot(num, square, type = "b",
     col = "green", lty = 2, pch = 2,
     main = "Quad refer", sub="just a square of number",
     xlab = "Month", ylab = "square of number",
     xlim=c(0,12), ylim = c(0,300), ann = FALSE
)
title(main="Quad ref", col.main = "red", 
      sub = "just a quad of number", col.sub = "blue",
      xlab = "month", ylab = "quad of number", col.lab = "green", cex.lab = 1)
x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly = TRUE)
par(mar=c(5,4,4,8) + 0.1)
plot(x,y,type="b", pch=21, col="red",yaxt="n",lty=3,ann=FALSE)
lines(x, z, type="b", pch=22,col="blue",lty=2)
axis(2, at = x, labels = x, col.axis = "red", las = 2)
axis(4, at=z, labels = round(z, digits = 2), col.axis = "blue", las = 2, cex.axis = 0.7, tck=-0.1)
mtext("y=1/x", side=4, line = 3, cex.lab=1,las = 2, col = "blue")
title("Example of creative axises", xlab = "Values of x", ylab = "Y=X")
legend("bottom", inset = 0.1, title = "two plots", c("Y=X", "Y=1/X"), 
       lty = c(1,2), pch=c(21,22), col = c("red", "blue"))
par(opar)

attach(mtcars)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(wt,mpg,main = "Diag of rasseyaniya for \n fuel rashoda and weight of car")
plot(wt,mpg,main = "Diag of rasseyaniya for \n opasity of engine and weight of car")
hist(wt, main="Raspredelenie of values \n weight of car")
boxplot(wt, main = "box-with-usami \n for weight of car")
par(opar)
detach(mtcars)

attach(mtcars)
layout(matrix(c(1,1,2,3), 2,2,byrow = TRUE), widths = c(3,1), heights= c(1,2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)



