# Students - S
X_lim <- seq(1,T,by=1)
plot(da$S.Student~X_lim, pch=15, col="black", main = "Compartment S - Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$S.Student, col="black", lty=1)
lines(da2$S.Student, col="Orange", lty=1)
lines(da4$S.Student, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)
text(90, da$S.Student[90],round(da$S.Student[90],3), cex = 0.6, pos = 1, col="black", lty=1)
text(90, da2$S.Student[90],round(da2$S.Student[90],3), cex = 0.6, pos = 4, col="Orange", lty=1)
text(90, da4$S.Student[90],round(da4$S.Student[90],3), cex = 0.6, pos = 3, col="blue", lty=1)

# Students - E
X_lim <- seq(1,T,by=1)
plot(da$E.Student~X_lim, pch=15, col="black", main = "Compartment E - Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$E.Student, col="black", lty=1)
lines(da2$E.Student, col="Orange", lty=1)
lines(da4$E.Student, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

# Students - D
X_lim <- seq(1,T,by=1)
plot(da$D.Student~X_lim, pch=15, col="black", main = "Compartment D - Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$D.Student, col="black", lty=1)
lines(da2$D.Student, col="Orange", lty=1)
lines(da4$D.Student, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

# Students - U
X_lim <- seq(1,T,by=1)
plot(da$U.Student~X_lim, pch=15, col="black", main = "Compartment U - Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$U.Student, col="black", lty=1)
lines(da2$U.Student, col="Orange", lty=1)
lines(da4$U.Student, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

# Students - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Student~X_lim, pch=15, col="black", main = "Compartment Q - Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$Q.Student, col="black", lty=1)
lines(da2$Q.Student, col="Orange", lty=1)
lines(da4$Q.Student, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

# Students - R
X_lim <- seq(1,T,by=1)
plot(da$R.Student~X_lim, pch=15, col="black", main = "Compartment R - Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$R.Student, col="black", lty=1)
lines(da2$R.Student, col="Orange", lty=1)
lines(da4$R.Student, col="blue", lty=1)

legend(160, 30 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

text(90, da$R.Student[90],round(da$R.Student[90],3), cex = 0.6, pos = 2, col="black", lty=1)
text(90, da2$R.Student[90],round(da2$R.Student[90],3), cex = 0.6, pos = 4, col="Orange", lty=1)
text(90, da4$R.Student[90],round(da4$R.Student[90],3), cex = 0.6, pos = 1, col="blue", lty=1)

###################################################################################################################

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
# Staffs - S
X_lim <- seq(1,T,by=1)
plot(da$S.Staff~X_lim, pch=15, col="black", main = "Compartment S - Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$S.Staff, col="black", lty=1)
lines(da2$S.Staff, col="Orange", lty=1)
lines(da4$S.Staff, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)
text(90, da$S.Staff[90],round(da$S.Staff[90],3), cex = 0.6, pos = 1, col="black", lty=1)
text(90, da2$S.Staff[90],round(da2$S.Staff[90],3), cex = 0.6, pos = 1, col="Orange", lty=1)
text(90, da4$S.Staff[90],round(da4$S.Staff[90],3), cex = 0.6, pos = 3, col="blue", lty=1)

# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da$E.Staff~X_lim, pch=15, col="black", main = "Compartment E - Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$E.Staff, col="black", lty=1)
lines(da2$E.Staff, col="Orange", lty=1)
lines(da4$E.Staff, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da$D.Staff~X_lim, pch=15, col="black", main = "Compartment D - Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$D.Staff, col="black", lty=1)
lines(da2$D.Staff, col="Orange", lty=1)
lines(da4$D.Staff, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

# Staffs - U
X_lim <- seq(1,T,by=1)
plot(da$U.Staff~X_lim, pch=15, col="black", main = "Compartment U - Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$U.Staff, col="black", lty=1)
lines(da2$U.Staff, col="Orange", lty=1)
lines(da4$U.Staff, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

# Staffs - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Staff~X_lim, pch=15, col="black", main = "Compartment Q - Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$Q.Staff, col="black", lty=1)
lines(da2$Q.Staff, col="Orange", lty=1)
lines(da4$Q.Staff, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

# Staffs - R
X_lim <- seq(1,T,by=1)
plot(da$R.Staff~X_lim, pch=15, col="black", main = "Compartment R - Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$R.Staff, col="black", lty=1)
lines(da2$R.Staff, col="Orange", lty=1)
lines(da4$R.Staff, col="blue", lty=1)

legend(150, 50 ,c("Baseline", "S(5): 1 day on, 1 day off", "S(6): 1 week on, 1 week off"),
       col=c("black",  "Orange", "blue"),
       text.col=c("black", "Orange",  "blue"),lty=c(1,1,1),
       cex=0.57, bty="n" )

abline(v=90,col="dark grey", lty=2)

text(90, da$R.Staff[90],round(da$R.Staff[90],3), cex = 0.6, pos = 3, col="black", lty=1)
text(90, da2$R.Staff[90],round(da2$R.Staff[90],3), cex = 0.6, pos = 3, col="Orange", lty=1)
text(90, da4$R.Staff[90],round(da4$R.Staff[90],3), cex = 0.6, pos = 1, col="blue", lty=1)










