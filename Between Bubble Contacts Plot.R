# Plots  
par(mfrow=c(3, 2), lwd=1)


###################################################################################################################
# Students - S
X_lim <- seq(1,T,by=1)
plot(da$S.Student~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Sesceptible Students", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$S.Student, col="black", lty=1)
lines(da8$S.Student, col="blue", lty=1)
lines(da9$S.Student, col="orange", lty=1)
lines(da10$S.Student, col="green", lty=1)


legend(220, 70 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da$S.Student[90],round(da$S.Student[90],3), cex = 0.6, pos = 2, col="black", lty=1)
text(90, da8$S.Student[90],round(da8$S.Student[90],3), cex = 0.6, pos = 3, col="blue", lty=1)
text(90, da9$S.Student[90],round(da9$S.Student[90],3), cex = 0.6, pos = 1, col="Orange", lty=1)
text(90, da10$S.Student[90],round(da10$S.Student[90],3), cex = 0.6, pos = 1, col="green", lty=1)

###################################################################################################################
# Students - E
X_lim <- seq(1,T,by=1)
plot(da$E.Student~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Exposed Students", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$E.Student, col="black", lty=1)
lines(da8$E.Student, col="blue", lty=1)
lines(da9$E.Student, col="orange", lty=1)
lines(da10$E.Student, col="green", lty=1)


legend(220, 70 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - D
X_lim <- seq(1,T,by=1)
plot(da$D.Student~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Detectable / Symptomatic Students", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$D.Student, col="black", lty=1)
lines(da8$D.Student, col="blue", lty=1)
lines(da9$D.Student, col="orange", lty=1)
lines(da10$D.Student, col="green", lty=1)


legend(220, 70 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - U
X_lim <- seq(1,T,by=1)
plot(da$U.Student~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Undetectable / Asymptomatic Students", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$U.Student, col="black", lty=1)
lines(da8$U.Student, col="blue", lty=1)
lines(da9$U.Student, col="orange", lty=1)
lines(da10$U.Student, col="green", lty=1)


legend(220, 70 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Student~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Quarantine Students", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$Q.Student, col="black", lty=1)
lines(da8$Q.Student, col="blue", lty=1)
lines(da9$Q.Student, col="orange", lty=1)
lines(da10$Q.Student, col="green", lty=1)


legend(220, 70 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - R
X_lim <- seq(1,T,by=1)
plot(da$R.Student~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Recovered Students", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$R.Student, col="black", lty=1)
lines(da8$R.Student, col="blue", lty=1)
lines(da9$R.Student, col="orange", lty=1)
lines(da10$R.Student, col="green", lty=1)


legend(220, 70 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da$R.Student[90],round(da$R.Student[90],3), cex = 0.6, pos = 2, col="black", lty=1)
text(90, da8$R.Student[90],round(da8$R.Student[90],3), cex = 0.6, pos = 4, col="blue", lty=1)
text(90, da9$R.Student[90],round(da9$R.Student[90],3), cex = 0.6, pos = 4, col="Orange", lty=1)
text(90, da10$R.Student[90],round(da10$R.Student[90],3), cex = 0.6, pos = 3, col="green", lty=1)
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
plot(da$S.Staff~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Sesceptible Staffs", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$S.Staff, col="black", lty=1)
lines(da8$S.Staff, col="blue", lty=1)
lines(da9$S.Staff, col="orange", lty=1)
lines(da10$S.Staff, col="green", lty=1)


legend(220, 35 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da$S.Staff[90],round(da$S.Staff[90],3), cex = 0.6, pos = 1, col="black", lty=1)
text(90, da8$S.Staff[90],round(da8$S.Staff[90],3), cex = 0.6, pos = 3, col="blue", lty=1)
text(90, da9$S.Staff[90],round(da9$S.Staff[90],3), cex = 0.6, pos = 1, col="Orange", lty=1)
text(90, da10$S.Staff[90],round(da10$S.Staff[90],3), cex = 0.6, pos = 1, col="green", lty=1)

###################################################################################################################
# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da$E.Staff~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Exposed Staffs", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$E.Staff, col="black", lty=1)
lines(da8$E.Staff, col="blue", lty=1)
lines(da9$E.Staff, col="orange", lty=1)
lines(da10$E.Staff, col="green", lty=1)


legend(220, 35 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da$D.Staff~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Detectable / Symptomatic Staffs", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$D.Staff, col="black", lty=1)
lines(da8$D.Staff, col="blue", lty=1)
lines(da9$D.Staff, col="orange", lty=1)
lines(da10$D.Staff, col="green", lty=1)


legend(220, 35 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - U
X_lim <- seq(1,T,by=1)
plot(da$U.Staff~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Undetectable / Asymptomatic Staffs", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$U.Staff, col="black", lty=1)
lines(da8$U.Staff, col="blue", lty=1)
lines(da9$U.Staff, col="orange", lty=1)
lines(da10$U.Staff, col="green", lty=1)


legend(220, 35 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Staff~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Quarantine Staffs", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$Q.Staff, col="black", lty=1)
lines(da8$Q.Staff, col="blue", lty=1)
lines(da9$Q.Staff, col="orange", lty=1)
lines(da10$Q.Staff, col="green", lty=1)


legend(220, 35 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - R
X_lim <- seq(1,T,by=1)
plot(da$R.Staff~X_lim, pch=15, col="black", main = "Variations in Between Bubble Contact Number - Recovered Staffs", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$R.Staff, col="black", lty=1)
lines(da8$R.Staff, col="blue", lty=1)
lines(da9$R.Staff, col="orange", lty=1)
lines(da10$R.Staff, col="green", lty=1)


legend(220, 35 ,c("Baseline: Between Bubble Contact = 3", " Between Bubble Contact = 1"," Between Bubble Contact = 5", " Between Bubble Contact = 10"),
       col=c("black", "blue", "orange", "green"),
       text.col=c("black", "blue", "orange", "green"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da$R.Staff[90],round(da$R.Staff[90],3), cex = 0.6, pos = 1, col="black", lty=1)
text(90, da8$R.Staff[90],round(da8$R.Staff[90],3), cex = 0.6, pos = 3, col="blue", lty=1)
text(90, da9$R.Staff[90],round(da9$R.Staff[90],3), cex = 0.6, pos = 3, col="Orange", lty=1)
text(90, da10$R.Staff[90],round(da10$R.Staff[90],3), cex = 0.6, pos = 3, col="green", lty=1)


