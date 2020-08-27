
par(mfrow=c(3, 2), lwd=1)


###################################################################################################################
# Students - S
X_lim <- seq(1,T,by=1)
plot(da$S.Student~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Sesceptible Students", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$S.Student, col="black", lty=1)
lines(da_epsilon1$S.Student, col="blue", lty=1)
lines(da_epsilon2$S.Student, col="orange", lty=1)


legend(220, 70 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - E
X_lim <- seq(1,T,by=1)
plot(da$E.Student~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Exposed Students", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$E.Student, col="black", lty=1)
lines(da_epsilon1$E.Student, col="blue", lty=1)
lines(da_epsilon2$E.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - D
X_lim <- seq(1,T,by=1)
plot(da$D.Student~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Detectable / Symptomatic Students", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$D.Student, col="black", lty=1)
lines(da_epsilon1$D.Student, col="blue", lty=1)
lines(da_epsilon2$D.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - U
X_lim <- seq(1,T,by=1)
plot(da$U.Student~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Undetectable / Asymptomatic Students", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$U.Student, col="black", lty=1)
lines(da_epsilon1$U.Student, col="blue", lty=1)
lines(da_epsilon2$U.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Student~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Quarantine Students", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$Q.Student, col="black", lty=1)
lines(da_epsilon1$Q.Student, col="blue", lty=1)
lines(da_epsilon2$Q.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - R
X_lim <- seq(1,T,by=1)
plot(da$R.Student~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Recovered Students", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$R.Student, col="black", lty=1)
lines(da_epsilon1$R.Student, col="blue", lty=1)
lines(da_epsilon2$R.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1", "epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

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
plot(da$S.Staff~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Sesceptible Staffs", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$S.Staff, col="black", lty=1)
lines(da_epsilon1$S.Staff, col="blue", lty=1)
lines(da_epsilon2$S.Staff, col="orange", lty=1)


legend(220, 35 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da$E.Staff~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Exposed Staffs", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$E.Staff, col="black", lty=1)
lines(da_epsilon1$E.Staff, col="blue", lty=1)
lines(da_epsilon2$E.Staff, col="orange", lty=1)

legend(220, 35 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da$D.Staff~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Detectable / Symptomatic Staffs", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$D.Staff, col="black", lty=1)
lines(da_epsilon1$D.Staff, col="blue", lty=1)
lines(da_epsilon2$D.Staff, col="orange", lty=1)

legend(220, 35 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - U
X_lim <- seq(1,T,by=1)
plot(da$U.Staff~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Undetectable / Asymptomatic Staffs", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$U.Staff, col="black", lty=1)
lines(da_epsilon1$U.Staff, col="blue", lty=1)
lines(da_epsilon2$U.Staff, col="orange", lty=1)

legend(220, 35 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Staff~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Quarantine Staffs", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$Q.Staff, col="black", lty=1)
lines(da_epsilon1$Q.Staff, col="blue", lty=1)
lines(da_epsilon2$Q.Staff, col="orange", lty=1)

legend(220, 35 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1","epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - R
X_lim <- seq(1,T,by=1)
plot(da$R.Staff~X_lim, pch=15, col="black", main = "Sensitivity in epsilon - Recovered Staffs", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$R.Staff, col="black", lty=1)
lines(da_epsilon1$R.Staff, col="blue", lty=1)
lines(da_epsilon2$R.Staff, col="orange", lty=1)

legend(220, 35 ,c("Baseline: epsilon = 0.75", "epsilon = 0.1", "epsilon = 0.2"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)
