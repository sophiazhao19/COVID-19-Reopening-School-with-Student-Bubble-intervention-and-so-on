# Plots  
par(mfrow=c(2, 3), lwd=1)


###################################################################################################################
# Students - S
X_lim <- seq(1,T,by=1)
plot(da$S.Student~X_lim, pch=15, col="black", main = "Compartment S - Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$S.Student, col="black", lty=1)
lines(df$S, col="black", lty=2)


legend(56, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - E
X_lim <- seq(1,T,by=1)
plot(da$E.Student~X_lim, pch=15, col="black", main = "Compartment E - Student", type = "l",
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$E.Student, col="black", lty=1)
lines(df$E, col="black", lty=2)


legend(56, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - D
X_lim <- seq(1,T,by=1)
plot(da$D.Student~X_lim, pch=15, col="black", main = "Compartment D - Student", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$D.Student, col="black", lty=1)
lines(df$D, col="black", lty=2)


legend(56, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - U

X_lim <- seq(1,T,by=1)
plot(da$U.Student~X_lim, pch=15, col="black", main = "Compartment U - Student", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$U.Student, col="black", lty=1)
lines(df$U, col="black", lty=2)


legend(56, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Student~X_lim, pch=15, col="black", main = "Compartment Q - Student", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$Q.Student, col="black", lty=1)
lines(df$Q, col="black", lty=2)


legend(56, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - R
X_lim <- seq(1,T,by=1)
plot(da$R.Student~X_lim, pch=15, col="black", main = "Compartment R - Student", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$R.Student, col="black", lty=1)
lines(df$R, col="black", lty=2)


legend(56, 30 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

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
plot(da$S.Staff~X_lim, pch=15, col="black", main = "Compartment S - Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$S.Staff, col="black", lty=1)
lines(df$S.Staff, col="black", lty=2)


legend(56, 50 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da$E.Staff~X_lim, pch=15, col="black", main = "Compartment E - Staff", type = "l",
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$E.Staff, col="black", lty=1)
lines(df$E.Staff, col="black", lty=2)


legend(56, 50 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da$D.Staff~X_lim, pch=15, col="black", main = "Compartment D - Staff", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$D.Staff, col="black", lty=1)
lines(df$D.Staff, col="black", lty=2)


legend(56, 50 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - U

X_lim <- seq(1,T,by=1)
plot(da$U.Staff~X_lim, pch=15, col="black", main = "Compartment U - Staff", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$U.Staff, col="black", lty=1)
lines(df$U.Staff, col="black", lty=2)


legend(56, 50 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Staff~X_lim, pch=15, col="black", main = "Compartment Q - Staff", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$Q.Staff, col="black", lty=1)
lines(df$Q.Staff, col="black", lty=2)


legend(56, 50 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - R
X_lim <- seq(1,T,by=1)
plot(da$R.Staff~X_lim, pch=15, col="black", main = "Compartment R - Staff", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$R.Staff, col="black", lty=1)
lines(df$R.Staff, col="black", lty=2)


legend(56, 45 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.6, bty="n")

abline(v=90,col="dark grey", lty=2)
