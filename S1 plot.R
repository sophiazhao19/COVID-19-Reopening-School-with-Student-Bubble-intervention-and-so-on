# Staffs - S
X_lim <- seq(1,T,by=1)
plot(da$S.Staff~X_lim, pch=15, col="black", main = "Bubble Structure vs.No Interventions", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$S.Staff, col="black", lty=1)
lines(df$S.Staff, col="black", lty=2)


legend(75, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da$E.Staff~X_lim, pch=15, col="black", main = "Bubble Structure vs.No Interventions", type = "l",
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$E.Staff, col="black", lty=1)
lines(df$E.Staff, col="black", lty=2)


legend(75, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da$D.Staff~X_lim, pch=15, col="black", main = "Bubble Structure vs.No Interventions", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$D.Staff, col="black", lty=1)
lines(df$D.Staff, col="black", lty=2)


legend(75, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - U

X_lim <- seq(1,T,by=1)
plot(da$U.Staff~X_lim, pch=15, col="black", main = "Bubble Structure vs.No Interventions", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$U.Staff, col="black", lty=1)
lines(df$U.Staff, col="black", lty=2)


legend(75, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Staff~X_lim, pch=15, col="black", main = "Bubble Structure vs.No Interventions", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$Q.Staff, col="black", lty=1)
lines(df$Q.Staff, col="black", lty=2)


legend(75, 60 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - R
X_lim <- seq(1,T,by=1)
plot(da$R.Staff~X_lim, pch=15, col="black", main = "Bubble Structure vs.No Interventions", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$R.Staff, col="black", lty=1)
lines(df$R.Staff, col="black", lty=2)


legend(75, 45 ,c("Bubble Structure", "No Interventions"),
       col=c("black", "black"),
       text.col=c("black", "black"),lty=c(1,2),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)