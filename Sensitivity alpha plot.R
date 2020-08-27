
par(mfrow=c(2, 3), lwd=1)


###################################################################################################################
# Students - S
X_lim <- seq(1,T,by=1)
plot(da$S.Student~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Sesceptible Students", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$S.Student, col="black", lty=1)
lines(da_alpha1$S.Student, col="black", lty=2)
lines(da_alpha2$S.Student, col="black", lty=3)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - E
X_lim <- seq(1,T,by=1)
plot(da$E.Student~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Exposed Students", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$E.Student, col="black", lty=1)
lines(da_alpha1$E.Student, col="black", lty=2)
lines(da_alpha2$E.Student, col="black", lty=3)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - D
X_lim <- seq(1,T,by=1)
plot(da$D.Student~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Detectable / Symptomatic Students", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$D.Student, col="black", lty=1)
lines(da_alpha1$D.Student, col="black", lty=2)
lines(da_alpha2$D.Student, col="black", lty=3)


abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - U
X_lim <- seq(1,T,by=1)
plot(da$U.Student~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Undetectable / Asymptomatic Students", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$U.Student, col="black", lty=1)
lines(da_alpha1$U.Student, col="black", lty=2)
lines(da_alpha2$U.Student, col="black", lty=3)


abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Student~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Quarantine Students", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$Q.Student, col="black", lty=1)
lines(da_alpha1$Q.Student, col="black", lty=2)
lines(da_alpha2$Q.Student, col="black", lty=3)


abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - R
X_lim <- seq(1,T,by=1)
plot(da$R.Student~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Recovered Students", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$R.Student, col="black", lty=1)
lines(da_alpha1$R.Student, col="black", lty=2)
lines(da_alpha2$R.Student, col="black", lty=3)

legend(150, 30 ,c(expression(paste(alpha, " = ", 0.2)),
                  expression(paste(alpha, " = ", 0.14)),
                  expression(paste(alpha, " = ", 0.24))),
       col="black",
       text.col="black",lty=c(1,2,3),
       cex=0.9, bty="n")

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
plot(da$S.Staff~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Sesceptible Staffs", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$S.Staff, col="black", lty=1)
lines(da_alpha1$S.Staff, col="black", lty=2)
lines(da_alpha2$S.Staff, col="black", lty=3)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da$E.Staff~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Exposed Staffs", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$E.Staff, col="black", lty=1)
lines(da_alpha1$E.Staff, col="black", lty=2)
lines(da_alpha2$E.Staff, col="black", lty=3)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da$D.Staff~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Detectable / Symptomatic Staffs", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$D.Staff, col="black", lty=1)
lines(da_alpha1$D.Staff, col="black", lty=2)
lines(da_alpha2$D.Staff, col="black", lty=3)


abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - U
X_lim <- seq(1,T,by=1)
plot(da$U.Staff~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Undetectable / Asymptomatic Staffs", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$U.Staff, col="black", lty=1)
lines(da_alpha1$U.Staff, col="black", lty=2)
lines(da_alpha2$U.Staff, col="black", lty=3)


abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Staff~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Quarantine Staffs", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$Q.Staff, col="black", lty=1)
lines(da_alpha1$Q.Staff, col="black", lty=2)
lines(da_alpha2$Q.Staff, col="black", lty=3)


abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - R
X_lim <- seq(1,T,by=1)
plot(da$R.Staff~X_lim, pch=15, col="black", main = "Sensitivity Analysis  - Recovered Staffs", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da$R.Staff, col="black", lty=1)
lines(da_alpha1$R.Staff, col="black", lty=2)
lines(da_alpha2$R.Staff, col="black", lty=3)

legend(150, 45 ,c(expression(paste(alpha, " = ", 0.2)),                   
                  expression(paste(alpha, " = ", 0.14)),                   
                  expression(paste(alpha, " = ", 0.24))),
       col="black",
       text.col="black",lty=c(1,2,3),
       cex=0.9, bty="n")

abline(v=90,col="dark grey", lty=2)