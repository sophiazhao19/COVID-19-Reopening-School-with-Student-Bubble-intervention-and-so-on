# Plots  
par(mfrow=c(3, 2), lwd=1)


###################################################################################################################
# Students - S
X_lim <- seq(1,T,by=1)
plot(da$S.Student~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Sesceptible Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$S.Student, col="black", lty=1)
lines(da1$S.Student, col="black", lty=2)
lines(da2$S.Student, col="Orange", lty=1)
lines(da3$S.Student, col="Orange", lty=2)
lines(da4$S.Student, col="blue", lty=1)
lines(da5$S.Student, col="blue", lty=2)

legend(220,100 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)
text(90, da$S.Student[90],round(da$S.Student[90],3), cex = 0.6, pos = 1, col="black", lty=1)
text(90, da1$S.Student[90],round(da1$S.Student[90],3), cex = 0.6, pos = 1, col="black", lty=2)
text(90, da2$S.Student[90],round(da2$S.Student[90],3), cex = 0.6, pos = 4, col="Orange", lty=1)
text(90, da3$S.Student[90],round(da3$S.Student[90],3), cex = 0.6, pos = 4, col="Orange", lty=2)
text(90, da4$S.Student[90],round(da4$S.Student[90],3), cex = 0.6, pos = 3, col="blue", lty=1)
text(90, da5$S.Student[90],round(da5$S.Student[90],3), cex = 0.6, pos = 3, col="blue", lty=2)

###################################################################################################################
# Students - E
X_lim <- seq(1,T,by=1)
plot(da$E.Student~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Exposed Student", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$E.Student, col="black", lty=1)
lines(da1$E.Student, col="black", lty=2)
lines(da2$E.Student, col="Orange", lty=1)
lines(da3$E.Student, col="Orange", lty=2)
lines(da4$E.Student, col="blue", lty=1)
lines(da5$E.Student, col="blue", lty=2)

legend(220, 120 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)


###################################################################################################################
# Students - D
X_lim <- seq(1,T,by=1)
plot(da$D.Student~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Detectable / Symptomatic Student", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$D.Student, col="black", lty=1)
lines(da1$D.Student, col="black", lty=2)
lines(da2$D.Student, col="Orange", lty=1)
lines(da3$D.Student, col="Orange", lty=2)
lines(da4$D.Student, col="blue", lty=1)
lines(da5$D.Student, col="blue", lty=2)

legend(220, 120 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - U
X_lim <- seq(1,T,by=1)
plot(da$U.Student~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Undetectable / Asymptomatic Student", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$U.Student, col="black", lty=1)
lines(da1$U.Student, col="black", lty=2)
lines(da2$U.Student, col="Orange", lty=1)
lines(da3$U.Student, col="Orange", lty=2)
lines(da4$U.Student, col="blue", lty=1)
lines(da5$U.Student, col="blue", lty=2)

legend(220, 120 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Student~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Quarantine Student", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$Q.Student, col="black", lty=1)
lines(da1$Q.Student, col="black", lty=2)
lines(da2$Q.Student, col="Orange", lty=1)
lines(da3$Q.Student, col="Orange", lty=2)
lines(da4$Q.Student, col="blue", lty=1)
lines(da5$Q.Student, col="blue", lty=2)

legend(220, 120 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)
###################################################################################################################
# Students - R
X_lim <- seq(1,T,by=1)
plot(da$R.Student~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Recovered Student", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$R.Student, col="black", lty=1)
lines(da1$R.Student, col="black", lty=2)
lines(da2$R.Student, col="Orange", lty=1)
lines(da3$R.Student, col="Orange", lty=2)
lines(da4$R.Student, col="blue", lty=1)
lines(da5$R.Student, col="blue", lty=2)

legend(220, 58 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)

text(90, da$R.Student[90],round(da$R.Student[90],3), cex = 0.6, pos = 2, col="black", lty=1)
text(90, da1$R.Student[90],round(da1$R.Student[90],3), cex = 0.6, pos = 2, col="black", lty=2)
text(90, da2$R.Student[90],round(da2$R.Student[90],3), cex = 0.6, pos = 4, col="Orange", lty=1)
text(90, da3$R.Student[90],round(da3$R.Student[90],3), cex = 0.6, pos = 4, col="Orange", lty=2)
text(90, da4$R.Student[90],round(da4$R.Student[90],3), cex = 0.6, pos = 1, col="blue", lty=1)
text(90, da5$R.Student[90],round(da5$R.Student[90],3), cex = 0.6, pos = 1, col="blue", lty=2)

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
plot(da$S.Staff~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Sesceptible Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$S.Staff, col="black", lty=1)
lines(da1$S.Staff, col="black", lty=2)
lines(da2$S.Staff, col="Orange", lty=1)
lines(da3$S.Staff, col="Orange", lty=2)
lines(da4$S.Staff, col="blue", lty=1)
lines(da5$S.Staff, col="blue", lty=2)

legend(220, 23 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)

text(90, da$S.Staff[90],round(da$S.Staff[90],3), cex = 0.5, pos = 1, col="black", lty=1)
text(90, da1$S.Staff[90],round(da1$S.Staff[90],3), cex = 0.5, pos = 1, col="black", lty=2)
text(90, da4$S.Staff[90],round(da4$S.Staff[90],3), cex = 0.5, pos = 3, col="blue", lty=1)
text(90, da5$S.Staff[90],round(da5$S.Staff[90],3), cex = 0.5, pos = 3, col="blue", lty=2)
###################################################################################################################
# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da$E.Staff~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Exposed Staff", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$E.Staff, col="black", lty=1)
lines(da1$E.Staff, col="black", lty=2)
lines(da2$E.Staff, col="Orange", lty=1)
lines(da3$E.Staff, col="Orange", lty=2)
lines(da4$E.Staff, col="blue", lty=1)
lines(da5$E.Staff, col="blue", lty=2)

legend(220, 35 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da$D.Staff~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Detectable / Symptomatic Staff", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$D.Staff, col="black", lty=1)
lines(da1$D.Staff, col="black", lty=2)
lines(da2$D.Staff, col="Orange", lty=1)
lines(da3$D.Staff, col="Orange", lty=2)
lines(da4$D.Staff, col="blue", lty=1)
lines(da5$D.Staff, col="blue", lty=2)

legend(220, 35 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - U
X_lim <- seq(1,T,by=1)
plot(da$U.Staff~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Undetectable / Asymptomatic Staff", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$U.Staff, col="black", lty=1)
lines(da1$U.Staff, col="black", lty=2)
lines(da2$U.Staff, col="Orange", lty=1)
lines(da3$U.Staff, col="Orange", lty=2)
lines(da4$U.Staff, col="blue", lty=1)
lines(da5$U.Staff, col="blue", lty=2)

legend(220, 35 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)
###################################################################################################################
# Staffs - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Staff~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Quarantine Staff", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$Q.Staff, col="black", lty=1)
lines(da1$Q.Staff, col="black", lty=2)
lines(da2$Q.Staff, col="Orange", lty=1)
lines(da3$Q.Staff, col="Orange", lty=2)
lines(da4$Q.Staff, col="blue", lty=1)
lines(da5$Q.Staff, col="blue", lty=2)

legend(220, 35 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - R
X_lim <- seq(1,T,by=1)
plot(da$R.Staff~X_lim, pch=15, col="black", main = "Variations in School Opening Days - Recovered Staff", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,35))

lines(da$R.Staff, col="black", lty=1)
lines(da1$R.Staff, col="black", lty=2)
lines(da2$R.Staff, col="Orange", lty=1)
lines(da3$R.Staff, col="Orange", lty=2)
lines(da4$R.Staff, col="blue", lty=1)
lines(da5$R.Staff, col="blue", lty=2)

legend(220, 35 ,c("Baseline", "Long Weekend", "1 day on, 1 day off","2 days on, 2 days off", "1 week on, 1 week off", "2 weeks on, 2 weeks off"),
       col=c("black", "black", "Orange", "Orange", "blue", "blue"),
       text.col=c("black", "black", "Orange", "Orange", "blue", "blue"),lty=c(1,2,1,2,1,2),
       cex=0.57, bty="n", y.intersp = 0.2)

abline(v=90,col="dark grey", lty=2)

text(90, da$R.Staff[90],round(da$R.Staff[90],3), cex = 0.6, pos = 3, col="black", lty=1)
text(90, da1$R.Staff[90],round(da1$R.Staff[90],3), cex = 0.6, pos = 3, col="black", lty=2)
text(90, da4$R.Staff[90],round(da4$R.Staff[90],3), cex = 0.6, pos = 1, col="blue", lty=1)
text(90, da5$R.Staff[90],round(da5$R.Staff[90],3), cex = 0.6, pos = 1, col="blue", lty=2)
###################################################################################################################

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
par(mfrow=c(2, 1), lwd=1)

# The minimum number of students at school
C2_da.std = C2da1.std = C2da2.std = C2_da3.std = C2_da4.std = C2_da5.std = c(rep(0,91))

C2_da.std = da$S.Student[1:90] + da$E.Student[1:90] + da$D.Student[1:90] + da$U.Student[1:90] + da$R.Student[1:90] 

C2_da1.std = da1$S.Student[1:90] + da1$E.Student[1:90] + da1$D.Student[1:90] + da1$U.Student[1:90] + da1$R.Student[1:90] 

C2_da2.std = da2$S.Student[1:90] + da2$E.Student[1:90] + da2$D.Student[1:90] + da2$U.Student[1:90] + da2$R.Student[1:90]

C2_da3.std = da3$S.Student[1:90] + da3$E.Student[1:90] + da3$D.Student[1:90] + da3$U.Student[1:90] + da3$R.Student[1:90] 

C2_da4.std = da4$S.Student[1:90] + da4$E.Student[1:90] + da4$D.Student[1:90] + da4$U.Student[1:90] + da4$R.Student[1:90] 

C2_da5.std = da5$S.Student[1:90] + da5$E.Student[1:90] + da5$D.Student[1:90] + da5$U.Student[1:90] + da5$R.Student[1:90] 

X_lim <- seq(1,90,by=1)
plot(C2_da.std~X_lim, pch=15, col="black", main = "Scenario 1 | Thresholds of Student", type = "l",
     xlab = "Time (day)", ylab = "Total number of Students availiable to come to school", xlim = c(0,90), ylim = c(0,100))


lines(C2_da.std, col="black", lty=1)
lines(C2_da1.std, col="black", lty=2)
lines(C2_da2.std, col="Orange", lty=1)
lines(C2_da3.std, col="Orange", lty=2)
lines(C2_da4.std, col="blue", lty=1)
lines(C2_da5.std, col="blue", lty=2)

abline(h=da$S.Student[1]*2/3, col="red", lty=2)

legend(20, 10 , "Thresholds = 2/3 of stydents are availiable to come to school",
       col="red",
       text.col="red", lty=2,
       cex=0.57, bty="n", y.intersp = 0.2)

###################################################################################################################
# The minimum number of Staffs at school
C2_da.stf = C2da1.stf = C2da2.stf = C2_da3.stf = C2_da4.stf = C2_da5.stf = c(rep(0,91))

C2_da.stf = da$S.Staff[1:90] + da$E.Staff[1:90] + da$D.Staff[1:90] + da$U.Staff[1:90] + da$R.Staff[1:90] 

C2_da1.stf = da1$S.Staff[1:90] + da1$E.Staff[1:90] + da1$D.Staff[1:90] + da1$U.Staff[1:90] + da1$R.Staff[1:90] 

C2_da2.stf = da2$S.Staff[1:90] + da2$E.Staff[1:90] + da2$D.Staff[1:90] + da2$U.Staff[1:90] + da2$R.Staff[1:90]

C2_da3.stf = da3$S.Staff[1:90] + da3$E.Staff[1:90] + da3$D.Staff[1:90] + da3$U.Staff[1:90] + da3$R.Staff[1:90] 

C2_da4.stf = da4$S.Staff[1:90] + da4$E.Staff[1:90] + da4$D.Staff[1:90] + da4$U.Staff[1:90] + da4$R.Staff[1:90] 

C2_da5.stf = da5$S.Staff[1:90] + da5$E.Staff[1:90] + da5$D.Staff[1:90] + da5$U.Staff[1:90] + da5$R.Staff[1:90] 


X_lim <- seq(1,90,by=1)
plot(C2_da.stf~X_lim, pch=15, col="black", main = "Scenario 1 | Thresholds of Teaching Staff", type = "l",
     xlab = "Time (day)", ylab = "Total number of Staffs availiable to work", xlim = c(0,90), ylim = c(0,55))

lines(C2_da.stf, col="black", lty=1)
lines(C2_da1.stf, col="black", lty=2)
lines(C2_da2.stf, col="Orange", lty=1)
lines(C2_da3.stf, col="Orange", lty=2)
lines(C2_da4.stf, col="blue", lty=1)
lines(C2_da5.stf, col="blue", lty=2)
abline(h=da$S.Staff[1]*2/3, col="red", lty=2)

legend(20, 10 , "Thresholds = 2/3 of staffs are availiable to work",
       col="red",
       text.col="red", lty=2,
       cex=0.57, bty="n", y.intersp = 0.2)


