# Plots  
par(mfrow=c(3, 2), lwd=1)


###################################################################################################################
# Students - S
X_lim <- seq(1,T,by=1)
plot(da$S.Student~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Sesceptible Students", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$S.Student, col="black", lty=1)
lines(da6$S.Student, col="blue", lty=1)
lines(da7$S.Student, col="orange", lty=1)


legend(220, 70 ,c("Baseline: 10 Students in a Bubble", "5 Students in a Bubble","20 Students in a Bubble"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da$S.Student[90],round(da$S.Student[90],3), cex = 0.6, pos = 3, col="black", lty=1)
text(90, da6$S.Student[90],round(da6$S.Student[90],3), cex = 0.6, pos = 4, col="blue", lty=1)
text(90, da7$S.Student[90],round(da7$S.Student[90],3), cex = 0.6, pos = 2, col="Orange", lty=1)
###################################################################################################################
# Students - E
X_lim <- seq(1,T,by=1)
plot(da$E.Student~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Exposed Students", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$E.Student, col="black", lty=1)
lines(da6$E.Student, col="blue", lty=1)
lines(da7$E.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: 10 Students in a Bubble", "5 Students in a Bubble","20 Students in a Bubble"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - D
X_lim <- seq(1,T,by=1)
plot(da$D.Student~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Detectable / Symptomatic Students", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$D.Student, col="black", lty=1)
lines(da6$D.Student, col="blue", lty=1)
lines(da7$D.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: 10 Students in a Bubble", "5 Students in a Bubble","20 Students in a Bubble"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - U
X_lim <- seq(1,T,by=1)
plot(da$U.Student~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Undetectable / Asymptomatic Students", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$U.Student, col="black", lty=1)
lines(da6$U.Student, col="blue", lty=1)
lines(da7$U.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: 10 Students in a Bubble", "5 Students in a Bubble","20 Students in a Bubble"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Student~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Quarantine Students", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$Q.Student, col="black", lty=1)
lines(da6$Q.Student, col="blue", lty=1)
lines(da7$Q.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: 10 Students in a Bubble", "5 Students in a Bubble","20 Students in a Bubble"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - R
X_lim <- seq(1,T,by=1)
plot(da$R.Student~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Recovered Students", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da$R.Student, col="black", lty=1)
lines(da6$R.Student, col="blue", lty=1)
lines(da7$R.Student, col="orange", lty=1)

legend(220, 70 ,c("Baseline: 10 Students in a Bubble", "5 Students in a Bubble", "20 Students in a Bubble"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da$R.Student[90],round(da$R.Student[90],3), cex = 0.6, pos = 4, col="black", lty=1)
text(90, da6$R.Student[90],round(da6$R.Student[90],3), cex = 0.6, pos = 4, col="blue", lty=1)
text(90, da7$R.Student[90],round(da7$R.Student[90],3), cex = 0.6, pos = 2, col="Orange", lty=1)
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
plot(da$S.Staff~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Sesceptible Staffs", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,55))

lines(da$S.Staff, col="black", lty=1)
lines(da6$S.Staff, col="blue", lty=1)
lines(da7$S.Staff, col="orange", lty=1)


legend(200, 45 ,c("Baseline: 3 Staffs for each Bubble of size 10", "2 Staffs in each Bubble of size 5","3 Staffs in each  Bubble of size 20"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da$S.Staff[90],round(da$S.Staff[90],3), cex = 0.6, pos = 1, col="black", lty=1)
text(90, da6$S.Staff[90],round(da6$S.Staff[90],3), cex = 0.6, pos = 3, col="blue", lty=1)
text(90, da7$S.Staff[90],round(da7$S.Staff[90],3), cex = 0.6, pos = 3, col="Orange", lty=1)

###################################################################################################################
# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da$E.Staff~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Exposed Staffs", type = "l", 
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,55))

lines(da$E.Staff, col="black", lty=1)
lines(da6$E.Staff, col="blue", lty=1)
lines(da7$E.Staff, col="orange", lty=1)

legend(200, 45 ,c("Baseline: 3 Staffs for each Bubble of size 10", "2 Staffs in each Bubble of size 5","3 Staffs in each  Bubble of size 20"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da$D.Staff~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Detectable / Symptomatic Staffs", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,55))

lines(da$D.Staff, col="black", lty=1)
lines(da6$D.Staff, col="blue", lty=1)
lines(da7$D.Staff, col="orange", lty=1)

legend(200, 45 ,c("Baseline: 3 Staffs for each Bubble of size 10", "2 Staffs in each Bubble of size 5","3 Staffs in each  Bubble of size 20"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - U
X_lim <- seq(1,T,by=1)
plot(da$U.Staff~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Undetectable / Asymptomatic Staffs", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,55))

lines(da$U.Staff, col="black", lty=1)
lines(da6$U.Staff, col="blue", lty=1)
lines(da7$U.Staff, col="orange", lty=1)

legend(200, 45 ,c("Baseline: 3 Staffs for each Bubble of size 10", "2 Staffs in each Bubble of size 5","3 Staffs in each  Bubble of size 20"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - Q
X_lim <- seq(1,T,by=1)
plot(da$Q.Staff~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Quarantine Staffs", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,55))

lines(da$Q.Staff, col="black", lty=1)
lines(da6$Q.Staff, col="blue", lty=1)
lines(da7$Q.Staff, col="orange", lty=1)

legend(200, 45 ,c("Baseline: 3 Staffs for each Bubble of size 10", "2 Staffs in each Bubble of size 5","3 Staffs in each  Bubble of size 20"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - R
X_lim <- seq(1,T,by=1)
plot(da$R.Staff~X_lim, pch=15, col="black", main = "Variations in Bubble Size - Recovered Staffs", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,55))

lines(da$R.Staff, col="black", lty=1)
lines(da6$R.Staff, col="blue", lty=1)
lines(da7$R.Staff, col="orange", lty=1)

legend(200, 45 ,c("Baseline: 3 Staffs for each Bubble of size 10", "2 Staffs in each Bubble of size 5", "3 Staffs in each  Bubble of size 20"),
       col=c("black", "blue", "orange"),
       text.col=c("black", "blue", "orange"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da$R.Staff[90],round(da$R.Staff[90],3), cex = 0.6, pos = 3, col="black", lty=1)
text(90, da6$R.Staff[90],round(da6$R.Staff[90],3), cex = 0.6, pos = 3, col="blue", lty=1)
text(90, da7$R.Staff[90],round(da7$R.Staff[90],3), cex = 0.6, pos = 1, col="Orange", lty=1)

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
C2_da.std = C2da6.std = C2da7.std = c(rep(0,91))

C2_da.std = da$S.Student[1:90] + da$E.Student[1:90] + da$D.Student[1:90] + da$U.Student[1:90] + da$R.Student[1:90] 

C2_da6.std = da6$S.Student[1:90] + da6$E.Student[1:90] + da6$D.Student[1:90] + da6$U.Student[1:90] + da6$R.Student[1:90] 

C2_da7.std = da7$S.Student[1:90] + da7$E.Student[1:90] + da7$D.Student[1:90] + da7$U.Student[1:90] + da7$R.Student[1:90]


X_lim <- seq(1,90,by=1)
plot(C2_da.std~X_lim, pch=15, col="black", main = "Scenario 2 | Thresholds of Students", type = "l",
     xlab = "Time (day)", ylab = "Total number of Students availiable to come to school", xlim = c(0,90), ylim = c(0,100))

lines(C2_da.std, col="black", lty=1)
lines(C2_da6.std, col="blue", lty=1)
lines(C2_da7.std, col="orange", lty=1)
abline(h=da$S.Student[1]*2/3, col="red", lty=2)

legend(20, 10 , "Thresholds = 2/3 of stydents are availiable to come to school",
       col="red",
       text.col="red", lty=2,
       cex=0.57, bty="n")

###################################################################################################################

# The minimum number of staffs at school
C2_da.stf = C2da6.stf = C2da7.stf = c(rep(0,91))

C2_da.stf = da$S.Staff[1:90] + da$E.Staff[1:90] + da$D.Staff[1:90] + da$U.Staff[1:90] + da$R.Staff[1:90] + da$Staff.rp[1:90]

C2_da6.stf = da6$S.Staff[1:90] + da6$E.Staff[1:90] + da6$D.Staff[1:90] + da6$U.Staff[1:90] + da6$R.Staff[1:90] + d6a$Staff.rp[1:90]

C2_da7.stf = da7$S.Staff[1:90] + da7$E.Staff[1:90] + da7$D.Staff[1:90] + da7$U.Staff[1:90] + da7$R.Staff[1:90] + da7$Staff.rp[1:90]


X_lim <- seq(1,90,by=1)
plot(C2_da.stf~X_lim, pch=15, col="black", main = "Scenario 2 | Thresholds of Teaching Staffs", type = "l",
     xlab = "Time (day)", ylab = "Total number of Staffs availiable to work", xlim = c(0,90), ylim = c(0,55))

lines(C2_da.stf, col="black", lty=1)
lines(C2_da6.stf, col="blue", lty=1)
lines(C2_da7.stf, col="orange", lty=1)
abline(h=da$S.Staff[1]*2/3, col="red", lty=2)

legend(20, 10 , "Thresholds = 2/3 of staffs are availiable to work",
       col="red",
       text.col="red", lty=2,
       cex=0.57, bty="n")


