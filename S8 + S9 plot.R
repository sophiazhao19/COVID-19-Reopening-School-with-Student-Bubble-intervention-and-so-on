par(mfrow=c(3, 2), lwd=1)
par(mfrow=c(3, 2), lwd=1)

# Students - S
X_lim <- seq(1,T,by=1)
plot(da_rp_t$S.Student~X_lim, pch=15, col="black", main = "Compartment S - Student", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da_rp_t$S.Student, col="black", lty=1)
lines(da_rp_t_S8$S.Student, col="orange", lty=1)
lines(da_rp_t_S9$S.Student, col="blue", lty=1)


legend(130, 110 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group & 
                 1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group & 
                 1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da_rp_t$S.Student[90],round(da$S.Student[90],3), cex = 0.6, pos = 2, col="black", lty=1)
text(90, da_rp_t_S8$S.Student[90],round(da_rp_t_S8$S.Student[90],3), cex = 0.6, pos = 4, col="orange", lty=1)
text(90, da_rp_t_S9$S.Student[90],round(da_rp_t_S9$S.Student[90],3), cex = 0.6, pos = 3, col="blue", lty=1)


###################################################################################################################
# Students - E
X_lim <- seq(1,T,by=1)
plot(da_rp_t$E.Student~X_lim, pch=15, col="black", main = "Compartment E - Student", type = "l",
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da_rp_t$E.Student, col="black", lty=1)
lines(da_rp_t_S8$E.Student, col="orange", lty=1)
lines(da_rp_t_S9$E.Student, col="blue", lty=1)


legend(130, 110 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group & 
                   1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group & 
                   1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)
###################################################################################################################
# Students - D
X_lim <- seq(1,T,by=1)
plot(da_rp_t$D.Student~X_lim, pch=15, col="black", main = "Compartment D - Student", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da_rp_t$D.Student, col="black", lty=1)
lines(da_rp_t_S8$D.Student, col="orange", lty=1)
lines(da_rp_t_S9$D.Student, col="blue", lty=1)


legend(130, 110 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &             
                   1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &    
                   1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - U

X_lim <- seq(1,T,by=1)
plot(da_rp_t$U.Student~X_lim, pch=15, col="black", main = "Compartment U - Student", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da_rp_t$U.Student, col="black", lty=1)
lines(da_rp_t_S8$U.Student, col="orange", lty=1)
lines(da_rp_t_S9$U.Student, col="blue", lty=1)


legend(130, 110 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &              
                   1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &            
                   1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - Q

X_lim <- seq(1,T,by=1)
plot(da_rp_t$Q.Student~X_lim, pch=15, col="black", main = "Compartment Q - Student", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da_rp_t$Q.Student, col="black", lty=1)
lines(da_rp_t_S8$Q.Student, col="orange", lty=1)
lines(da_rp_t_S9$Q.Student, col="blue", lty=1)


legend(130, 110 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &           
                   1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &      
                   1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Students - R


X_lim <- seq(1,T,by=1)
plot(da_rp_t$R.Student~X_lim, pch=15, col="black", main = "Compartment R - Student", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,100))

lines(da_rp_t$R.Student, col="black", lty=1)
lines(da_rp_t_S8$R.Student, col="orange", lty=1)
lines(da_rp_t_S9$R.Student, col="blue", lty=1)


legend(140, 45 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &                  
                  1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &    
                  1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################


# Staffs - S
X_lim <- seq(1,T,by=1)
plot(da_rp_t$S.Staff~X_lim, pch=15, col="black", main = "Compartment S - Staff", type = "l",
     xlab = "Time (day)", ylab = "Sesceptible Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da_rp_t$S.Staff, col="black", lty=1)
lines(da_rp_t_S8$S.Staff, col="orange", lty=1)
lines(da_rp_t_S9$S.Staff, col="blue", lty=1)


legend(150, 50 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &             
                  1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &        
                  1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

text(90, da_rp_t$S.Staff[90],round(da$S.Staff[90],3), cex = 0.6, pos = 2, col="black", lty=1)
text(90, da_rp_t_S8$S.Staff[90],round(da_rp_t_S8$S.Staff[90],3), cex = 0.6, pos = 4, col="orange", lty=1)
text(90, da_rp_t_S9$S.Staff[90],round(da_rp_t_S9$S.Staff[90],3), cex = 0.6, pos = 3, col="blue", lty=1)


###################################################################################################################
# Staffs - E
X_lim <- seq(1,T,by=1)
plot(da_rp_t$E.Staff~X_lim, pch=15, col="black", main = "Compartment E - Staff", type = "l",
     xlab = "Time (day)", ylab = "Exposed Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da_rp_t$E.Staff, col="black", lty=1)
lines(da_rp_t_S8$E.Staff, col="orange", lty=1)
lines(da_rp_t_S9$E.Staff, col="blue", lty=1)


legend(150, 50 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &             
                  1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &       
                  1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)
###################################################################################################################
# Staffs - D
X_lim <- seq(1,T,by=1)
plot(da_rp_t$D.Staff~X_lim, pch=15, col="black", main = "Compartment D - Staff", type = "l",
     xlab = "Time (day)", ylab = "Detectable / Symptomatic Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da_rp_t$D.Staff, col="black", lty=1)
lines(da_rp_t_S8$D.Staff, col="orange", lty=1)
lines(da_rp_t_S9$D.Staff, col="blue", lty=1)


legend(150, 50 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &           
                  1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &                
                  1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - U

X_lim <- seq(1,T,by=1)
plot(da_rp_t$U.Staff~X_lim, pch=15, col="black", main = "Compartment U - Staff", type = "l",
     xlab = "Time (day)", ylab = "Undetectable / Asymptomatic Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da_rp_t$U.Staff, col="black", lty=1)
lines(da_rp_t_S8$U.Staff, col="orange", lty=1)
lines(da_rp_t_S9$U.Staff, col="blue", lty=1)


legend(150, 50 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &                  
                  1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &          
                  1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - Q

X_lim <- seq(1,T,by=1)
plot(da_rp_t$Q.Staff~X_lim, pch=15, col="black", main = "Compartment Q - Staff", type = "l",
     xlab = "Time (day)", ylab = "Quarantine Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da_rp_t$Q.Staff, col="black", lty=1)
lines(da_rp_t_S8$Q.Staff, col="orange", lty=1)
lines(da_rp_t_S9$Q.Staff, col="blue", lty=1)


legend(150, 50 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group &                  
                  1 day on, 1 day off", "S(9) : Allow Staff Replacement & Tutor Group &       
                  1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)

###################################################################################################################
# Staffs - R


X_lim <- seq(1,T,by=1)
plot(da_rp_t$R.Staff~X_lim, pch=15, col="black", main = "Compartment R - Staff", type = "l",
     xlab = "Time (day)", ylab = "Recovered Individuals", xlim = c(0,T), ylim = c(0,50))

lines(da_rp_t$R.Staff, col="black", lty=1)
lines(da_rp_t_S8$R.Staff, col="orange", lty=1)
lines(da_rp_t_S9$R.Staff, col="blue", lty=1)


legend(150, 50 ,c("Baseline", "S(8) : Allow Staff Replacement & Tutor Group & 
                 1 day on, 1 day off","S(9) : Allow Staff Replacement & Tutor Group & 
                 1 week on, 1 week off"),
       col=c("black", "orange","blue"),
       text.col=c("black", "orange", "blue"),lty=c(1,1,1),
       cex=0.57, bty="n")

abline(v=90,col="dark grey", lty=2)
