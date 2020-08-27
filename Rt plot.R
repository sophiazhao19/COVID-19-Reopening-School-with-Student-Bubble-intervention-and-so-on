
par(mfrow=c(1, 1))

# Rt Student and staff Baseline:
Rt_std <- colMeans(matrix(da$Rt.Student[1:98], 7))  
Rt_stf <- c(colMeans(matrix(c(1,1,da$Rt.Staff[3:98]), 7)))
Rt <- (Rt_std + Rt_stf)/2

# Rt Student and staff S9:
Rt_std.S9 <- colMeans(matrix(da_rp_t_S9$Rt.Student[1:98], 14))  
Rt_stf.S9 <- c(colMeans(matrix(c(1,1,da_rp_t_S9$Rt.Staff[3:98]), 14)))
Rt.S9 <- (Rt_std.S9 + Rt_stf.S9)/2


# Rt student and staff no bubble:
Rt_std.df <- colMeans(matrix(df$Rt.Student[1:98], 7))  
Rt_stf.df <- c(colMeans(matrix(c(1,1,df$Rt.Staff[3:98]), 7)))
Rt.df <- (Rt_std.df + Rt_stf.df)/2


# Rt plot 
# average on every two weeks ( 1 week on/off )
X_lim <- seq(1,14,by=1)
plot(Rt~X_lim, pch=15, col="black", main = "Rt", type = "l", xlab = "Time (average in week)", ylab = "Rt value", xlim = c(0,14), ylim = c(0.8,1.4))

lines(Rt.S9~c(2,4,6,8,10,12,14), col="orange", lty=1)
lines(Rt.df, col="blue", lty=1)
abline(h=1,col="dark red", lty=2)

legend(10, 1.4, c("Baseline","Scenario 9","No Bubble"),col=c("black", "orange","blue"),
       text.col=c("black", "orange","blue"),lty=c(1,1,1),
       cex=0.8, y.intersp = 2)

text(6, Rt.S9[3],round(Rt.S9[3],3), cex = 1, pos = 1, col="orange", lty=1)

