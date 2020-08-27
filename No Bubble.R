
###################################################################################################################

# Parameters
Ta = matrix(c(0.64,1), nrow = 2) # transmissibility of student and staff
Ca = matrix(c(0.79,1), nrow = 1)  # susceptibility of student and staff
tau = mean(0.54, 0.54/2)  # individual transmission rate within and outside the bubble
# set p: the transmission rate: 
p1 = Ta %*% Ca
# p = (student-student, staff-student, student-staff, staff-staff )
p = c(p1[1,1]*tau, p1[2,1]*tau, p1[1,2]*tau, p1[2,2]*tau) #RHO

a = 0.2  # ALPHA: rate from latent to infection
d = c(0.1,0.3) # d:probability of an infection is detectable/symptomatic for student and staff
e = 0.138 # EPSILON: the relative transmission for undetectable compared to detectable
o = 0.75  # THETA: rate of getting quanrantined 
y = 0.1 # GAMMA:recovery rate (differ by age and compartments)

r.values = c(20, 3, 20, 5)  # r:contact r people (should be changing with time), in the same order as p (RHO)

T = 100
N = 10 # student number in each bubble


###################################################################################################################

# Set initials
R = Q = U = D = E = S=matrix(rep(0), T+1)

S.Staff=S1=S2=S3=S4=S
E.Staff=E1=E2=E3=E4=E
D.Staff=D1=D2=D3=D4=D
U.Staff=U1=U2=U3=U4=U
Q.Staff=Q1=Q2=Q3=Q4=Q
R.Staff=R1=R2=R3=R4=R


###################################################################################################################

B <- matrix(rep(0), T, 1)
A <- array(rep(B), dim = c(T, 6))

A[1,] <- c(99,1,0,0,0,0)   # initials

dimnames(A) <- list(1:T, c("S","E","D","U","Q","R"))  # name the array

head(A)
df <- as.data.frame(A)
###################################################################################################################

# i : time step start from 1 
# j : bubble number 1 to 10

for (i in 1:T){

    ###### For STUDENTS
    
    # initals
    S[1] =  99
    E[1] =  1
    D[1] =  0
    U[1] =  0
    Q[1] =  0
    R[1] =  0
    
    # STUDENT to STUDENT 0
    S1[1] =  99
    E1[1] =  1
    D1[1] =  0
    U1[1] =  0
    Q1[1] =  0
    R1[1] =  0

    # This formula comes from the dynamic equations (in powerpoint slide 4)
    S1[i+1] = S[i] - r.values[1]*p[1]*(D[i]+e*U[i])
    E1[i+1] = E[i] + r.values[1]*p[1]*(D[i]+e*U[i]) - a*E[i]
    D1[i+1] = D[i] + a*d[1]*E[i] - o*D[i]
    U1[i+1] = U[i] + a*(1-d[1])*E[i] - y*U[i]
    Q1[i+1] = Q[i] + o*D[i] - y*Q[i]
    R1[i+1] = R[i] + y*(Q[i]+U[i])

    
    
    # STAFF to STUDENTs they teach 
    # initial - there is no change of S and E initially
    S2[1] = 0
    E2[1] = 0

    # The change in S and E for bubble j depends on the number of D and U of staffs
    S2[i+1] = - r.values[2]*p[2]*(D.Staff[i]+e*U.Staff[i])   
    E2[i+1] = r.values[2]*p[2]*(D.Staff[i]+e*U.Staff[i]) 

    
    
    # TOTAL on STUDNETS 
    if (S1[i+1] + S2[i+1] >= 0){   # The total number of S has to be >= 0
      S[i+1] = S1[i+1] + S2[i+1]
      E[i+1] = E1[i+1] + E2[i+1]
    }
    else {  # If not, S = 0, 
      #         E = previous (time step) amout of E - the same amount of students reduced in S compartment - those move to the next compartments
      S[i+1] = 0
      E[i+1] = E[i] - (S[i+1]-S[i]) - a*E[i]
    }
    if (E[i+1] >= 0){
      
    }
    else {
      E[i+1] = 0
    }
      
    D[i+1] = D1[i+1] 
    U[i+1] = U1[i+1]
    Q[i+1] = Q1[i+1]
    R[i+1] = R1[i+1]
    
    
    ###### For STAFF
    
    # initials
    S.Staff[1] =  50
    E.Staff[1] =  0
    D.Staff[1] =  0
    U.Staff[1] =  0
    Q.Staff[1] =  0
    R.Staff[1] =  0
    
    # STUDENT to STAFF
    # initials
    S3[1] = 0

    # Only S and E compartments will change directly by the infection cause by studnets (E3 = -S3)
    S3[i+1] = - r.values[3]*p[3]*(D[i]+e*U[i])

    
    # STAFF to STAFF 
    # initials
    S4[1] = 50
    
    S4[i+1] = S.Staff[i] - r.values[4]*p[4]*(D.Staff[i]+e*U.Staff[i])
    E4[i+1] = E.Staff[i] + r.values[4]*p[4]*(D.Staff[i]+e*U.Staff[i]) - a*E.Staff[i]
    D4[i+1] = D.Staff[i] + a*d[2]*E.Staff[i] - o*D.Staff[i]
    U4[i+1] = U.Staff[i] + a*(1-d[2])*E.Staff[i] - y*U.Staff[i]
    Q4[i+1] = Q.Staff[i] + o*D.Staff[i] - y*Q.Staff[i]
    R4[i+1] = R.Staff[i] + y*(Q.Staff[i]+U.Staff[i])
    
    # TOTAL on STAFF
    if (S4[i+1] + S3[i+1] >= 0){   # The total number of S has to be >= 0
      S.Staff[i+1] = S4[i+1] + S3[i+1]
      E.Staff[i+1] = E4[i+1] - S3[i+1] 
    }
    else {  # If not, S = 0, 
      #         E = previous (time step) amout of E - the same amount of students reduced in S compartment - those move to the next compartments
      S.Staff[i+1] = 0
      E.Staff[i+1]= E.Staff[i] + S.Staff[i] - a*E.Staff[i]
    }

    D.Staff[i+1] = D4[i+1] 
    U.Staff[i+1] = U4[i+1]
    Q.Staff[i+1] = Q4[i+1] 
    R.Staff[i+1] = R4[i+1] 
    
    
    
    df$S[i] =S[i]
    df$E[i] =E[i]
    df$D[i] =D[i]
    df$U[i] =U[i]
    df$Q[i] =Q[i]
    df$R[i] =R[i]
    
    df$S.Staff[i] =S.Staff[i]
    df$E.Staff[i] =E.Staff[i]
    df$D.Staff[i] =D.Staff[i]
    df$U.Staff[i] =U.Staff[i]
    df$Q.Staff[i] =Q.Staff[i]
    df$R.Staff[i] =R.Staff[i]

    
    # The change on Weekends
    # There will not be new infections from 0 A.M. on Saturday till 0 A.M. on the following Monday
    
    offschool = c(seq(0,T,7)-1,seq(0,T,7)-0)
    
    if (!is.na(match(i, setdiff(0:T,offschool))) == TRUE){  #if time i is weekday
    }
    
    else{  #if time i is weekend
      S[i+1] = S[i]
      E[i+1] = E[i] - a*E[i]
      
      S.Staff[i+1] = S.Staff[i]
      E.Staff[i+1] = E.Staff[i] - a*E.Staff[i]
      
    }
    
}

for (i in 1:T){
  
  # reproduction rate Rt
  df$Rt.Student[i] <- (df$E[i+1] + df$D[i+1] + df$U[i+1] + df$Q[i+1]) / 
    (df$E[i] + df$D[i] + df$U[i] + df$Q[i])
  
  df$Rt.Staff[i] <- (df$E.Staff[i+1] + df$D.Staff[i+1] + df$U.Staff[i+1] + df$Q.Staff[i+1]) / 
    (df$E.Staff[i] + df$D.Staff[i] + df$U.Staff[i] + df$Q.Staff[i])
}




###################################################################################################################
# Plot SEDUQR graphs
par(mfrow=c(2, 1), lwd=1)

# Students
X_lim <- seq(1,T,by=1)
plot(df$S~X_lim, pch=15, col="black", main = "SEDUQR Model for Students - No Bubble", type = "l", xlab = "Time (day)", ylab = "Number of Students", xlim = c(0,T), ylim = c(0,100))

lines(df$S, col="DeepPink", lty=1)
lines(df$E, col="DarkTurquoise", lty=1)
lines(df$D, col="RosyBrown", lty=1)
lines(df$U, col="green", lty=1)
lines(df$Q, col="darkgreen", lty=1)
lines(df$R, col="darkblue", lty=1)

legend(92, 80 ,c("S","E","D", "U", "Q", "R"),col=c("DeepPink","DarkTurquoise","RosyBrown", "green", "darkgreen", "darkblue"),
       text.col=c("DeepPink","DarkTurquoise","RosyBrown", "green", "darkgreen", "darkblue"),lty=c(1,1,1,1,1,1),
       cex=0.5)

abline(v=90,col="dark grey", lty=2)

# Staffs
X_lim <- seq(1,T,by=1)
plot(df$S.Staff~X_lim, pch=15, col="black", main = "SEDUQR Model for Staffs - No Bubble", type = "l", xlab = "Time (day)", ylab = "Number of Staffs", xlim = c(0,T), ylim = c(0,50))

lines(df$S.Staff, col="DeepPink", lty=1)
lines(df$E.Staff, col="DarkTurquoise", lty=1)
lines(df$D.Staff, col="RosyBrown", lty=1)
lines(df$U.Staff, col="green", lty=1)
lines(df$Q.Staff, col="darkgreen", lty=1)
lines(df$R.Staff, col="darkblue", lty=1)

legend(92, 35,c("S","E","D", "U", "Q", "R"),col=c("DeepPink","DarkTurquoise","RosyBrown", "green", "darkgreen", "darkblue"),
       text.col=c("DeepPink","DarkTurquoise","RosyBrown", "green", "darkgreen", "darkblue"),lty=c(1,1,1,1,1,1),
       cex=0.5)

abline(v=90,col="dark grey", lty=2)

