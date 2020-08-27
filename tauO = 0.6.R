###################################################################################################################

# Parameters
Ta = matrix(c(0.64,1), nrow = 2) # transmissibility of student and staff
Ca = matrix(c(0.79,1), nrow = 1)  # susceptibility of student and staff
tau = c(0.54, 0.54*0.6)  # individual transmission rate within and outside the bubble
# set p: the transmission rate: 
p1 = Ta %*% Ca
# p = (student-student same bubble, student-student different, staff-student, student-staff, staff-staff both teaching same or different bubbles)
p = c(p1[1,1]*tau[1], p1[1,1]*tau[2], p1[2,1]*tau[2], p1[1,2]*tau[2], p1[2,2]*tau[2]) #RHO

a = 0.2  # ALPHA: rate from latent to infection
d = c(0.1,0.3) # d:probability of an infection is detectable/symptomatic for student and staff
e = 0.138 # EPSILON: the relative transmission for undetectable compared to detectable
o = 0.75  # THETA: rate of getting quanrantined 
y = 0.1 # GAMMA:recovery rate (differ by age and compartments)

r.values = c(9,3,10,3,3)  # r:contact r people (should be changing with time), in the same order as p (RHO)

T = 366
N = 10 # student number in each bubble


###################################################################################################################

# Define dataframe - students bubbles 1-10 and total staff
# Define array 
B <- matrix(rep(0), T, 7)
A <- array(rep(B), dim = c(T, 7, 11))

A[,1,] <- 10 #  S
A[,7,] <- 10 #  N
A[,1,11] <- 30  # 30 staffs are S initially
A[,7,11] <- 55 #  55 staffs in total (20 of them work from home)
A[1,,1] <- c(9,1,0,0,0,0,10)   # initials

dimnames(A) <- list(1:T, c("S","E","D","U","Q","R","N"), c(sprintf("B%d", 1:10),"Staff"))  # name the array

head(A)
da_tauO2 <- as.data.frame(A)

###################################################################################################################

# Set initials
S <- matrix(rep(0), T+1, 10)
E <- matrix(rep(0), T+1, 10)
D <- matrix(rep(0), T+1, 10)
U <- matrix(rep(0), T+1, 10)
Q <- matrix(rep(0), T+1, 10)
R <- matrix(rep(0), T+1, 10)
r <- matrix(rep(0), T+1, 10)

S.Staff=S1=S2=S3=S4=S5=S6=S
E.Staff=E1=E2=E3=E4=E5=E6=E
D.Staff=D1=D2=D3=D4=D5=D6=D
U.Staff=U1=U2=U3=U4=U5=U6=U
Q.Staff=Q1=Q2=Q3=Q4=Q5=Q6=Q
R.Staff=R1=R2=R3=R4=R5=R6=R
r1=r2=r3=r4=r5=r

S5.diff=S2.diff=S
E5.diff=E2.diff=E
D5.diff=D2.diff=D
U5.diff=U2.diff=U
R5.diff=R2.diff=R
r5.diff=r2.diff=r

S.Staff.change = S.Staff

# Staff replacement
Staff.School = S.Staff  
Staff.rp = S.Staff  

# Tutorial Group
S.Std_Std = S
S.Std1=S.Std2=S
S.Stf=E.Stf=D.Stf=U.Stf=Q.Stf=R.Stf=S


###################################################################################################################
# The Symbols for the Dynamic system 

# STUDENTS
# S, E, D, U, Q, R : (without subscript of numbers) TOTAL number of students in each compartment (for an individual bubble)

# S1, E1, D1, U1, Q1, R1: TOTAL number of students in each compartments with ONLY the intractions WITHIN bubbles (for an individual bubble)

# S2.diff, E2.diff, D2.diff, U2.diff, R2.diff : TOTAL number of students that are not in the specified bubble (for an individual bubble)

# S2, E2, D2, U2, Q2, R2: CHANGE of number of students in each compartments by BETWEEN-BUBBLES interactions (for an individual bubble)

# S3, E3, D3, U3, Q3, R3 : CHANGE of number of students in each compartments by their teaching staffs (for an individual bubble)

# STAFFS
# S.Staff, E.Staff, D.Staff, U.Staff, Q.Staff, R.Staff : (without subscript of numbers) TOTAL number of STAFF in each compartment (for those teaching an individual bubble)

# S4, E4, D4, U4, Q4, R4: CHANGE of number of staffs in each compartments by the students they teach (for those teaching an individual bubble)

# S5.diff, E5.diff, D5.diff, U5.diff, R5.diff : TOTAL number of staffs that are not teaching the specified bubble (for those teaching an individual bubble)

# S5, E5, D5, U5, Q5, R5 : CHANGE of number of staffs in each compartments by staffs teaching other bubbles (for those teaching an individual bubble)

# S6, E6, D6, U6, Q6, R6 : CHANGE of number of staffs in each compartments by the staffs teaching the same bubble (for those teaching an individual bubble)

# r are the number of sesceptible contacts that an infection case would contact (vary with situations)

# Staff.School : the number of staff at school of teaching each bubble

# Staff.rp : the number of staff that can come to teach the bubble that were working from home

# TUTOR GROUP

# S.Std_Std : CHANGE of number of student in tutor group by infections within tutor group

# S.Std1 : CHANGE of number of student in bubble j by infections within tutor group

# S.Std2 : CHANGE of number of student in bubble j by infections from the tutor

# S.Stf, E.Stf, D.Stf, U.Stf, Q.Stf, R.Stf : Total number of tutors being infected in tutor group j

###################################################################################################################

# i : time step start from 1 
# j : bubble number 1 to 10

for (i in 1:T){
  for (j in 1:10){  
    
    ###### For STUDENTS
    
    # initals
    S[1,j] =  da_tauO2[1,paste("S.B", j, sep = "")]
    E[1,j] =  da_tauO2[1,paste("E.B", j, sep = "")]
    D[1,j] =  da_tauO2[1,paste("D.B", j, sep = "")]
    U[1,j] =  da_tauO2[1,paste("U.B", j, sep = "")]
    Q[1,j] =  da_tauO2[1,paste("Q.B", j, sep = "")]
    R[1,j] =  da_tauO2[1,paste("R.B", j, sep = "")]
    
    # STUDENT to STUDENT in the SAME BUBBLE
    S1[1,j] =  da_tauO2[1,paste("S.B", j, sep = "")]
    E1[1,j] =  da_tauO2[1,paste("E.B", j, sep = "")]
    D1[1,j] =  da_tauO2[1,paste("D.B", j, sep = "")]
    U1[1,j] =  da_tauO2[1,paste("U.B", j, sep = "")]
    Q1[1,j] =  da_tauO2[1,paste("Q.B", j, sep = "")]
    R1[1,j] =  da_tauO2[1,paste("R.B", j, sep = "")]
    r1[1,j] =  S[1,j]  # number of contact depends on how many S in the bubble
    
    # This formula comes from the dynamic equations (in powerpoint slide 4)
    S1[i+1,j] = S[i,j] - r1[i,j]*p[1]*(D[i,j]+e*U[i,j])
    E1[i+1,j] = E[i,j] + r1[i,j]*p[1]*(D[i,j]+e*U[i,j]) - a*E[i,j]
    D1[i+1,j] = D[i,j] + a*d[1]*E[i,j] - o*D[i,j]
    U1[i+1,j] = U[i,j] + a*(1-d[1])*E[i,j] - y*U[i,j]
    Q1[i+1,j] = Q[i,j] + o*D[i,j] - y*Q[i,j]
    R1[i+1,j] = R[i,j] + y*(Q[i,j]+U[i,j])
    r1[i+1,j] = S[i,j] 
    
    
    # STUDENT to STUDENT in DIFFERENT BUBBLES #
    # number of contact for each individual in bubble j to students in other bubbles
    r2.diff[1,j] =  3  # each student in j contact 3 students in other bubbles
    r2.diff[i+1,j] = r2.diff[i,j]    
    # the total number of contacts of the students in bubble j
    m = round(r2.diff[i,j]*(S[i,j]))  # only Susceptible individuals in j might be infected by students in other bubbles
    
    # inital values of total number of students at school not in bubble j
    # (only S,E,D,U are at school, currently not considered recovery come back to school yet)
    S2.diff[1,j] = 90
    E2.diff[1,j] = 0
    D2.diff[1,j] = 0
    U2.diff[1,j] = 0
    R2.diff[1,j] = 0
    
    # Subset for S,E,D,U compartments of all bubbles 
    all.S = (c(1:ncol(da_tauO2)) %% 7) == 1
    all.E = (c(1:ncol(da_tauO2)) %% 7) == 2
    all.D = (c(1:ncol(da_tauO2)) %% 7) == 3
    all.U = (c(1:ncol(da_tauO2)) %% 7) == 4
    all.R = (c(1:ncol(da_tauO2)) %% 7) == 6
    
    #total number of students at school not in bubble j (and not staff: the reason of removing the last column)
    # The number of S, E, D, U at time i, different to bubble j
    S2.diff[i,j] = round(sum((da_tauO2[i,(all.S)][,-j])[,c(1:ncol(da_tauO2[i,(all.S)][,-j])-1)]))
    E2.diff[i,j] = round(sum((da_tauO2[i,(all.E)][,-j])[,c(1:ncol(da_tauO2[i,(all.E)][,-j])-1)]))
    D2.diff[i,j] = round(sum((da_tauO2[i,(all.D)][,-j])[,c(1:ncol(da_tauO2[i,(all.D)][,-j])-1)]))
    U2.diff[i,j] = round(sum((da_tauO2[i,(all.U)][,-j])[,c(1:ncol(da_tauO2[i,(all.U)][,-j])-1)]))
    R2.diff[i,j] = round(sum((da_tauO2[i,(all.R)][,-j])[,c(1:ncol(da_tauO2[i,(all.R)][,-j])-1)]))
    
    # make a list of S, E, D, U not in bubble j (the times of repeat is the number of students in each compartment)
    list = c(rep("S", S2.diff[i,j]), rep("E", E2.diff[i,j]), rep("D", D2.diff[i,j]), rep("U", U2.diff[i,j]), rep("R", R2.diff[i,j]))
    
    #count the number of S,E,D,U that students in j contacted to
    # " sample(list, m, replace = TRUE) ": Randomly select m from list
    set.seed(123)
    countS = sum((sample(list, m, replace = TRUE))=="S") # for students in bubble j: the number of contact outside bubble j are S
    countE = sum((sample(list, m, replace = TRUE))=="E") # for students in bubble j: the number of contact outside bubble j are E
    countD = sum((sample(list, m, replace = TRUE))=="D") # for students in bubble j: the number of contact outside bubble j are D
    countU = sum((sample(list, m, replace = TRUE))=="U") # for students in bubble j: the number of contact outside bubble j are U
    countR = sum((sample(list, m, replace = TRUE))=="R") # for students in bubble j: the number of contact outside bubble j are R
    
    
    # The number of change in S and E compartments by interactions from other bubbles to bubbles j (only the partial effect)
    # initial (no intractions between bubbles at time 1)
    S2[1,j] = 0
    E2[1,j] = 0
    
    # The change in S and E for bubble j only depends on the number of:
    # 1. (   susceptible students in bubble j: r1[i,j] = S[i,j]   ) and 
    # 2. (   number of detectable/symtomatic and undetectable/asymtomatic that are contacted in other bubbles: countD+e*countU   
    #                                                        -- e is because the effectiveness on D and U in infection rate are different)
    S2[i+1,j] = -p[2]*(countD+e*countU)  # Only the change because of other bubbles
    E2[i+1,j] = p[2]*(countD+e*countU)  # Only the change because of other bubbles, so those Es goes into D/U compartments are included in E1
    
    
    # STAFF to STUDENTs they teach 
    # initial - there is no change of S and E initially
    S3[1,j] = 0
    E3[1,j] = 0
    r3[1,j] = r.values[3]
    
    # The change in S and E for bubble j depends on the number of D and U of staffs
    S3[i+1,j] = - r1[i,j]*p[3]*(D.Staff[i,j]+e*U.Staff[i,j])   
    E3[i+1,j] = r1[i,j]*p[3]*(D.Staff[i,j]+e*U.Staff[i,j])  
    r3[i+1,j] =  S[i,j]
    
    
    
    ### TUTOR GROUP
    
    # STUDENTs TO STUDENTs in the same tutorial group
    
    # A tutor group contains 2 bubbles of students and a staff that is not teaching the bubbles
    # With the tutor meeting of 15mins and a typical school day of 6 hours, the time length of contact is 1/24 of that in a bubble
    # initial (S.Std_Std is the change of S in tutor group j with students in both bubble j and 11-j)
    S.Std_Std[1,] = c(rep(0,10))  # S.Std_Std : total change of students in tutor group
    S.Std1[1,] = c(rep(0,10))
    
    
    # The change of S for tutor group j with students in both bubble j and 11-j
    if (j <= 5){ # 5 tutor groups in total
      S.Std_Std[i+1,j] = - (S[i,j]+S[i,11-j])*p[1]*((D[i,j]+e*U[i,j])+(D[i,11-j]+e*U[i,11-j]))*1/24
      
      # As the change is less than 0.5, separate the new cases of infections according to the percentage of S in each bubble
      if (S[i,j]+S[i,11-j] > 0){
        S.Std1[i+1,j] = S.Std_Std[i+1,j]*S[i,j]/(S[i,j]+S[i,11-j])  # S.Std1 : total change of students in  bubble j by having tutor group
        S.Std1[i+1,11-j] = S.Std_Std[i+1,j]*S[i,11-j]/(S[i,j]+S[i,11-j])
      }
      
      else{
        S.Std1[i+1,j] = 0
        S.Std1[i+1,11-j] = 0
      }
    }
    
    else{ # avoid to count the infections twice
    }
    
    
    # STAFF TO STUDENTs in the same tutorial group
    
    # There is only 1 staff for each tutor group
    # S.Std2 : the change of studnets in bubble j by infections from the tutor
    # initial - there is no change of S and E initially
    S.Std2[1,] = c(rep(0,10)) 
    
    # The change of S for tutor group j with students in both bubble j and 11-j
    if (j <= 5){ # 5 tutor groups in total
      if (S[i,j]+S[i,11-j] > 0){
        S.Std2[i+1,j] = - ((S[i,j]+S[i,11-j])*p[3]*(D.Stf[i,j]+e*U.Stf[i,j])*1/24)*(S[i,j]/(S[i,j]+S[i,11-j]))
        S.Std2[i+1,11-j] = - ((S[i,j]+S[i,11-j])*p[3]*(D.Stf[i,j]+e*U.Stf[i,j])*1/24)*(S[i,11-j]/(S[i,j]+S[i,11-j]))
      }
      
      else{
        S.Std2[i+1,j] = 0
        S.Std2[i+1,11-j] = 0
      }
    }
    
    else{ # avoid to count the infections twice
    }
    
    
    
    # TOTAL on STUDNETS 
    if (S1[i+1,j] + S2[i+1,j] + S3[i+1,j] + S.Std1[i+1,j] + S.Std2[i+1,j] >= 0){   # The total number of S has to be >= 0
      S[i+1,j] = S1[i+1,j] + S2[i+1,j] + S3[i+1,j] + S.Std1[i+1,j] + S.Std2[i+1,j]
      E[i+1,j] = E1[i+1,j] + E2[i+1,j] + E3[i+1,j] - S.Std1[i+1,j] - S.Std2[i+1,j]
    }
    else {  # If not, S = 0, 
      #         E = previous (time step) amout of E - the same amount of students reduced in S compartment - those move to the next compartments
      S[i+1,j] = 0
      E[i+1,j] = E[i,j] - (S[i+1,j]-S[i,j]) - a*E[i,j]
    }
    
    D[i+1,j] = D1[i+1,j]
    U[i+1,j] = U1[i+1,j]
    Q[i+1,j] = Q1[i+1,j]
    R[i+1,j] = R1[i+1,j]
    
    
    ###### For STAFF
    
    # Assume there are 55 staffs in total, initially 30 of them teach students. 
    # The overall effect on STAFF (stdent -> staff  &   staff -> staff in same / different bubble)
    
    # initials
    S.Staff[1,] =  3
    E.Staff[1,] =  0
    D.Staff[1,] =  0
    U.Staff[1,] =  0
    Q.Staff[1,] =  0
    R.Staff[1,] =  0
    
    # STUDENT to STAFF
    # initials
    S4[1,j] = 0
    r4[1,j] = r.values[4]  
    
    # Only S and E compartments will change directly by the infection cause by studnets (E3 = -S3)
    S4[i+1,j] = - r4[i]*p[4]*(D[i,j]+e*U[i,j])
    r4[i+1,j] = S.Staff[i,j]
    
    
    # STAFF to STAFF (teach same bubble)
    # initials
    S6[1,j] = 0
    
    # Only S and E compartments will change directly by the infection cause by staffs teach same bubble (E6 = -S6)
    S6[i+1,j] = - r4[i]*p[5]*(D.Staff[i,j]+e*U.Staff[i,j])
    
    
    # STAFF to STAFF (teach different bubbles)
    # number of contact for each individual staff teach bubble j to the staffs teach other bubbles
    r5.diff[1,j] =  3  # each staff teach j contact 3 staffs teach other bubbles
    r5.diff[i+1,j] = r5.diff[i,j]    
    # the total number of contacts of the students in bubble j
    m.Staff = round(r5.diff[i,j]*(S.Staff[i,j]))  # only Susceptible staffs in j might be infected 
    
    # inital values of total number of staff at school not teach bubble j
    # (only S,E,D,U are at school, currently not considered recovery come back to school yet)
    S5.diff[1,] = 27
    E5.diff[1,] = 0
    D5.diff[1,] = 0
    U5.diff[1,] = 0
    R5.diff[1,] = 0
    
    # The number of S, E, D, U at time i, not teaching to bubble j
    S5.diff[i,j] = round(sum(S.Staff[i,])-S.Staff[i,j])
    E5.diff[i,j] = round(sum(E.Staff[i,])-E.Staff[i,j])
    D5.diff[i,j] = round(sum(D.Staff[i,])-D.Staff[i,j])
    U5.diff[i,j] = round(sum(U.Staff[i,])-U.Staff[i,j])
    R5.diff[i,j] = round(sum(R.Staff[i,])-R.Staff[i,j])
    
    # make a list of S, E, D, U not teach bubble j (the times of repeat is the number of staff in each compartment)
    list.Staff = c(rep("S", S5.diff[i,j]), rep("E", E5.diff[i,j]), rep("D", D5.diff[i,j]), rep("U", U5.diff[i,j]), rep("R", R5.diff[i,j]))
    
    #count the number of S,E,D,U that staffs teaching bubble j contacted to
    # " sample(list.Staff, m.Staff, replace = TRUE) ": Randomly select m.Staff from list
    set.seed(123)
    count.StaffS = sum((sample(list.Staff, m.Staff, replace = TRUE))=="S") # for students in bubble j: the number of contact outside bubble j are S
    count.StaffE = sum((sample(list.Staff, m.Staff, replace = TRUE))=="E") # for students in bubble j: the number of contact outside bubble j are E
    count.StaffD = sum((sample(list.Staff, m.Staff, replace = TRUE))=="D") # for students in bubble j: the number of contact outside bubble j are D
    count.StaffU = sum((sample(list.Staff, m.Staff, replace = TRUE))=="U") # for students in bubble j: the number of contact outside bubble j are U
    count.StaffR = sum((sample(list.Staff, m.Staff, replace = TRUE))=="R") # for students in bubble j: the number of contact outside bubble j are R
    
    # The number of change in S and E compartments by staffs teach j and other staffs contacts
    # initial (no transmission at time 1)
    S5[1,j] = 0
    
    # The change in S and E for staffs teach bubble j only depends on the number of:
    # 1. (   susceptible staffs teach bubble j: r5[i,j] = S.Staff[i,j]   ) and 
    # 2. (   number of detectable/symtomatic and undetectable/asymtomatic staffs teach other bubbles: count.StaffD+e*count.StaffU
    #                                                        -- e is because the effectiveness on D and U in infection rate are different)
    S5[i+1,j] = - p[5]*(count.StaffD+e*count.StaffU)  # Only the change because of staff teach other bubbles
    # E5 = -S5
    
    
    # TUTOR GROUP
    
    # The STUDENTS in the tutor group to STAFF
    # Assume for tutor group 1-5, the tutor is not teaching any bubbles
    # There is no staff to replace tutor if he is infected, but he will work again when he has recovered
    # initial - there is no change of S and E initially
    S.Stf[1,] = c(rep(1,5), rep(0,5)) 
    E.Stf[1,] = D.Stf[1,] = U.Stf[1,] = Q.Stf[1,] = R.Stf[1,] = rep(0,10)
    
    
    # The total number of S for staff in tutor group j 
    if (j <= 5){ # 5 tutor groups in total
      if (S.Stf[i,j] > 0){
        S.Stf[i+1,j] = S.Stf[i,j] - S.Stf[i,j]*p[4]*((D[i,j]+e*U[i,j])+(D[i,11-j]+e*U[i,11-j]))*1/24
      }
      
      else{
        S.Stf[i+1,j] = 0
      }
    }
    
    else{ # avoid to count the infections twice
      S.Stf[i+1,j] = 0
    }
    
    E.Stf[i+1,j] = E.Stf[i,j] + (S.Stf[i+1,j] - S.Stf[i,j]) - a*E.Stf[i,j]
    D.Stf[i+1,j] = D.Stf[i,j] + a*d[1]*E.Stf[i,j] - o*D.Stf[i,j]
    U.Stf[i+1,j] = U.Stf[i,j] + a*(1-d[1])*E.Stf[i,j] - y*U.Stf[i,j]
    Q.Stf[i+1,j] = Q.Stf[i,j] + o*D.Stf[i,j] - y*Q.Stf[i,j]
    R.Stf[i+1,j] = R.Stf[i,j] + y*(Q.Stf[i,j]+U.Stf[i,j])
    
    
    # TOTAL on STAFF
    # the total change of staff
    
    S.Staff.change[1,j] = 0
    S.Staff.change[i+1,j] = -(S4[i+1,j] + S5[i+1,j] + S6[i+1,j])    
    
    # With larger number in each group, remove the overlapping cases of new infection by:
    # (There might be chance an individual staff is infected by both students he teach and other staff he contact with)
    # Here, use randomly choosing to reduce the overlapping change effect
    # # # count.StaffS_overlap = - round(S3[i+1,j] + S5[i+1,j] + S6[i+1,j]) # the infected cases/the change in S (with overlap)
    
    # This formula comes from the dynamic equations (in powerpoint slide 4)
    S.Staff[i+1,j] = S.Staff[i,j] - S.Staff.change[i+1,j]
    E.Staff[i+1,j] = E.Staff[i,j] + S.Staff.change[i+1,j] - a*E.Staff[i,j]
    D.Staff[i+1,j] = D.Staff[i,j] + a*d[2]*E.Staff[i,j] - o*D.Staff[i,j]
    U.Staff[i+1,j] = U.Staff[i,j] + a*(1-d[2])*E.Staff[i,j] - y*U.Staff[i,j]
    Q.Staff[i+1,j] = Q.Staff[i,j] + o*D.Staff[i,j] - y*Q.Staff[i,j]
    R.Staff[i+1,j] = R.Staff[i,j] + y*(Q.Staff[i,j]+U.Staff[i,j])
    
    
    # STAFF replacement (remove this part to see if there is no extra staff that teaching the same course)
    # We assume initially 30 out of 55 staffs are teaching bubbles and the other 20 staffs are working from home.
    # Assume each bubble has 3 teaching staff and 2 other staffs working from home that can teach the same course.
    # If one of the staff was teaching at school is isolated, a staff that is "teaching the same course" will come back to school and teach the bubble. (within 24h)
    # When an istolated staff is recovered, he will come back to school on the next day. (within 24h)
    # If that result in more than 3 staffs teaching the bubble, let some susceptible staffs to work from home.  
    # So, each bubble will have 3 teaching staffs until there are not enough staffs available.
    
    Staff.rp[1,j] = 2  # the staff can come to teach in the next day that are working from home
    Staff.School[1,j] = 3  # total number of staffs that are teaching the bubble
    Staff.School[i,j] = S.Staff[i,j] + E.Staff[i,j] + D.Staff[i,j] + U.Staff[i,j] + R.Staff[i,j]
    
    if (Staff.School[i,j] < 3) {  # if the number of staff teaching bubble j is less than 3
      
      if ((3 - Staff.School[i,j]) <= Staff.rp[i,j]) {  # if there are enough staff to come back and teach
        S.Staff[i+1,j] = S.Staff[i+1,j] + (3 - Staff.School[i,j])
        Staff.rp[i+1,j] = Staff.rp[i,j] - (3 - Staff.School[i,j])
      }
      
      else{  # if there is not enought staff to come back and teach
        S.Staff[i+1,j] = S.Staff[i+1,j] + Staff.rp[i,j]
        Staff.rp[i+1,j] = 0
      }
    }
    
    else{  
      if (round(Staff.School[i,j]) > 3) { # if after the staff combing back from isolation, there are more than 3 staffs teaching the bubble j
        S.Staff[i+1,j] = S.Staff[i+1,j] - (Staff.School[i,j] - 3)
        Staff.rp[i+1,j] = Staff.rp[i,j] + (Staff.School[i,j] - 3)
      } 
      
      else {# if there are 3 staffs teaching the bubble j
        S.Staff[i+1,j] = S.Staff[i+1,j]
        Staff.rp[i+1,j] = Staff.rp[i,j] 
      }
    }
    
    
    
    # The change on Weekends
    # There will not be new infections from 0 A.M. on Saturday till 0 A.M. on the following Monday
    
    offschool = c(seq(0,T,7)-1,seq(0,T,7)-0)
    
    if (!is.na(match(i, setdiff(0:T,offschool))) == TRUE){  #if time i is weekday
    }
    
    else{  #if time i is weekend
      S[i+1,j] = S[i,j]
      E[i+1,j] = E[i,j] - a*E[i,j]
      
      S.Staff[i+1,j] = S.Staff[i,j]
      E.Staff[i+1,j] = E.Staff[i,j] - a*E.Staff[i,j]
      
    }
    
    # Add the results of each time step i and each bubble j of each compartments to the dataframe da_tauO2
    da_tauO2[i,paste("S.B", j, sep = "")] <- S[i,j]
    da_tauO2[i,paste("E.B", j, sep = "")] <- E[i,j]
    da_tauO2[i,paste("D.B", j, sep = "")] <- D[i,j]
    da_tauO2[i,paste("U.B", j, sep = "")] <- U[i,j]
    da_tauO2[i,paste("Q.B", j, sep = "")] <- Q[i,j]
    da_tauO2[i,paste("R.B", j, sep = "")] <- R[i,j]
    
    da_tauO2[i,paste("S.", "Staff", sep = "")] <- sum(S.Staff[i,]) + sum(S.Stf[i,])
    da_tauO2[i,paste("E.", "Staff", sep = "")] <- sum(E.Staff[i,]) + sum(E.Stf[i,])
    da_tauO2[i,paste("D.", "Staff", sep = "")] <- sum(D.Staff[i,]) + sum(D.Stf[i,])
    da_tauO2[i,paste("U.", "Staff", sep = "")] <- sum(U.Staff[i,]) + sum(U.Stf[i,])
    da_tauO2[i,paste("Q.", "Staff", sep = "")] <- sum(Q.Staff[i,]) + sum(Q.Stf[i,])
    da_tauO2[i,paste("R.", "Staff", sep = "")] <- sum(R.Staff[i,]) + sum(R.Stf[i,])
    
    # Sum up the total dynamic of students for all bubbles
    da_tauO2$S.Student[i] <- sum(S[i,])
    da_tauO2$E.Student[i] <- sum(E[i,])
    da_tauO2$D.Student[i] <- sum(D[i,])
    da_tauO2$U.Student[i] <- sum(U[i,])
    da_tauO2$Q.Student[i] <- sum(Q[i,])
    da_tauO2$R.Student[i] <- sum(R[i,])
    da_tauO2$N.Student[i] <- 100
    
  }
}

# Add extra columns 
for (i in 1:T){
  
  # total number of Staffs working from home
  da_tauO2$Staff.rp[i] <- sum(Staff.rp[i,])  
  
  # reproduction rate Rt
  da_tauO2$Rt.Student[i] <- (da_tauO2$E.Student[i+1] + da_tauO2$D.Student[i+1] + da_tauO2$U.Student[i+1] + da_tauO2$Q.Student[i+1]) / 
    (da_tauO2$E.Student[i] + da_tauO2$D.Student[i] + da_tauO2$U.Student[i] + da_tauO2$Q.Student[i])
  
  da_tauO2$Rt.Staff[i] <- (da_tauO2$E.Staff[i+1] + da_tauO2$D.Staff[i+1] + da_tauO2$U.Staff[i+1] + da_tauO2$Q.Staff[i+1]) / 
    (da_tauO2$E.Staff[i] + da_tauO2$D.Staff[i] + da_tauO2$U.Staff[i] + da_tauO2$Q.Staff[i])
}



###################################################################################################################
# Plot SEDUQR graphs
par(mfrow=c(2, 1), lwd=1)

# Students
X_lim <- seq(1,T,by=1)
plot(da_tauO2$S.Student~X_lim, pch=15, col="black", main = "SEDUQR Model for Students", type = "l", xlab = "Time (day)", ylab = "Number of Students", xlim = c(0,T), ylim = c(0,100))

lines(da_tauO2$S.Student, col="DeepPink", lty=1)
lines(da_tauO2$E.Student, col="darkTurquoise", lty=1)
lines(da_tauO2$D.Student, col="RosyBrown", lty=1)
lines(da_tauO2$U.Student, col="green", lty=1)
lines(da_tauO2$Q.Student, col="darkgreen", lty=1)
lines(da_tauO2$R.Student, col="darkblue", lty=1)

legend(300, 95 ,c("S","E","D", "U", "Q", "R"),col=c("DeepPink","darkTurquoise","RosyBrown", "green", "darkgreen", "darkblue"),
       text.col=c("DeepPink","darkTurquoise","RosyBrown", "green", "darkgreen", "darkblue"),lty=c(1,1,1,1,1,1),
       cex=0.5)

abline(v=90,col="dark grey", lty=2)

# Staffs
X_lim <- seq(1,T,by=1)
plot(da_tauO2$S.Staff~X_lim, pch=15, col="black", main = "SEDUQR Model for Staffs", type = "l", xlab = "Time (day)", ylab = "Number of Staffs", xlim = c(0,T), ylim = c(0,35))

lines(da_tauO2$S.Staff, col="DeepPink", lty=1)
lines(da_tauO2$E.Staff, col="darkTurquoise", lty=1)
lines(da_tauO2$D.Staff, col="RosyBrown", lty=1)
lines(da_tauO2$U.Staff, col="green", lty=1)
lines(da_tauO2$Q.Staff, col="darkgreen", lty=1)
lines(da_tauO2$R.Staff, col="darkblue", lty=1)

legend(300, 35,c("S","E","D", "U", "Q", "R"),col=c("DeepPink","darkTurquoise","RosyBrown", "green", "darkgreen", "darkblue"),
       text.col=c("DeepPink","darkTurquoise","RosyBrown", "green", "darkgreen", "darkblue"),lty=c(1,1,1,1,1,1),
       cex=0.5)

abline(v=90,col="dark grey", lty=2)
