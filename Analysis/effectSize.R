#Algorithm to calculate effect size between to groups
#Instead, we could use the Vargha-Delaney A measure, 
# which tells us how often, on average, one technique outperforms the other. 
# When applied to two populations (like the results of our two techniques), 
# the A measure is a value between 0 and 1: when the A measure is exactly 0.5, 
# then the two techniques achieve equal performance; when A is less than 0.5, 
# the first technique is worse; and when A is more than 0.5, the second technique 
# is worse. The closer to 0.5, the smaller the difference between the techniques; 
# the farther from 0.5, the larger the difference.

#Example
# In our case, the A measure is 0.34, indicating that rocket launchers (T1) performed 
# worse than fruit cake (T2), because 0.34 is less than 0.5. We can interpret the value as follows:  
# 34% of the time, rocket launchers will work better than fruit cake. Equivalently, 66% of the time, 
# fruit cake will work better than rocket launchers. (Anyone who has ever tried fruit cake can 
# understand its true destructive power.) We can deduce that you have twice the changes of surviving 
# should you forgo the military equipment and instead put Aunt Gertrude's culinary creation to good use. 

AMeasure <- function(a,b){
  
  # Compute the rank sum (Eqn 13)
  r = rank(c(a,b))
  r1 = sum(r[seq_along(a)])
  # Compute the measure (Eqn 14) 
  m = length(a)
  n = length(b)
  A = (2*r1-m*(m+1))/(2*m*n)
  return(A)
}