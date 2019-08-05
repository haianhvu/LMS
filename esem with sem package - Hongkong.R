install.packages("sem")

library(sem)

R.Expl1 <- factanal(data2, factors = 3)

F <- matrix(R.Expl1$loadings[1:27],9,3) 

F1 <- rbind(F[6,], F[9,], F[2,]) 

H <- solve(F1) %*% diag (sqrt(diag(F1 %*% t(F1))))
Rotated.F <- zapsmall (F %*% H)
Rotated.F 

solve(H) %*% t(solve(H)) 

Model.esem1<-specifyModel() 
F1-> ECO501001,lam11,.652
F1-> Y2,lam21, .000
F1-> Y3,lam31,.160
F1-> Y4,lam41,-.111
F1-> Y5,lam51,.527
F1-> Y6,lam61,.754
F1-> Y7,lam71,.688
F1-> Y8,lam81,.366
F1-> Y9,lam91,.000
#F1-> X,lam111,0
#F2-> X,lam112,0

F3-> Y1,NA,0
F3-> Y2,NA,0
F3-> Y3,NA,0
F3-> Y4,NA,0
F3-> Y5,NA,0
F3-> Y6,NA,0
F3-> Y7,NA,0
F3-> Y8,NA,0
F3-> Y9,NA,0
F3-> Y10,NA,0
F3-> X,lam113,1
F1 <-> F2, NA,.9
F1 <-> F1, NA,1
F2 <-> F2, NA,1
F3 <-> F3, NA,1
F1 <- F3, NA,.5
F2 <- F3, NA,1 

