
! ======================================================================
 function Conformal(t)
 use dataModule
 implicit double precision(a-h,m-z)
 Conformal=(1.d0-dsqrt(1.d0-t/tin))/(1.d0+dsqrt(1.d0-t/tin))
 return
 end

! ======================================================================
 function factorIso(s)
 use dataModule
 implicit double precision(a-h,m-z)
 complex*16 G3
 G3=1.d0+data_kappa*s/(s-momega**2+dcmplx(0.d0,1.d0)*momega*Gammaomega)
 AbsG3=real(G3)*real(G3)+aimag(G3)*aimag(G3)!G3*conjg(G3)
 factorIso=AbsG3 
 return
 end

 function CoefxxSq(rSq,FH,g0,g1,gH,ggQ2,Q2,gQ3,Q3,FFQ3,zHuber,valI)
 implicit double precision(a-h,m-z)

! Straight from the mathematiica output ! but needs CAREFUL check
 CoefxxSq=(-(ggQ2**2*zHuber**4*(-1.d0+zHuber*Conformal(Q2))**2* &
    (-1.d0+Conformal(Q2)**2)*Conformal(Q3)**4*(-1.d0+                 &
     zHuber*Conformal(Q3))**2*(-1.d0+Conformal(Q2)*Conformal(Q3))**2)+ &
     ggQ2**2*zHuber**2*(-1.d0+zHuber**2)*(-1.d0+                       &
     zHuber*Conformal(Q2))**2*(-1.d0+Conformal(Q2)**2)* &
     Conformal(Q3)**4*(-1.d0+Conformal(Q2)*Conformal(Q3))**2*(-1.d0+ &
     Conformal(Q3)**2))/((-1.d0+zHuber**2)*(-1.d0+zHuber* &
     Conformal(Q2))**2*(-1.d0+Conformal(Q2)**2)*(-1.d0+zHuber* &
     Conformal(Q3))**2*(-1.d0+Conformal(Q2)*Conformal(Q3))**2* &
     (-1.d0+Conformal(Q3)**2))

 return
 end
	

 function Coefxx(rSq,FH,g0,g1,gH,ggQ2,Q2,gQ3,Q3,FFQ3,zHuber,valI)
 implicit double precision(a-h,m-z)
	
 Coefxx=(ggQ2*Conformal(Q2)**2*(-1.d0+zHuber*Conformal(Q2))*(-1+ &
     Conformal(Q2)**2)*Conformal(Q3)**2*(-1 + zHuber*Conformal(Q3))* &
     (-1.d0+Conformal(Q2)*Conformal(Q3))*(-1.d0+Conformal(Q3)**2)* &
     (-(zHuber*(-g0+FH*gH-g1*zHuber)*(-1.d0+zHuber**2)*(-1.d0+ &
     zHuber*Conformal(Q2))*Conformal(Q3)**2)+zHuber**4*(-1.d0+ &
     zHuber*Conformal(Q2))*(-g0+FFQ3*gQ3-g1*Conformal(Q3))*(-1.d0+ &
     zHuber*Conformal(Q3)))+(g0-FFQ3*gQ3+g1*Conformal(Q3))*(-1.d0+ &
     zHuber*Conformal(Q3))*(-1.d0+Conformal(Q2)*Conformal(Q3))*(-1.d0+ &
     Conformal(Q3)**2)*(-(ggQ2*zHuber**4*Conformal(Q2)**2*(-1.d0+ & 
     zHuber*Conformal(Q2))**2*(-1.d0+Conformal(Q2)**2)* &
     Conformal(Q3)**2*(-1.d0+zHuber*Conformal(Q3)))+ggQ2*zHuber**3* &
     (-1.d0+zHuber**2)*Conformal(Q2)**2*(-1.d0+zHuber*Conformal(Q2))* &
     (-1.d0+Conformal(Q2)**2)*Conformal(Q3)**2*(-1.d0+Conformal(Q2)* &
     Conformal(Q3)))-Conformal(Q3)**4*(-1.d0+zHuber*Conformal(Q3))* &
     (-1.d0+Conformal(Q2)*Conformal(Q3))*(ggQ2*zHuber**2*(g0-FH*gH+ &
     g1*zHuber)*(-1.d0+zHuber**2)*Conformal(Q2)**2*(-1.d0+ &
     zHuber*Conformal(Q2))*(-1.d0+Conformal(Q2)**2)*(-1.d0+ &
     zHuber*Conformal(Q3))*(-1.d0+Conformal(Q2)*Conformal(Q3)) -  &
     ggQ2*zHuber**4*(g0+g1*Conformal(Q2))*(-1.d0+zHuber*Conformal(Q2)) &
     **2*(-1.d0+Conformal(Q2)**2)*(-1.d0+zHuber*Conformal(Q3))* &
     (-1.d0+Conformal(Q2)*Conformal(Q3))+ggQ2*(-1.d0+ &
     zHuber*Conformal(Q2))*(-1.d0+Conformal(Q2)**2)*(-1.d0+ &
     Conformal(Q2)*Conformal(Q3))*(-(zHuber**2*(-g0+FH*gH-g1*zHuber)* &
     (-1.d0+zHuber**2)*Conformal(Q2)**2*(-1.d0+zHuber*Conformal(Q3)))+  &
     zHuber**4*(-g0-g1*Conformal(Q2))*(-1.d0+zHuber*Conformal(Q2))* &
     (-1.d0+zHuber*Conformal(Q3))))-zHuber*Conformal(Q3)**2*(-1.d0+ &
     Conformal(Q2)*Conformal(Q3))*(-1.d0+Conformal(Q3)**2)*(-(ggQ2* &
     (g0-FH*gH+g1*zHuber)*(-1.d0+zHuber**2)*Conformal(Q2)**2*(-1.d0+ &
     zHuber*Conformal(Q2))**2*(-1.d0+Conformal(Q2)**2)* &
     Conformal(Q3)**2*(-1.d0+zHuber*Conformal(Q3)))+ggQ2*zHuber* &
     (-1.d0+zHuber**2)*(g0+g1*Conformal(Q2))*(-1.d0+ &
     zHuber*Conformal(Q2))**2*(-1.d0+Conformal(Q2)**2)* &
     Conformal(Q3)**2*(-1.d0+Conformal(Q2)*Conformal(Q3))+  &
     ggQ2*(-1.d0+zHuber*Conformal(Q2))*(-1.d0+Conformal(Q2)**2)* &
     (-1.d0+Conformal(Q2)*Conformal(Q3))*                         &
     (-(zHuber*(-1.d0+zHuber**2)*(-g0-g1*Conformal(Q2))*(-1.d0+  &
     zHuber*Conformal(Q2))*Conformal(Q3)**2) +                   &
     zHuber**2*(-1.d0+zHuber**2)*Conformal(Q2)**2*(-g0+FFQ3*gQ3- &
     g1*Conformal(Q3))*(-1.d0+zHuber*Conformal(Q3)))))/           &
     ((-1.d0+zHuber**2)*(-1.d0+zHuber*Conformal(Q2))**2*(-1.d0+     &
     Conformal(Q2)**2)*(-1.d0+zHuber*Conformal(Q3))**2*(-1.d0+       &
     Conformal(Q2)*Conformal(Q3))**2*(-1.d0+Conformal(Q3)**2))


 return
 end


 function CoefCnst(rSq,FH,g0,g1,gH,ggQ2,Q2,gQ3,Q3,FFQ3,zHuber, valI)
 implicit double precision(a-h,m-z)

 Cnst=(-(Conformal(Q3)**4*(-1.d0+zHuber*Conformal(Q3))*(-1.d0+ &
     Conformal(Q2)*Conformal(Q3))*(-(Conformal(Q2)**4*(-1.d0+        &
     zHuber*Conformal(Q2))*(-1.d0+Conformal(Q2)*Conformal(Q3))*      &
     ((g0**2+g1**2-valI)*zHuber**4*(-1.d0+zHuber*Conformal(Q2))*     &
     (-1.d0+zHuber*Conformal(Q3))+(-g0+FH*gH-g1*zHuber)*(g0-FH*gH+   &
     g1*zHuber)*(-1.d0+zHuber**2)*(-1.d0+zHuber*Conformal(Q2))*(-1.d0+ &
     zHuber*Conformal(Q3))))-(g0+g1*Conformal(Q2))*(-1.d0+          &
     zHuber*Conformal(Q2))*(-1.d0+Conformal(Q2)**2)*(-1.d0+             &
     Conformal(Q2)*Conformal(Q3))*(-(zHuber**2*(-g0+FH*gH-g1*zHuber)*    & 
     (-1.d0+zHuber**2)*Conformal(Q2)**2*(-1.d0+zHuber*Conformal(Q3)))+ &
     zHuber**4*(-g0-g1*Conformal(Q2))*(-1.d0+zHuber*Conformal(Q2))* &
     (-1.d0+zHuber*Conformal(Q3)))+zHuber**2*Conformal(Q2)**2*(-1.d0+ &
     Conformal(Q2)**2)*(-1.d0+Conformal(Q2)*Conformal(Q3))* &
     ((g0**2+g1**2-valI)*zHuber**2*(-1.d0+zHuber**2)*Conformal(Q2)**2* &
     (-1.d0+zHuber*Conformal(Q3))+(g0-FH*gH+g1*zHuber)*(-1.d0+ &
     zHuber**2)*(-g0-g1*Conformal(Q2))*(-1.d0+zHuber*Conformal(Q2))* &
     (-1.d0+zHuber*Conformal(Q3)))))+(g0-FFQ3*gQ3+g1*Conformal(Q3))* &
     (-1.d0+zHuber*Conformal(Q3))*(-1.d0+Conformal(Q2)*Conformal(Q3))* &
     (-1.d0+Conformal(Q3)**2)*(-(Conformal(Q2)**2*(-1.d0+zHuber* &
     Conformal(Q2))*(-1.d0+Conformal(Q2)**2)*Conformal(Q3)**2* &
     (-(zHuber**2*(-g0+FH*gH-g1*zHuber)*(-1.d0+zHuber**2)* &
     Conformal(Q2)**2*(-1.d0+zHuber*Conformal(Q3)))+zHuber**4*(-g0- & 
     g1*Conformal(Q2))*(-1.d0+zHuber*Conformal(Q2))*(-1.d0+zHuber* &
     Conformal(Q3))))-zHuber**2*Conformal(Q2)**2*(-1.d0+ &
     Conformal(Q2)**2)*(-1.d0+Conformal(Q2)*Conformal(Q3))* &
     (-(zHuber*(-1.d0+zHuber**2)*(-g0-g1*Conformal(Q2))*(-1.d0+ & 
     zHuber*Conformal(Q2))*Conformal(Q3)**2)+zHuber**2*(-1.d0+ &
     zHuber**2)*Conformal(Q2)**2*(-g0+FFQ3*gQ3-g1*Conformal(Q3))* &
     (-1.d0+zHuber*Conformal(Q3)))+Conformal(Q2)**4*(-1.d0+zHuber* &
     Conformal(Q2))*(-1.d0+Conformal(Q2)*Conformal(Q3))*(-(zHuber*(-g0 & 
     +FH*gH-g1*zHuber)*(-1.d0+zHuber**2)*(-1.d0+zHuber*Conformal(Q2))* &
     Conformal(Q3)**2)+zHuber**4*(-1.d0+zHuber*Conformal(Q2))*(-g0+ &
     FFQ3*gQ3-g1*Conformal(Q3))*(-1.d0+zHuber*Conformal(Q3))))-  &
     zHuber*Conformal(Q3)**2*(-1.d0+Conformal(Q2)*Conformal(Q3))* &
     (-1.d0+Conformal(Q3)**2)*(-(Conformal(Q2)**2*(-1.d0+zHuber* &
     Conformal(Q2))*(-1.d0+Conformal(Q2)**2)*Conformal(Q3)**2* &
     ((g0**2+g1**2-valI)*zHuber**2*(-1.d0+zHuber**2)*Conformal(Q2)**2* &
     (-1.d0+zHuber*Conformal(Q3))+(g0-FH*gH+g1*zHuber)* &
     (-1.d0+zHuber**2)*(-g0-g1*Conformal(Q2))*(-1.d0+zHuber* &
     Conformal(Q2))*(-1.d0+zHuber*Conformal(Q3))))-(g0+ &
     g1*Conformal(Q2))*(-1.d0+zHuber*Conformal(Q2))*(-1.d0+ &
     Conformal(Q2)**2)*(-1 + Conformal(Q2)*Conformal(Q3))* &
     (-(zHuber*(-1.d0+zHuber**2)*(-g0-g1*Conformal(Q2))*(-1.d0+ &
     zHuber*Conformal(Q2))*Conformal(Q3)**2)+zHuber**2*(-1.d0+ &
     zHuber**2)*Conformal(Q2)**2*(-g0+FFQ3*gQ3-g1*Conformal(Q3))* &
     (-1.d0+zHuber*Conformal(Q3)))+Conformal(Q2)**4*(-1.d0+zHuber* &
     Conformal(Q2))*(-1.d0+Conformal(Q2)*Conformal(Q3))*  &
     (-((-g0**2-g1**2+valI)*zHuber*(-1.d0+zHuber**2)*(-1.d0+zHuber* &
     Conformal(Q2))*Conformal(Q3)**2)+(g0-FH*gH+g1*zHuber)* &
     (-1.d0+zHuber**2)*(-1.d0+zHuber*Conformal(Q2))*(-g0+FFQ3*gQ3- &
     g1*Conformal(Q3))*(-1.d0+zHuber*Conformal(Q3))))+ &
     Conformal(Q2)**2*Conformal(Q3)**2*(-1.d0+zHuber*Conformal(Q3))* &
     (-1.d0+Conformal(Q3)**2)*(-(Conformal(Q2)**2*(-1.d0+zHuber* &
     Conformal(Q2))*(-1.d0+Conformal(Q2)**2)*Conformal(Q3)**2* &
     ((g0**2+g1**2-valI)*zHuber**4*(-1.d0+zHuber*Conformal(Q2))* &
     (-1.d0+zHuber*Conformal(Q3))+(-g0+FH*gH-g1*zHuber)*(g0-FH*gH+ &
     g1*zHuber)*(-1.d0+zHuber**2)*(-1.d0+zHuber*Conformal(Q2))* &
     (-1.d0+zHuber*Conformal(Q3))))-(g0+g1*Conformal(Q2))* &
     (-1.d0+zHuber*Conformal(Q2))*(-1.d0+Conformal(Q2)**2)* &
     (-1.d0+Conformal(Q2)*Conformal(Q3))* &
     (-(zHuber*(-g0+FH*gH-g1*zHuber)*(-1.d0+zHuber**2)*(-1.d0+zHuber* &
     Conformal(Q2))*Conformal(Q3)**2)+zHuber**4*(-1.d0+zHuber* &
     Conformal(Q2))*(-g0+FFQ3*gQ3-g1*Conformal(Q3))*(-1.d0+zHuber* &
     Conformal(Q3)))+zHuber**2*Conformal(Q2)**2*(-1.d0+ &
     Conformal(Q2)**2)*(-1.d0+Conformal(Q2)*Conformal(Q3))* &
     (-((-g0**2-g1**2+valI)*zHuber*(-1.d0+zHuber**2)*(-1.d0+zHuber* &
     Conformal(Q2))*Conformal(Q3)**2)+(g0-FH*gH+g1*zHuber)*(-1.d0+ &
     zHuber**2)*(-1.d0+zHuber*Conformal(Q2))*(-g0+FFQ3*gQ3- &
     g1*Conformal(Q3))*(-1.d0+zHuber*Conformal(Q3))))) &
     /((-1.d0+zHuber**2)*(-1.d0+zHuber*Conformal(Q2))**2*(-1.d0+ &
     Conformal(Q2)**2)*(-1.d0+zHuber*Conformal(Q3))**2*(-1.d0+ &
     Conformal(Q2)*Conformal(Q3))**2*(-1.d0+Conformal(Q3)**2))

 return
 end















