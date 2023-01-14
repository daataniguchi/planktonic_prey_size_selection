c******  Prey physiological parameters *********
c *** Use this code to calculate phytoplankton physiological parameters

      subroutine pparamchoice(pnum,pr,pv, mu0,muexp,muE,mutmpcoeff,
     &       kBoltz,tmp,pi,
     &       ks0,ksexp,etatmp, mu, ks, Dp, pm,u,muflag)
    
      implicit none
      integer pnum
      real*8 pr(pnum)
      real*8 pv(pnum)
      real*8 mu0
      real*8 muexp
      real*8 muE
      real*8 kBoltz
      real*8 tmp
      double precision pi
      real*8 mu(pnum)
      real*8 pm(pnum)
      real*8 u(pnum)
      real*8 ks0
      real*8 ksexp
      real*8 ks(pnum)
      real*8 etatmp
      real*8 Dp(pnum)
      real*8 mutmpcoeff
      integer muflag
      integer i

      do 45 i = 1,pnum
         if (muflag .eq. 1) then 
          mu(i) =  (( mutmpcoeff*( (2.0*pr(i)) *10.0**6)**muexp )) 
     &      *exp(-muE/kBoltz/(273.15+tmp)) 
          end if 

      ks(i) = ks0*( (2.0*pr(i)) *10.0**6)**ksexp

      Dp(i) = ((tmp+273.15)*(1.38e-20))/(6.0*pi*etatmp*pr(i))

      pm(i) = pr(i)*0.0 + 0.005/24.0/3600.0

      u(i) = 0.0 + pr(i)*0.0

45      continue
      return
      end



