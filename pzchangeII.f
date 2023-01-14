c **********  Change in prey and grazer biomass with time *******

c *** Use this subroutine to calculate change in bioass wtih each time step

      subroutine pzchangeII(pnum,zmax,fnum,minind,maxind, 
     &     dt,Ctot,
     &     mu, Cdis, ks, pm,gprey,
     &     zm, R, 
     &    zgamma, ggrazer,
     &    mortalityflag,
     &     ptemp, ptemp2,
     &     ztemp, ztemp2)
   
      implicit none
      integer zmax, pnum,fnum
      real*8 minind(fnum)
      real*8 maxind(fnum)
      real*8 dt 
      real*8 Ctot
      real*8 mu(pnum)
      real*8 Cdis
      real*8 ks(pnum)
      real*8 pm(pnum)
      real*8 gprey(pnum)
      real*8 zm(zmax,fnum)
      real*8 R(zmax,fnum)
      real*8 zgamma
      real*8 ggrazer(zmax,fnum)
      integer mortalityflag
      real*8 ptemp(pnum)
      real*8 ptemp2(pnum)
      real*8 ztemp(zmax,fnum)
      real*8 ztemp2(zmax,fnum)

      integer i, j,k

       do 130 i = 1,pnum
         ptemp(i) = max(ptemp(i), 10.0**(-20.0)) 
          ptemp2(i)  = ptemp(i)+ 
     &       (ptemp(i) * 
     &       (mu(i)*Cdis/(Cdis + ks(i)) - pm(i)) -
     &       gprey(i) )*dt
       ptemp2(i) = max(ptemp2(i),10.0**(-20.0)) 
130    continue

      if (mortalityflag .eq. 1) then 
      do 140 k=1,fnum
          do 150 j = 1,zmax
          ztemp(j,k) = max(ztemp(j,k), 10.0**(-20.0)) 
          ztemp2(j,k) =  ztemp(j,k) + 
     &        (ztemp(j,k)*(-zm(j,k) - R(j,k))  
     &        - (0.63*ggrazer(j,k))
     &        + zgamma*ggrazer(j,k))*dt
          ztemp2(j,k) = max(ztemp2(j,k), 10.0**(-20.0)) 
150       continue
140   continue
      end if

      end !pz_change subroutine



