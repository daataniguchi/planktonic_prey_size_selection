c ****** Calculating carbon content per cell and size class and number of cells per size class

         subroutine pCcount(pnum,pr,pv,pCcoeff,pCexp,
     &       pCsize,pCcell,pcount)

        implicit none
        integer pnum
        real*8 pr(pnum) 
        real*8 pv(pnum)
        real*8 pCcoeff 
        real*8 pCexp 
        real*8 pCsize(pnum)       
        real*8 pCcell(pnum) 
        real*8 pcount(pnum) 
        integer i

      do 50 i = 1,pnum
      pCcell(i) = (0.216*((pv(i)*10.0**18)**0.939))*10.0**(-12)
      pCsize(i) = pCcoeff*(pr(i)**pCexp)
      pcount(i) = pCsize(i)/pCcell(i)
50     continue
       return
       end


