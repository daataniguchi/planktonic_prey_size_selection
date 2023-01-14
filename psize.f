c       Prey size class values 

      subroutine psize(pnum, psizewidth, psizemin, pi, pr,pv)
       implicit none
        double precision pi
        integer pnum

        real*8 psizewidth 
        real*8 psizemin 
        real*8 pr(pnum) 
        real*8 pv(pnum) 
        integer ii

       do 200 ii = 1,pnum
          pr(ii) = psizemin*(psizewidth**(ii-1))
          pv(ii) = 4.0/3.0*pi*(pr(ii)**3)
200    continue

       return
       end


