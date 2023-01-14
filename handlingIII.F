c *********** Handling time
c ** This subroutine calculates the handling time 

      subroutine handlingIII(zmax, pnum,fnum,znumf,
     &   gmax, pCcell, zCcell,zrmx,pr,
     &    minind,maxind, hflag,
     &     h)

      implicit none
      integer zmax, pnum,fnum, hflag
      integer znumf(fnum)
      real*8 minind(fnum)
      real*8 maxind(fnum)
       
      real*8 gmax(zmax,pnum,fnum)
      real*8 zCcell(zmax,fnum)
      real*8 pCcell(pnum)
      real*8 zrmx(zmax,fnum)
      real*8 pr(pnum)
      real*8 h(zmax,pnum,fnum)

      integer  k, j,l,kk,jj,ll,lll,jjj,kkkk
      integer ind1, ind2 

       if (hflag .eq. 3) then 
      do 275 lll = 1,fnum 
        ind1 = minind(lll)
      do 375 jjj = 1, znumf(lll) 
          ind2 = ind1 + jjj -1
         do 475 kkkk = 1, pnum 
         h(ind2,kkkk,lll) = 382.981*(zrmx(ind2,lll)/pr(kkkk))**(-1.006)
475      continue 
375   continue 
275   continue
         endif

      end! handling subroutine



