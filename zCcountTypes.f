c **************** Calculating carbon and abundance related grazer variables *****

       subroutine zCcountTypes(fnum,zmax,zrmx,zvmx,zCcoeff,zCexp,
     &      functype,
     &     zCcell,zCsize,zcount)

      implicit none
      integer fnum 
      integer zmax 
      real*8 zvmx(zmax,fnum) 
      real*8 zrmx(zmax,fnum) 
      real*8 zCcoeff(fnum) 
      real*8 zCexp(fnum) 
      integer functype(fnum) 
      real*8 zCsize(zmax,fnum) 
      real*8 zCcell(zmax,fnum) 
      real*8 zcount(zmax,fnum) 
      integer l, j1,j2 

      do 350 l = 1,fnum
        if (functype(l) .eq. 3) then 
        do 450 j2 = 1,zmax
      zCcell(j2,l)=(0.216*((zvmx(j2,l)*10.0**18.0)**0.939))
     &          *(10.0**(-12.0))
      zCsize(j2,l) = zCcoeff(l)*(zrmx(j2,l)**zCexp(l)) 
450     continue
        endif 

350   continue

      return
      end
 



