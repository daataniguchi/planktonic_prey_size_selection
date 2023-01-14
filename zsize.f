c ******** Grazer Sizes *************
c *** Use this subroutine to calculate the sizes (radius and volume) 
c *** of grazers 

      subroutine zsize(zmax,fnum,pi,sizemax,sizemin,zsizewidth,
     &   zrmx,znumf,zvmx,minind, maxind)

      implicit none
      integer zmax 
      integer fnum
      double precision pi
      real*8 sizemin(fnum)
      real*8 sizemax(fnum)
      real*8 zsizewidth 
      real*8 maxexp 
      integer maxexpint 
      real*8 zrall(zmax) 
      real*8 zrmx(zmax,fnum)
      real*8 minind(fnum) 
      real*8 maxind(fnum) 
      integer znum 
      integer znumf(fnum) 
      real*8 zvmx(zmax,fnum) 
      integer ind1, ind2 
      integer i, j, k 
 

      maxexp = maxval(sizemax)/minval(sizemin)
      maxexp = log(maxexp)/log(zsizewidth) 
      maxexpint = nint(maxexp)
     
      do 100 i = 1,maxexpint
      zrall(i) = minval(sizemin)*(zsizewidth**(i-1)) 
100   continue

       do 150 j = 1,fnum
       minind(j) = minloc(abs(zrall-sizemin(j)),1)
       maxind(j) = minloc(abs(zrall-sizemax(j)),1)
150    continue

       znumf = maxind - minind
       znumf = znumf +1
       zrmx = 0.d0
       do 300 k = 1,fnum
            ind1 = minind(k)
            ind2 = maxind(k)
            zrmx(ind1:ind2,k) = zrall(ind1:ind2)
            zvmx(:,k) =4.0/3.0*pi*(zrmx(:,k)**3) 
300    continue

      return
      end
