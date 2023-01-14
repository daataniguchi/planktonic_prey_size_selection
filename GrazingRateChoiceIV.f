c ************  Grazing **************

c *** Use this code to determine which size class of prey 
c *** is grazed by each predator 

      subroutine GrazingRateChoiceIV(pnum,zmax,fnum,
     &  minind,maxind,
     &  pCcell, zCcell, 
     &  pCsize,
     &  zCsize, 
     &  beta, h, gmax, clearance,
     &  capture, grazeflag,prefpreyclass,
     &  gtimetemp,gtemp,
     &   gprey,ggrazer)

      implicit none

      integer pnum
      integer zmax
      integer fnum
      real*8 minind(fnum)
      real*8 maxind(fnum)

      real*8 pCcell(pnum)
      real*8 zCcell(zmax,fnum)
      real*8 pCsize(pnum)
      real*8 zCsize(zmax,fnum)
      real*8 beta(zmax,pnum,fnum)
      real*8 h(zmax,pnum,fnum)
      real*8 gmax(zmax,pnum,fnum)
      real*8 clearance(zmax,pnum,fnum)
      real*8 capture(zmax,pnum,fnum)
      real*8 gpressure(zmax,pnum)
      integer prefpreyclass(fnum)
      real*8 gtimetemp(zmax,fnum)
      real*8 ggrazerbio(zmax,pnum)
      real*8 ggrazer(zmax,fnum) 
      real*8 gprey(pnum) 
      real*8 gtemp(zmax,pnum)
      real*8 tempval
      integer maxi(zmax)
      integer maxj(zmax)

      integer grazeflag
      integer grazeflag1
      integer grazeflag2
      grazeflag1 = grazeflag
      grazeflag2 = grazeflag

      integer ind1, ind2
      integer indi, indj
      integer i,j,jj,ii,ij,f

         do 220 f = 1,fnum
          ind1 = minind(f) 
          ind2 = maxind(f)

      do 900 i = 1,zmax
        do 1000 j = 1,pnum
            gpressure(i,j) = 0.0
            ggrazerbio(i,j) = 0.0
            gtemp(i,j) = 0.0
1000    continue
900   continue

      if (grazeflag1 .eq. 1) then 
      do 700 i=ind1,ind2
       do 800 j=1,pnum
         gtemp(i,j) = gmax(i,j,f)*(pCsize(j)/(pCsize(j)
     &      + pCcell(j)/beta(i,j,f)/h(i,j,f)/capture(i,j,f) ))
800    continue
700   continue
       end if
    
         tempval = gtemp(1,1) 
         maxi = 0
         maxj = 0

      do 1100 i = ind1,ind2
        do 1200 j = 1,pnum
          if (j .eq. 1) then
               tempval = gtemp(i,1)
               maxj(i) = j
         end if
            maxi(i) = i
          if ( gtemp(i,j) .gt. tempval )then
             maxj(i) = j
             tempval = gtemp(i,j)
          end if

1200    continue
1100    continue

      if (grazeflag2 .eq. 1) then 
      do 1300 i= ind1,ind2
               indi = maxi(i)
               indj = maxj(i)
      gpressure(indi, indj) = gtemp(indi, indj)
      gtimetemp(i,f) = indj 
1300    continue

      end if 

          do 350 i= ind1,ind2
              do 450 j=1,pnum
                  ggrazerbio(i,j) = gpressure(i,j)*zCsize(i,f)
450           continue
350       continue

          do 150 j=1,pnum
           do 155 jj = ind1,ind2
          gprey(j) = gprey(j) + ggrazerbio(jj,j)
155          continue
150       continue

          do 250 ii= ind1,ind2
            do 255 ij = 1,pnum
              ggrazer(ii,f) = ggrazer(ii,f) + ggrazerbio(ii,ij)
255          continue
250       continue
          grazeflag = grazeflag1

220     continue

       end ! grazing_rate subroutine     







