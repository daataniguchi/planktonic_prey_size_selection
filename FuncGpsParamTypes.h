c ****** Parameters for grazer and prey groups and environmental parameters ********

      real*8 tmp
      real*8 etaW 
      real*8 etatmp 
      real*8 sal 
      real*8 kBoltz 
      real*8 Cdis 
      real*8 Ctot 

        real*8 psizewidth 
        real*8 pr(pnum) 
        real*8 psizemin 

        real*8 pv(pnum)
        real*8 pCcoeff
        real*8 pCexp
        real*8 pCsize(pnum)        
        real*8 pCcell(pnum) 
        real*8 pcount(pnum) 

      real*8 mu0 
      real*8 muexp 
      real*8 mutmpcoeff 
      real*8 muE 
      real*8 mu(pnum) 
      real*8 pm(pnum) 
      real*8 ks0 
      real*8 ksexp 
      real*8 ks(pnum) 
      real*8 u(pnum) 
      real*8 Dp(pnum) 

      integer functype(fnum)

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
 
      real*8 zCcoeff(fnum)
      real*8 zCexp(fnum)
      real*8 zCsize(zmax,fnum) 
      real*8 zcount(zmax,fnum) 
      real*8 zCcell(zmax,fnum) 

      real*8 zgamma 
      real*8 vmin(zmax,fnum) 
      real*8 vmax(zmax,fnum) 
      real*8 chi(fnum) 
      real*8 v(zmax, fnum)
      real*8 vsizewidth 
      real*8 bodylengths(fnum) 
      real*8 alpha 
      real*8 Dz(zmax,fnum) 
      real*8 gmax0 
      real*8 gtmpcoeff 
      real*8 gmaxexp 
      real*8 gE 
      real*8 gmax(zmax,pnum,fnum) 
      real*8 g(zmax,fnum) 
      real*8 zm0(fnum)     
      real*8 zm(zmax,fnum) 
      real*8 z0R(zmax,fnum) 
      real*8 R0(zmax,fnum)  
      real*8 RE 
      real*8 efficiency 
      real*8 Rv(zmax,fnum) 
      real*8 R(zmax,fnum)  
      integer prefpreyclass(fnum) 

      real*8 h(zmax,pnum,fnum) 
      real*8 beta(zmax,pnum,fnum)
      real*8 gpressure(zmax,pnum)
      real*8 gtimetemp(zmax,fnum)
      real*8 gtemp(zmax,pnum)
      real*8 gpressuremx(zmax,pnum,fnum)
      real*8 gcountmx(zmax,pnum,fnum)
      real*8 gprey(pnum) 
      real*8 ggrazer(zmax,fnum)
      real*8 ggrazerbio(zmax,pnum)
      real*8 Ctemp
      real*8 clearance(zmax,pnum,fnum)
      real*8 capture(zmax,pnum,fnum) 
      real*8 capexp(fnum) 
 
      real*8 pchange(pnum,tnum) 
      real*8 zchange(zmax,tnum,fnum) 
      real*8 Cdischange(tnum) 
      real*8 gtimechange(zmax,tnum,fnum) 
      real*8 gchange(zmax,tnum,fnum) 
       real*8 zchangetemp1(zmax,fnum) 
       real*8 zchangetemp2(zmax,fnum)
       real*8 pchangetemp1(pnum)
       real*8 pchangetemp2(pnum)
       real*8 Cdischangetemp

      integer reclen


