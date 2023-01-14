c ******* Functional Groups of Grazers *******

c **** This is the main code to examine change in prey and grazer biomass
c *** with time

      program FuncGpsDifGrazeParamIV


      implicit none
        double precision pi
        parameter (pi = 3.14159265359)
        integer tnum 
        parameter(tnum = 1e7)
        real*8 dt 
        integer tcount 
        integer savestep 

        integer pnum
        parameter(pnum=10)

      integer fnum 
      parameter(fnum = 1)
      integer zmax 
      parameter(zmax = 9)
      integer zfnum
      parameter(zfnum = zmax*fnum)
 
      integer pzfnum
      parameter(pzfnum = pnum*zmax*fnum)
      integer pznum
      parameter(pznum = pnum*zmax)
      integer zftnum
      parameter(zftnum = zmax*tnum*fnum)
      integer ptnum
      parameter(ptnum = pnum*tnum)
      integer writeflag 
      integer grazeflag 
      integer encounterflag 
      integer captureflag 
      integer mortalityflag 
      integer swimregflag(fnum)
      integer swimflag(fnum) 
      integer muflag 
      integer hflag 
      integer debugflag 
      integer choosezflag 
 
      integer t
      integer i
      integer kk
      integer ij
      integer ii
      integer pp
      integer ppz
      integer zz
      integer ff
      integer iiii
      integer ssi, ssii, mi,ma
      integer ind1, ind2,ind3
      integer indI, indII, indIII
      integer indo, indt
      integer indiv,indv
      integer q, st
      integer qq, qqq
      integer indq1, indq2
      
      include "FuncGpsParamTypes.h"

      writeflag = 1  
      grazeflag = 1 
      encounterflag = 5         
      captureflag = 2 
      mortalityflag = 1
      swimregflag(1) = 2
      swimflag(1) =1 
      muflag = 1
      hflag = 3 
      debugflag = 0
      choosezflag = 0

           dt = 100.0
           savestep = 10
           tcount = 1

      tmp = 15.0 
      sal = 0.00035 
      etaW = 4.2844*10**(-5.0)+(0.157*(tmp+64.993)**2.0-91.296)**(-1.0)
      etatmp = etaW* 
     & ( 1+ (1.541+(1.998*10**(-2.0))*tmp-(9.52*10**(-5.0))*tmp**2.0)* 
     &     (sal) + 
     &     ((7.974-7.561*10**(-2.0))*tmp+
     &         (4.724*10**(-4.0))*tmp**2.0)*(sal**2.0))*1000
      kBoltz = 8.617*10.0**(-5)

      Ctot = 1.5 

          functype(1) = 3

      sizemin(1) =  2.500e-6 ! 25.0e-6! 250.0e-6! min size for grazers; change based on grazer system
      sizemax(1) = 251.0e-6! approximate max size; change if only want one grazer size class

      zsizewidth = 10.0

      zCcoeff(1) = 0.3
      zCexp(1) = 0.1

      alpha = 0.0 !(legacy code)

      zgamma = 0.97
      zm0(1) = 0.05/24.0/3600.0   

      RE = 0.63
      efficiency = 0.01 

      gE = 0.67
      gmax0 = 33.96
      gmaxexp = -0.66
      gtmpcoeff = gmax0*exp(gE/(273.15+tmp)/kBoltz)

      capexp(1) = 3.0

      bodylengths(1) = 20.0
      chi (1) = 0.1 
      prefpreyclass(1) = 4 !legacy code

        data zchange/zftnum*0.0/
        data pchange/ptnum*0.0/
        data Cdischange/tnum*0.0/
        data gcountmx/pzfnum*0.0/
        data gtimechange/zftnum*0.0/
        data clearance/pzfnum*0.0/
        data beta/pzfnum*0.0/
        data v/zfnum*0.0/
        Cdischangetemp = 0.0

       psizewidth = 2.0
       psizemin = 0.5e-6

       call psize(pnum, psizewidth, psizemin,pi, pr, pv)
      
       pCcoeff = 0.1
       pCexp = 0.1
       call pCcount(pnum,pr,pv,pCcoeff,pCexp,pCsize,pCcell,pcount)
       pchange(:,1) = pCsize
       pchangetemp1 = pCsize 

      mu0 = 1.36/24.0/3600.0
      muexp = -0.16
      muE = 0.36 
      mutmpcoeff = mu0*exp(muE/(tmp+273.15)/kBoltz)

      ks0 = 0.33*106.0/16.0*12.0/1000.0
      ksexp=0.48

      call pparamchoice(pnum,pr,pv,mu0,muexp,muE,mutmpcoeff,
     &    kBoltz,tmp,pi, 
     &       ks0,ksexp,etatmp, mu, ks, Dp, pm,u,muflag)

      call zsize(zmax,fnum,pi,sizemax,sizemin,zsizewidth,
     &     zrmx,znumf,zvmx,minind,maxind)

      call  zCcountTypes(fnum,zmax,zrmx,zvmx,zCcoeff,zCexp,
     &     functype,
     &     zCcell,zCsize,zcount)

        do 220 kk = 1,fnum
           zchange(:,1,kk) = zCsize(:,kk)
           zchangetemp1(:,kk) = zCsize(:,kk)
220     continue

        call zparamtypes(fnum,zmax,znumf,zrmx,zvmx,pi,pnum,pr,
     &   minind, maxind,tmp,etatmp,
     &   gtmpcoeff, gmaxexp,gE,kBoltz,
     &   bodylengths,functype,
     &   efficiency,zm0,RE,zCcell,
     &   v,Dz,gmax,zm,R0,Rv,R,swimregflag)

        do 132 ssi=1,fnum
            mi = minind(ssi)
            ma = maxind(ssi)
          do 133 ssii = mi,ma
          
              vmax(ssii,ssi) = v(ssii,ssi)
              vmin(ssii,ssi) = chi(ssi)*v(ssii,ssi)
133       continue
132    continue
       
      Cdis = Ctot - sum(zCsize) - sum(pCsize)
      Cdischange(1) = Cdis
      Cdischangetemp = Cdis 
 
       call handlingIII (zmax, pnum,fnum,znumf,
     &   gmax, pCcell, zCcell,zrmx,pr,
     &    minind,maxind, hflag,
     &     h)

       call EncounterKernelTypes (zmax, pnum,fnum,znumf,
     &     Dp, Dz, pr, zrmx,zvmx, pi,minind,v,alpha,functype,
     &     beta,encounterflag)
 
      if (captureflag .eq. 2) 
       data capture/pzfnum*0.0/

      call CaptureEfficiency(zmax, pnum,fnum,
     &         minind,maxind, zrmx,pr,capexp, 
     &         capture)
        endif


        do 3003 t = 1,tnum-1

        do 101 pp = 1,pnum
         gprey(pp) = 0.0
          do 202 ppz = 1,zmax
           gpressure(ppz,pp) = 0.0
           ggrazerbio(ppz,pp) = 0.0
           gtemp(ppz,pp) = 0.0
202       continue
101     continue

        do 303 ff = 1,fnum
           do 404 zz = 1,zmax
             ggrazer(zz,ff) = 0.0
             gtimetemp(zz,ff) = 0.0       
404         continue
303     continue
       Ctemp = 0.0

        call  GrazingRateChoiceIV (pnum,zmax,fnum,
     &   minind,maxind,
     &   pCcell, zCcell, 
     &   pchangetemp1,
     &   zchangetemp1,
     &   beta,h,gmax,clearance,
     &   capture,grazeflag, prefpreyclass,  
     &   gtimetemp,gtemp,
     &   gprey,ggrazer)

          call pzchangeII(pnum,zmax,fnum,minind,maxind, 
     &     dt,Ctot,
     &     mu, Cdischangetemp, ks, pm, gprey,
     &     zm, R,
     &     zgamma,ggrazer,
     &     mortalityflag,
     &     pchangetemp1, pchangetemp2,
     &    zchangetemp1,
     &   zchangetemp2)


       do 2002 ii=1,fnum
          ind1 = minind(ii)
          ind2 =  maxind(ii)
       gtimechange(ind1:ind2,t,ii) = gtimetemp(ind1:ind2,ii)
       Ctemp = sum(zchangetemp2(ind1:ind2,ii)) + Ctemp
       zchangetemp1(ind1:ind2,ii) = zchangetemp2(ind1:ind2,ii)
2002    continue

        pchangetemp1 = pchangetemp2
        Cdischangetemp = Ctot - sum(pchangetemp2) - Ctemp
        Cdischangetemp = max(Cdischangetemp, 10.0**(-20.0))


                 if (mod(t, savestep) .eq. 0) then

         tcount = tcount + 1
         pchange(:,tcount) = pchangetemp2
          Cdischange(tcount) = Cdischangetemp

          do 4004 iiii = 1,fnum
            indo = minind(iiii)
            indt = maxind(iiii)
           zchange(indo:indt,tcount,iiii) = 
     &  zchangetemp2(indo:indt,iiii)
4004     continue

                   end if 
3003     continue 


        if (debugflag .eq. 1) then

        print*,'pchange(:,1)',pchange(:,1)
        print*,'pchange(:,tcount)',pchange(:,tcount)

        print*,'zchange(:,1,1)',zchange(:,1,1)
        print*,'zchange(:,tcount,1)',zchange(:,tcount,1)

        print*,'Cdischange(1)',Cdischange(1)  
        print*,'Cdischange(tcount)',Cdischange(tcount)

          endif 


        if (writeflag .eq. 1) then

        reclen = pnum*(tcount)*8
        open(17,file='pchange.bin',status='replace',access='direct',
     &  recl = reclen)
        write(17,rec=1)pchange(:,1:tcount)
        close(17)

        reclen = zmax*fnum*tcount*8
        open(17,file='zchange.bin',status='replace',access='direct',
     &  recl = reclen)
        write(17,rec=1)zchange(:,1:tcount,:)
        close(17)

        reclen = tcount*8
        open(17,file='Cdischange.bin',status='replace',
     &  access='direct', recl = reclen)
        write(17,rec=1)Cdischange(1:tcount)
        close(17)

        reclen = 8
        open(17,file='Ctot.bin',status='replace',
     &  access='direct', recl = reclen)
        write(17,rec=1)Ctot
        close(17)

       reclen = zmax*pnum*fnum*8
        open(17,file='gcountmx.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)gcountmx
         close(17)

      reclen = zmax*tnum*fnum*8
        open(17,file='gtimechange.bin',status='replace',access='direct',
     & recl = reclen)
        write(17,rec=1)gtimechange
        close(17)

      reclen = 8
        open(17,file='dt.bin',status='replace',access='direct', 
     &  recl = reclen)
        write(17,rec=1)dt
        close(17)

        reclen = 8
        open(17,file='tnum.bin',status='replace',access='direct', 
     &  recl = reclen)
        write(17,rec=1)tnum
        close(17)

        reclen = 8
        open(17,file='tcount.bin',status='replace',access='direct', 
     &  recl = reclen)
        write(17,rec=1)tcount
        close(17)

        reclen = 8
        open(17,file='savestep.bin',status='replace',access='direct', 
     &  recl = reclen)
        write(17,rec=1)savestep
        close(17)

       reclen = 8
        open(17,file='pnum.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)pnum
         close(17)

       reclen = pnum*8
        open(17,file='pr.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)pr
         close(17)

       reclen = pnum*8
        open(17,file='pCcell.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)pCcell
         close(17)

       reclen = pnum*8
        open(17,file='mu.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)mu
         close(17)

       reclen = 8
        open(17,file='zmax.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)zmax
         close(17)

       reclen = fnum*8
        open(17,file='znumf.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)znumf
 
       reclen = fnum*8
        open(17,file='minind.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)minind
         close(17)

       reclen = fnum*8
        open(17,file='maxind.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)maxind
         close(17)

       reclen = 8
        open(17,file='fnum.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)fnum
         close(17)

       reclen = zmax*fnum*8
        open(17,file='zrmx.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)zrmx
         close(17)

       reclen = zfnum*8
        open(17,file='zCcell.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)zCcell(:,1)
         close(17)

      reclen = 8
        open(17,file='zsizewidth.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)zsizewidth
         close(17)

       reclen = zfnum*8  
        open(17,file='v.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)v
         close(17)

       reclen = 8
        open(17,file='grazeflag.bin',status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)grazeflag
         close(17)

       reclen = 8
        open(17,file='encounterflag.bin',
     &  status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)encounterflag
         close(17)

       reclen = 8
        open(17,file='captureflag.bin',
     &  status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)captureflag
         close(17)

       reclen = 8
        open(17,file='mortalityflag.bin',
     &  status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)mortalityflag
         close(17)

        reclen = 8
        open(17,file='muflag.bin',
     &  status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)muflag
         close(17)

       reclen = 8
        open(17,file='swimflag.bin',
     &  status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)swimflag
         close(17)

       reclen = 8
        open(17,file='swimregflag.bin',
     &  status='replace',access='direct',
     &  recl= reclen)
        write(17,rec=1)swimregflag
         close(17)

      endif

      end ! Main program











