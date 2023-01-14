c ******** Grazer physiological and associated parameters ************

        subroutine zparamtypes(fnum,zmax,znumf,zrmx,zvmx,pi,pnum,pr,
     &   minind, maxind,tmp,etatmp,
     &   gtmpcoeff, gmaxexp,gE,kBoltz,
     &   bodylengths,functype,
     &   efficiency,zm0,RE,zCcell,
     &   v,Dz,gmax,zm,R0, Rv,R,swimregflag)

       implicit none
       integer fnum 
       integer zmax 
       integer znumf(fnum) 
       real*8 zrmx(zmax,fnum)
       real*8 zvmx(zmax,fnum)
       double precision pi
       integer pnum 
       real*8 pr(pnum)
       real*8 minind(fnum) 
       real*8 maxind(fnum) 
       real*8 v(zmax,fnum)
       real*8 Dz(zmax,fnum) 
       real*8 gmax(zmax,pnum,fnum) 
       real*8 gE 
       real*8 gmax0 
       real*8 gtmpcoeff 
       real*8 gmaxexp 
       real*8 kBoltz 
       real*8 zm0(fnum)      
       real*8 zm(zmax,fnum) 
       real*8 z0R(zmax,fnum) 
       real*8 R0(zmax,fnum) 
       real*8 efficiency 
       real*8 Rv(zmax,fnum) 
       real*8 RE 
       real*8 zCcell(zmax,fnum) 
       real*8 tmp 
       real*8 etatmp 
       real*8 R(zmax,fnum)  
       real*8 bodylengths(fnum) 
       integer swimregflag(fnum) 
       integer functype(fnum) 

       integer q,st 
       integer ind1, ind2 
       integer di, ci, gi, gp_i, dp_i, cp_i 

      do 500 q = 1,fnum 
         ind1 = minind(q)
         ind2 = maxind(q) 

        if (functype(q). eq. 3) then 
           do 503 gi = ind1,ind2
            do 504 gp_i = 1,pnum

          gmax(gi,gp_i,q) = 0.0035*((zrmx(gi,q)/pr(gp_i))** (-1.9431))
504       continue

          v(gi,q) = zrmx(gi,q)*2.0*bodylengths(q)
503       continue 
        endif 

        do 700 st = ind1,ind2 
        Dz(st,q) = ((tmp+273.15)*(1.38e-20))/(6*pi*etatmp*zrmx(st,q))
      zm(st,q) = zm0(q) + zm0(q)*v(st,q)
      z0R(st,q)=10.0**(0.75*log(zvmx(st,q)*10.0**18.0)/log(10.0)-4.09)
      R0(st,q) = 0.0 
      Rv(st,q) = ((3.0*pi*zrmx(st,q)*2.0*(v(st,q)**2.0))*etatmp/1000.0
     &        *(10.0**(-3.0))/20.2/(tmp+273.15) 
     &        / 0.082*12.0/zCcell(st,q)/efficiency)
       R(st,q) = R0(st,q) + Rv(st,q)

700    continue 
500    continue 

       end
