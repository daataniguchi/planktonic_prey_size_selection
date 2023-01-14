c ************* Encounter Kernel for Specific Grazers **************
c *** This subroutine calculates the encounter kernel 
c *** for each predator encountering each prey 

       subroutine EncounterKernelTypes (zmax, pnum,fnum,znumf,
     &     Dp, Dz, pr, zrmx, zvmx, pi,minind,v,alpha,
     &     functype,
     &     beta,encounterflag)

       implicit none
       integer zmax
       integer pnum
       integer fnum
       integer znumf(fnum)

       double precision pi
       real*8 Dp(pnum)
       real*8 Dz(zmax,fnum)
       real*8 pr(pnum)
       real*8 zrmx(zmax,fnum)
       real*8 zvmx(zmax,fnum)
       real*8 minind(fnum)
       real*8 v(zmax,fnum)
       real*8 alpha
       integer functype(fnum)
       real*8 beta(zmax,pnum,fnum)
       integer encounterflag

       real*8 betatemp
       real*8 betatempv
       integer ind1, ind2
       integer l,j,k

       do 700 l=1,fnum
             ind1 = minind(l)
       do 5000 j =1, znumf(l) 
              ind2 = ind1 +j - 1
          do 600 k = 1,pnum

        if (encounterflag .eq. 1) then 
           betatempv = pi*v(ind2,l)*(pr(k) + zrmx(ind2,l))**2.0
      endif
           betatemp = 4.0*pi*(Dp(k)+Dz(ind2,l) )*(pr(k) + zrmx(ind2,l))
            beta(ind2,k,l) = betatempv + betatemp
600       continue
5000      continue 
700     continue

      end ! EncounterKernelTypes subroutine




