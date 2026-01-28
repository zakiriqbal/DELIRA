!                     ___________________________
!                    |                           |
!                    | Code: DELIRA              |
!                    | Version: 2021.03          |
!                    | File: fd1_main_i04.f      |
!                    | Last modified: 2021-03-28 |
!======================================================================
!********************   SUBROUTINES in this file   ********************
!======================================================================
!     program DELIRA
!     subroutine MKFLDR(pathww)
!     subroutine RDINPT
!     subroutine CHECKINPT
!     subroutine MEMALLO
!     subroutine RWDUMP(job)
!     subroutine MATDB
!     subroutine JOBINIT :: contains: function FZER0PR(rr)
!     subroutine WHEADER(lww)
!.........................................................beg MAINLOOP:
!     subroutine UPDEOS
!     subroutine PFKIN
!     subroutine DRIVE
!     subroutine KINBUR
!     subroutine PRINHIS
!     subroutine PRINOUT(prinww,lww)
!     subroutine PRIPR_C
!     subroutine PRIPR_V
!     subroutine WMEM_VTK(ifwfile)
!     subroutine TSTEP
!     subroutine EBALNC
!     subroutine UPSLOI
!.........................................................end MAINLOOP.
!     subroutine TIMING(lww)
!     subroutine RELCON(ishell,vjj,Tejj,Tijj,Trjj,Hzjj,yijj,YIONOU,
!    &         CAPEOU,CAPIOU,HIEIOU,ETHZOU,CAPROU,HIEROU,ETV0OU,ETV1OU)
!     subroutine BNDVAL(tyme)
!     subroutine BESTO2(e1g,dg,Teg,Tig,yig,imat,BESTOP)
!     subroutine EOSDRV(imat,vj,Tej,Tij,xipfij,hipfij,YOU,PEOU,PIOU,
!    &                  EEOU,EEVOU,EETOU,EIOU,EIVOU,EITOU,US2OU)
!     subroutine URSAPB(nsub,d,Te,Ti,job,FAPP,FLAPP,DLFAR,DLFAT,
!    &                  PIAP,DPILR,DPILT,EIAP,DEILR,DEILT)
!     subroutine FEREOS(ya,va,Tea,Tia,aferfit,PEFOU,EEFOU,DPELTF,
!    &    DEELTF,DPELVF,DEELVF,PIFOU,EIFOU,DPILTF,DEILTF,DPILVF,DEILVF)
!     subroutine TFEREOS(ya,va,pa,aferfit,TFOU)
!     subroutine GZERA(F,xl,xr,eps,nfcallmax,imethd,XOUT,FOUT,NFCALLS)
!     subroutine DM0PRO(r0,r1,I,ngauss,DM0PR) :: contains:
!           function RO0PR(rrab,I)
!           function FDM0PR(rr)

!**********************************************************************
!                  I N S T R U C T I O N S
!**********************************************************************
! >>> Input files: <<<
!  Minimum configuration:
!  The minimum configuration must include
!     (i)  two Fortran-90 code files 'fd0_comod.f', 'fd1_main.f', and
!     (ii) one input file 'input/deinput.ii'.

!  If the "DEITA3" version of tabular equation of state (EOS) is used,
!  or IIFOPAC=3 option is chosen for opacity, file 'input/eosta3.ii'
!  with the EOS and opacity tables must be prepared by the TABIN3
!  package.
!  If the ion drive is used, file 'input/bemta3.ii' with the
!  stopping-power data must be prepared by the TABIN3 package.

! >>> To start a new run: <<<
!  1. Assign an appropriate NNZ value (NNZ is the upper bound on the
!     possible number NZ of target layers) in module COMDEI,
!     file 'fd0_comod.f'.
!  2. Prepare the input file 'input/deinput.ii'.
!  3. If necessary, edit the initial density profile in function RO0PR.
!  4. Compile and run the code.

! >>> To continue an old run (saved in file 'ddump'): <<<
!  1. Set ISTART=1 in the input file 'input/deinput.ii'.
!  2. Provide the necessary 'ddump' file in the main project folder
!     from where the job is started.
!  3. Run the code.

! >>> To add a new EOS model: <<<
!  1. Insert and program a new CASE in subroutine EOSDRV, and then
!     assign the new CASE (EOS-model) number to propmat(1,imat).

! >>> To modify target drive: <<<
!  1. Write a new version of subroutine DRIVE.

! >>> To modify transport coefficients: <<<
!  1. Edit (or write a new version of) subroutine RELCON.

! >>> Boundary conditions: <<<
!  Following types of hydrodynamic boundary conditions are envisaged:
!  1) right boundary, r=R(N+1): prescribed boundary pressure PBR(t),
!     external rad. temperature TREX(t) and magnetic field HZBR(t);
!  2) left boundary, r=R(1):
!     IFLBND=0  -> a fixed center of symmetry, R(1)=U(1)=0;
!                  all diffusion fluxes are zero;
!     IFLBND=1  -> a closed void cavity at 0 < r < R(1) with
!                  a prescribed boundary pressure PBL(t);
!                  all diffusion fluxes are zero;
!     IFLBND=-1 -> an open halfspace r < R(1)[allowed for IGEO=0 only]:
!                  prescribed boundary pressure PBL(t), external rad.
!                  temperature TRLEX(t) and magnetic field HZBL(t);
!                  in this case the artificial t-viscosity is turned
!                  off; to compensate -> increase the value of SMU2.
!  The time dependence of the boundary values of the relevant physical
!  quantities must be programmed in the subroutine BNDVAL(t).

! >>> To set up times for printouts: <<<
!  Assign the desired printout times (including the calls of PRIPR_C)
!  as a sequence of values of TPRINII(1:100) in the input file
!  'deinput.ii'; this sequence is allowed to contain any number
!  (between 1 and 100) of user-defined time moments.
!  If TPRINII(i+1) =< TPRINII(i) is encountered, then, starting from
!  t=TPRINII(i), all printouts are inacted with a constant time-step
!  dtprin=TPRINII(i+1).

! >>> To set up printout of history plots: <<<
!  Edit the subroutine PRINHIS.

!**********************************************************************
!                             OPTIONS
!**********************************************************************
!                       >>> GEOMETRY <<<
! IGEO = 0 -> plane-parallel target geometry,
!      = 1 -> cylindrical target geometry,
!      = 2 -> spherical target geometry; def=iundef.


!                   >>> BOUNDARY CONDITIONS <<<
! IFLBND = 0 -> center of symmetry at the left boudary: R(1)=U(1)=0;
!        = 1 -> a closed void cavity at 0 < r < R(1): P_bl=P_bl(t);
!        =-1 -> open halfspace at r < R(1): P_bl=P_bl(t);
!               allowed with IGEO=0 only -> artificial t-viscosity off!
!               def=0.

! HEBR   = external heat flux for electron heat conduction; positive
!          in the outward direction; def=0.d0; added on 12 Mar 2008.

!                   >>> TEMPERATURE OPTIONS <<<
! IITEMP = 1 -> 1T model: T_i=T_e, T_r=0 =>
!                         e-i T-relaxion rate \chi_ei=infinity,
!                         e-rad T-relaxion rate \chi_er=0,
!                         no rad.diffusion \kappa_r=0;
!        = 2 -> 2T model: T_i.neq.T_e, T_r=0 =>
!                         e-i T-relaxion rate \chi_ei=finite,
!                         e-rad T-relaxion rate \chi_er=0,
!                         no rad.diffusion \kappa_r=0;
!        = 3 -> 2T model with eq.rad.: T_i.neq.T_e, T_r=T_e =>
!                         e-i T-relaxion rate \chi_ei=finite,
!                         e-rad T-relaxion rate \chi_er=infinity,
!                         no rad.diffusion \kappa_r=0;
!        = 4 -> full 3T model: T_i.neq.T_e, T_r.neq.T_e =>
!                         e-i T-relaxion rate \chi_ei=finite,
!                         e-rad T-relaxion rate \chi_er=finite,
!                         rad.diffusion \kappa_r=finite; def=4.

!                   >>> LASER DEPOSITION <<<
! IIFLAS = 0 -> no laser deposition;
!        = 1 -> laser deposition; def=0.

!                   >>> PHYSICAL VISCOSITY <<<
! IIFVIS = 0 -> no physical viscosity;
!        = 1 -> full physical viscosity (scalar + tensor); def=1.

!                   >>> THERMAL CONDUCTION <<<
! IIFCAPEI = 0 -> zero electron and ion thermal conduction coefficients
!                 CAPE=CAPI=0;
!          = 1 -> non-zero electron thermal conduction, CAPE > 0, and
!                 zero ion thermal conduction, CAPI=0;
!          = 2 -> zero electron thermal conduction, CAPE=0, and
!                 non-zero ion thermal conduction, CAPI > 0;
!          = 3 -> both electron and ion thermal conduction are
!                 accounted for, CAPE > 0, CAPI > 0; def=3.

!                   >>> MAGNETIC FIELD <<<
! IIFHZ = 0 -> no equation for the magnetic field is solved;
!       = 1 -> frozen-in magnetic field Hz (i.e. zero resistivity in
!              diffusion eq-n for Hz); meaningful for IGEO=0,1 only.
!       = 2 -> a diffusion equation for z-component of the magnetic
!              field Hz is solved with finite resistivity; meaningful
!              for IGEO=0,1 only; def=0.

!      >>> FUSION BURN: user-control via logical flag IFBURN <<<
! IFBURN = .false. -> no burn initiated in this job; => JFLGBURN=-1;
!        = .true.  -> starts with no burn (JFLGBURN=0) and checks for
!           burn initiation in the process of target evolution; def=0;

! - internal integer flag for burn control:
! JFLGBURN  =-1 -> burn bypassed, no check for ignition;
!           = 0 -> burn bypassed, check for ignition;
!           = 1 -> intermediate value when turning the burn on;
!           = 2 -> burn calculated, no check for being stopped.

!                 >>> CHARGED FUSION PRODUCTS <<<
! IIFAL(IIFP3,IIFP14) = 0 -> ignored;
!                     = 1 -> diffusion;
!                     = 2 (or any other) -> local; def=0;
! KALPRI = 1 -> print EAL, HIAL;
!        = 2 -> print EP3, HIP3;
!        = 3 -> print EP14, HIP14; def=1.

!         >>> NEUTRON HEATING of the central fuel "sphere": <<<
! IIFN14(IIFN2) = 0 -> ignored;
!               = 1 -> 1-st scattering;
!               = 2 -> local;
!               = 3 -> spread uniformly;
! IIFN14        = 4 -> sphere, spread according to diffusion profile;
! IIFN2         = 4 -> reserved for the future;
! IIFN14(IIFN2) > 4 -> not allowed; def=0.

!         >>> NEUTRON HEATING of possible outer fuel layers: <<<
! IIFN14(IIFN2) = 0 -> ignored;
!              >= 1 -> local;

! Neutron heating of non-fuel layers is not accounted for.

!                       >>> OPACITY <<<
! IIFOPAC < 2 -> default (zero) values of CAPROU and HIEROU, implying
!               no coupling to the radiation filed;
!         = 2 -> calculate CAPROU and HIEROU from fast formulas of the
!               DEIRA-2 model;
!         = 3 -> calculate CAPROU and HIEROU by integrating absorption
!                the cross-section (DEIRA-3 model); requires more
!                atomic data (from file 'input/eosta3.ii') than the
!                DEIRA-2 model;
!         > 3 -> default values of CAPROU and HIEROU; def=2.

!**********************************************************************
!                       COMMENTS
!**********************************************************************
! The code DELIRA is based on the DEIRA version 4.2 dated 24 Nov 06,
! with the last modification of 12 Mar 2008.
! What is new in version DEIRA-4.2 compared to version DEIRA-4:
!     1) nuclear scattering has been accounted for in the transport-
!        relaxation of the energy of 14-MeV protons (Nov 03);
!     2) a new option (IIFN14=4) for 14-MeV neutron heating has been
!        added (Nov 06): the neutron energy is spread according to the
!        diffusion profile in spherical geometry.

!  Any block between the markers      -----------beg #
!  and     ===========end #  can be harmlessly deleted.

! IMPORTANT:
! 1. Never change the order of CALL statements in the HARDCORE block.
! 2. If any control parameter from the NAMELIST/INPUT/ is saved for
!    continuation into the 'ddump' file (like IITEMP, for example),
!    its value cannot be changed by reassigning it in the
!    'input/deinput.ii' file and restarting the job.

!**********************************************************************

!**********************************************************************
!                       DELIRA
!************************************************************beg DELIRA
      program DELIRA
      use COMDEI, only: czdriv,czdt,dfloor,dfuzz,dt,dtcfl,dtprin,dtq,
     &    ee,eet,eev,ei,eit,eiv,floor,ifburn,ifupeos,igeo,iifHz,
     &    iifopac,ifpfkin,istart,itimpri,itimprmax,j_CFL,jrorp,k_CFL,
     &    linpt,lpro,lrun,lvirtl,ncyc,ncycdpri,ncycfin,ncycpri,
     &    ncycprii,nddump,ndrlin,ndump,nfu,nmesh,nn,nt_vtk,nprin,nrun,
     &    nz,pathoutpt,pe,pi,qe,qi,qjl,qsDT,qsDD,qsDHe,rorfu,rorp,
     &    secstart,tdrcal,tdrfin,Teincd,Teincr,tfin,Ti,Tiincr,time,
     &    tprin,tprinii,Trincr,Tvaccd,u,us,vincd,vincr,rorfum,
     &    w1w01,w1w02,w1w03,w1w04,w1w05,w1w06,w1w07,w1w08,w1w09,w1w10,
     &    w1w11,w1w12,w1w13,w1w14,w1w15,
     &    R1B,R2B,P1OWER,TP,te,ucbet,peden,qdriv,xB0,
     &    rz0,roz0,xT0,xD0,imatz0,ifmfu0,njz,r,te0,ti0,pinum,v,hz,ethz,
     &    clogaree,clogarei,tnun2,tnun14,ezfus,hz0,ezdr,ez0,erin
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     Calls: MKFLDR,RDINPT,CHECKINPT,MEMALLO,RWDUMP,PRINOUT,JOBINIT,
!            UPDEOS,DRIVE,KINBUR,TSTEP,UPSLOI,EBALNC,PFKIN,SECSCPU,
!            TIMING
!======================================================================
! Local variables:
      integer(4) :: i,iww,j,k,ji,jf
      real(8) :: dtww,rab,rab0,rabn,p_D,rabtd,v_0,v_D,v00,ww0,ww1,wwA,
     &      wwgam,wwt0
      logical :: ifdump,iffin,ifprin,ifww,lexist,lopen
!     pusher radius: inner R location ->r2pn, outter r location ->r1pn
      integer :: r2pn,r1pn,layer=1
      real(8) :: rmp,rmf,rmi,peclet,xe,temp,devide
      real(8) :: tempte=0,tempti=0,tempCr,nn2total=0.0d0,rhoRgas=0.0d0
     &,tempMass=0.0d0,tempEP2=0.0d0,tempB=0.0d0
      logical :: iffind,iffirst,resRecord
      integer :: varhz,varrho,Ti10=1,Ti5=1
      real(4) :: sigma=0.4247 !FHWM=1mm 对应的高斯分布的\sigma
      real(8) :: Gain=0.0d0,timeL=0.0d0,tamperN2=0.0d0,tamperN14=0.0d0
      real(8) :: Pressure_e(100),Pressure_i(100),Pressure_den(500),
     & inEnergyI(400),inEnergyE(400)
      real(8) :: MassTiTemp=0.0d0,BRgas=0.0d0,mBgas=0.0d0
     &,rhoRfuel=0.0d0,temprhoR(3)=(/0.0d0,0.0d0,0.0d0/)
      real(8) :: BangTime(9)=0.0d0,StagTime(9)=0.0d0,BmaxTime(9)=0.0d0
     &        ,EP2Time(9)=0.0d0,nn2Temp(3)=0.0d0,nn2index=0.0d0
!======================================================================

!----------------------------------------------------------------------
!     Step 1: Initialize, open output files, read input.
!----------------------------------------------------------------------
! Initialize timing variables:
      iffind=.false.
      iffirst=.true.
      secstart=SECSCPU()

! Create output folder:
      call MKFLDR(pathoutpt)

! Read problem input:
      call RDINPT
!     target setting

      p1ower=15.d0/(TP*10d0)/1.0d3

!     create saving file:---------------记录中子份额数据
      inquire(file='MatterStateWshell.dat',EXIST=resRecord)
      if(resRecord) then
!     file exist
      continue
      else
!     file not exist
      open(2,file='MatterStateWshell.dat',status='new')
      close(2)
      continue
      endif

! Check input data:

      call CHECKINPT

! Allocate memory, read from the dump file 'ddump':
      if(istart.eq.0) then
        nn=0
        do i=1,nz
          nn=nn+nmesh(i)
        enddo
        call MEMALLO
      else
        call RWDUMP('r')
      endif

! Write header, DEIRA units and explanations:
      call WHEADER(6)
      call WHEADER(lrun)
      call WHEADER(lpro)
      call PRINOUT('prinun',lpro)

! Load tables with material properties:
      call MATDB

      pe => w1w01
      pi => w1w02
      eet => w1w03
      eev => w1w04
      eit => w1w05
      eiv => w1w06
      us => w1w07
      ee => w1w08
      ei => w1w09
      qe => w1w10
      qi => w1w11

      if(iifHz.ge.1) qjl => w1w12
      if(ifburn) then
        qsDT => w1w13
        qsDD => w1w14
        qsDHe => w1w15
      endif
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: w1w01-w1w15;
! vacant:   none;

! Job initialization (construct the mesh, initialize variables):
      call JOBINIT
!......................................................................
! Here non-standard initial values of variables can be assigned       .
! Also, certain values taken from 'ddump' can be overwritten!         .
!......................................................................

!----------------------------------------------------------------------
!     Step 2: Loop over hydrocycles.
!----------------------------------------------------------------------
! Set initial values of the print, terminate, and save flags:

      ifprin=.true.
      iffin=.false.
      ifdump=.false.

! Print the initial target state:
      call PRINOUT('prinst',lpro)
      nullify(pe,pi,eet,eev,eit,eiv,us,ee,ei,qe,qi)
      if(iifHz.ge.1) nullify(qjl)
      if(ifburn) nullify(qsDT,qsDD,qsDHe)
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: none;
! vacant:   w1w01-w1w15;
!      open(1,file='B0T15KJCompressionNp150ns.dat'
!    &,status='new')
!::::::::::::::::::::::::::::::::::::::::::::::::::::::: begin MAINLOOP

 2000 continue
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     Step 2.1: Compute EOS, rates and transport coefficients.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Update EOS:
      pe => w1w01
      pi => w1w02
      eet => w1w03
      eev => w1w04
      eit => w1w05
      eiv => w1w06
      us => w1w07
      ee => w1w08
      ei => w1w09
      ifupeos(:)=.true.
      call UPDEOS
!========================================================
!========================================================

      do j=1,njz(nz+1)-1  !   # NZ 层的时候
        Pressure_e(j)=Pe(j)
        Pressure_i(j)=Pi(j)
      enddo
!========================================================

! Phase-flip kinetics:
      if(ifpfkin) then
        call PFKIN
        call UPDEOS
      endif
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: w1w01-w1w09;
! vacant:   w1w10-w1w15;

! External energy deposition:
      Tvaccd=Tvaccd+vincd+Teincd
!     if(Tvaccd.ge.czdriv) call DRIVE
!      czdriv=0.03d0
!      加热方向改成轴向加热
!     if(Tvaccd.ge.czdriv) call DRIVE_AX(R1B,R2B,P1OWER)
!     call DRIVE
      call DRIVE_Gauss(2.8d0,P1OWER)
!     DRIVE_AX(R1B,R2B,P1OWER)
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: w1w01-w1w09;
! vacant:   w1w10-w1w15;

! Burn rates, transport and kinetic coefficients:

      qe => w1w10
      qi => w1w11
      if(iifHz.ge.1) qjl => w1w12
      if(ifburn) then
        qsDT => w1w13
        qsDD => w1w14
        qsDHe => w1w15
      endif
      call KINBUR

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: w1w01-w1w15;
! vacant:   none;

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     Step 2.2: Conditions for job termination, dump, print-out.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@for_user_to_edit:
! CONDITIONS for job termination:
! - universal termination conditions:
!     if(ncyc.ge.ncycfin .or. time.ge.tfin) then
      if(time > tfin) then
        print*,time,tfin,ncyc,ncycfin
        iffin=.true.
      else
! - eventual task-dependent job continuation conditions:
        continue
!@@        iffin=.true.
!@@        if(Ti(1).gt.1.d0) iffin=.false.
!@@        if(nfu.ge.1) then
!@@          if(u(nfu).lt.1.d-3) iffin=.false.
!@@        endif
!@@        if(rorfu.gt.5.d0) iffin=.false.
      endif

      if(iffin) then
        ifdump=.true.
        ifprin=.true.
      endif

! CONDITIONS for dump:
      if(nddump*(ncyc/nddump).eq.ncyc .and. ncyc.ne.0)
     &      ifdump=.true.

! CONDITIONS for print-out:
      if(ncyc.eq.ncycpri) then
        ifprin=.true.
        if(ncyc.lt.ncycprii(1)) then
          ncycpri=ncycprii(1)
        else
          do i=1,99
            if(ncyc.ge.ncycprii(i)) then
              if(ncyc.lt.ncycprii(i+1)) then
                ncycpri=ncycprii(i+1)
                exit

                elseif(ncycprii(i).ge.ncycprii(i+1)) then
                ncycdpri=max(1,ncycprii(i+1))
                exit
              endif
            endif
          enddo
        endif
        if(ncyc.eq.ncycpri) ncycpri=ncyc+ncycdpri
      endif

      if(time.ge.tprin) then
        ifprin=.true.
        itimpri=min(itimpri+1,itimprmax+1)
        if(itimpri.le.itimprmax) then
          tprin=tprinii(itimpri)
        else
          tprin=tprin+dtprin
        endif
      endif

! CONDITION of extremum of the fuel <RO*R>:
!   RORFU=<RO*R> of the central fuel sphere, calculated in 'KINBUR';
!@@      if(rorfu.ge.1.01d0*rorp) goto 2240
!@@      if(rorfu.gt..99d0*rorp) goto 2280
!@@      rorp=rorfu
!@@      if(jrorp.eq.-1) goto 2280
! - local maximum of RORFU:
!@@      jrorp=-1
!@@      ifprin=.true.
!@@      goto 2280
!@@ 2240 rorp=rorfu
!@@      if(jrorp.eq.1) goto 2280
! - local minimum of RORFU:
!@@      jrorp=1
!@@ 2280 continue
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@end_user-edit.

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     Step 2.3: Do print-out and/or dump.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call PRINHIS

      if(nt_vtk.gt.0) then
        ifww=iffin.or.ifdump
        call WMEM_VTK(ifww)
      endif

      if(ndrlin*(ncyc/ndrlin).eq.ncyc) then
! - write the "running line":
        write(*,9019) ncyc,time,dt,dtcfl,j_CFL,k_CFL,dtq
        write(lrun,9020) ncyc,time,dt,dtcfl,j_CFL,k_CFL,dtq,
     &                   vincr,Teincr,Tiincr,Trincr
      endif

      if(ifprin) then
        call PRINOUT('prinoij',lpro)
        call PRIPR_C
        call PRIPR_V
        ifprin=.false.
      endif

      if(ifdump) then
        call RWDUMP('w')
        ndump=ndump+1
        write(*,9045) ndump,ncyc,time
        write(lrun,9045) ndump,ncyc,time
        ifdump=.false.
      endif

      if(iffin) goto 3000
      nullify(ee,ei)                ! w1w08,w1w09 are vacant!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: w1w01-w1w07, w1w10-w1w15;
! vacant:   w1w08,w1w09;

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     Step 2.4: Compute dt and advance to a new time layer (t-sloi).
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! The new time step DT is calculated here:
      call TSTEP
      if(iifHz.ge.1) nullify(qjl)         ! w1w12 is freed!

      dtww=dt
! - ensure call DRIVE in the next hydrocycle at TIME=TDRFIN:
      if(time+dt.ge.tdrfin .and. time.lt.tdrfin) then
        Tvaccd=1.d0+1.1d0*abs(czdriv)
        if(time+dt.gt.tdrfin) then
          dt=max(tdrfin-time,1.d-3*dtww)
        endif
      endif

! - ensure call DRIVE in the next hydrocycle at TIME=TDRCAL:
      if(time+dt.ge.tdrcal .and. time.lt.tdrcal) then
        Tvaccd=1.d0+1.1d0*abs(czdriv)
        if(time+dt.gt.tdrcal) then
          dt=max(tdrcal-time,1.d-3*dtww)
        endif
      endif

! - ensure printout when time=tprin:
      if(time+dt.gt.tprin .and. time.lt.tprin)
     &   dt=max(tprin-time,1.d-3*dtww)

! - ensure finish at exactly time=tfin:
      if(time+dt.gt.tfin) dt=tfin-time+time*dfuzz

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!WARNING:
!!! The time step DT must not be changed below this point !!!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: w1w01-w1w07, w1w10-w1w15;
! vacant:   w1w08,w1w09;

! Add-up increments to various energy components:
      call EBALNC
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: w1w01-w1w07, w1w10-w1w15;
! vacant:   w1w08,w1w09;
! Update principal variables to the new time layer:
      call UPSLOI

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: ??????????????;
! vacant:   ??????????????;

      ncyc=ncyc+1
      time=time+dt

      if (time*10.0d0 >= timeL) then
       open(1,file='MatterStateWshell.dat',status='old'
     &,position='append')
        ji=1
        jf=njz(nz+1)-1
       do j=ji,jf
        write(1,*) time*10d0,r(j),1/v(j),Ti(j),Te(j),Pressure_e(j),
     & Pressure_i(j)
       enddo
      close(1)

       timeL=timeL+0.1d0
       else
        continue
      endif



      goto 2000
!::::::::::::::::::::::::::::::::::::::::::::::::::::::::: end MAINLOOP

!----------------------------------------------------------------------
!     Step 3: Finalize the job.
!----------------------------------------------------------------------
 3000 continue

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-arrays:
! occupied: w1w01-w1w15;
! vacant:   none;
!     聚变能量,加速器沉积能量，增益
!     open(2,file='B30T_rhoD_25E-1_rhoG_1E-4_burnFraction.dat',
!    &status='old',position='append')
!      write(2,*) TE0(1),devide,rorfu,rorfum,tempte,tempCr,temp
!     close(2)
!!     在磁场为30T的初始强度下，记录一个脉冲结束后的中子通量
!     open(2,file='B30T_rhoD_25E-1_rhoG_1E-4_nn.dat',
!    &status='old',position='append')
!      write(2,*) TE0(1),devide,tnun2(1),tnun2(2),tnun14(1),tnun14(2)
!     close(2)
!!     聚变能量,加速器沉积能量，增益
!     open(2,file='B30T_rhoD_25E-1_rhoG_1E-4_energy.dat',
!     &status='old',position='append')
!      temp=ezdr(1)+ezdr(2)+ezdr(3)
!      Gain=(ezfus(1)+ezfus(2))/temp
!      write(2,*) TE0(1),devide,ezfus(1),ezfus(2),temp,Gain
!     close(2)


!      open(2,file='Case1_7_5kj_100T.dat',
!    &status='old',position='append')
!     do j=1,njz(2)-1
!     write(2,*) r(j),1/v(j),te(j),Pe(j)
!     enddo
!     close(2)

!     open(2,file='BangTimeXi.dat',
!    & status='old',position='append')
!         write(2,*) devide,BangTime
!     close(2)
!!     open(2,file='StagTimeXi.dat',
!    & status='old',position='append')
!         write(2,*) devide,StagTime
!     close(2)
!!     open(2,file='BmaxTimeXi.dat',
!    & status='old',position='append')
!         write(2,*) devide,BmaxTime
!     close(2)
!!     open(2,file='EP2maxTimeXi.dat',
!    & status='old',position='append')
!         write(2,*) devide,EP2Time
!     close(2)


!     temp=ezdr(1)+ezdr(2)+ezdr(3)
!     print*,temp,ezfus(1),ezfus(2)
      
!     tamperN2=tnun2(1)+tnun2(2)
!     tamperN14=tnun14(1)+tnun14(2)
!!     open(2,file='RationIntensityNeutron.dat',
!    & status='old',position='append')
!!      write(2,*) P1OWER,tempte,tamperN2,tnun2(2),tamperN14
!!     close(2)


      call PRINOUT('prinst',lpro)
      call PRINOUT('prinoo',lpro)
      call TIMING(lpro)
      call TIMING(lrun)
      call TIMING(0)
      write(lpro,9010) nrun
      write(*,9085) nrun
      write(lrun,9085) nrun

 9000 close(lrun)
      close(lpro)

 9002 format(79('='))
 9010 format(/48('*'),' DELIRA: end of run',I11/)
 9019 format('ncyc=',I8,' t=',es13.7,' dt=',es7.1,' CFL=',es7.1,
     &' /',I6,'(',I1,')',' dtq=',es7.1)
 9020 format('ncyc=',I8,' t=',es13.7,' dt=',es7.1,' CFL=',es7.1,
     &' /',I6,'(',I1,')',' dtq=',es7.1
     &/17x,'vincr=',es8.1,' Te,i,r-incr=',3es8.1)
 9045 format('## dump',I5,', ncyc=',I9,', time=',es14.8)
 9085 format(/10x,'* * *    End of DELIRA run NRUN=',I11,'    * * *')
      end program DELIRA
!____________________________________________________________end DELIRA


!**********************************************************************
!                            MKFLDR
!******************************************************* begin MKFLDR
      subroutine MKFLDR(pathww)
      use COMDEI, only: lvirtl
      implicit none
!======================================================================
!     This routine creates a folder 'TRIM(pathww)' - when it still does
!     exist - in the main project directory.

!     Called by: DELIRA
!     Calls    : none
!----------------------------------------------------------------------
! INPUT:
!     TRIM(pathww) is the name of the folder to be created.
!======================================================================
! Arguments:
      character(*), intent(in) :: pathww

! Local variables:
      character(512) :: chww
!======================================================================

! Check if the folder 'pathww' exists:
      open(lvirtl,file=TRIM(pathww)//'/xx.dat',err=20)
      close(lvirtl,status='delete')
      goto 9000

! Folder 'pathww' does not exist:
 20   chww='mkdir '//TRIM(pathww)
      call SYSTEM(TRIM(chww))
      open(lvirtl,file=TRIM(pathww)//'/xx.dat',err=30)
      close(lvirtl,status='delete')
      goto 9000

! Failed to create folder 'pathww':
 30   write(*,9001)
      write(*,9002) TRIM(pathww)
      write(*,9001)
      STOP 'STOP in MKFLDR: failed to create the "pathww" folder!'

 9000 return
 9001 format(79('='))
 9002 format(6x,'STOP in MKFLDR: failed to create the "',a,
     &'" folder')
      end subroutine MKFLDR
!_________________________________________________________ end MKFLDR


!**********************************************************************
!                             RDINPT
!**********************************************************begin RDINPT
      subroutine RDINPT
      use COMDEI, only: bro0pr,cz_pf,czddt,czdriv,czdt,czdtq,
     &    czdtvt,czTkin,czvkin,dfloor,dt0,dtmax,dtmin,
     &    eemin,eionb,flle,flli,fllr,floor,Hz0,Hzmin,iartvis,ifburn,
     &    iflbnd,ifmfu0,iifopac,igeo,iifal,iifcapei,iifHz,iiflas,
     &    iifn14,iifn2,iifp3,iifp14,iitemp,iifvis,imatadd,imatz0,
     &    istart,iundef,jobtit,kalpri,linpt,lpro,lrun,lvirtl,ncycfin,
     &    ncycprii,nddump,ndrlin,njpri,nmesh,nt_vtk,nrun,nz,nzeven,
     &    pathoutpt,propmat,qzeven,roz0,roz01,rz0,sig,sigt,smu1,smu2,
     &    tau_pf,Tburn0,tdrcal,tdrfin,Te0,tfin,Tfloor,Ti0,tprinii,
     &    tmu1,tmu2,Tr0,undef,uz0,wdriv,xB0,xbfloo,xHe0,xH0,xD0,xT0,
     &    R1B,R2B,TP
      implicit none
!======================================================================
!     This routine reads the input data into the NAMELIST /INPUT/ from
!     file '/input/deinput.ii' and loads certain secondary control
!     parameters.

!     Called by: DELIRA
!     Calls    : WHEADER
!======================================================================
! Local variables:
      integer(4) :: i
      real(8) :: ww0
      logical :: lexist
!======================================================================
      namelist/input/
!~~~~~~~~~~~~~~~~~~~~~~~~~ SAVED INTO DUMP FILES ~~~~~~~~~~~~~~~~~~~~~~
     &igeo,nrun,nz,nmesh,nt_vtk,nzeven,qzeven,iflbnd,ifburn,iifcapei,
     &iifHz,iiflas,iitemp,iifvis,ifmfu0,imatz0,rz0,roz0,roz01,bro0pr,
     &Te0,Ti0,Tr0,uz0,xD0,xT0,xHe0,xH0,xB0,Hz0,
!
!~~~~~~~~~~~~~~~~~~~~~ NOT SAVED INTO DUMP FILES ~~~~~~~~~~~~~~~~~~~~~~
     &istart,njpri,nddump,ncycfin,ndrlin,kalpri,ncycprii,iartvis,
     &iifopac,iifal,iifp3,iifp14,iifn14,iifn2,imatadd,cz_pf,tau_pf,
     &dt0,dtmin,dtmax,tfin,tprinii,czdt,czdtq,czdtvt,czddt,czdriv,
     &czTkin,czvkin,flle,flli,fllr,Tburn0,xbfloo,Tfloor,eemin,
     &Hzmin,eionb,wdriv,tdrfin,tdrcal,propmat,
     &smu1,smu2,tmu1,tmu2,sig,sigt,R1B,R2B,Tp

!----------------------------------------------------------------------
!     Step 1: Read-in and write-out NML /input/.
!----------------------------------------------------------------------
! Check if the file 'input/deinput.ii' exists:
      inquire(file='input/deinput.ii',exist=lexist)
      if(.not.lexist) then
        open(lrun,file=TRIM(pathoutpt)//'/run.dat',form='formatted')
        write(lrun,9002)
        write(lrun,*) 'STOP in RDINPT: file "deinput.ii" not found!'
        write(lrun,9002)
        close(lrun)
        STOP 'STOP in RDINPT: file "deinput.ii" not found!'
      endif

! Read into namelist /input/:
      open(linpt,file='input/deinput.ii',form='formatted',status='old',
     &     action='read')
      read(linpt,'(a)') jobtit
      read(linpt,NML=input)
      close(linpt)

! Write out namelist /input/:
      if(istart.eq.0) then
        open(lvirtl,file=TRIM(pathoutpt)//'/check-NMLinput.dat',form=
     &      'formatted')
      else
        open(lvirtl,file=TRIM(pathoutpt)//'/check-NMLinput.dat',form=
     &      'formatted',position='append')
      endif

      call WHEADER(lvirtl)
      write(lvirtl,*)
      write(lvirtl,NML=input)
      close(lvirtl)

!----------------------------------------------------------------------
!     Step 2: Open principal output files.
!----------------------------------------------------------------------
      if(istart.eq.0) then
        open(lrun,file=TRIM(pathoutpt)//'/run.dat',form='formatted')
        open(lpro,file=TRIM(pathoutpt)//'/pro.dat',form='formatted')
      else
        open(lrun,file=TRIM(pathoutpt)//'/run.dat',form='formatted',
     &                   position='append')
        open(lpro,file=TRIM(pathoutpt)//'/pro.dat',form='formatted',
     &                   position='append')
      endif

!----------------------------------------------------------------------
!     Step 3: Some checks and enforced initializations.
!----------------------------------------------------------------------
! If no burn, set ifmfu0(:)=0:
      if(.not.ifburn) ifmfu0(:)=0

 9000 return
 9002 format(79('='))
      end subroutine RDINPT
!____________________________________________________________end RDINPT


!**********************************************************************
!                       CHECKINPT
!*******************************************************begin CHECKINPT
      subroutine CHECKINPT
      use COMDEI, only: floor,iflbnd,ifmfu0,igeo,imatz0,iundef,lrun,
     &    nmesh,nnz,nz,roz0,roz01,rz0,undef,xD0,xB0
      implicit none
!======================================================================
!     This routine checks availability and mutual consistency of some
!     key input parameters.

!     Called by: DELIRA
!     Calls    : none
!======================================================================
! Local variables:
      integer(4) :: i,j
!======================================================================
      if(igeo.ne.0 .and. igeo.ne.1 .and. igeo.ne.2) then
        write(lrun,9010) igeo
        write(*,9010) igeo
        STOP 'STOP in CHECKINPT-9010: forbidden value of IGEO!'
      endif

      if(nz.le.0 .or. nz.gt.nnz) then
        write(lrun,9020) nnz,nz
        write(*,9020) nnz,nz
        STOP 'STOP in CHECKINPT-9020: NNZ too small !'
      endif

! Check mesh parameters:
      do I=1,nz
        if(nmesh(I).eq.iundef) then
          write(lrun,9030) I
          write(lrun,'(8I10)') (nmesh(j),j=1,nz)
          write(lrun,9002)
          write(*,9030) I
          write(*,'(8I10)') (nmesh(j),j=1,nz)
          write(*,9002)
          STOP 'STOP in CHECKINPT-9030: NMESH(I) not assigned !'
        endif
      enddo

! Check the radii:
      do I=1,nz+1
        if(rz0(I).eq.undef) then
          write(lrun,9036) i
          write(lrun,'(5es14.6)') (rz0(j),j=1,nz)
          write(lrun,9002)
          write(*,9036) i
          write(*,'(5es14.6)') (rz0(j),j=1,nz)
          write(*,9002)
          STOP 'STOP in CHECKINPT-9036: forgot to assign RZ0(I)'
        endif
      enddo

! Check the densities:
      do I=1,nz
        if(roz0(I).le.0.d0) then
          write(lrun,9040) I
          write(lrun,'(5es14.6)') (roz0(j),j=1,nz)
          write(lrun,9002)
          write(*,9040) I
          write(*,'(5es14.6)') (roz0(j),j=1,nz)
          write(*,9002)
          STOP 'STOP in CHECKINPT-9040: non-positive ROZ0(I)'
        endif
        if(roz01(I).le.0.d0 .or. roz01(I).eq.undef) roz01(I)=roz0(I)
      enddo

! Check material numbers:
      do I=1,nz
        if(imatz0(I).eq.iundef) then
          write(lrun,9042) I
          write(lrun,'(7I11)') (imatz0(j),j=1,nz)
          write(lrun,9002)
          write(*,9042) I
          write(*,'(7I11)') (imatz0(j),j=1,nz)
          write(*,9002)
          STOP 'STOP in CHECKINPT-9042: forgot to assign IMATZ0(I)'
        endif
      enddo

! Check consistency of the IFLBND, IGEO, RZ0(1) values:
      if(iflbnd.eq.0 .and. abs(rz0(1)).lt.floor) goto 58
      if(iflbnd.eq.1 .and. rz0(1).gt.floor) goto 58
      if(iflbnd.eq.-1 .and. igeo.eq.0) goto 58
      write(lrun,9050) igeo,iflbnd,rz0(1)
      write(*,9050) igeo,iflbnd,rz0(1)
      STOP 'STOP in CHECKINPT-9050: inadmissible IFLBND&IGEO&RZ0(1)'
 58   continue

! Check consistency of the burn parameters:
      do I=1,nz
        if(ifmfu0(I).eq.0) cycle
        if(xD0(I).lt.floor .and. xB0(I).lt.floor) then
          write(lrun,9060) ifmfu0(I),I,xD0(I),xB0(I)
          write(*,9060) ifmfu0(I),I,xD0(I),xB0(I)
          STOP 'STOP in CHECKINPT-9060: forgot to assign xD0(I),xB0(I)'
        endif
      enddo

 9000 return
 9002 format(79('='))
 9010 format(79('=')/'STOP in CHECKINPT-9010: IGEO=',I9,
     &' is forbidden!'/79('='))
 9020 format(79('=')/'STOP in CHECKINPT-9020: NNZ=',I9,
     &' must be not smaller than NZ=',I9/'Increase parameter NNZ ',
     &'in module COMDEI'/79('='))
 9030 format(79('=')/'STOP in CHECKINPT-9030: you forgot to assign ',
     &'NMESH(I) for I=',I9/'NMESH(I)=')
 9036 format(79('=')/'STOP in CHECKINPT-9036: you forgot to assign ',
     &'RZ0(I) for I=',I9/'RZ0(I)=')
 9040 format(79('=')/'STOP in CHECKINPT-9040: non-positive initial ',
     &'density in layer',I5/'ROZ0(I)=')
 9042 format(79('=')/'STOP in CHECKINPT-9042: unassigned IMATZ)(I) ',
     &'in layer',I5/'IMATZ0(I)=')
 9050 format(79('=')/'STOP in CHECKINPT-9050: the values IGEO=',I8,
     &' IFLBND=',I8/'RZ0(1)=',es14.7,' are mutually inconsistent!'
     &/79('='))
 9060 format(79('=')/'STOP in CHECKINPT-9060: having set IFMFU0(I)=',
     &I2,' in layer I=',I4/'you forgot to assign either xD0(I)=',
     &es12.4,' or xB0(I)=',es12.4/79('='))
      end subroutine CHECKINPT
!_________________________________________________________end CHECKINPT


!**********************************************************************
!                             MEMALLO
!***********************************************************beg MEMALLO
      subroutine MEMALLO
      use COMDEI, only: cape,capeb,capi,capib,capr,caprb,
     &      de_pf,de0_pf,dm,eal,eall,ee,ei,eouall,eoutal,eoute,eouti,
     &      eouthz,eoutp3,eoup14,eoutr,eoutw,ep3,ep14,ethz,etvi0,
     &      etvi1,ezal,ezcl,ezdr,ezfus,ezjl,ezn2,ezn14,ezp3,ezp14,
     &      hial,hiall,hiei,hier,hip3,hip14,Hz,Hzold,
     &      ifburn,ifdmfu,ifpfkin,ifupeos,iifHz,ims_pf,ishlj,matnum,
     &      nn,nt_vtk,nnz,plkal,plkall,plkp3,plkp14,pr_vtk,
     &      q_pf,q0_pf,qall,qdriv,qsBH,r,r_vtk,ro_vtk,
     &      t_vtk,Te,Teold,Ti,tnun2,tnun14,tnudhe3,Tr,u,v,vold,
     &      w1w01,w1w02,w1w03,w1w04,w1w05,w1w06,w1w07,w1w08,w1w09,
     &      w1w10,w1w11,w1w12,w1w13,w1w14,w1w15,xB,xD,xH,xHe,xT,yi,
     &      peden
      implicit none
!======================================================================
!     This routine allocates memory for field variables and associates
!     global pointers.

!     Called by: DELIRA,RWDUMP
!     Calls    : none
!======================================================================
! Principal field variables:
      allocate(dm(1:nn+1),ifupeos(1:nn),matnum(1:nn),r(1:nn+2),
     &      u(1:nn+2),v(1:nn+1),Te(1:nn+1),Ti(1:nn+1),Tr(1:nn+1))
      if(ifburn) allocate(eall(1:3*nn+3),ifdmfu(1:nn),xD(1:nn),
     &      xT(1:nn),xHe(1:nn),xH(1:nn),xB(1:nn))
      if(iifHz.ge.1) allocate(Hz(1:nn+1))

! Recalculated field variables:
      allocate(cape(1:nn+1),capeb(1:nn+1),capi(1:nn+1),capib(1:nn+1),
     &      capr(1:nn+1),caprb(1:nn+1),ee(1:nn+1),ei(1:nn+1),
     &      etvi0(1:nn+1),etvi1(1:nn+1),hiei(1:nn+1),hier(1:nn+1),
     &     ishlj(1:nn+1),qdriv(1:nn),Teold(1:nn),vold(1:nn),yi(1:nn+1))

      allocate(peden(1:nn+1))  ! 额外用来记录 电子的简并压力

      if(ifburn) allocate(qsBH(1:nn+1))
      if(iifHz.ge.1) allocate(ethz(1:nn+1),Hzold(1:nn))
      if(ifpfkin) allocate(ims_pf(1:nn),de_pf(1:nn),de0_pf(1:nn),
     &      q_pf(1:nn),q0_pf(1:nn))
      if(nt_vtk.gt.0) allocate(r_vtk(1:nn+1,1:nt_vtk+1),
     &      t_vtk(1:nt_vtk+1),ro_vtk(1:nn,1:nt_vtk+1),
     &      pr_vtk(1:nn,1:nt_vtk+1))

! Working memory:
      allocate(w1w01(1:nn+1),w1w02(1:nn+1),w1w03(1:nn+1),w1w04(1:nn+1),
     &      w1w05(1:nn+1),w1w06(1:nn+1),w1w07(1:nn+1),w1w08(1:nn+1),
     &      w1w09(1:nn+1),w1w10(1:nn+1),w1w11(1:nn+1),
     &      w1w12(1:nn+1),w1w13(1:nn+1),w1w14(1:nn+1),w1w15(1:nn+1))
      if(ifburn) allocate(hiall(1:3*nn+3),qall(1:3*nn+3))

! Global pointers associated once and for all with fixed target arrays:
      eoutal => eouall(1:nnz+1)
      eoutp3 => eouall(nnz+2:2*nnz+2)
      eoup14 => eouall(2*nnz+3:3*nnz+3)
      plkal => plkall(1:nnz+1)
      plkp3 => plkall(nnz+2:2*nnz+2)
      plkp14 => plkall(2*nnz+3:3*nnz+3)

      if(ifburn) then
        eal => eall(1:nn+1)
        ep3 => eall(nn+2:2*nn+2)
        ep14 => eall(2*nn+3:3*nn+3)
        hial => hiall(1:nn+1)
        hip3 => hiall(nn+2:2*nn+2)
        hip14 => hiall(2*nn+3:3*nn+3)
      endif

 9000 return
      end subroutine MEMALLO
!___________________________________________________________end MEMALLO


!**********************************************************************
!                             RWDUMP
!************************************************************beg RWDUMP
      subroutine RWDUMP(job)
      use COMDEI
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     This routine reads (writes) the dump file when job = 'r' ('w').

!     Called by: DELIRA
!     Calls    : MEMALLO,SECSCPU
!======================================================================
! Arguments:
      character(1),intent(in) :: job

! Local variables:
      integer(4) :: i,lww,nnz_dump,nnmatrls_dump
      real(8) :: ww0
      logical :: lexist
!======================================================================

      SELECT CASE(job)
!----------------------------------------------------------------------
      CASE('w') ! Write the dump file 'ddump'
!----------------------------------------------------------------------
      open(lvirtl,file='ddump',form='unformatted')
! Write control parameters for memory allocation:
      write(lvirtl) nn,nnz,nnmatrls,ifpfkin,nt_vtk

! Control parameters from NAMELIST/INPUT/:
      write(lvirtl) nrun,igeo,nmesh,nz,nzeven,ifmfu0,iflbnd,
     &      iitemp,iifcapei,iifvis,iiflas,ifburn,iifHz,qzeven
      write(lvirtl) imatz0,rz0,roz0,roz01,bro0pr,Te0,Ti0,Tr0,uz0,
     &      xD0,xT0,xHe0,xH0,xB0,Hz0

! Cumulative run time:
      ww0=SECSCPU()
      seccumul=seccumul+ww0-secstart
      secstart=ww0

! Time-dependent variables:
      write(lvirtl) ndump,nprin,ncyc,ncycpri,ncycdpri,ntbad,ntvbad,
     &      ntflle,ntflli,ntfllr,ndriv,jflgburn,jrorp,it_vtk
      write(lvirtl) time,dt,tprin,thist,dthist,rorafu,rorfu,rorfum,
     &      rorp,Tvaccd,Hzincr,Teincd,Teincr,Tiincr,Trincr,vincd,vincr,
     &      sec,seccumul,tw_vtk

! Boundary conditions:
      write(lvirtl) pbl,pbr,Trlex,Trex,Hzbl,Hzbr,pblsum,pbrsum,
     &      pblold,pbrold,Hblold,Hbrold,pblsol,pbrsol,hebr

! Balance quantities:
      write(lvirtl) ez0,erex,erin,ezdr,ezjl,ezfus,ezcl,ezn14,ezn2,ezal,
     &      ezp3,ezp14,eoutw,eoute,eouti,eoutr,eouthz,tnun2,tnun14,
     &      tnudhe3,eouall

! Principal field variables:
      write(lvirtl) dm,r,u,v,Te,Ti,Tr,matnum
      if(iifHz.ge.1) write(lvirtl) Hz
      if(ifburn) write(lvirtl) ifdmfu,xD,xT,xHe,xH,xB,eall
      if(ifpfkin) write(lvirtl) ims_pf,de_pf,de0_pf,q0_pf
      if(nt_vtk.gt.0) write(lvirtl) r_vtk,t_vtk,ro_vtk,pr_vtk
      close(lvirtl)

!----------------------------------------------------------------------
      CASE('r') ! Read the dump file 'ddump'
!----------------------------------------------------------------------
      inquire(file='ddump',exist=lexist)
      if(.not.lexist) then
        write(lrun,9010)
        write(*,9010)
        STOP 'STOP in RWDUMP(r): file "ddump" not found !'
      endif
      open(lvirtl,file='ddump',status='old',form='unformatted')

! Read control parameters for memory allocation:
      read(lvirtl) nn,nnz_dump,nnmatrls_dump,ifpfkin,nt_vtk
      if(nnz_dump.ne.nnz) then
        write(lrun,9012) nnz,nnz_dump
        write(*,9012) nnz,nnz_dump
        STOP 'STOP in RWDUMP(r)-9012: NNZ not equal to NNZ_DUMP !'
      endif
      if(nnmatrls_dump.ne.nnmatrls) then
        write(lrun,9014) nnmatrls,nnmatrls_dump
        write(*,9014) nnmatrls,nnmatrls_dump
        STOP 'STOP in RWDUMP(r)-9014: NNMATRLS .ne. NNMATRLS_DUMP !'
      endif
      call MEMALLO

! Control parameters from NAMELIST/INPUT/:
      read(lvirtl) nrun,igeo,nmesh,nz,nzeven,ifmfu0,iflbnd,
     &      iitemp,iifcapei,iifvis,iiflas,ifburn,iifHz,qzeven
      read(lvirtl) imatz0,rz0,roz0,roz01,bro0pr,Te0,Ti0,Tr0,uz0,
     &      xD0,xT0,xHe0,xH0,xB0,Hz0

! Time-dependent variables:
      read(lvirtl) ndump,nprin,ncyc,ncycpri,ncycdpri,ntbad,ntvbad,
     &      ntflle,ntflli,ntfllr,ndriv,jflgburn,jrorp,it_vtk
      read(lvirtl) time,dt,tprin,thist,dthist,rorafu,rorfu,rorfum,
     &      rorp,Tvaccd,Hzincr,Teincd,Teincr,Tiincr,Trincr,vincd,vincr,
     &      sec,seccumul,tw_vtk

! Boundary conditions:
      read(lvirtl) pbl,pbr,Trlex,Trex,Hzbl,Hzbr,pblsum,pbrsum,
     &      pblold,pbrold,Hblold,Hbrold,pblsol,pbrsol,hebr

! Balance quantities:
      read(lvirtl) ez0,erex,erin,ezdr,ezjl,ezfus,ezcl,ezn14,ezn2,ezal,
     &      ezp3,ezp14,eoutw,eoute,eouti,eoutr,eouthz,tnun2,tnun14,
     &      tnudhe3,eouall

! Principal field variables:
      read(lvirtl) dm,r,u,v,Te,Ti,Tr,matnum
      if(iifHz.ge.1) read(lvirtl) Hz
      if(ifburn) read(lvirtl) ifdmfu,xD,xT,xHe,xH,xB,eall
      if(ifpfkin) read(lvirtl) ims_pf,de_pf,de0_pf,q0_pf
      if(nt_vtk.gt.0) read(lvirtl) r_vtk,t_vtk,ro_vtk,pr_vtk
      close(lvirtl)

!----------------------------------------------------------------------
      CASE DEFAULT
!----------------------------------------------------------------------
      write(lrun,9020) job
      write(*,9020) job
      STOP 'STOP in RWDUMP: unacceptable value of JOB !'

      END SELECT

 9000 return
 9010 format(79('=')/'STOP in RWDUMP(r): file "ddump" not found !'
     &/79('='))
 9012 format(79('=')/'STOP in RWDUMP(r)-9012: parameter NNZ=',I9/
     &'must be equal to NNZ_DUMP=',I9,' read-out from the dump file'
     &/79('='))
 9014 format(79('=')/'STOP in RWDUMP(r)-9014: parameter NNMATRLS=',I9/
     &'must be equal to NNMATRLS_DUMP=',I9,' read-out from the dump',
     &' file'/79('='))
 9020 format(79('=')/'STOP in RWDUMP(job): JOB=',a,' must be either ',
     &'"r" or "w"'/79('='))
      end subroutine RWDUMP
!____________________________________________________________end RWDUMP


!**********************************************************************
!                             MATDB
!*************************************************************beg MATDB
      subroutine MATDB
      use COMDEI, only: de_pf,de0_pf,dfloor,eionb,floor,ifmfu0,ifpfkin,
     &    iifopac,imatadd,imatlist,ims_pf,istart,iundef,lpro,lrun,
     &    lvirtl,imatz0,matnum,nmatrls,nn,nnmatrls,nnz,nz,omdfloo,
     &    opdfloo,p_cri,propmat,q_pf,q0_pf,T_cri,tdrfin,undef,v_cri,
     &    wdriv,xb0,xd0,xhe0,xh0,xt0
      use COMTA3, only: Abeam,amu1,amu2,apin1,Asub,b1,b2,bet1,bet2,
     &    bopac,c11,c12,c13,c14,c15,c16,c17,c1r,dinp1,e00,esh2,farr,
     &    gb0,hlroo,hltemm,ne2,nsh2,nlager,nroo,ntemm,nwwww,p00,pot2,
     &    potofi,ptif1,pwei,qurs,roill,rwei,sgm1,sgm2,temill,
     &    wlager,xlager,Zbeam,Zsub
      implicit none
!======================================================================
!     This routine loads various data on material properties,
!     in particular, relevant tables for tabular EOS, beam drive, etc.

!     Called by: DELIRA
!     Calls    : PRINOUT
!======================================================================
! Local variables:
      integer(4) :: i,imat,imod,iww,j,k,kww,m
      real(8) :: ww0,ww1,wwx
      logical :: lexist
!======================================================================

!----------------------------------------------------------------------
!     Step 1: Compile the full list of materials.
!----------------------------------------------------------------------
! Initial materials assigned to target layers:
      kww=0
      do I=1,nz
        imat=imatz0(I)
        if(imat.le.0) cycle
        if(kww.eq.0) then
          kww=1
          imatlist(1)=imat
        else
          do k=1,kww
            if(imat.eq.imatlist(k)) goto 110
          enddo
          kww=kww+1
          imatlist(kww)=imat
        endif
 110    continue
      enddo

! Additional materials:
      do I=1,nnmatrls
        imat=imatadd(I)
        if(imat.le.0) cycle
        if(kww.eq.0) then
          kww=1
          imatlist(1)=imat
        else
          do k=1,kww
            if(imat.eq.imatlist(k)) goto 120
          enddo
          kww=kww+1
          imatlist(kww)=imat
        endif
 120    continue
      enddo
      nmatrls=kww


      if(nmatrls.gt.nnmatrls) then
        write(lrun,9010) nnmatrls,nmatrls
        write(*,9010) nnmatrls,nmatrls
        STOP 'STOP in MATDB-9010: NMATRLS > NNMATRLS !'
      endif

! Compute the value of the IFPFKIN flag:
      if(istart.eq.0) then
        ifpfkin=.false.
        do k=1,nmatrls
          imat=imatlist(k)
          if(int(propmat(10,imat)+dfloor).eq.2) ifpfkin=.true.
        enddo

! - allocate memory, needed for phase-flip kinetics only:
        if(ifpfkin) allocate(ims_pf(1:nn),de_pf(1:nn),de0_pf(1:nn),
     &                       q_pf(1:nn),q0_pf(1:nn))
      endif


!----------------------------------------------------------------------
!     Step 2: Check consistency of material numbers after restart from
!             a dump file.
!----------------------------------------------------------------------
      if(istart.eq.0) goto 220
      do j=1,nn
        do k=1,nmatrls
          if(matnum(j).eq.imatlist(k)) goto 210
        enddo
        write(lrun,9020) j,matnum(j),(imatlist(m),m=1,nmatrls)
        write(lpro,9020) j,matnum(j),(imatlist(m),m=1,nmatrls)
        STOP 'STOP in MATDB-9020: matnum(j) not in imatlist(1:nmatrls)'
 210    continue
      enddo
 220  continue

! Check if the EOS-model # is available for all materials:
      do kww=1,nmatrls
        imat=imatlist(kww)
        if(propmat(1,imat).eq.undef) then
          write(lrun,9022) imat
          write(lpro,9022) imat
          STOP 'STOP in MATDB-9022: EOS model not assigned !'
        endif
      enddo

!----------------------------------------------------------------------
!     Step 3: Read-out the "DEITA3" EOS table if at least one material
!             has EOS model #3; overtake A,Z,Z2 from this table.
!----------------------------------------------------------------------
      do kww=1,nmatrls
        imat=imatlist(kww)
        if(int(propmat(1,imat)+dfloor).ne.3 .and. iifopac.ne.3) cycle
        inquire(file='input/eosta3_W_Fe.ii',exist=lexist)
        if(lexist) then
          open(lvirtl,file='input/eosta3_W_Fe.ii',status='old',
     &         form='formatted')
          read(lvirtl,9999) Asub,Zsub,p00,e00
          read(lvirtl,9998) nroo
          read(lvirtl,9999) roill,hlroo
          read(lvirtl,9998) ntemm
          read(lvirtl,9999) temill,hltemm,farr,qurs
          read(lvirtl,9998) nlager,nwwww
          read(lvirtl,9999) potofi,bopac,xlager,wlager,rwei,pwei
          close(lvirtl)
          call PRINOUT('prinus',lpro)
          goto 390
        else
          write(lrun,9030) iifopac,propmat(1,imat),imat
          write(lpro,9030) iifopac,propmat(1,imat),imat
          STOP 'STOP in MATDB-9030: file "input/eosta3.ii" not found !'
        endif
      enddo
 390  continue


!----------------------------------------------------------------------
!     Step 4: Read-out the "GLT-f17" EOS table if at least one material
!             has EOS model #7.
!----------------------------------------------------------------------
 490  continue


!----------------------------------------------------------------------
!     Step 5: Check and tune up model-specific EOS parameters in
!             PROPMAT(i,imat) for all materials and EOS models.
!----------------------------------------------------------------------
      DO kww=1,nmatrls
        imat=imatlist(kww)
        print*,imatlist
        imod=int(propmat(1,imat)+dfloor)        ! EOS type
        print*,nmatrls,imod
        SELECT CASE(imod)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        CASE(1) ! Polytropic EOS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          do k=2,5
            if(propmat(k,imat).eq.undef) then
              write(lrun,9051) imat
              write(lpro,9051) imat
              STOP 'STOP in MATDB-9051: K(gam) not assigned for IG EOS'
            endif
          enddo
          propmat(10,imat)=0.d0

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        CASE(2) ! Fermi EOS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if(propmat(2,imat).lt.floor) then
            print*, imat, propmat(2,imat)
            write(lrun,9052) imat
            write(lpro,9052) imat
            STOP 'STOP in MATDB-9052: z_ion not assigned for Fermi EOS'
          endif
          if(propmat(25,imat).lt.floor) then
            print*, propmat(25,imat),imat
            write(lrun,9053) imat
            write(lpro,9053) imat
            print*,propmat(25,imat),imat
            STOP 'STOP in MATDB-9053: A_eos not assigned for Fermi EOS'
          endif
          propmat(10,imat)=0.d0

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        CASE(3) ! Tabular "DEITA3" EOS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if(propmat(2,imat).eq.undef) then
            write(lrun,9032) imat
            write(lpro,9032) imat
            STOP 'STOP in MATDB-9032: mat.# m in DEITA3 not assigned !'
          endif
          propmat(10,imat)=0.d0

! Load Zeos,Amean,Zmean,Z2mean from the "DEITA3" table (assuming an
! element) for all materials with EOS model #3:
          m=int(propmat(2,imat)+dfloor)
          write(lrun,9034) imat,m
          write(lpro,9034) imat,m
          propmat(25,imat)=Asub(m)        ! Aeos
          propmat(26,imat)=Zsub(m)        ! Zeos
          propmat(27,imat)=Asub(m)        ! Amean
          propmat(28,imat)=Zsub(m)        ! Zmean

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        CASE(4) ! Linear EOS of the Mie-Gruneisen type
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          do k=2,8
            if(propmat(k,imat).eq.undef) then
              write(lrun,9054) imat
              write(lpro,9054) imat
         STOP 'STOP in MATDB-9064: param-s not assigned for linear EOS'
            endif
          enddo
          propmat(10,imat)=0.d0

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        CASE(5) ! GWEOS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! - check n and c_v:
          if(propmat(2,imat).lt.opdfloo .or.
     &       propmat(3,imat).lt.floor) then
            write(lrun,9055) propmat(2,imat),propmat(3,imat),imat
            write(lpro,9055) propmat(2,imat),propmat(3,imat),imat
          STOP 'STOP in MATDB-9055: n>1 or cv>0 not assigned for GWEOS'
          endif

! - parameters of the critical point:
          do k=4,6
            if(propmat(k,imat).lt.floor) goto 540
          enddo
          goto 542
 540      if(propmat(4,imat).lt.floor .and. propmat(5,imat).lt.floor
     &       .and. propmat(6,imat).lt.floor) then
            propmat(4,imat)=1.d0
            propmat(5,imat)=1.d0
            propmat(6,imat)=1.d0
            write(lrun,9154) imat
            write(lpro,9154) imat
          else
            write(lrun,9156) imat
            write(lpro,9156) imat
           STOP 'STOP in MATDB-9156: rho,p,T_cr not assigned for GWEOS'
          endif

 542      v_cri(imat)=1.d0/propmat(4,imat)
          T_cri(imat)=propmat(5,imat)
          p_cri(imat)=propmat(6,imat)

! - ionization degree:
          if(propmat(7,imat).lt.0.d0) then
            propmat(7,imat)=1.d0
            write(lrun,9159) propmat(7,imat),imat
            write(lpro,9159) propmat(7,imat),imat
          endif

! - atomic mass A_eos, atomic number Z_eos:
          ww0=(propmat(2,imat)+1.d0)/(propmat(2,imat)-1.d0)
          propmat(25,imat)=9.64853d0*ww0*propmat(4,imat)*
     &                 propmat(5,imat)/(propmat(6,imat)*(ww0**2-1.d0))
          propmat(26,imat)=1.d0

          iww=int(propmat(10,imat)+dfloor)
          if(iww.lt.0 .or. iww.gt.2) then
            write(lrun,9155) propmat(10,imat),imat
            write(lpro,9155) propmat(10,imat),imat
            STOP 'STOP in MATDB-9065: Maxw.flag not assigned for GWEOS'
          endif

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        CASE DEFAULT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          write(lrun,9150) propmat(1,imat),imat
          write(lpro,9150) propmat(1,imat),imat
          STOP 'STOP in MATDB: forbidden value of IEOSMOD'
        END SELECT
      ENDDO


!----------------------------------------------------------------------
!     Step 6: Check/assign other required values of propmat(i,imat).
!----------------------------------------------------------------------
! Load Zeos,Amean,Zmean,Z2mean for the fuel materials:
      DO kww=1,nmatrls
        imat=imatlist(kww)
        iww=int(propmat(1,imat)+dfloor)
        m=int(propmat(2,imat)+dfloor)
        do I=1,nz
! - check whether material imat is a fuel in layer I, or imat-EOS is
!   from "DEITA3" and has the same #m in DEITA3-table as fuel layer I:
          if(ifmfu0(I).eq.1) then
            if(imat.eq.imatz0(I) .or.
     &        (int(propmat(1,imatz0(I))+dfloor).eq.3 .and. iww.eq.3
     &         .and. m.eq.int(propmat(2,imatz0(I))+dfloor))) then
              wwx=xd0(I)+xt0(I)+xhe0(I)+xh0(I)+xb0(I)
              propmat(27,imat)=(2.014102d0*xd0(I)+3.016049d0*xt0(I)+
     &            3.016029d0*xhe0(I)+1.007825d0*xh0(I)+11.009305d0*
     &            xb0(I))/wwx                         ! Amean
              propmat(28,imat)=(xd0(I)+xt0(I)+xh0(I)+2.d0*xhe0(I)+
     &            5.d0*xb0(I))/wwx                    ! Zmean
              exit
            endif
          endif
        enddo           ! I-loop
      ENDDO

! Check whether Amean,Zmean have been assigned to all materials:
      DO kww=1,nmatrls
        imat=imatlist(kww)
        print*,imat,propmat(27,imat),undef
        if(propmat(27,imat).eq.undef) then
          write(lrun,9152) imat
          write(lpro,9152) imat
          STOP 'STOP in MATDB-9152: Amean not assigned !'
        endif
        if(propmat(28,imat).eq.undef) then
          write(lrun,9153) imat
          write(lpro,9153) imat
          STOP 'STOP in MATDB-9153: Zmean not assigned !'
        endif
      ENDDO

! Assign the fit parameter g_ei=propmat(39,imat) for the heat and
! electrical conductivity at room temperature:
      do kww=1,nmatrls
        imat=imatlist(kww)
        if(propmat(39,imat).ne.undef) cycle
        propmat(39,imat)=1.d0
        iww=int(propmat(28,imat)+dfloor)
        if(iww.eq.29) propmat(39,imat)=3.d0
        if(iww.eq.74) propmat(39,imat)=10.d0
        if(iww.eq.79) propmat(39,imat)=6.d0
      enddo


!----------------------------------------------------------------------
!     Step 9: Read-out data for the ion-beam drive.
!----------------------------------------------------------------------
      if(eionb.gt.floor .and. wdriv.gt.floor .and. tdrfin.gt.floor)then
        inquire(file='input/bemta3_U.ii',exist=lexist)
        if(lexist) then
          open(lvirtl,file='input/bemta3_U.ii',status='old',
     &         form='formatted')
          read(lvirtl,9999) c11,c12,c13,c14,c15,c16,c17,c1r,Abeam,
     &      Zbeam,b1,bet1,sgm1,amu1,ptif1,dinp1,apin1,pot2
          read(lvirtl,9998) nsh2,ne2
          read(lvirtl,9999) esh2,gb0,b2,bet2,sgm2,amu2
          close(lvirtl)
          call PRINOUT('prinbe',lpro)
        else
          write(lrun,9090) eionb,wdriv,tdrfin
          write(lpro,9090) eionb,wdriv,tdrfin
          STOP 'STOP in MATDB-9090: file "input/eosta3.ii" not found !'
        endif

! When ion-beam drive is inacted, all materials must have EOS model #3,
! i.e. tabular "DEITA3" EOS model:
        do kww=1,nmatrls
          imat=imatlist(kww)
          if(int(propmat(1,imat)+dfloor).ne.3) then
            write(lrun,9092) eionb,wdriv,tdrfin
            write(lpro,9092) eionb,wdriv,tdrfin
         STOP 'STOP in MATDB-9092: not all materials with "DEITA3" EOS'
          endif
        enddo
      endif

 9000 return
 9010 format(/79('=')/'STOP in MATDB-9010: parameter NNMATRLS=',I6,
     &' must not be smaller than'/'NMATRLS=',I6,'; increase the value',
     &' of NNMATRLS in module COMDEI accordingly!'/79('='))
 9020 format(/79('=')/'STOP in MATDB-9020: in cell j=',I9,
     &'  matnum(j)=',I6,','/'read-out from file "ddump", is not in ',
     &'the full list of materials'/'IMATLIST(1:nmtrls)=',10I5,/15I5
     &/79('='))
 9022 format(/79('=')/'STOP in MATDB-9022: you forgot to assign EOS ',
     &'model'/'[defined by the value of propmat(1,imat)] to material ',
     &'imat=',I4/79('='))
 9030 format(/79('=')/'STOP in MATDB-9030: either IIFOPAC=',I3,
     &' or propmat(1,imat)=',F4.1,' for imat=',I3/'require file ',
     &'"input/eosta3.ii" that was not found'/79('='))
 9032 format(/79('=')/'STOP in MATDB-9032: you forgot to assign ',
     &'the sequential material number'/'m=propmat(2,imat) in table ',
     &'"DEITA3" for imat= ',I4/79('='))
 9034 format('##WARN from MATDB-9034: propmat(25:29) for imat=',I3,
     &' taken from DEITA3, m=',I2)
 9051 format(/79('=')/'STOP in MATDB-9051: you forgot to assign ',
     &'one of propmat(k,imat), k=2-5'/'for imat= ',I3,
     &' for polytropic EOS model #1'/79('='))
 9052 format(/79('=')/'STOP in MATDB-9052: you forgot to assign ',
     &'z_ion=propmat(2,imat)'/'for imat= ',I4,' for Fermi EOS #2'
     &/79('='))
 9053 format(/79('=')/'STOP in MATDB-9053: you forgot to assign ',
     &'A_eos=propmat(25,imat)'/'for imat= ',I4,' for Fermi EOS #2'
     &/79('='))
 9054 format(/79('=')/'STOP in MATDB-9054: you forgot to assign ',
     &'one of propmat(k,imat), k=2-8'/'for imat= ',I3,
     &' for linear EOS model #4'/79('='))
 9055 format(/79('=')/'STOP in MATDB-9155: you forgot to assign ',
     &'for EOS=GWEOS either'/
     &'n=propmat(2,imat)=',es20.12,' (must be > 1), or'/
     &'c_v=propmat(3,imat)=',es12.4,' (must be > 0) for imat= ',
     &I3/79('='))
 9090 format(79('=')/'STOP in MATDB-9090: because file "input/bemta3.',
     &'ii" was not found; one of the'/'EIONB=',es12.4,' WDRIV=',es12.4,
     &' or TDRFIN=',es12.4/'must be zero to suppression- beam drive'
     &/79('='))
 9092 format(79('=')/'STOP in MATDB-9092: because not all materials ',
     &'have "DEITA3" EOS, one of the'/'EIONB=',es12.4,' WDRIV=',es12.4,
     &' or TDRFIN=',es12.4/'must be zero to suppress ion-beam drive'
     &/79('='))
 9150 format(/79('=')/'STOP in MATDB-9150: forbidden propmat(1,imat)=',
     &es12.4,' for imat=',I3/79('='))
 9152 format(/79('=')/'STOP in MATDB-9152: you forgot to assign ',
     &'Amean=propmat(27,imat) for imat=',I3/79('='))
 9153 format(/79('=')/'STOP in MATDB-9153: you forgot to assign ',
     &'Zmean=propmat(28,imat) for imat=',I3/79('='))
 9154 format('###WARNING from MATDB-9154, EOS=GWEOS: ',
     &'unassigned rho_cr,T_cr,P_cr ='/
     &3x,'propmat(4-6,imat) are forced to be equal to 1 for imat=',I3)
 9155 format(/79('=')/'STOP in MATDB-9155: you forgot to assign ',
     &'Maxw.flag=propmat(10,imat)=',es10.2/'(must be 0, 1, or 2) ',
     &'for imat= ',I3,' in GWEOS, i.e. EOS #5'/79('='))
 9156 format(/79('=')/'STOP in MATDB-9156: you forgot to assign ',
     &'for EOS=GWEOS one of'/
     &'rho_cr=propmat(4,imat)=',es12.4,
     &', or T_cr=propmat(5,imat)=',es12.4/
     &'or P_cr=propmat(6,imat)=',es12.4,' for imat= ',I3/79('='))
 9159 format('###WARNING from MATDB-9159, EOS=GWEOS: ',
     &'unassigned z_ion = y_mean ='/
     &5x,'propmat(7,imat) is forced to equal',es12.4,' for imat=',I3)

 9998 format(6I10)
 9999 format(1P5E15.8)
      end subroutine MATDB
!_____________________________________________________________end MATDB


!**********************************************************************
!                             JOBINIT
!***********************************************************beg JOBINIT
      subroutine JOBINIT
      use COMDEI, only: Amol,amz,amsfu,asbol,ceiling,csurf,czdtvt,
     &    de_pf,de0_pf,dfloor,dm,dt,dt0,dthist,dtprin,eal,ee,ei,ep14,
     &    ep3,erex,erin,eouall,eoutw,eoute,eouti,eoutr,eouthz,ez0,
     &    ezdr,ezjl,ezfus,ezcl,ezn14,ezn2,ezal,ezp3,ezp14,
     &    floor,Hblold,Hbrold,Hz,Hz0,Hzbl,Hzbr,
     &    Hzincr,Hzold,i1geo,ifburn,ifdmfu,iflbnd,ifmfu0,ifpfkin,
     &    ifupeos,igeo,iifHz,iifn14,iifn2,imatz0,ims_pf,ishlj,istart,
     &    itimpri,itimprmax,jflgburn,jrorp,lrun,matnum,n1,n2,n1max,
     &    ncyc,ncycpri,ncycprii,ndriv,ndump,nfu,njz,nn,nmesh,
     &    nprin,ntbad,ntflle,ntflli,ntfllr,ntvbad,nz,nzeven,
     &    opdfloo,pbl,pblold,pblsol,pblsum,pbr,pbrold,pbrsol,pbrsum,
     &    pinum,propmat,q_pf,q0_pf,qzeven,r,rorfu,rorfum,rorp,roz0,rz0,
     &    smol,Te,Te0,Tfloor,Ti,Ti0,Tr,Tr0,Trex,Trlex,Teincd,Teincr,
     &    Teold,thist,Tiincr,time,tnun2,tnun14,tnudhe3,
     &    tprinii,tprin,Trincr,u,ucbet,uz0,
     &    v,v_cri,vincd,vincr,vold,xb0,xB,xd0,xD,xh0,xH,xHe,xhe0,
     &    xmol,xT0,xT,Z2mol,Zmol,clogaree,clogarei
      use MATKIT, only: ZN,ZERA0
      implicit none
!======================================================================
!     This routine initializes the target.

!     Called by:  DELIRA
!     Calls    :  BNDVAL,DM0PRO,DRIVE,KINBUR,UPDEOS,ZERA0
!======================================================================
! Local variables:
      integer(4) :: i,id,idir,iirab,imat,iMxw,ip,j,j1,jf,ji,jjrab,jm1,k
      real(8) :: dmww,dr3,h,hhrab,qz,rab,rab0,rab1,rrab0,rs1,rs1p,rsp
!======================================================================

!----------------------------------------------------------------------
!     Step 1: Unconditional initializations for any value of ISTART.
!----------------------------------------------------------------------
! Load additional geometry parameters:
      i1geo=igeo+1
      csurf=1.d0
      if(igeo.ge.1) csurf=2*igeo*pinum

! Load additional mesh parameters:
      n1max=nn+1
      n1=nn+1
      n2=nn+2

! - compute njz(I):
      njz(1)=1
      do I=1,nz
        njz(I+1)=njz(I)+nmesh(I)
      enddo

! - compute ishlj(:):
      do I=1,nz
        ji=njz(I)
        jf=njz(I+1)-1
        do j=ji,jf
          ishlj(j)=I          ! layer # for cell j
        enddo
      enddo
!     nn = sum(nmesh),indecate the right radii of target
      ishlj(n1)=ishlj(nn)

! Compute reduced layer masses amz(I), reduced central-fuel
! mass amsfu, nfu:
      nfu=0
      amsfu=0.d0
      do I=1,nz
! - the "reduced" mass (=\int\rho r^igeo dr) of layer I:
        rab0=rz0(I)
        rab1=rz0(I+1)
        call DM0PRO(rab0,rab1,I,96,DMWW)
        amz(I)=dmww
!@@      amz(I)=roz0(I)*(rz0(I+1)**i1geo-rz0(I)**i1geo)/dble(i1geo)
! - parameters of the central fuel sphere:
        if(ifmfu0(I).eq.1 .and. njz(I).eq.nfu+1) then
          nfu=njz(I+1)-1
          amsfu=amsfu+amz(I)
        endif
      enddo

! Initial fuel concentrations in non-fuel layers:
      do I=1,nz
        if(ifmfu0(I).ne.0) cycle
        xd0(I)=0.d0
        xt0(I)=0.d0
        xhe0(I)=0.d0
        xh0(I)=0.d0
        xb0(I)=0.d0
      enddo

! Make neutron heating "fool-proof":
      iifn2=max(0,iifn2)
      iifn14=max(0,iifn14)
      if(iflbnd.eq.-1 .or. amsfu.lt.floor) then
        if(iifn2.gt.0) iifn2=2
        if(iifn14.gt.0) iifn14=2
      endif

! Compute "molecular" masses, charges, etc. in layers:
      do I=1,nz
        if(ifmfu0(I).eq.0) then
! - non-fuel layer:
          xmol(I)=1.d0
          Amol(I)=propmat(27,imatz0(I))
          Zmol(I)=propmat(28,imatz0(I))
          Z2mol(I)=Zmol(I)**2
          smol(I)=1.d0/(sqrt(Amol(I))*Zmol(I)**2)
        else
! - fuel layer:
          xmol(I)=xd0(I)+xt0(I)+xhe0(I)+xh0(I)+xb0(I)
          Amol(I)=2.014102d0*xd0(I)+3.016049d0*xt0(I)+3.016029d0*
     &            xhe0(I)+1.007825d0*xh0(I)+11.009305d0*xb0(I)
          Zmol(I)=xd0(I)+xt0(I)+xh0(I)+2.d0*xhe0(I)+5.d0*xb0(I)
          Z2mol(I)=xd0(I)+xt0(I)+xh0(I)+4.d0*xhe0(I)+25.d0*xb0(I)
          smol(I)=xd0(I)/(sqrt(2.014102d0))+xt0(I)/(sqrt(3.016049d0))+
     &         xh0(I)/sqrt(1.007825d0)+xhe0(I)/(4.d0*sqrt(3.016029d0))+
     &            xb0(I)/(25.d0*sqrt(11.009305d0))
        endif
      enddo

! Control parameters for calling PRINOUT, PRIPR_C:
      dtprin=ceiling
      itimprmax=100
      do i=1,itimprmax-1
        if(tprinii(i+1).gt.tprinii(i)*opdfloo) cycle
        itimprmax=i
        dtprin=max(floor,tprinii(i+1))
        exit
      enddo


      IF(istart.eq.0) THEN
!----------------------------------------------------------------------
!     Step 2: Conditional initializations for ISTART=0 only.
!----------------------------------------------------------------------
        ndriv=0
        ndump=0
        nprin=0
        ntflle=0
        ntflli=0
        ntfllr=0
        ncyc=0
        ntbad=0
        ntvbad=0
        jflgburn=-1
        if(ifburn) jflgburn=0
        jrorp=1
        itimpri=1
        ncycpri=max(1,ncycprii(1))

        Teincr=0.d0
        Teincd=0.d0
        Tiincr=0.d0
        Trincr=0.d0
        Hzincr=0.d0
        vincr=0.d0
        vincd=0.d0
        rorfum=0.d0

        ezdr(:)=0.d0
        ezjl(:)=0.d0
        ezfus(:)=0.d0
        ezcl(:)=0.d0
        ezn14(:)=0.d0
        ezn2(:)=0.d0
        ezal(:)=0.d0
        ezp3(:)=0.d0
        ezp14(:)=0.d0
        eoutw(:)=0.d0
        eoute(:)=0.d0
        eouti(:)=0.d0
        eoutr(:)=0.d0
        eouthz(:)=0.d0
        tnun2(:)=0.d0
        tnun14(:)=0.d0
        tnudhe3(:)=0.d0
        eouall(:)=0.d0


        erin=0.d0
        erex=0.d0
        time=0.d0
        thist=0.d0
        dthist=ceiling
        dt=dt0
        tprin=tprinii(itimpri)

! Construct the radial mesh in layer I=NZEVEN:
        ji=njz(nzeven)
        jf=njz(nzeven+1)-1
        r(ji)=rz0(nzeven)
! - set the multiplier of the geometric progression for cell masses:
!       #todo， 所有燃料区的质量密度分配都可以变为均匀分布
        qz=qzeven
        if(qz.eq.1.d0) h=amz(nzeven)/(jf-ji+1)
        if(qz.ne.1.d0) h=amz(nzeven)*(qz-1.d0)/(qz**(jf-ji+1)-1.d0)
        iirab=nzeven
        jjrab=1

        do j=ji,jf
          rab0=r(j)
          rab1=rz0(nzeven+1)
          rab=.5d0*(rab0+rab1)
          rrab0=rab0
          hhrab=h
          r(j+1)=ZERA0(FZER0PR,rab0,rab1,rab)
!@@        dr3=h*dble(i1geo)/roz0(nzeven)
!@@        if(igeo.ne.0) r(j+1)=(r(j)**i1geo+dr3)**(1.d0/dble(i1geo))
!@@        if(igeo.eq.0) r(j+1)=r(j)+dr3
          dm(j)=h
          h=h*qz
        enddo

! Construct the radial mesh in remaining layers:
        DO idir=1,2
! - IDIR=1 -> construct outward, IDIR=2 -> inward;
          id=3-2*idir
          jjrab=id
          j=njz(nzeven+1)-1
          if(idir.eq.2) j=njz(nzeven)
          h=dm(j)

          do ip=1,nz
            i=nzeven+ip*id
            if(I.le.0.or.I.gt.nz) exit
            iirab=I
            ji=njz(I)
            jf=njz(I+1)-1
            qz=ZN(jf-ji+1,amz(I),h)

            do k=ji,jf
              dr3=h*dble(i1geo)/roz0(I)
              j=k
              if(idir.eq.2) j=ji+jf-k
              dm(j)=h
              j=j+idir-1
              j1=j+id
              if(j1.eq.1) then
                r(j1)=rz0(1)
              else
!@@              if(igeo.eq.0) r(j1)=r(j)+id*dr3
!@@        if(igeo.ne.0) r(j1)=(r(j)**i1geo+id*dr3)**(1.d0/dble(i1geo))
                if(idir.eq.1) then
                  rab0=r(j)
                  rab1=rz0(I+1)
                else
                  rab0=rz0(I)
                  rab1=r(j)
                endif
                rab=.5d0*(rab0+rab1)
                rrab0=r(j)
                hhrab=h
                r(j1)=ZERA0(FZER0PR,rab0,rab1,rab)
              endif
              h=h*qz
            enddo       ! k-loop
            h=h/qz
          enddo         ! ip-loop
        ENDDO           ! idir-loop
        r(n2)=r(n1)
        dm(n1)=dm(nn)

! Initial values of field variables:
        do I=1,nz
          ji=njz(I)
          jf=njz(I+1)-1
          do j=ji,jf
            matnum(j)=imatz0(I)           ! initial material number
            u(j+1)=uz0(I)                       ! initial velocity
            rs1=r(j)
            if(igeo.ge.1) rs1=r(j)**i1geo
            rs1p=r(j+1)
            if(igeo.ge.1) rs1p=r(j+1)**i1geo
            v(j)=(rs1p-rs1)/(dble(i1geo)*dm(j)) ! ini.specific volume
            Te(j)=max(Te0(I),Tfloor)
            Ti(j)=max(Ti0(I),Tfloor)
            Tr(j)=max(Tr0(I),Tfloor)
            if(iifHz.ge.1) Hz(j)=Hz0            ! initial magnetic field

            if(ifburn) then
              eal(j)=0.d0
              ep3(j)=0.d0
              ep14(j)=0.d0
              ifdmfu(j)=ifmfu0(I)           ! ini.value of fuel flag
              xD(j)=xD0(I)                  ! initial D content
              xT(j)=xT0(I)                  ! initial T content
              xHe(j)=xHe0(I)                ! initial He3 content
              xH(j)=xH0(I)                  ! initial H content
              xB(j)=xB0(I)                  ! initial B content
            endif
          enddo   ! j-loop
        enddo     ! I-loop
        if(iflbnd.eq.0) then
          u(1)=0.d0                       ! initial velocity
        else
          u(1)=uz0(1)                     ! initial velocity
        endif

! Load the values of variables needed for pf-relaxation:
        if(ifpfkin) then
          de_pf(:)=0.d0
          de0_pf(:)=0.d0
          q0_pf(:)=0.d0
          do j=1,nn
            imat=matnum(j)
            iMxw=int(propmat(10,imat)+dfloor)
            if(iMxw.eq.1) then
              ims_pf(j)=0
            else
              if(v(j).gt.v_cri(imat)) then
                ims_pf(j)=-1    ! MS-vapor state
              else
                ims_pf(j)=1     ! MS-liquid state
              endif
            endif
          enddo
        endif

! - eventual correction for radiation temperature at boundaries:
        rsp=1.d0
        if(igeo.ge.1) rsp=r(n1)**igeo
        rab=(.25d0*ucbet*dt)*(rsp/(dm(nn)*v(nn)))
        rab=Trex*sqrt(sqrt(rab/(czdtvt+rab)))
        if(Tr(nn).lt.rab) Tr(nn)=rab
        if(iflbnd.eq.-1) then
          rab=.25d0*ucbet*dt/(dm(1)*v(1))
          rab=Trlex*sqrt(sqrt(rab/(czdtvt+rab)))
          if(Tr(1).lt.rab) Tr(1)=rab
        endif

! Initial energy in layers -> for balance; only when  ISTART=0:
        ifupeos(:)=.true.
        call UPDEOS
        do I=1,nz
          ez0(I)=0.d0
          ji=njz(I)
          jf=njz(I+1)-1
          do j=ji,jf
            jm1=max(1,j-1)
            ez0(I)=ez0(I)+csurf*(dm(j)*(ee(j)+ei(j)+asbol*tr(j)**4*
     &           v(j))+.125d0*((dm(jm1)+dm(j))*u(j)**2+(dm(j)+dm(j+1))*
     &             u(j+1)**2))
            if(ifburn) ez0(I)=ez0(I)+csurf*dm(j)*v(j)*(eal(j)+ep3(j)+
     &                        ep14(j))
            if(iifHz.ge.1) ez0(I)=ez0(I)+csurf*dm(j)*v(j)*Hz(j)**2/
     &                     (8.d0*pinum)
          enddo
        enddo

        call DRIVE
        if(iifHz.ge.1) Hzold(:)=0.d0
        Teold(:)=0.d0
        vold(:)=0.d0

        call BNDVAL(time)
        pblold=pbl
        pbrold=pbr
        Hblold=Hzbl
        Hbrold=Hzbr
        pblsol=pblsum
        pbrsol=pbrsum
        call KINBUR
        rorp=rorfu

      ELSE

!----------------------------------------------------------------------
!     Step 3: Conditional initialization for ISTART.ne.0.
!----------------------------------------------------------------------
! Initialization of the run continued from 'ddump':
        ifupeos(:)=.true.
        call UPDEOS
        call DRIVE
        if(iifHz.ge.1) Hzold(:)=0.d0
        Teold(:)=0.d0   ! unconditional recalc-n of transp.coefficients
        vold(:)=0.d0    ! unconditional recalc-n of transp.coefficients
        call KINBUR


        itimpri=itimprmax
        tprin=max(time,tprin)
        if(time.lt.tprinii(itimprmax)) then
          do i=1,itimprmax
            if(time.gt.tprinii(i)) cycle
            itimpri=i
            tprin=tprinii(i)
            exit
          enddo
        endif
        ncycpri=max(ncyc,ncycpri)

      ENDIF

!----------------------------------------------------------------------
!     Step 4: Concluding operations.
!----------------------------------------------------------------------
! Convert "reduced" layer masses into full masses (for any ISTART):
      amz(:)=csurf*amz(:)

 9000 return

      contains

!**********************************************************************
!                       FZER0PR
!***********************************************************beg FZER0PR
      function FZER0PR(rr)
      implicit none
!======================================================================
!     This routine computes the function to be zeroed when costructing
!     the m-mesh in target layer I.

!     Called by:  ZERA0
!     Calls    :  DM0PRO
!======================================================================
! Arguments:
      real(8) :: fzer0pr
      real(8),intent(in) :: rr

! Local variables:
      real(8) :: dmww_
!======================================================================
      if(jjrab.eq.1) then
        call DM0PRO(rrab0,rr,iirab,16,DMWW_)
      else
        call DM0PRO(rr,rrab0,iirab,16,DMWW_)
      endif
      fzer0pr=DMWW_/hhrab-1.d0

      return
      end function FZER0PR
!___________________________________________________________end FZER0PR

      end subroutine JOBINIT
!__________________________________________________________ end JOBINIT


!**********************************************************************
!                         WHEADER
!***********************************************************beg WHEADER
      subroutine WHEADER(lww)
      use COMDEI, only: dt,istart,jobtit,ncyc,time
      implicit none
!======================================================================
!     This routine writes out to the unit LWW the header to the current
!     job.

!     Called by:  DELIRA
!     Calls    :  none
!======================================================================
! Arguments:
      integer(4),intent(in) :: lww
!======================================================================
      write(lww,9008)
      write(lww,9004)
      if(lww.eq.6 .or. lww.eq.0) then
        write(lww,'(a5,a74)') 'Job: ',jobtit
      else
        write(lww,'(2a)') 'Job: ',jobtit
      endif
      if(istart.ne.0) write(lww,9012) ncyc,time,dt
      write(lww,9004)
!@@      write(lww,*)

 9000 return
 9004 format(79('*'))
 9008 format(13x,50('*')/
     &       13x,'*        Code DELIRA: version 2021.03-i04        *')
 9012 format(5x,'Restart from ddump: ncyc=',I9,', time=',
     &es15.8,', dt=',es10.3)
      end subroutine WHEADER
!___________________________________________________________end WHEADER


!**********************************************************************
!                             UPDEOS
!************************************************************beg UPDEOS
      subroutine UPDEOS
      use COMDEI, only: asbol,dfloor,ee,eet,eev,ei,eit,eiv,floor,
     &    ims_pf,Hz,ifpfkin,ifupeos,iifHz,matnum,nn,pe,pi,pinum,
     &    propmat,sec,Te,Ti,Tr,us,v,yi,peden
      use MATKIT, only: SECSCPU
      implicit none
!=====================================================================
!     This is a master routine for updating EOS.
!     EOS is updated in those cells j only where ifupeos(j)=.true.!

! INPUT:
!     ifupeos(1:nn) -> logical flag for updating EOS
!     v(1:nn)     -> specific volume, in units cm**3/g;
!     Te(1:nn),Ti(1:nn),Tr(1:nn) -> temperatures in keV;
!     Hz(1:nn)    -> magnetic field, in units 10**7 Gauss;
!     ims_pf(1:nn)-> integer flag for choosing between MS and EQ EOS.

! OUTPUT
!     YI(1:nn) - ionization degree;
!     PE(1:nn) - electron pressure, in units 10**14 ergs/cm**3;
!     PI(1:nn) - ion pressure, in units 10**14 ergs/cm**3;
!     EE(1:nn) - electron specific energy, in units 10**14 ergs/g;
!     EI(1:nn) - ion specific energy, in units 10**14 ergs/g;
!     EEV(J) = d(EE(J))/d(V(J));
!     EET(J)=d(EE(J))/d(TE(J));
!     EIV(J)=d(EI(J))/d(V(J));
!     EIT(J)=d(EI(J))/d(TI(J)),
!     US(J)=sound speed, in units 10**7 cm/s.

!     Called by:  DELIRA,JOBINIT
!     Calls    :  EOSDRV,SECSCPU
!=====================================================================
! Local variables:
      integer(4) :: i,imat,iMxw,j,jf,ji
      real(8) :: eeou,eevou,eetou,eiou,eivou,eitou,peou,piou,rab,
     &      Tej,Tij,tin,tout,us2ou,vj,you,pedenTemp
      logical :: ifMSeos
!=====================================================================
      tin=SECSCPU()

      do j=1,nn
        if(.not.ifupeos(j)) cycle
        vj=v(j)
        Tej=Te(j)
        Tij=Ti(j)
        imat=matnum(j)

        ifMSeos=.true.
        iMxw=int(propmat(10,imat)+dfloor)
        if(ifpfkin) then
          if(abs(ims_pf(j)).ne.1) ifMSeos=.false.
        else
          if(iMxw.eq.1) ifMSeos=.false.
        endif

        call EOSDRV(imat,vj,Tej,Tij,ifMSeos,YOU,PEOU,PIOU,EEOU,EEVOU,
     &              EETOU,EIOU,EIVOU,EITOU,US2OU)

        yi(j)=you
        pe(j)=peou
        pi(j)=piou
        ee(j)=eeou
        ei(j)=eiou
        eev(j)=eevou
        eet(j)=eetou
        eiv(j)=eivou
        eit(j)=eitou
        rab=max(0.d0,us2ou)+.444444444d0*asbol*tr(j)**4*vj
!us        rab=us2ou**2+.444444444d0*asbol*tr(j)**4*vj
        if(iifHz.ge.1) rab=rab+vj*Hz(j)**2/(4.d0*pinum)
        us(j)=max(floor,sqrt(rab))

       call EOSDRV(imat,vj,0.1d-7,0.1d-7,ifMSeos,YOU,PEOU,PIOU,EEOU
     &          ,EEVOU,EETOU,EIOU,EIVOU,EITOU,US2OU)
        peden(j)=peou

      enddo     ! j-loop



 9000 tout=SECSCPU()
      sec(3)=sec(3)+max(0.d0,tout-tin)
      return

      end subroutine UPDEOS
!____________________________________________________________end UPDEOS


!**********************************************************************
!                             PFKIN
!*************************************************************beg PFKIN
      subroutine PFKIN
      use COMDEI, only: ceiling,cz_pf,czdt,de_pf,de0_pf,dfloor,
     &    ee,ei,floor,ifupeos,ims_pf,lrun,matnum,nn,ncyc,p_cri,pe,pi,
     &    propmat,q0_pf,r,sec,tau_pf,T_cri,Te,Ti,time,us,v,v_cri
      use GWEOS_rd, only: BIND_NGWR,PE_GWR,PES_EQGWR,PTS_EQGWR
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     This routine performs MS-EOS -> EQ-EOS (or EQ-EOS -> MS-EOS)
!     phase-flip when crossing the spinodal inwards (or the binodal
!     outwards).

! INPUT:  r(:),matnum(:),v(:),Te(:),Ti(:),de_pf(:),ims_pf(:)
! OUTPUT: Te(:),Ti(:),de_pf(:),de0_pf(:),q0_pf(:),ims_pf(:),ifupeos(:)

!     Called by:  DELIRA - but only when IFPFKIN=.true.!
!     Calls    :  SECSCPU,BIND_NGWR,GZERA,PES_EQGWR,PTS_EQGWR
!======================================================================
! Local variables:
      integer(4) :: ieosmod,imat,imeth,iMxw,j,jm1,jp1,nfcx_pf,nfgz,
     &      nfphj
      real(8) :: cs2PJ,cs2ww,cv_gw,dedtet,dpdr,dpdtet,
     &      dpmww,dppww,drww,drmww,drpww,dt_cfl,dt_ss,dt_pf,dt_ra,
     &      dt_ru,e0j,e01j,eouww,euni,fgz,fphj,gw_n,gw_n1,
     &      p00j,p01j,p0j,pspj,pPJj,rjgas,rljgas,rjliq,roj,sww,
     &      tet01j,tetfl,tetj,tetsp,tin,tout,v00,ww0,ww1,ww2,wwi,ya
      real(8) :: acs2ww(1),adedtww(1),adpdrww(1),adpdtww(1),aeww(1),
     &      apww(1),arww(1),arlww(1),asww(1)
      real(8),parameter :: eps_pft=1.d-6,Tspmarg=0.d0,pspmarg=0.0d0
!======================================================================
      tin=SECSCPU()

      nfcx_pf=60  ! max numb.of calls of F5_t01_pf by computing tet01j
      ifupeos(:)=.false.

      DO j=1,nn                     ! begin j-loop over mesh cells
        imat=matnum(j)
        iMxw=int(propmat(10,imat)+dfloor)

        if(ims_pf(j).eq.2) then
!###############################################################deb:
!cc      if(j.eq.int(.9d0*nn) .and. 2*(ncyc/2).eq.ncyc)
!cc     &              write(*,'(a,I4,a,I6,a,es9.2)')
!cc     &'::pfe=> j=',j,' ncyc=',ncyc,' de/de0=',de_pf(j)/de0_pf(j)
!cc      if(j.eq.int(.9d0*nn) .and. 2*(ncyc/2).eq.ncyc)
!cc     &              write(lrun,'(a,I4,a,I6,a,es9.2)')
!cc     &'::pfe=> j=',j,' ncyc=',ncyc,' de/de0=',de_pf(j)/de0_pf(j)
!###############################################################deb.
          if(abs(de_pf(j)).lt.floor) then
            ims_pf(j)=0
            de_pf(j)=0.d0
            de0_pf(j)=0.d0
            q0_pf(j)=0.d0
            if(iMxw.eq.0) goto 1980  ! switch to pure MS EOS
          endif
          goto 1990                  ! remain with the EQ EOS
        elseif(ims_pf(j).eq.0) then
          if(iMxw.eq.0) goto 1980    ! switch to pure MS EOS
          if(iMxw.eq.1) goto 1990    ! remain with the EQ EOS
        else
          if(iMxw.eq.0) then
            goto 1980                ! remain with the MS EOS
          elseif(iMxw.eq.1) then
            ims_pf(j)=0
            goto 1990                ! switch to the EQ EOS
          endif
        endif
!......................................................................
! Having arrived here, we have iMxw=2 and ims_pf(j)=0, or -1, or +1:
!......................................................................
        ieosmod=int(propmat(1,imat)+dfloor)

! Jump to appropriate EOS:
        SELECT CASE(ieosmod)

!----------------------------------------------------------------------
        CASE(1) ! Polytropic EOS.
!----------------------------------------------------------------------
        cycle

!----------------------------------------------------------------------
        CASE(2) ! Fermi EOS.
!----------------------------------------------------------------------
        cycle

!----------------------------------------------------------------------
        CASE(3) ! Unknown EOS.
!----------------------------------------------------------------------
        cycle

!----------------------------------------------------------------------
        CASE(4) ! Linear EOS of the Mie-Gruneisen type.
!----------------------------------------------------------------------
        cycle

!----------------------------------------------------------------------
        CASE(5) ! Generalized van-der-Waals EOS (GWEOS-NE).
!----------------------------------------------------------------------
        if(Te(j).ge.T_cri(imat)) goto 1980
        euni=p_cri(imat)*v_cri(imat)
        ya=propmat(7,imat)
        wwi=1.d0/(ya+1.d0)
! - reduced temperature and density:
        tetj=Te(j)/T_cri(imat)
        roj=v_cri(imat)/v(j)
        gw_n=propmat(2,imat)
        gw_n1=gw_n+1.d0
        cv_gw=propmat(3,imat)

        IF(ims_pf(j).eq.0) THEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Domain of EQ-EOS, no pf-heating:
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Check emergence from under the binodal:
!####################################################to be modified:
!####### by using rjliq,rjgas from preceeding EOS update to save CPU!!
          call BIND_NGWR(tetj,gw_n,cv_gw,RJLIQ,RJGAS,RLJGAS,FPHJ,
     &                   NFPHJ)
          if(roj.le.rjgas .or. roj.ge.rjliq) then
!##################################################################deb:
            write(40,9083) ncyc,time,j,(pe(j)+pi(j))/p_cri(imat),
     &                     roj,rjgas,rjliq
 9083 format('Emerg.Over Binod: ncyc=',I7,' t=',es15.8,' j=',I4,
     &' p,rho=',2es11.4,' ro_gas,liq=',2es11.4)
!##################################################################deb.
            goto 1980
          endif

        ELSE                        ! here |ims_pf(j)|=1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Domain of MS-EOS above the spinodal:
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Check fulfillment of the phase-flip criterion (a dive under spinodal)
          v00=(gw_n-1.d0)/gw_n1
! - starting pressure P0J (in units of P_cr) and starting energy E0J
!   (in units of P_cr*V_cr) for which the phase flip is to be made:
          p0j=(pe(j)+pi(j))/p_cri(imat)
          p00j=max(0.d0,p0j)
          e0j=(ee(j)+ei(j))/euni
!................... The pressure criterion ...........................
! - spinodal pressure:
          pspj=roj**gw_n1*(gw_n1/roj-gw_n)
! - phase-flip criterion for PRESSURE:
          if(p0j.gt.pspj+pspmarg) goto 1980
!......................................................................

!@@!....................The temperature criterion .....................
!@@! - spinodal temperature:
!@@          tetsp=((1.d0/roj-v00)/(1.d0-v00))**2*roj**gw_n1
!@@! - phase-flip criterion for TEMPERATURE:
!@@          if(tetj.gt.tetsp*(1.d0+Tspmarg*(1.d0-tetsp))) goto 1980
!@@!...................................................................

! - the final pressure pPJj (in units of P_cr) of the full isochoric
!   phase-jump:
          call PTS_EQGWR(roj,e0j,gw_n,cv_gw,PPJJ,TETFL,EOUWW,SWW,
     &      CS2PJ,DPDR,DPDTET,DEDTET,RJLIQ,RJGAS,RLJGAS,FPHJ,NFPHJ)

          if(tau_pf.lt.dfloor .or. pPJj-p00j.le.cz_pf) then
! - a single full isochoric phase-jump:
            Te(j)=T_cri(imat)*tetfl
            Ti(j)=Te(j)
            ifupeos(j)=.true.
            ims_pf(j)=0
!##################################################################deb:
            write(40,9081) ncyc,time,j,p0j,pPJj,r(j)
 9081 format('One-step PF-jump: ncyc=',I7,' t=',es15.8,' j=',I4,
     &' p_0,PJ=',2es10.3,' r_j=',es15.8)
!##################################################################deb.
            goto 1990
          endif

! - the starting EQ-pressure p01j for the subsequent pf-e relaxation:
!cc          p01j=p00j+cz_pf*(pPJj-p00j)
          p01j=min(p00j+cz_pf,pPJj)                   ! 2019-22-06
! - compute the starting EQ temperature tet01j and energy e01j such
!   that p_EQ(roj,tet01j)=p01j:
          arww(1)=roj
          arlww(1)=log(roj)
          imeth=1
          ww0=min(1.d-5,.5d0*tetfl)
          call GZERA(F5_t01_pf,ww0,tetfl,eps_pft,nfcx_pf,imeth,TET01J,
     &               FGZ,NFGZ)
          call PES_EQGWR(arww,arlww,tet01j,gw_n,cv_gw,1,APWW,AEWW,ASWW,
     &             ACS2WW,ADPDRWW,ADPDTWW,ADEDTWW,RJLIQ,RJGAS,RLJGAS)
          e01j=aeww(1)
          de0_pf(j)=(e0j-e01j)*euni
          de_pf(j)=de0_pf(j)

! Compute the pf-e relaxion time dt_pf:
          jm1=max(1,j-1)
          jp1=min(nn,j+1)
          drww=abs(r(j+1)-r(j))
          drmww=abs(r(j)-r(jm1))
          drpww=abs(r(j+2)-r(j+1))
          ww2=max(0.d0,cs2PJ*euni)
          dt_ss=drww/(sqrt(ww2)+floor)    ! sonic time after full jump
          ww0=drww/v(j)
          ww0=1.d0/(ww0+drmww/v(jm1))+1.d0/(ww0+drpww/v(jp1))
          ww1=p_cri(imat)*max(cz_pf,pPJj-p0j)
          dt_ra=sqrt(drww/(ww1*ww0+floor))      ! 2019-08-11
          dt_pf=tau_pf*min(dt_ra,dt_ss)
          q0_pf(j)=de0_pf(j)/dt_pf

          Te(j)=T_cri(imat)*tet01j
          Ti(j)=Te(j)
          ifupeos(j)=.true.
          ims_pf(j)=2

!##################################################################deb:
         write(40,9082) ncyc,time,j,p0j,pPJj,tetj,tetfl,tet01j,p01j,
     &    apww(1)-p01j,e0j-e01j,dt_ra,dt_ss,dt_pf,fgz,nfgz
 9082 format('PF-ki: nc=',I7,' t=',es13.6,' j=',I4,
     &' p0,pPJ=',2es10.3,' tet(0,PJ,01)=',3es10.3,' p01,dp=',es11.3,
     &es8.1,' de=',es9.2,' dt_ra,ss=',2es9.2,' dt_pf=',es9.2,' F=',
     &es8.1,' nF=',I3)
!##################################################################deb.
        ENDIF
        goto 1990

!----------------------------------------------------------------------
!@@        CASE(7) ! Tabular GLT-EOS.
!----------------------------------------------------------------------

        END SELECT

 1980   if(v(j).gt.v_cri(imat)) then
          ims_pf(j)=-1        ! pure MS-vapor
        else
          ims_pf(j)=1         ! pure MS-liquid
        endif
 1990   continue
      ENDDO                         ! end j-loop over mesh cells

 9000 tout=SECSCPU()
      sec(7)=sec(7)+max(0.d0,tout-tin)
      return

      contains

!**********************************************************************
!                         F5_T01_PF
!*********************************************************beg F5_T01_PF
      function F5_T01_PF(x_)
      implicit none
!======================================================================
!     This routine computes the function to be zeroed when computing
!     tet01j in cell j.

!     Called by: GZERA
!     Calls    : PES_EQGWR
!======================================================================
! Arguments:
      real(8) :: F5_t01_pf
      real(8),intent(in) :: x_

! Local variables:
      integer(4) :: nfpf_
      real(8) :: rliq_,rgas_,rlgas_,tww_
!======================================================================
      tww_=x_
      call PES_EQGWR(arww,arlww,tww_,gw_n,cv_gw,1,APWW,AEWW,ASWW,
     &             ACS2WW,ADPDRWW,ADPDTWW,ADEDTWW,RLIQ_,RGAS_,RLGAS_)
      F5_t01_pf=apww(1)-p01j

      return
      end function F5_T01_PF
!_________________________________________________________end F5_T01_PF
      end subroutine PFKIN
!_____________________________________________________________end PFKIN


!**********************************************************************
!                             DRIVE
!*************************************************************beg DRIVE
      subroutine DRIVE
      use COMDEI, only: csurf,dm,eionb,floor,iflbnd,iiflas,matnum,
     &    ndriv,njz,nz,qdriv,r,sec,tdrfin,time,Te,Ti,Tvaccd,v,wdriv,yi
      use COMTA3, only: Abeam
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     This routine calculates the rate of external heating (in addition
!     to an eventual radiation drive, set by the boundary radiation
!     temperatures TREX and TRLEX) of the target fluid in units
!     [TW/mg] -> cell-centered array QDRIV(1:nn);

! INPUT:
!     matnum(1:nn) -> material numbers for mesh cells;
!     r(1:n2)    -> left radii of mesh cells, in units mm;
!     v(1:nn)    -> specific volume, in units cm**3/g;
!     Te(1:nn),Ti(1:nn),Tr(1:nn) -> temperatures in keV;
!     yi(1:nn)   -> ionization degree;
!     dm(1:nn)   -> reduced cell masses.

! OUTPUT
!     QDRIV(1:nn) -> energy deposition rate, in units
!                    TW/mg=10**22 erg/g/s;

!     Called by:  DELIRA
!     Calls    :  BESTO2,SECSCPU
!======================================================================
! Local variables:
      integer(4),parameter :: iifdriv3=0
      integer(4) :: i,id,iz,j,jf,ji,jj,k
      real(8) :: bestop,ebk,ebn,ebr,eps,rab,roj,tin,tout
!======================================================================
      tin=SECSCPU()

      Tvaccd=0.d0
      ndriv=ndriv+1
      qdriv(:)=0.d0
      if(time.ge.tdrfin .or. wdriv.lt.floor) goto 9000
!@@ The value of TDRCAL may be changed here.

!----------------------------------------------------------------------
!     Step 1: Heavy-ion beam drive.
!----------------------------------------------------------------------
      if(eionb.lt.floor) goto 410
      eps=1.d-5*abeam
      ebk=eionb

! Forward-backward (along beam direction) loop:
      DO id=1,2
        if(id.eq.2.and.iflbnd.eq.-1) goto 410
! Loop over target layers:
        do iz=1,nz
          i=(nz-iz+1)*(2-id)+iz*(id-1)
! Loop over mesh cells:
          ji=njz(i)
          jf=njz(i+1)-1
          do jj=ji,jf
            j=(ji+jf-jj)*(2-id)+jj*(id-1)
! Integrate over the current cell, RUNGE-KUTTA-4:
            ebn=ebk
            rab=0.d0
            roj=1.d0/v(j)
            do k=1,4
              ebr=ebn-rab
              if(k.eq.4) ebr=ebr-rab
              call BESTO2(ebr,roj,Te(j),Ti(j),yi(j),matnum(j),BESTOP)
              rab=.5d0*roj*bestop*(r(j+1)-r(j))
              ebk=ebk-(k*(5-k)-2)*rab/6.d0
            enddo       ! k-loop
            if(ebk.le.eps) ebk=0.d0
            qdriv(j)=qdriv(j)+wdriv*(ebn-ebk)/(eionb*csurf*dm(j))
            if(ebk.le.eps) goto 410
          enddo         ! jj-loop
        enddo           ! iz-loop
      ENDDO             ! id-loop
 410  continue

!----------------------------------------------------------------------
!     Step 2: Laser-beam drive.
!----------------------------------------------------------------------
      if(iiflas.eq.0) goto 1990

 1990 continue

!----------------------------------------------------------------------
!     Step 3: Alternative way of energy deposition.
!----------------------------------------------------------------------
      if(iifdriv3.eq.0) goto 9000

 9000 tout=SECSCPU()
      sec(6)=sec(6)+max(0.d0,tout-tin)
      return
      end subroutine DRIVE
!____________________________________________________________end DRIVE





!**********************************************************************
!                             DRIVE_AX
!*************************************************************beg DRIVE_AX
      subroutine DRIVE_AX(r1,r2,power)
      use COMDEI, only: csurf,dm,eionb,floor,iflbnd,iiflas,matnum,
     &    ndriv,njz,nz,qdriv,r,sec,tdrfin,time,Te,Ti,Tvaccd,v,wdriv,yi
      use COMTA3, only: Abeam
      use MATKIT, only: SECSCPU

      implicit none
!======================================================================
!     This routine calculates the rate of external heating (in addition
!     to an eventual radiation drive, set by the boundary radiation
!     temperatures TREX and TRLEX) of the target fluid in units
!     [TW/mg] -> cell-centered array QDRIV(1:nn);

! INPUT:
!     matnum(1:nn) -> material numbers for mesh cells;
!     r(1:n2)    -> left radii of mesh cells, in units mm;
!     v(1:nn)    -> specific volume, in units cm**3/g;
!     Te(1:nn),Ti(1:nn),Tr(1:nn) -> temperatures in keV;
!     yi(1:nn)   -> ionization degree;
!     dm(1:nn)   -> reduced cell masses.

! OUTPUT
!     QDRIV(1:nn) -> energy deposition rate, in units
!                    TW/mg=10**22 erg/g/s;

!     Called by:  DELIRA
!     Calls    :  BESTO2,SECSCPU
!======================================================================
! Local variables:
      integer(4),parameter :: iifdriv3=0
      integer(4) :: i,id,iz,j,jf,ji,jj,k
      real(8) :: bestop,ebk,ebn,ebr,eps,rab,roj,tin,tout
      real(8) :: r1,r2,power
!======================================================================
      tin=SECSCPU()
      Tvaccd=0.d0
      ndriv=ndriv+1
      qdriv(:)=0.d0
      if(time.ge.tdrfin) goto 9000
!@@ The value of TDRCAL may be changed here.
      do iz=1,nz
        ji=njz(iz)
        jf=njz(iz+1)
         do jj=ji,jf
            if(r(jj)-r2>=0.0d0.and.r(jj)-r1<=0.0d0)  qdriv(jj)=power
         enddo
      enddo

      print*,r(1)
      print*,qdriv(1)
 9000 tout=SECSCPU()
      sec(6)=sec(6)+max(0.d0,tout-tin)
      return
      end subroutine DRIVE_AX
!____________________________________________________________end DRIVE






!**********************************************************************
!                             DRIVE_Gauss
!*************************************************************beg DRIVE_Gauss
      subroutine DRIVE_Gauss(Rc,power)
      use COMDEI, only: csurf,dm,eionb,floor,iflbnd,iiflas,matnum,
     &    ndriv,njz,nz,qdriv,r,sec,tdrfin,time,Te,Ti,Tvaccd,v,wdriv,yi
      use COMTA3, only: Abeam
      use MATKIT, only: SECSCPU

      implicit none
!======================================================================
!     This routine calculates the rate of external heating (in addition
!     to an eventual radiation drive, set by the boundary radiation
!     temperatures TREX and TRLEX) of the target fluid in units
!     [TW/mg] -> cell-centered array QDRIV(1:nn);

! INPUT:
!     matnum(1:nn) -> material numbers for mesh cells;
!     r(1:n2)    -> left radii of mesh cells, in units mm;
!     v(1:nn)    -> specific volume, in units cm**3/g;
!     Te(1:nn),Ti(1:nn),Tr(1:nn) -> temperatures in keV;
!     yi(1:nn)   -> ionization degree;
!     dm(1:nn)   -> reduced cell masses.

! OUTPUT
!     QDRIV(1:nn) -> energy deposition rate, in units
!                    TW/mg=10**22 erg/g/s;

!     Called by:  DELIRA
!     Calls    :  BESTO2,SECSCPU
!======================================================================
! Local variables:
      integer(4),parameter :: iifdriv3=0
      integer(4) :: i,id,iz,j,jf,ji,jj,k
      real(8) :: bestop,ebk,ebn,ebr,eps,rab,roj,tin,tout
      real(8) :: r1,r2,power,sigma,sigma2,Rc
!======================================================================
      tin=SECSCPU()
      sigma=0.4247d0
      sigma2=2.0d0*(sigma**2.0d0)
      Tvaccd=0.d0
      ndriv=ndriv+1
      qdriv(:)=0.d0
      if(time.ge.tdrfin) goto 9000
!@@ The value of TDRCAL may be changed here.
      do iz=1,nz
        ji=njz(iz)
        jf=njz(iz+1)-1
         do jj=ji,jf
            qdriv(jj)=power*exp(-(r(jj)-Rc)**2.0d0/sigma2)
         enddo
      enddo


 9000 tout=SECSCPU()
      sec(6)=sec(6)+max(0.d0,tout-tin)
      return
      end subroutine DRIVE_Gauss
!____________________________________________________________end DRIVE



!**********************************************************************
!                             KINBUR
!************************************************************beg KINBUR
      subroutine KINBUR
      use COMDEI, only: amol,amsfu,apro,asbol,cape,capeb,capi,capib,
     &    capr,caprb,ceiling,czTkin,czvkin,de_pf,de0_pf,dm,ethz,etvi0,
     &    etvi1,flle,flli,fllr,floor,Hblold,Hbrold,hial,hiall,hiei,
     &    hier,hip3,hip14,hip14n,Hz,Hzold,ifdmfu,iflbnd,ifpfkin,igeo,
     &    iifal,iifcapei,iifHz,iifn2,iifn14,iifp3,iifp14,iitemp,
     &    ims_pf,jflgburn,jtau140,lrun,lpro,matnum,n1,n1max,nfu,njz,
     &    nn,ntflle,ntflli,ntfllr,nz,pinum,propmat,q_pf,q0_pf,qdriv,
     &    qe,qi,qjl,qsDT,qsDD,qsDHe,qsbh,r,rorafu,rorfu,rorfum,sec,
     &    tau140,Tburn0,Te,Teold,Ti,Tr,ucbet,upro,v,vold,wn2,wn14,
     &    xD,xT,xmol,yi,zmol,zpro,
     &    clogaree,clogarei
      use COMTA3, only: Asub,Zsub
      use MATKIT, only: ELLICK,SECSCPU
      implicit none
!======================================================================
!     This routine calculates all the transport coefficients,
!     the thermonuclear burn and energy deposition rates.

! OUTPUT:
! - ei-coupling:
!     cape(:)   - electron therm.conduction coefficient;
!     capeb(:)  - limited electron therm.conduction coefficient;
!     capi(:)   - ion therm.conduction coefficient;
!     capib(:)  - limited ion therm.conduction coefficient;
!     hiei(:)   - Te-Ti relaxation rate;
!     etvi0(:)  - zero ion viscosity coefficient;
!     etvi1(:)  - first ion viscosity coefficient (see Braginskii);
! - r-coupling:
!     capr(:)   - radiation-energy conduction coefficient;
!     caprb(:)  - limited radiation-energy conduction coefficient;
!     hier(:)   - Te-Tr relaxation coefficient;
! - Hz-coupling:
!     ethz(:)   - electrical resistivity;
!     qjl(:)    - Joule heating rate;
! - tn-burn:
!     hial(:)   - alpha-particle energy relaxation rate;
!     hip3(:)   - 3-MeV-proton energy relaxation rate;
!     hip14(:)  - 14-MeV-proton energy relaxation rate;
!     qsdt(:)   - D+T fusion reaction rate;
!     qsdd(:)   - D+D fusion reaction rate;
!     qsdhe(:)  - D+He3 fusion reaction rate;
!     qsbh(:)   - B+H fusion reaction rate;
! - general:
!     qe(:)     - electron heating rate (per unit mass);
!     qi(:)     - ion heating rate (per unit mass);
!     q_pf(:)   - eventual pf-kinetics heating rate;
!     rorfu     = <rho*r> of the central fuel sphere;
!     rorafu    = <rho*r/Amean> of the central fuel sphere;

!     Called by:  DELIRA,JOBINIT
!     Calls    :  RELCON,ELLICK,SECSCPU
!======================================================================
! Local variables:
      integer(4) :: i,ifpro(3),ik,irab0,j,jf,ji,jk,jrab0,k
      real(8),parameter :: cpro(1:3)=(/.077d0,.12d0,.13d0/)
      real(8),allocatable :: rorafj(:)
      real(8) :: aal,amean,amol2,by,cl,efe,fehe3,fen2,fen14,fet,
     &      fystar,gjk,Hzast,Hzastp,Hzj,q2,q14,qn2,qn14,rab,rab0,
     &      rab00,rab1,rabc2,rabphi,rabtau,rabtaur,rj12,rk12,rle,rlem,
     &      rli,rlim,rlr,rlrm,rya,sqTfe,T13,Tej,Tfe,Tij,tin,Tm23,tout,
     &      Trj,u0,uvk,vj,wwi,xe,xe3,yae,yij,ystar,z,zal
      real(8) :: capeou,capiou,caprou,ethzou,etv0ou,etv1ou,yionou,
     &      hieiou,hierou
      real(8) :: tempCLEE=0.0d0,tempCLEI=0.0d0
      logical,allocatable :: ifrclc(:)
!======================================================================
      tin=SECSCPU()

!----------------------------------------------------------------------
!     Step 0: Initialize.
!----------------------------------------------------------------------
      allocate(ifrclc(1:nn))
      ifrclc(:)=.false.
      rorfu=0.d0
      rorafu=0.d0
! "Zero option" for the heating rates:
      qe(1:nn)=qdriv(1:nn)
      qi(1:nn)=0.d0
      wn2(:)=0.d0
      wn14(:)=0.d0

! Add eventual pf-kinetics heating:
      if(ifpfkin) then
        do j=1,nn
          if(ims_pf(j).ne.2) cycle
          wwi=1.d0/(yi(j)+1.d0)
          q_pf(j)=abs(q0_pf(j))*sign(1.d0,de_pf(j))
!@@          rab=1.d0-abs(de_pf(j)/de0_pf(j))
!@@          q_pf(j)=(1.d0-.5d0*rab)*abs(q0_pf(j))*sign(1.d0,de_pf(j))
          rab=wwi*q_pf(j)
          qe(j)=qe(j)+rab*yi(j)
          qi(j)=qi(j)+rab
        enddo
      endif

!----------------------------------------------------------------------
!     Step 1: EI-coupling physics: electron and ion thermal conduction,
!             temperature relaxation, viscosity.
!---------------------------------------------------------BEG EI-COUPL:
      DO I=1,nz
        Amean=Amol(I)/xmol(I)
        Amol2=Amol(I)**2
        ji=njz(I)
        jf=njz(I+1)-1

        do j=ji,jf
          z=propmat(28,matnum(j))
          vj=v(j)
          Tej=Te(j)
          Tij=Ti(j)
          Trj=Tr(j)
          yij=yi(j)
          if(yij.lt.1.d-9) yij=1.d-9
          if(yij.gt.z) yij=z
          ystar=yij
          if(yij.lt.1.d0) ystar=1.d0
          fystar=1.d0+yij-ystar
          yae=yij*Zmol(I)/(z*Amol(I))
          rya=yae/vj
          if(iifHz.ge.1) then
            Hzj=Hz(j)
          else
            Hzj=0.d0
          endif

! - prepare for the flux limiting:
          capeb(j)=flle*1280.d0*rya*Tej*sqrt(Tej)
          capib(j)=flli*29.97d0*Tij*sqrt(Tij/Amean)/(vj*Amean)
          caprb(j)=fllr*(asbol*ucbet*Trj**2)*Trj**2

! - <rho*r> of the central fuel sphere:
          if(j.le.nfu) then
            rab=(r(j+1)-r(j))/vj
            rorfu=rorfu+rab
            rorafu=rorafu+rab/Amean
          endif

!-------------------------------------------------------------beg_ei_1:
! Check the condition for recalculation of the transport coefficients
! in mesh cell J; affects recalculation of the fusion-products coupling
! coefficients hial(:),hip3(:),hip14(:) as well;
! if 'TRUE', goto 110:
          if(vold(j).lt.floor .or. Teold(j).lt.floor) goto 110
          rab=vj/vold(j)
          if(rab.lt.1.d0) rab=1.d0/rab
          if(rab.ge.1.d0+czvkin) goto 110
          rab=(Tej+Trj)/Teold(j)
          if(rab.lt.1.d0) rab=1.d0/rab
          if(rab.ge.1.d0+czTkin) goto 110
          if(iifHz.ge.1) then
            if(Hzold(j).lt.floor) goto 110
            rab=Hzj/Hzold(j)
            if(rab.lt.1.d0) rab=1.d0/rab
            if(rab.ge.1.d0+czvkin) goto 110
          endif
          cycle
 110      ifrclc(j)=.true.
          vold(j)=vj
          Teold(j)=Tej+Trj
          if(iifHz.ge.1) Hzold(j)=Hzj
!=============================================================end_ei_1.

!-------------------------------------------------------------beg_ei_2:
! "Zero option" for temperature relaxation, heat conduction, viscosity
! and resistivity coefficients:
! - ei-coupl:
          cape(j)=0.d0
          capi(j)=0.d0
          hiei(j)=ceiling
          etvi0(j)=0.d0
          etvi1(j)=0.d0
! - r-coupl:
          capr(j)=0.d0
          hier(j)=0.d0
! - Hz-coupl:
          if(iifHz.ge.1) ethz(j)=0.d0

! Electron and radiative temperature relaxation and heat conduction
! coefficients, electrical resistivity, ion viscosity:
          call RELCON(I,vj,Tej,Tij,Trj,Hzj,yij,YIONOU,CAPEOU,CAPIOU,
     &                HIEIOU,ETHZOU,CAPROU,HIEROU,ETV0OU,ETV1OU)
          if(j==1) then
          tempclee=clogaree
          tempclei=clogarei
          endif
!         print*,clogaree,clogarei
          cape(j)=capeou
          capi(j)=capiou
          hiei(j)=hieiou
          etvi0(j)=etv0ou
          etvi1(j)=etv1ou
          capr(j)=caprou
          hier(j)=hierou
          if(iifHz.ge.1) ethz(j)=ethzou
!=============================================================end_ei_2.
        enddo     ! j-loop
      ENDDO       ! I-loop
!     轴向的库伦对数
       clogaree=tempclee
       clogarei=tempclei
      if(rorfu.gt.rorfum) rorfum=rorfu

!-------------------------------------------------------------beg_Hz_1:
! Compute the Joule heating QJL(J) (to be used in TSTEP only):
      if(iifHz.eq.2) then
        qJl(:)=0.d0
        Hzastp=Hz(1)
        if(iflbnd.eq.-1) Hzastp=Hblold
        rab=(ucbet/(4.d0*pinum))**2
        do j=1,nn
          Hzast=Hzastp
          if(j.ne.nn) Hzastp=Hz(j)+(Hz(j+1)-Hz(j))*((r(j+1)-r(j))/
     &                       (r(j+2)-r(j)))
          if(j.eq.nn) Hzastp=Hbrold
          qjl(j)=rab*ethz(j)*v(j)*((Hzastp-Hzast)/(r(j+1)-r(j)))**2
        enddo
      endif
!=============================================================end_Hz_1.

! Impose flux limits;  CAPE,CAPI,CAPR  are non-limited conduction
! coefficients at the centers of mesh cells;   CAPEB,CAPIB,CAPRB  are
! flux limited coefficients at the interfaces between mesh cells:
!-------------------------------------------------------------beg_ei_4:
      IF(iifcapei.eq.1 .or. iifcapei.eq.3) THEN
        rle=capeb(1)
        do j=2,nn
          rlem=rle
          rle=capeb(j)
! - compute RAB=elec.cond.coefficient at the interface r=R(J):
          rab=.5d0*(cape(j-1)+cape(j))
! - impose flux limit:
          rab1=Te(j)-Te(j-1)
          if(rab1.gt.0.d0) rlem=rle
          rlem=rlem*.5d0*(r(j+1)-r(j-1))
          if(rab*abs(rab1).gt.rlem) then
            rab=rlem/abs(rab1)
            ntflle=ntflle+1
          endif
          capeb(j)=rab
        enddo
        capeb(1)=cape(1)
        capeb(n1)=0.d0
      ELSE
        capeb(:)=0.d0
      ENDIF
!=============================================================end_ei_4.

!-------------------------------------------------------------beg_ei_5:
      IF(iifcapei.eq.2 .or. iifcapei.eq.3) THEN
        rli=capib(1)
        do j=2,nn
          rlim=rli
          rli=capib(j)
! - compute RAB=ion cond.coefficient at the interface r=R(J):
          rab=.5d0*(capi(j-1)+capi(j))
! - impose flux limit:
          rab1=Ti(j)-Ti(j-1)
          if(rab1.gt.0.d0) rlim=rli
          rlim=rlim*.5d0*(r(j+1)-r(j-1))
          if(rab*abs(rab1).gt.rlim) then
            rab=rlim/abs(rab1)
            ntflli=ntflli+1
          endif
          capib(j)=rab
        enddo
        capib(1)=capi(1)
        capib(n1)=0.d0
      ELSE
        capib(:)=0.d0
      ENDIF
!=============================================================end_ei_5.

!-------------------------------------------------------------beg_r_6:
      IF(iitemp.eq.4) THEN
        rlr=caprb(1)
        do j=2,nn
          rlrm=rlr
          rlr=caprb(j)
! - compute RAB=rad.cond.coefficient at the interface r=R(J):
          rab=.5d0*(capr(j-1)+capr(j))
! - impose flux limit:
          rab1=tr(j)-tr(j-1)
          if(rab1.gt.0.d0) rlrm=rlr
          rlrm=rlrm*.5d0*(r(j+1)-r(j-1))
          if(rab*abs(rab1).gt.rlrm) then
            rab=rlrm/abs(rab1)
            ntfllr=ntfllr+1
          endif
          caprb(j)=rab
        enddo
        caprb(1)=capr(1)
        caprb(n1)=0.d0
      ELSE
        caprb(:)=0.d0
      ENDIF
!=============================================================end_r_6.
!=========================================================END EI-COUPL.

!----------------------------------------------------------------------
!     Step 4: Thermonuclear burn.
!-------------------------------------------------------------BEG BURN:
      if(jflgburn.le.-1) goto 499
!--------------------------------------------------------------beg_b_1:
! Check the onset of thermonuclear burn:
! JFLGBURN < 0 -> no check, no burn anticipated !
! JFLGBURN = 0 -> not burning yet;  JFLGBURN = 2 -> burning;
      if(jflgburn.lt.2) then
        do j=1,nn
          if(ifdmfu(j).ne.1) cycle
          if(Ti(j).gt.Tburn0) jflgburn=1
        enddo
      endif
!==============================================================end_b_1.

! "Zero option" for thermonuclear burn rates:
      qsDT(:)=0.d0
      qsDD(:)=0.d0
      qsDHe(:)=0.d0
      qsBH(:)=0.d0

      if(jflgburn.eq.0) then
! "Zero option" for energy dissipation by charged products:
        hiall(:)=ceiling
        goto 499
      endif

      allocate(rorafj(1:nn+1))
      ifpro(1)=iifal
      ifpro(2)=iifp3
      ifpro(3)=iifp14
      rorafj(1)=0.d0

      DO I=1,nz
        Amean=Amol(I)/xmol(I)
        Amol2=Amol(I)**2
        ji=njz(I)
        jf=njz(I+1)-1

        do j=ji,jf
          z=propmat(28,matnum(j))
          vj=v(j)
          Tej=Te(j)
          Tij=Ti(j)
          Trj=Tr(j)
          yij=yi(j)
          if(yij.lt.1.d-9) yij=1.d-9
          if(yij.gt.z) yij=z
          ystar=yij
          if(yij.lt.1.d0) ystar=1.d0
          fystar=1.d0+yij-ystar
          yae=yij*Zmol(I)/(z*Amol(I))
          rya=yae/vj

          if(j.le.nfu) then
            rab=(r(j+1)-r(j))/vj
            rorafj(j+1)=rorafj(j)+rab/Amean
          endif

!--------------------------------------------------------------beg_b_2:
! Coefficients of energy dissipation by charged fusion products
! HIAL, HIP3, HIP14:
          if(.not.ifrclc(j) .and. jflgburn.ne.1) goto 430
          efe=.026d0*rya**.6666666667d0
          Tfe=sqrt(Tej**2+(.6666666667d0*efe)**2)
          sqtfe=sqrt(Tfe)
          do k=1,3
            if(ifpro(k).ne.1) cycle
            u0=upro(k)
            xe=u0/(187.5d0*sqtfe)
            xe3=xe*xe*xe
            by=.353d0+xe*xe*(2.34d0+xe3)/(11.d0+xe3)
            aal=apro(k)
            zal=zpro(k)
            cl=7.207d0+.5d0*log((amean/(1.d0+amean/aal))**2*tfe*
     &         by*u0*u0/(rya*(1.d0+(39.d0*zal*ystar/u0)**2)))
            uvk=(ystar*Zmol(I)/(Amol(I)*z))**2*(143.d0/u0)**3*
     &          sqrt(1.d0+amean/aal)*cl*fystar*zal**2/aal
            rab=139.d0*tfe*by/sqrt(rya)
            cl=log(1.d0+rab/(1.d0+.5d0/sqrt(rab)))
          uvk=uvk+yae*(1746.d0/u0)**3*(zal**2/aal)*cl/(1.d0+1.33d0/xe3)
            jk=j+(k-1)*n1max
            hiall(jk)=uvk*(2.5d0-1.5d0/(1.d0+2.4d-3/(5.d-4+xe3)+
     &                cpro(k)*xe3))
            if(k.eq.3) then
! -  nuclear-scattering contribution to hip14:
              hip14n(I)=7.95d0/amean
              hiall(jk)=hiall(jk)+hip14n(I)
            endif
          enddo
 430      continue
!==============================================================end_b_2.

!--------------------------------------------------------------beg_b_3:
! Thermonuclear burn rates (recalculated in every hydrocycle):
          if(ifdmfu(j).ne.1 .or. Tij.lt.Tburn0) cycle
          T13=Tij**.333333333d0
          Tm23=1.d0/T13**2
          qsDT(j)=1.58d4*Tm23*((1.d0+.16d0*Tij)*
     &            exp(-19.98d0/T13-(Tij/10.34d0)**2)+
     &            .0108d0*exp(-45.07d0/Tij))
          qsDD(j)=81.4d0*Tm23*(1.d0+.01d0*Tij)*exp(-18.81d0/T13)

          qsDHe(j)=1.3d4*Tm23*(1.d0+5.d-4*Tij**2)*
     &             exp(-31.72d0/T13-(Tij/27.14d0)**2)+
     &             40.5d0*exp(-148.2d0/Tij)/sqrt(Tij)
          qsBH(j)=5.05d4*Tm23*(1.d0+.008d0*T13+.063d0*T13*T13+
     &            .0034d0*Tij+.0056d0*Tij*T13+7.9d-4*Tij*T13*T13)*
     &            exp(-53.423d0/T13-(Tij/174.06d0)**2)+
     &      (59.2d0*exp(-149.33d0/Tij)+6.51d4*exp(-618.45d0/Tij))/
     &      (Tij*sqrt(Tij))+3.336d2*Tm23*exp(-1094.d0/Tij)

! Local component of the energy deposition by charged fusion products:
          feT=7.d0/(7.d0+Tej)
          feHe3=5.6d0/(5.6d0+Tej)
          rab=.5d0*xD(j)**2*qsDD(j)/(vj*Amol2)
          qe(j)=qe(j)+rab*(.97d4*feHe3+.79d4*feT)
          qi(j)=qi(j)+rab*(.97d4*(1.d0-feHe3)+.79d4*(1.d0-fet))
!==============================================================end_b_3.
        enddo     ! j-loop
      ENDDO       ! I-loop
      if(jflgburn.eq.1) jflgburn=2

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!             - NEUTRONS -
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if(iifn2.eq.0 .and. iifn14.eq.0) goto 490
!------------------------------------------------------------- begin 61
! Prepare for the diffusion-like spread of 14-MeV neutrons:
      if(iifn14.ne.4) goto 440
      if(igeo.ne.2) then
       write(*,9010) iifn14,igeo
       write(lrun,9010) iifn14,igeo
       write(lpro,9010) iifn14,igeo
       STOP 'STOP in KINBUR-9010: iifn14=4.and.igeo.ne.2 is forbidden!'
      ENDIF

! Compute TAU140 and the total N14 power (=Q14):
      jrab0=1
      irab0=1
      rab0=-1.d120
      q14=0.d0
      I=1
      do j=1,nfu
        if(j.ge.njz(I+1)) I=I+1
        rab=qsdt(j)*xd(j)*xt(j)*dm(j)/(v(j)*Amol(I)**2)
        rab00=rab/(r(j+1)-r(j))
        if(rab00.gt.rab0) then
          jrab0=j
          Irab0=I
          rab0=rab00
         endif
         q14=q14+rab
      enddo

      I=Irab0
      do j=jrab0,nfu
       if(j.ge.njz(I+1)) I=I+1
       rab=qsdt(j)*xd(j)*xt(j)*dm(j)/(v(j)*Amol(I)**2*(r(j+1)-r(j)))
       if(rab.lt..5d0*rab0) then
        jtau140=j-1
        goto 438
       endif
      enddo
      jtau140=nfu
 438  tau140=rorafj(jtau140+1)/20.d0
      rabc2=.5d0*(tau140-1.d0+(tau140+1.d0)*exp(-2.d0*tau140))

 440  continue
!--------------------------------------------------------------- end 61

!-------------------------------------------------------------- begin 7
! Lay away for the uniformly distributed neutron heating to be
! calculated below:
      if(iifn2.ne.3.and.iifn14.ne.3) goto 450
      q2=0.d0
      q14=0.d0
      I=1
      do j=1,nfu
        if(j.ge.njz(I+1)) I=I+1
        if(iifn14.eq.3) q14=q14+qsDT(j)*xD(j)*xT(j)*dm(j)/
     &     (v(j)*Amol(I)**2)
        if(iifn2.eq.3) q2=q2+qsDD(j)*(xD(j)/Amol(I))**2*dm(j)/v(j)
      enddo
      if(iifn14.eq.3) q14=q14*13.6d4*rorafu/((rorafu+80.d0)*amsfu)
      if(iifn2.eq.3) q2=q2*2.37d4*.5d0*rorafu/((rorafu+26.d0)*amsfu)
!================================================================ end 7

 450  continue
! Main loop for neutron heating:
      DO I=1,nz
        amean=Amol(I)/xmol(I)
        amol2=Amol(I)**2
        ji=njz(I)
        jf=njz(I+1)-1

        do j=ji,jf
          if(ifdmfu(j).ne.1) cycle
          qn2=0.d0
          qn14=0.d0
          if(j.gt.nfu) goto 470

! Neutron heating of the central fuel sphere:
!-------------------------------------------------------------- begin 8
! - approximation of the  1-st scattering:
          if(iifn2.ne.1 .and. iifn14.ne.1) goto 455
          if(igeo.eq.0) goto 455
          rj12=.5d0*(r(j)+r(j+1))
          ik=1
          do k=1,nfu
            if(k.ge.njz(ik+1)) ik=ik+1
            rk12=.5d0*(r(k)+r(k+1))
            if(k.eq.j) rk12=(r(k)+2.d0*r(k+1))/3.d0
            if(igeo.eq.2) gjk=log((rk12+rj12)/abs(rk12-rj12))/(rk12*
     &                        rj12)
            if(igeo.eq.1) gjk=(2.d0/(rj12+rk12))*
     &                        ELLICK(2.d0*sqrt(rk12*rj12)/(rk12+rj12))
            if(iifn14.eq.1) qn14=qn14+(1100.d0/amean)*qsdt(k)*xd(k)*
     &                           xt(k)*gjk*dm(k)/(v(k)*Amol(ik)**2)
            if(iifn2.eq.1) qn2=qn2+(325.d0/amean)*qsdd(k)*(xd(k)/
     &                         Amol(ik))**2*gjk*dm(k)/v(k)
          enddo
!================================================================ end 8

! Branching for 14-MeV neutrons:
 455      if(iifn14.eq.0) goto 460
          goto (460,456,457,458),iifn14
! - local deposition:
 456      qn14=13.6d4*qsdt(j)*xd(j)*xt(j)/(v(j)*amol2)
          goto 460
! - uniformly distributed deposition:
 457      qn14=q14
          goto 460
! - deposition spread according to the diffusion profile:
 458      continue
          rabtau=(rorafj(j)+rorafj(j+1))/40.d0
          if(j.eq.1) then
            rabtaur=rorafj(j+1)/(20.d0*r(j+1))
          else
            rabtaur=(rorafj(j)/r(j)+rorafj(j+1)/r(j+1))/40.d0
          endif

          if(rabtau.lt.tau140) then
            rabphi=1.d0-.5d0*(1.d0+tau140)*(exp(rabtau-tau140)-
     &      exp(-rabtau-tau140))/rabtau
          else
            rabphi=rabc2*exp(tau140-rabtau)/rabtau
          endif
          rabphi=rabphi*3.d0/tau140**3
          qn14=13.6d4*q14*rabphi*rabtaur**2/(20.d0*amean)

! Branching for 2-Mev neutrons:
 460      if(iifn2.eq.0) goto 480
          goto (480,462,464,466),iifn2
! - local deposition:
 462      qn2=2.37d4*.5d0*qsDD(j)*xD(j)**2/(v(j)*amol2)
          goto 480
! - uniform deposition:
 464      qn2=q2
          goto 480
! - reserve:
 466      continue
          goto 480

! Neutron heating in shells detached from the central fuel sphere:
 470      if(iifn14.gt.0) qn14=13.6d4*qsDT(j)*xD(j)*xT(j)/(v(j)*amol2)
          if(iifn2.gt.0) qn2=2.37d4*.5d0*qsDD(j)*xD(j)**2/(v(j)*amol2)

! Distribute neutron heating over mesh cells:
 480      rab=(1.d0+Amean)**2*te(j)
          fen14=750.d0/(750.d0+rab)
          fen2=130.d0/(130.d0+rab)
          qe(j)=qe(j)+qn14*fen14+qn2*fen2
          qi(j)=qi(j)+qn14*(1.d0-fen14)+qn2*(1.d0-fen2)
! - for energy balance:
          wn14(I)=wn14(I)+qn14*dm(j)
          wn2(I)=wn2(I)+qn2*dm(j)
        enddo     ! j-loop
      ENDDO       !I-loop
 490  continue

      deallocate(rorafj)
 499  continue
!=============================================================END BURN.

      deallocate(ifrclc)

 9000 tout=SECSCPU()
      sec(1)=sec(1)+max(0.d0,tout-tin)
      return
 9010 FORMAT(/'STOP in KINBUR: IIFN14=',I6,'  IGEO=',I6
     &/'whereas (IIFN14=4 and IGEO.NE.2) is unacceptable!')
      end subroutine KINBUR
!____________________________________________________________end KINBUR


!**********************************************************************
!                       PRINHIS
!***********************************************************beg PRINHIS
      subroutine PRINHIS
      use COMDEI, only: dthist,floor,istart,jobtit,lhist,nn,pathoutpt,
     &    pe,pi,thist,time,u
      implicit none
!======================================================================
!     This routine prints out data for history plots.

!     Called by:  DELIRA
!     Calls    :  ...
!======================================================================
! Local variables:
      integer(4) :: i,j,jmin,lww
      real(8) :: ww0,ww1,wwns
      character(256) :: fnam
      logical :: lexist,lopened
!======================================================================
      wwns=1.d1
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@for_user_to_edit:
      lww=lhist
      fnam=TRIM(pathoutpt)//'/his-u_br-p_min.dat'
      inquire(lww, opened=lopened)
      inquire(file=TRIM(fnam),exist=lexist)
      if(.not.lopened) then
        if(istart.eq.0 .or. .not.lexist) then
! - open for the first time to start writing:
          open(lww,file=TRIM(fnam),form='formatted')
          write(lww,'(a5,a)') 'Job: ',jobtit
          write(lww,9002)
!@@          write(lww,'(a,11x,a,8x,a,9x,a,9x,a)') '   t','u_br','p_870',
          write(lww,'(a,11x,a,8x,a,9x,a)') '   t','u_br',
     &       'p_min','j_min'
        else
! - open to continue writing:
          open(lww,file=TRIM(fnam),form='formatted',
     &                     status='old',position='append')
        endif
      endif

!----------------------------------------------------------------------
!     Step 2: Determine whether to write out history.
!----------------------------------------------------------------------
      if(time.lt.thist) goto 9000
!      dthist=1.d-3
      dthist=1.d-4
      thist=max(time,thist+dthist)

!----------------------------------------------------------------------
!     Step 3: History of outer-edge velocity
!----------------------------------------------------------------------
! - find the minimum pressure:
      ww1=1.d99
      jmin=0
      do j=1,nn
        ww0=pe(j)+pi(j)
        if(ww0.lt.ww1) then
          ww1=ww0
          jmin=j
        endif
      enddo
!@@      write(lww,'(es13.6,es12.4,2es14.6,I7)') time,u(nn),
!@@     & pe(870)+pi(870),ww1,jmin
      write(lww,'(es13.6,es12.4,es14.6,I7)') time,u(nn),
     & ww1,jmin
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@end_user-edit.

 9000 return
 9002 format(79('-'))
      end subroutine PRINHIS
!___________________________________________________________end PRINHIS


!**********************************************************************
!                       PRINOUT
!***********************************************************beg PRINOUT
      subroutine PRINOUT(prinww,lww)
      use COMDEI, only: Amol,amz,asbol,cape,capi,capr,csurf,cz_pf,
     &    czdt,czddt,czdtq,czdtvt,czvkin,czTkin,de_pf,dfloor,
     &    dm,dt,dt0,dtmax,dtmin,eal,eall,ee,ei,eionb,eoutal,eouthz,
     &    eoute,eouti,eoutp3,eoup14,eoutr,eoutw,ep3,ep14,erex,erin,
     &    ez0,ezal,ezcl,ezdr,ezfus,ezjl,ezn2,ezn14,ezp3,ezp14,
     &    Hblold,Hbrold,hiall,hiei,hier,Hz,Hz0,
     &    i1geo,iartvis,ifburn,ifdmfu,iflbnd,ifmfu0,ifpfkin,iifopac,
     &    iifal,iifcapei,iifHz,iifn2,iifn14,iifp3,iifp14,iitemp,
     &    imatlist,imatz0,jtau140,kalpri,lrun,n1,n1max,ncycfin,
     &    ndriv,njpri,njz,nmatrls,nn,nnz,nprin,nrun,ntbad,nddump,
     &    ntflle,ntflli,ntfllr,ncycdpri,ntvbad,ncyc,nz,
     &    pblold,pbrold,pe,pi,pinum,propmat,qe,qi,r,rorfu,rorfum,roz0,
     &    rz0,smu1,smu2,tau_pf,tau140,tdrfin,Te,tfin,Ti,time,tmu1,tmu2,
     &    tnun2,tnun14,tnudhe3,Tr,Trex,Trlex,u,undef,v,wdriv,
     &    xb0,xbfloo,xd,xd0,xB,xB0,xH,xH0,xhe,xhe0,xmol,xt,xt0,yi
      use COMTA3, only: Asub,Zsub,p00,e00,nroo,roill,hlroo,nTemm,
     &    Temill,hlTemm,abeam,zbeam
      implicit none
!======================================================================
!     This routine performs all regular printouts.

!     Called by:  DELIRA
!     Calls    :  none
!======================================================================
! Arguments:
      integer(4),intent(in) :: lww
      character(*),intent(in) :: prinww

! Local variables:
      integer(4) :: i,imat,iww,iww1,j,jf,jhi1,ji,jm1,jrab,k
      real(8),allocatable :: ptot(:)
      real(8) :: Aeos,Amean,eblnc,emt(31),emtl(8),emz(1:nnz,14),et_pf,
     &      ez_pf(1:nnz),rab,rab1,totdhe3,totn14,totn2,Zeos,Zmean

      character(3),parameter :: chww_pf(3)=(/'-MS','-EQ','-ki'/)
      character(6),parameter :: chwweos(9)=(/'id.gas',' Fermi','DEITA3'
     &,'linear',' GWEOS','      ','GLTf17','      ','      '/)
      character(6),parameter :: lgeo(3)=(/'PLANAR','CYLIND','SPHERE'/),
     &      lhz(2)=(/'NO    ','YES   '/),
     &      lneu(5)=(/'IGNORE','1 SCAT','LOCAL ','UNIFRM','D_PROF'/),
     &      lal(3)=(/'IGNORE','DIFFUS','LOCAL '/),
     &      lal1(3)=(/' E ALF','  E P3',' E P14'/),
     &      lal2(3)=(/'  HIAL','  HIP3',' HIP14'/),
     &      lal3(3)=(/'  HIER','  HIEI','  HZ  '/)
      character(17),parameter :: llbnd(3)=(/'"fixed center",  ',
     &      '"void cavity",   ','"open halfspace",'/)
!======================================================================

      SELECT CASE(TRIM(prinww))
!----------------------------------------------------------------------
      CASE('prinun') ! Print units
!----------------------------------------------------------------------
      write(lww,9210)
      write(lww,9212)
      write(lww,9213)
      goto 9000

!----------------------------------------------------------------------
      CASE('prinus') ! Print EOS info
!----------------------------------------------------------------------
      write(lww,9219)
      jf=0
      k=0

      do I=1,5
        if(Zsub(I).lt..5d0) cycle
        write(lww,9220) i,Zsub(i),Asub(i)
        rab=11.206d0*asub(I)*exp(roill(I))
        rab1=rab*exp(hlroo(I)*(nroo(I)-1))
        ji=jf+1
        jf=jf+3*nroo(I)*ntemm(3,I)
        j=k+1
        k=k+4*nroo(I)
        write(lww,9230) rab,rab1,ji,jf
        rab=.02721d0*exp(temill(1,I))
        rab1=rab*exp(hltemm(1,I)*(ntemm(1,I)-1)+hltemm(2,I)*
     &     (ntemm(2,I)-ntemm(1,I))+hltemm(3,I)*(ntemm(3,I)-ntemm(2,I)))
        write(lww,9240) rab,rab1,j,k
        rab=2.942d0*p00(I)
        rab1=2.942d0*e00(I)/(11.206d0*Asub(I))
        write(lww,9250) rab,rab1
      enddo
      goto 9000

!----------------------------------------------------------------------
      CASE('prinbe') ! Print ion-beam-drive info
!----------------------------------------------------------------------
      write(lww,9260) Zbeam,Abeam
      goto 9000

!----------------------------------------------------------------------
      CASE('prinst') ! Print start-up info
!----------------------------------------------------------------------
! Main run parameters:
      j=1
      if(iflbnd.eq.1) j=2
      if(iflbnd.eq.-1) j=3
      write(lww,9270) nrun,lgeo(i1geo),llbnd(j),rz0(1),njpri,nddump
      if(ifburn) then
        write(lww,'(a,I3,3x,a)') 'Temperatures: IITEMP=',iitemp,
     &  'THERMNCL.BURN: YES'
        write(lww,9280) lneu(iifn14+1),lneu(iifn2+1),lal(iifal+1),
     &                  lal(iifp3+1),lal(iifp14+1)
      else
        write(lww,'(a,I3,a,3x,a)') 'Temperatures: IITEMP=',iitemp,',',
     &  'thermonuclear burn: NO'
      endif
      write(lww,9300) eionb,wdriv,tdrfin
      write(lww,9305) lhz(min(1,iifHz)+1),Hz0,iifopac
      if(ifpfkin) write(lww,9306) cz_pf,tau_pf
      write(lww,9307) iartvis,smu1,smu2,tmu1,tmu2
      write(lww,9335) czdt,czddt,czvkin,czTkin,czdtq,czdtvt
      write(lww,9337) tfin,dt0,dtmin,dtmax,ncycfin

      write(lww,9302)
      write(*,9302)
      do k=1,nmatrls
        imat=imatlist(k)
        iww=int(propmat(1,imat)+dfloor)
        iww1=int(propmat(10,imat)+dfloor)+1
        write(lww,'(I4,I5,a9)') imat,iww,chwweos(iww)//chww_pf(iww1)
        write(*,'(I4,I5,a9)') imat,iww,chwweos(iww)//chww_pf(iww1)
      enddo

      if(ifburn) then
        write(lww,9310)
      else
        write(lww,9311)
      endif
      if(ncyc.eq.0) write(*,9311)

      do I=1,nz
        j=njz(I+1)-1
        rab=csurf*dm(j)
        rab1=dm(j)/dm(j-1)
        j=j+1-njz(I)
        if(ifburn) then
          write(lww,9320) I,imatz0(I),j,amz(I),rab,rab1,roz0(I),
     &      rz0(I+1),xd0(I),xt0(I),xhe0(I),xh0(I),xb0(I)
        else
          write(lww,9322) I,imatz0(I),j,amz(I),rab,rab1,roz0(I),
     &      rz0(I+1)
        endif
        if(ncyc.eq.0) write(*,9322) I,imatz0(I),j,amz(I),rab,rab1,
     &     roz0(I),rz0(I+1)
      enddo
      if(ncyc.eq.0) write(*,*)
      goto 9000

!----------------------------------------------------------------------
      CASE('prinoo','prinoij') ! Print layer-by-layer, cell-by-cell
!----------------------------------------------------------------------
!  EMT(K) is the total (summed over layers) mass (energy) of sort 'K';
!  EMZ(I,K) is mass (energy) of sort 'K' in target layer 'I';

!  EMT(1)   = fluid mass;
!  EMT(2)   = mass of D in fluid;
!  EMT(3)   = mass of T in fluid;
!  EMT(4)   = mass of He3 in fluid;
!  EMT(5)   = <rho*r> of fluid;
!  EMT(6)   = EMT(7)+EMT(8)+...+EMT(14) = total energy in target;
!  EMT(7)   = kinetic energy of fluid;
!  EMT(8)   = electron internal energy of fluid;
!  EMT(9)   = ion internal energy of fluid;
!  EMT(10)  = energy in radiation field;
!  EMT(11)  = energy in magnetic field Hz;
!  EMT(12)  = energy in fast alphas;
!  EMT(13)  = energy in fast 3-MeV protons;
!  EMT(14)  = energy in fast 14-MeV protons;
!  EMT(15)  = external (drive) energy deposition into fluid;
!  EMT(16)  = energy deposition into fluid through Joule heating;
!  EMT(17)  = total energy liberation in fusion reactions;
!  EMT(18)  = local en.depositn into fluid from slow charged products;
!  EMT(19)  = energy deposition into fluid from 14-MeV neutrons;
!  EMT(20)  = energy deposition into fluid from 2.5-MeV neutrons;
!  EMT(21)  = energy generation in the form of 3.5-MeV alphas;
!  EMT(22)  = energy generation in the form of 3-MeV protons;
!  EMT(23)  = energy generation in the form of 14-MeV protons;
!  EMT(24)  = energy leak from target through PdV work;
!  EMT(25)  = energy leak from target through electron conduction;
!  EMT(26)  = energy leak from target through ion conduction;
!  EMT(27)  = energy leak from target through radiation;
!  EMT(28)  = energy leak from target through Hz magnetic field;
!  EMT(29)  = energy leak from target through 3.5-MeV alphas;
!  EMT(30)  = energy leak from target through 3-MeV protons;
!  EMT(31)  = energy leak from target through 14-MeV protons;
!......................................................................
      nprin=nprin+1
      allocate(ptot(1:nn+1))

! Compute various balance quantities:
!@@      call UPDEOS
      emt(:)=0.d0
      et_pf=0.d0
      eblnc=0.d0
      totn2=0.d0
      totn14=0.d0
      totdhe3=0.d0
      dm(n1)=dm(nn)

      emz(:,:)=0.d0
      ez_pf(:)=0.d0
      DO I=1,nz
        ji=njz(I)
        jf=njz(I+1)-1

        do j=ji,jf
          ptot(j)=pe(j)+pi(j)+asbol*Tr(j)**4/3.d0
          rab=csurf*dm(j)
          emz(I,1)=emz(I,1)+rab
          if(ifpfkin) ez_pf(I)=ez_pf(I)+rab*de_pf(j)
          if(ifburn) then
            if(ifdmfu(j).eq.1) rab1=rab/Amol(I)
            emz(I,2)=emz(I,2)+2.d0*xd(j)*rab1
            emz(I,3)=emz(I,3)+3.d0*xt(j)*rab1
            emz(I,4)=emz(I,4)+3.d0*xhe(j)*rab1
          endif
          emz(I,5)=emz(I,5)+(r(j+1)-r(j))/v(j)
          jm1=max(1,j-1)
          emz(I,7)=emz(I,7)+csurf*.125d0*((dm(jm1)+dm(j))*u(j)**2+
     &             (dm(j)+dm(j+1))*u(j+1)**2)
          emz(I,8)=emz(I,8)+rab*ee(j)
          emz(I,9)=emz(I,9)+rab*ei(j)

          rab=rab*v(j)
          emz(I,10)=emz(I,10)+rab*asbol*Tr(j)**4
          if(iifHz.ge.1) emz(I,11)=emz(I,11)+rab*Hz(j)**2/(8.d0*pinum)
          if(ifburn) then
            emz(I,12)=emz(I,12)+rab*eal(j)
            emz(I,13)=emz(I,13)+rab*ep3(j)
            emz(I,14)=emz(I,14)+rab*ep14(j)
          endif
        enddo

        et_pf=et_pf+ez_pf(I)
        do k=7,14
          emz(I,6)=emz(I,6)+emz(I,k)
        enddo
        do k=1,14
          emt(k)=emt(k)+emz(I,k)
        enddo
        emt(15)=emt(15)+ezdr(I)
        emt(16)=emt(16)+ezjl(I)
        emt(17)=emt(17)+ezfus(I)
        emt(18)=emt(18)+ezcl(I)
        emt(19)=emt(19)+ezn14(I)
        emt(20)=emt(20)+ezn2(I)
        emt(21)=emt(21)+ezal(I)
        emt(22)=emt(22)+ezp3(I)
        emt(23)=emt(23)+ezp14(I)

        totn2=totn2+tnun2(I)
        totn14=totn14+tnun14(I)
        totdhe3=totdhe3+tnudhe3(I)
        eblnc=eblnc+ez0(I)
      ENDDO

! Energies that leaked out through the right boundary:
      emt(24)=eoutw(nz+1)
      emt(25)=eoute(nz+1)
      emt(26)=eouti(nz+1)
      emt(27)=eoutr(nz+1)
      emt(28)=eouthz(nz+1)
      emt(29)=eoutal(nz+1)
      emt(30)=eoutp3(nz+1)
      emt(31)=eoup14(nz+1)

! Energies that entered target through the left boundary:
      emtl(1)=eoutw(1)
      emtl(2)=eoute(1)
      emtl(3)=eouti(1)
      emtl(4)=eoutr(1)
      emtl(5)=eouthz(1)
      emtl(6)=eoutal(1)
      emtl(7)=eoutp3(1)
      emtl(8)=eoup14(1)

!@@      CALL KINBUR
      write(lww,9400) ncyc,time,dt,ndriv,ntbad,ntvbad,ntflle,
     &      ntflli,ntfllr
! Thermonuclear energy gain:
      rab=emt(15)+abs(emt(15))+eblnc+abs(eblnc)+erin+abs(erin)
      if(rab.gt.0.d0) rab=2.d0*emt(17)/rab
! Energy balance:
      eblnc=emt(6)-eblnc-emt(15)-
     &      emt(18)-emt(19)-emt(20)-emt(21)-emt(22)-emt(23)+
     &      emt(24)+emt(25)+emt(26)+emt(27)+emt(28)+emt(29)+emt(30)+
     &      emt(31)-emtl(1)-emtl(2)-emtl(3)-emtl(4)-emtl(5)-emtl(6)-
     &      emtl(7)-emtl(8)+et_pf
      write(lww,9410) emt
      write(lww,9420) pblold,pbrold,hblold,hbrold,Trlex,Trex,erex,erin,
     &      Tr(nn),rorfu,rorfum,totn2,totn14
      if(ifpfkin) then
        if(ifburn) then
          write(lww,9422) eblnc,rab,totdhe3,et_pf
        else
          write(lww,'(2(a,es11.4))') 'EBLNC=',eblnc,' ETOT_PF=',et_pf
        endif
      else
        if(ifburn) then
          write(lww,9424) eblnc,rab,totdhe3
        else
          write(lww,'(a,es11.4)') 'EBLNC=',eblnc
        endif
      endif

      if(iifn14.eq.4) write(lww,9404) jtau140,tau140
      write(*,9402) nprin,ncyc,time,dt
      write(lrun,9402) nprin,ncyc,time,dt
      if(TRIM(prinww).eq.'prinoo') then
        deallocate(ptot)
        goto 9000
      endif

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Further on -> CASE('prinoij') only:
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      write(lww,9412) emtl
! Print information on each layer:
      DO I=1,nz
        Aeos=max(0.d0,propmat(25,imatz0(I)))
        Zeos=max(0.d0,propmat(26,imatz0(I)))
        Amean=propmat(27,imatz0(I))
        Zmean=propmat(28,imatz0(I))
        write(lww,9430) I,Zeos,Aeos,Zmean,Amean,time
        eblnc=emz(I,6)-ez0(I)-ezdr(I)-ezcl(I)-ezn14(I)-ezn2(I)-
     &        ezal(I)-ezp3(I)-ezp14(I)+
     &        eoutw(I+1)+eoute(I+1)+eouti(I+1)+eoutr(I+1)+eouthz(I+1)+
     &        eoutal(I+1)+eoutp3(I+1)+eoup14(I+1)-
     &        eoutw(I)-eoute(I)-eouti(I)-eoutr(I)-eouthz(I)-
     &        eoutal(I)-eoutp3(I)-eoup14(I)+ez_pf(I)

        write(lww,9410) (emz(I,k),k=1,14),ezdr(I),ezjl(I),ezfus(I),
     &      ezcl(I),ezn14(I),ezn2(I),ezal(I),ezp3(I),ezp14(I),
     &      eoutw(I+1),eoute(I+1),eouti(I+1),eoutr(I+1),eouthz(I+1),
     &      eoutal(I+1),eoutp3(I+1),eoup14(I+1)
        if(ifburn) then
          if(ifpfkin) then
           write(lww,9436) tnun2(I),tnun14(I),tnudhe3(I),eblnc,ez_pf(I)
          else
            write(lww,9435) tnun2(I),tnun14(I),tnudhe3(I),eblnc
          endif
        else
          if(ifpfkin) then
           write(lww,'(4x,2(a,es11.4))') 'EBLNC=',eblnc,
     &           ' EZ_PF=',ez_pf(I)
          else
            write(lww,'(4x,a,es11.4)') 'EBLNC=',eblnc
          endif
        endif

        jhi1=1
        if(iitemp.lt.4) jhi1=2
        if(iifHz.ge.1) jhi1=3
        write(lww,9440) lal3(jhi1)
        if(I.eq.1) then
          jrab=0
          write(lww,9451) jrab,r(1),u(1)
        endif
        ji=njz(I)
        jf=njz(I+1)-1

        do j=ji,jf
          if(njpri*(j/njpri).ne.j.and.j.ne.ji.and.j.ne.jf) cycle
          rab=1.d0/v(j)
          if(iifHz.ge.1) then
            write(lww,9450) j,r(j+1),u(j+1),rab,Te(j),Ti(j),Tr(j),
     &            ptot(j),hz(j)
          elseif(jhi1.eq.1) then
            write(lww,9450) j,r(j+1),u(j+1),rab,Te(j),Ti(j),Tr(j),
     &            ptot(j),hier(j)
          elseif(jhi1.eq.2) then
            write(lww,9450) j,r(j+1),u(j+1),rab,Te(j),Ti(j),Tr(j),
     &            ptot(j),hiei(j)
          endif
        enddo

        if(.not.ifburn .and. iifcapei.eq.0 .and. iitemp.lt.4) cycle
        if(xb0(I).lt.xbfloo) then
          write(lww,9442) lal1(kalpri),lal2(kalpri)
        else
          write(lww,9444) lal1(kalpri),lal2(kalpri)
        endif
        do j=ji,jf
          if(njpri*(j/njpri).ne.j.and.j.ne.ji.and.j.ne.jf) cycle
          k=j+(kalpri-1)*n1max
          rab1=qe(j)+qi(j)
          if(xB0(I).lt.xbfloo) then
            write(lww,9452) j,yi(j),cape(j),capi(j),capr(j),eall(k),
     &          hiall(k),xD(j),xT(j),rab1
          else
            write(lww,9452) j,yi(j),cape(j),capi(j),capr(j),eall(k),
     &          hiall(k),xH(j),xB(j),rab1
          endif
        enddo
      ENDDO
      deallocate(ptot)
!......................................................................
!  Electron QE(J) and ion QI(J) heating rates do not include the energy
!  deposition by al,p3,p14:
!     QE=QDRIV + QCHARGEDLOCAL_e + QNEUTRON_e
!     QI=QCHARGEDLOCAL_i + QNEUTRON_i
!......................................................................

!----------------------------------------------------------------------
      CASE DEFAULT
!----------------------------------------------------------------------

      STOP 'STOP in PRINOUT: unrecognized argument PRINWW !'

      END SELECT

 9000 return

 9210 format('DEIRA units: ','[time]',13X,'= 1.E-8 sec'/
     &13X,'[length]           = 0.1 cm = 1 mm'/
     &13X,'[velocity]         = 1.E+7 cm/s'/
     &13X,'[mass]             = 1 mg/mm**(2-IGEO)'/
     &13X,'[energy]           = 1.E+11 ergs/mm**(2-IGEO)'
     &' = 10 kJ/mm**(2-IGEO)'/
     &13X,'[density]          = 1 g/cm**3'/
     &13X,'[pressure]         = 1.E+14 ergs/cm**3'/
     &13X,'[temperature]      = 1 keV = 1.16E+7 K'/
     &13X,'[magnetic field]   = 1.E+7 Gauss'/
     &13X,'[power]            = 1 TW/mm**(2-IGEO)'/
     &13X,'[spec. power]      = 1.E+22 ergs/(g*sec) = 1 TW/mg'/
     &13X,'[heat cond.coeff.] = 1.E+20 ergs/(sec*cm*keV)'/
     &13X,'[elec.resistivity] = 1.E-8 sec'/
     &13X,'[<RO*R>]           = 1 mg/mm**2 = 0.1 g/cm**2')
 9212 format(79('=')/20x,'EXPLANATIONS TO THE OUTPUT'/
     &'MASS: TOT=total target (layer) mass; D=mass of deuterium; '/
     &'      T=mass of tritium; HEL3=mass of helium-3.'/
     &'ENERGY: TOT=total target (layer) energy; KIN=kinetic energy;'/
     &'        EL=electron, IONS=ion, R=radiation, H=magnetic field'/
     &'        components of the internal energy;'/
     &'        AL=amount of energy in fast (3.5-MeV) alpha particles;'/
     &'        P3=energy in 3-MeV protons, P14=in 14-MeV protons;'/
     &'HEATING: DR=ext.-drive energy deposited in the target (layer);'/
     &'         JL=Joule heating; TN=total amount of thermonuclear'/
     &'         energy released; CH=energy deposited locally by'/
     &'         charged fusion products; N14(N2)=energy deposited by'/
     &'         14(2)-MeV neutrons;'/
     &'DEP TO: AL(P3)(P14)=energy released in the form of 3.5-MeV'/
     &'        alphas (3-MeV protons) (14-MeV protons)')
 9213 format(
     &'LEAK: various components of energy transported outward across'/
     &'      the outer (right) boundary of the target (of a layer);'/
     &'Left influx: the same components of energy transported inward'/
     &'             across the left boundary of the target;'/
     &'BNDRS: P_bl (P_br) = left (right) boundary pressure,'/
     &'       H_bl (H_br) = left (right) boundary magnetic field;'/
     &'       TR_lex (TR_ex) = temperature of the external radiation ',
     &'field,'/7x,'driving the target from the left (right) boundary'/
     &7x,'EREX = time integral of (sigma*TRLEX**4*area+sigma*TREX**4*',
     &'area) ='/12x,
     &'= energy of external radiation field that enters the target;'/
     &7x,'ERIN = EOUTR(1)-EOUTR(NZ+1) = net radiative energy input ',
     &'into target;'/'Tr(nn) = rad.temperature in the last cell j=nn;'/
     &'RORF (RORF_m) = <RO*R> (maximum <RO*R>) of the central fuel;'/
     &'NN2 [NN14] = total number of 2.5-MeV [14-MeV] neutrons ',
     &'generated;'/'EBLNC = absolute energy balance in the target ',
     &'(a layer);'/'GAIN = thermonuclear energy gain of the target;'/
     &'column Q gives QE+QI, where'
     &/5x,'QE=QDRIV+QCHARGEDLOCAL_e + QNEUTRON_e,'
     &/5x'QI=QCHARGEDLOCAL_i + QNEUTRON_i'/79('='))
 9219 format(/20x,'PARAMETERS OF THE "DEITA3" EOS TABLES:')
 9220 format('Substance',I2,': Z=',F5.1,5X,'A=',F6.2)
 9230 format(13X,'RO_min=',es9.2,5X,'RO_max=',es9.2
     &,8X,'FARR:',I6,' -',I6)
 9240 format(13X,'T_min=',es10.2,5X,'T_max=',es10.2
     &,8X,'QURS:',I6,' -',I6)
 9250 format(13X,'P00=',es12.2,5X,'E00=',es12.2)
 9260 format('Ion beam: ZBEAM =',F5.1,5X,'ABEAM =',F6.2)
 9270 format(/79('=')/79('=')/'Run',I11,2X,A6,': left boundary=',A17,
     &' R(1)=',es16.8/'Print every',I6,'-th mesh cell, dump every',
     &I11,'-th time step.')
 9280 format('Fusion products: n14-',a6,'  n2-',a6,
     &'  alpha-',a6,'  p3-',a6,'  p14-',a6,'.')
 9300 format('Ion driver: EIONB=',es11.4,'GeV,  WDRIV='
     &,es11.4,'TW,  TDRFIN=',es13.6)
 9302 format(/25x,'ENGAGED MATERIALS'/'imat EOS# EOS-type')
 9305 format('Magnetic z-field: ',A6,' HZ0=',es12.5,'; IIFOPAC=',I2)
 9306 format('PF-kinetics: cz_pf=',f9.6,', tau_pf=',es11.4)
 9307 format('Artif.visc.: iartvis=',I2,' smu1=',f8.4,' smu2=',
     &f8.4,' tmu1=',f8.4,' tmu2=',f8.4)
 9310 format(/25x,'TARGET STRUCTURE'/
     &'  I imatz0   NJ    mass  right DM_j prog-factor  ',
     &'density    R(I+1)     DEUT TRIT HEL3 H    B11')
 9311 format(25x,'TARGET STRUCTURE'/
     &'  I imatz0   NJ    mass  right DM_j  prog-factor  ',
     &'density     R(I+1)')
 9320 format(I3,I4,I8,2es10.3,es13.6,es10.3,g12.5,0p5f5.2)
 9322 format(I3,I4,I8,2es10.3,es13.6,es12.5,g14.7)
 9335 format('Safety factors: czdt=',f7.4,' czddt=',f7.4,' czvkin=',
     &f7.4,' czTkin=',f7.4/16x,'czdtq=',f7.4,' czdtvt=',f7.4)
 9337 format('Run-time: tfin=',es13.6,' dt0=',es9.2,' dtmin=',es9.2,
     &' dtmax=',es9.2/9x,' ncycfin=',I11)
 9400 format(/90('=')/'NCYC=',I9,2X,'TIME='
     &,1PG15.8,2X,'DT=',1PG10.3,4X,'DRIVE CALLS =',I5/
     &3X,'bad stps=',I6,' v.bad stps=',I6,
     &2X,'fl.lim: e-',I7,' i-',I7,' r-',I7)
 9402 format('...print No.',I6,' ncyc=',I9,' t=',es14.8,' dt=',
     &es9.3)
 9404 format(10x,'JTAU140=',I4,' TAU140=',es10.3)
 9410 format('MASS: TOT=',es11.4,' D=',es10.3
     &,' T=',es10.3,' HEL3=',es10.3,' RO*R=',es10.3          ! 5th
     &/'ENRG: TOT=',es11.4,' KIN=',es11.4,' EL=',es11.4
     &,' IONS=',es11.4/8X,'R=',es11.4,' H=',es11.4          ! 11th
     &,' AL=',es11.4,' P3=',es11.4,' P14=',es11.4
     &/'HEATING: DR=',es10.3,' JL=',es10.3,' TN=',es10.3    ! 17th
     &,' CH=',es10.3,' N14=',es10.3
     &/9X,'N2=',es10.3,3X,'DEP TO: AL=',es10.3              ! 21th
     &,' P3=',es10.3,' P14=',es10.3                         ! 23th
     &/'LEAK: WRK=',es11.4,' E-COND=',es11.4,' I-COND=',es11.4
     &,' R-COND=',es11.4/8X,'H=',es11.4,' AL=',es11.4,' P3=',es11.4
     &,' P14=',es11.4)
 9412 format(/'Left influx: W=',es11.4,' E-CON=',es11.4
     &,'I-CON=',es11.4,' R-CON=',es11.4/13x,'H=',es11.4
     &,' AL=',es11.4,' P3=',es11.4,' P14=',es11.4)
 9420 format('BNDRS: P_bl=',es10.3,' P_br=',es10.3,' H_bl=',es10.3
     &,' H_br=',es10.3/7X,'TR_lex=',es10.3,' TR_ex=',es10.3
     &,' EREX=',es10.3,' ERIN=',es10.3/'Tr(nn)=',es10.3
     &,' RORF=',es9.2,' RORF_m=',es9.2,' NN2=',es10.3,' NN14=',es10.3)
 9422 format('EBLNC=',es11.4,' GAIN=',es10.3,' NUDHE3=',es10.3,
     &' ETOT_PF=',es11.4)
 9424 format('EBLNC=',es11.4,' GAIN=',es10.3,' NUDHE3=',es10.3)
 9430 format(/90('-')/'Layer',I3,': Z,A_eos=',F4.1,',',
     &F6.2,'; Z,A_kin=',F4.1,',',F6.2,'; t=',es16.9/90('-'))
 9435 format(6x,'NN2=',es11.4,' NN14=',es11.4,' NUDHE3=',es11.4
     &,' EBLNC=',es11.4)
 9436 format(4x,'NN2=',es11.4,' NN14=',es11.4,' NUDHE3=',es11.4
     &,' EBLNC=',es11.4,' EZ_PF=',es11.4)
 9440 format(/5X,'J',4X,'R(J+1)    U(J+1)    DENSITY',4X
     &,'T ELEC    T ION     T RAD    P(e+i+r)  ',a6)
 9442 format(/5x,'J','  IONIZ   CAPE      CAPI      CAPR',5x,
     &a6,3x,a6,7x,'XD      XT    Q(dr+cl+n)')
 9444 format(/5x,'J','  IONIZ   CAPE      CAPI      CAPR',5x,
     &a6,3x,a6,7x,'XH      XB    Q(dr+cl+n)')
 9450 format(I6,es13.6,7es10.3)
 9451 format(I6,es13.6,es10.3)
 9452 format(I6,F8.4,5es10.3,2F8.4,es10.3)
      end subroutine PRINOUT
!___________________________________________________________end PRINOUT


!**********************************************************************
!                       PRIPR_C
!***********************************************************beg PRIPR_C
      subroutine PRIPR_C
      use COMDEI, only: dfloor,floor,ifpfkin,imatz0,ims_pf,istart,
     &    jobtit,lpr1,lpr2,lvirtl,lvirtl1,matnum,ncyc,njz,nn,nprin,nz,
     &    omdfloo,pathoutpt,pe,pi,propmat,r,rz0,Te,Ti,time,Tr,u,v,yi
      use GWEOS_rd, only: PES_EQGWR,PE_GWR,S_GWR
      implicit none
!======================================================================
!     This routine writes out data for profile plots of cell-centered
!     quantities.

!     Called by:  DELIRA
!     Calls    :  S_GWR,PE_GWR,PES_EQGWR
!======================================================================
! Local variables:
      integer(4) :: i,imsww,iMxw,j,jf,ji,lww,njpr
      real(8) :: fvgww,fmgww,pjww,Tjww,Tww,v00,vww,ww0,ww1,ww2,ww3,wwxi
      real(8) :: aCS2(1),aDEDTET(1),aDPDR(1),aDPDTET(1),aEww(1),
     &      aPww(1),arlww(1),arww(1),aSww(1),cv_gw,gw_n,RBNLIQ,
     &      RBNGAS,RLBNGAS
      character(256) :: fnam,fnam1
      character(4) :: fseqn
      character(8) :: tstamp
      logical :: ifMSeos,lexist,lopened
!======================================================================
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@for_user_to_edit:
      do I=1,nz
        if(int(propmat(1,imatz0(I))+dfloor).eq.5) goto 10
      enddo
      goto 300

!----------------------------------------------------------------------
!     Step 1: Prepare for writing profiles when EOS=GWEOS.
!----------------------------------------------------------------------
 10   lww=lpr1
!@@      njpr=max(1,nn/20000)
      njpr=1

! - open main file for profiles:
      fnam=TRIM(pathoutpt)//'/pro-ccenters.dat'
      inquire(lww, opened=lopened)
      inquire(file=TRIM(fnam),exist=lexist)
      if(.not.lopened) then
        if(istart.eq.0 .or. .not.lexist) then
! - open for the first time to start writing:
          open(lww,file=TRIM(fnam),form='formatted')
          write(lww,'(2a)') 'Job: ',jobtit
          write(lww,9002)
          write(lww,'(a)')
     & 'Layer-combined profiles of principal variables (cell-centered)'
          write(lww,9002)
        else
! - open to continue writing:
          open(lww,file=TRIM(fnam),form='formatted',
     &                     status='old',position='append')
        endif
      endif

! - open sequential file for plot-profiles:
      write(fseqn,'(I4.4)') nprin-1
      write(tstamp,'(f8.6)') time
      fnam1=TRIM(pathoutpt)//'/ppl-c'//'_'//fseqn//'.dat'
      open(lvirtl,file=TRIM(fnam1),form='formatted')
      write(lvirtl,'(a,4x,a,10x,a,8x,a)') '  r-t='//tstamp,'u'//fseqn,
     & 'rho'//fseqn,'p'//fseqn

!##############################################################pfkin19:
!h05      fnam=TRIM(pathoutpt)//'/rho-p_c.dat'
!h05      inquire(lvirtl1, opened=lopened)
!h05      if(lopened) close(lvirtl1)
!h05      open(lvirtl1,file=TRIM(fnam),form='formatted')
!h05      write(lvirtl1,9028)
!##############################################################pfkin19.
!----------------------------------------------------------------------
!     Step 2: Write profiles when EOS=GWEOS.
!----------------------------------------------------------------------
      write(lww,*)
      write(lww,'(a,es15.8,a,I9)') 'time=',time,',  ncyc=',ncyc
!@@      write(lww,9020)
      write(lww,9021)

      do I=1,nz
        ji=njz(I)
        jf=njz(I+1)-1
        do j=ji,jf
          if(njpr*(j/njpr).ne.j .and. j.ne.ji .and. j.ne.jf) cycle
          gw_n=propmat(2,matnum(j))
          cv_gw=propmat(3,matnum(j))
          v00=(gw_n-1.d0)/(gw_n+1.d0)
          iMxw=int(propmat(10,matnum(j))+dfloor)
          ww0=.5d0*(r(j)+r(j+1))
          wwxi=(.5d0*(r(j)+r(j+1))-rz0(nz+1))/(time+floor)
          pjww=pe(j)+pi(j)
          vww=v(j)*propmat(4,matnum(j))
          arww=1.d0/vww
          arlww=log(arww)
          Tjww=.5d0*(Te(j)+Ti(j))
          Tww=Tjww/propmat(5,matnum(j))

          if(arww(1).lt.1.d0) then
            fvgww=1.d0
            fmgww=1.d0
          else
            fmgww=0.d0
            fvgww=0.d0
          endif

          ifMSeos=.false.
          if(ifpfkin) then
            imsww=ims_pf(j)
            if(abs(imsww).eq.1) ifMSeos=.true.
          else
            imsww=1-iMxw
            if(iMxw.eq.0) ifMSeos=.true.
          endif

          if(ifMSeos) then
! MS-EOS:
            call S_GWR(arww(1),arlww(1),Tww,gw_n,cv_gw,aSww(1))
            call PE_GWR(arww(1),Tww,gw_n,cv_gw,aPww(1),aEww(1),
     &                  aCS2(1),aDPDR(1),aDPDTET(1),aDEDTET(1))
          else
! EQ-EOS:
            call PES_EQGWR(arww,arlww,Tww,gw_n,cv_gw,1,aPww,aEww,
     &           aSww,aCS2,aDPDR,aDPDTET,aDEDTET,RBNLIQ,RBNGAS,RLBNGAS)
            if(Tww.lt.1.d0) then
              if(arww(1).lt.rbngas) then
                fvgww=1.d0
                fmgww=1.d0
              elseif(arww(1).lt.rbnliq) then
                fvgww=(rbnliq-arww(1))/(rbnliq-rbngas)
                ww1=rbngas/rbnliq
                fmgww=(rbngas/arww(1)-ww1)/(1.d0-ww1)
              endif
            endif
          endif

! Margin above spinodal T/T_sp-1:
          ww3=Tww*vww**(gw_n+1.d0)*((1.d0-v00)/(vww-v00))**2-1.d0

! - write to main file:
!@@          write(lww,'(I6,2es16.8,6es13.5,3es10.2,I3)') j,ww0,wwxi,
          write(lww,'(I6,es16.8,6es13.5,3es10.2,I3)') j,ww0,
     &      .5d0*(u(j)+u(j+1)),1.d0/v(j),Tjww,pjww,
     &      acs2(1)*propmat(6,matnum(j))/propmat(4,matnum(j)),asww(1),
     &      ww3,fvgww,fmgww,imsww

! - write to sequential file:
          write(lvirtl,'(es16.8,3es15.7)') ww0,.5d0*(u(j)+u(j+1)),
     &      1.d0/v(j),pjww


!##############################################################pfkin19:
!h05          write(lvirtl1,'(3es16.8)') wwxi,1.d0/v(j),pjww
!##############################################################pfkin19.
        enddo
      enddo
!##############################################################pfkin19:
!h05      close(lvirtl1)
!##############################################################pfkin19.
      close(lvirtl)
      goto 9000

!----------------------------------------------------------------------
!     Step 3: Write profiles when EOS.ne.GWEOS.
!----------------------------------------------------------------------
 300  lww=lpr1
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@end_user-edit.

 9000 return
 9002 format(79('-'))
 9020 format(5x,'j  r(j+1/2)',8x,'xi=(r-R)/t',6x,
     &'u(j+1/2)     rho(j)       Te(j)',8x,'p(j)',9x,'c_s^2',8x,
     &'s(j)',9x,'T/T_sp-1  f_vgas',4x,'f_mgas',2x,'ims_pf')
 9021 format(5x,'j  r(j+1/2)',8x,'u(j+1/2)     rho(j)       T(j)',
     &9x,'p(j)',9x,'c_s^2',8x,'s(j)',9x,'T/T_sp-1  f_vgas',4x,
     &'f_mgas',2x,'ims_pf')
 9028 format('  xi=(r-R)/t',6x,'rho',13x,'p')
      end subroutine PRIPR_C
!___________________________________________________________end PRIPR_C


!**********************************************************************
!                       PRIPR_V
!***********************************************************beg PRIPR_V
      subroutine PRIPR_V
      use COMDEI, only: dfloor,floor,ifpfkin,imatz0,ims_pf,istart,
     &    jobtit,lpr1,lpr2,lvirtl,matnum,njz,nn,ncyc,nz,omdfloo,
     &    pathoutpt,pe,pi,propmat,r,rz0,Te,Ti,time,Tr,u,v,yi
      use GWEOS_rd, only: PES_EQGWR,PE_GWR,S_GWR
      implicit none
!======================================================================
!     This routine writes out data for histogram-like profile plots of
!     vertex-centered quantities.

!     Called by:  DELIRA
!     Calls    :  S_GWR,PE_GWR,PES_EQGWR
!======================================================================
! Local variables:
      integer(4) :: i,imsww,iMxw,j,jf,ji,lww,njpr
      real(8) :: fvgww,fmgww,pjww,pww,Tjww,Tww,ww0,ww1,ww2,ww3,wwxi,
     &      wwxi1
      real(8) :: aCS2(1),aDEDTET(1),aDPDR(1),aDPDTET(1),aEww(1),
     &      aPww(1),arlww(1),arww(1),aSww(1),cv_gw,gw_n,RBNLIQ,
     &      RBNGAS,RLBNGAS
      character(256) :: fnam
      logical :: ifMSeos,lexist,lopened
!======================================================================
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@for_user_to_edit:
      do I=1,nz
        if(int(propmat(1,imatz0(I))+dfloor).eq.5) goto 10
      enddo
      goto 300

!----------------------------------------------------------------------
!     Step 1: Prepare for writing profiles when EOS=GWEOS.
!----------------------------------------------------------------------
 10   lww=lpr2
      njpr=max(1,nn/20000)
      fnam=TRIM(pathoutpt)//'/pro-vertices.dat'
      inquire(lww, opened=lopened)
      inquire(file=TRIM(fnam),exist=lexist)
      if(.not.lopened) then
        if(istart.eq.0 .or. .not.lexist) then
! - open for the first time to start writing:
          open(lww,file=TRIM(fnam),form='formatted')
          write(lww,'(2a)') 'Job: ',jobtit
          write(lww,9002)
          write(lww,'(a)')
     & 'Layer-combined histogram-like profiles of principal variables'
          write(lww,9002)
        else
! - open to continue writing:
          open(lww,file=TRIM(fnam),form='formatted',
     &                     status='old',position='append')
        endif
      endif

!----------------------------------------------------------------------
!     Step 2: Write profiles when EOS=GWEOS.
!----------------------------------------------------------------------
      write(lww,*)
      write(lww,'(a,es15.8,a,I9)') 'time=',time,',  ncyc=',ncyc
!@@      write(lww,9020)
      write(lww,9021)

      do I=1,nz
        ji=njz(I)
        jf=njz(I+1)-1
        do j=ji,jf
          if(njpr*(j/njpr).ne.j .and. j.ne.ji .and. j.ne.jf) cycle
          gw_n=propmat(2,matnum(j))
          cv_gw=propmat(3,matnum(j))
          iMxw=int(propmat(10,matnum(j))+dfloor)
          pjww=pe(j)+pi(j)
          arww=1.d0/(v(j)*propmat(4,matnum(j)))
          arlww=log(arww)
          Tjww=.5d0*(Te(j)+Ti(j))
          Tww=Tjww/propmat(5,matnum(j))

          if(arww(1).lt.1.d0) then
            fvgww=1.d0
            fmgww=1.d0
          else
            fmgww=0.d0
            fvgww=0.d0
          endif

          ifMSeos=.false.
          if(ifpfkin) then
            imsww=ims_pf(j)
            if(abs(imsww).eq.1) ifMSeos=.true.
          else
            imsww=1-iMxw
            if(iMxw.eq.0) ifMSeos=.true.
          endif

          if(ifMSeos) then
! MS-EOS:
            call S_GWR(arww(1),arlww(1),Tww,gw_n,cv_gw,aSww(1))
            call PE_GWR(arww(1),Tww,gw_n,cv_gw,aPww(1),aEww(1),
     &                  aCS2(1),aDPDR(1),aDPDTET(1),aDEDTET(1))
          else
! EQ-EOS:
            call PES_EQGWR(arww,arlww,Tww,gw_n,cv_gw,1,aPww,aEww,aSww,
     &                aCS2,aDPDR,aDPDTET,aDEDTET,RBNLIQ,RBNGAS,RLBNGAS)
            if(Tww.lt.1.d0) then
              if(arww(1).lt.rbngas) then
                fvgww=1.d0
                fmgww=1.d0
              elseif(arww(1).lt.rbnliq) then
                fvgww=(rbnliq-arww(1))/(rbnliq-rbngas)
                ww1=rbngas/rbnliq
                fmgww=(rbngas/arww(1)-ww1)/(1.d0-ww1)
              endif
            endif
          endif

          wwxi=(r(j)-rz0(nz+1))/(time+floor)
          wwxi1=(r(j+1)-rz0(nz+1))/(time+floor)

!@@          write(lww,'(I6,2es16.8,6es13.5,2es10.2,I3)') j,r(j),wwxi,
          write(lww,'(I6,es16.8,6es13.5,2es10.2,I3)') j,r(j),
     &      u(j),1.d0/v(j),Tjww,pjww,
     &      acs2(1)*propmat(6,matnum(j))/propmat(4,matnum(j)),asww(1),
     &      fvgww,fmgww,imsww
!@@          write(lww,'(I6,2es16.8,6es13.5,2es10.2,I3)') j,r(j+1),wwxi1,
          write(lww,'(I6,es16.8,6es13.5,2es10.2,I3)') j,r(j+1),
     &      u(j+1),1.d0/v(j),Tjww,pjww,
     &      acs2(1)*propmat(6,matnum(j))/propmat(4,matnum(j)),asww(1),
     &      fvgww,fmgww,imsww
        enddo
      enddo
      goto 9000

!----------------------------------------------------------------------
!     Step 3: Write profiles when EOS.ne.GWEOS.
!----------------------------------------------------------------------
 300  lww=lpr2
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@end_user-edit.

 9000 return
 9002 format(79('-'))
 9020 format(5x,'j  r(j,j+1)',8x,'xi=(r-R)/t',6x,
     &'u(j,j+1)     rho(j)       T(j)',9x,'p(j)',9x,'c_s^2',
     &8x,'s(j)',9x,'f_vgas',4x,'f_mgas',2x,'ims_pf')
 9021 format(5x,'j  r(j,j+1)',8x,'u(j,j+1)     rho(j)       T(j)',
     &9x,'p(j)',9x,'c_s^2',8x,'s(j)',9x,'f_vgas',4x,
     &'f_mgas',2x,'ims_pf')
      end subroutine PRIPR_V
!___________________________________________________________end PRIPR_V


!**********************************************************************
!                       WMEM_VTK
!**********************************************************beg WMEM_VTK
      subroutine WMEM_VTK(ifwfile)
      use COMDEI, only: it_vtk,jobtit,lvirtl,nn,nt_vtk,ncyc,
     &    pathoutpt,pe,pi,pr_vtk,r,r_vtk,ro_vtk,
     &    t_vtk,Te,tfin,Ti,time,Tr,tw_vtk,u,v,yi
      implicit none
!======================================================================
!     This routine stores in memory and writes out into a *.vtk file
!     the data for color rt-diagrams.

! INPUT:
!     ifwfile = .false. -> store in memory only;
!             = .true.  -> store in memory and write into file.

!     Called by:  DELIRA
!     Calls    :  none
!======================================================================
! Arguments:
      logical, intent(in) :: ifwfile

! Local variables:
      integer(4) :: i,i1,i2,iww,iww0,iww1,iww2,iww3,iww4,j,lww,
     &      ncellsww,nclistww,npntsww
      real(8) :: ww0,ww1,ww2,ww3
      real(4) :: bufww1,bufww2,bufww3
      character(256) :: fnam
      logical :: lexist,lopened
!======================================================================
      if(time.lt.tw_vtk .and. .not.ifwfile) goto 9000
      it_vtk=it_vtk+1
      tw_vtk=time+tfin/dble(nt_vtk)

!----------------------------------------------------------------------
!     Step 1: Store in memory.
!----------------------------------------------------------------------
      if(it_vtk.eq.1) then
        t_vtk(1)=time
        r_vtk(1:nn+1,1)=r(1:nn+1)
        ro_vtk(1:nn,1)=1.d0/v(1:nn)
        pr_vtk(1:nn,1)=pe(1:nn)+pi(1:nn)
        goto 190
      endif

      if(it_vtk.gt.nt_vtk+1) goto 9000
      t_vtk(it_vtk)=time
      r_vtk(1:nn+1,it_vtk)=r(1:nn+1)
      do j=1,nn
        ww0=1.d0/v(j)
        ro_vtk(j,it_vtk)=ww0
        ro_vtk(j,it_vtk-1)=.5d0*(ww0+ro_vtk(j,it_vtk-1))
        ww1=pe(j)+pi(j)
        pr_vtk(j,it_vtk)=ww1
        pr_vtk(j,it_vtk-1)=.5d0*(ww1+pr_vtk(j,it_vtk-1))
      enddo

 190  if(.not.ifwfile .and. it_vtk.ne.nt_vtk+1) goto 9000
      if(it_vtk.eq.nt_vtk+1) goto 200

! Fill in the remaining unassigned values:
      do i=it_vtk+1,nt_vtk+1
        t_vtk(i)=t_vtk(i-1)+tfin/dble(nt_vtk)
        r_vtk(1:nn+1,i)=r(1:nn+1)
        ro_vtk(1:nn,i)=1.d-99
        pr_vtk(1:nn,i)=1.d-99
      enddo

!----------------------------------------------------------------------
!     Step 2: Write out into the vtk-file.
!----------------------------------------------------------------------
 200  lww=lvirtl
      fnam=TRIM(pathoutpt)//'/rho-pr.vtk'
      open(lww,file=TRIM(fnam),form='formatted')

! Number of points and cells:
      npntsww=(nn+1)*(nt_vtk+1)
      ncellsww=nn*nt_vtk
      nclistww=5*ncellsww

! Write header:
      write(lww,'(a26)') '# vtk DataFile Version 2.0'
      write(lww,'(a5,a)') 'Job: ',jobtit
      write(lww,'(a5)') 'ASCII'
      write(lww,'(a25)') 'DATASET UNSTRUCTURED_GRID'

!@@      write(lww,'(a17)') 'FIELD FieldData 2'
      write(lww,'(a17)') 'FIELD FieldData 1'
      write(lww,'(a15)') 'TIME 1 1 double'
      write(lww,'(g13.5)') time
!@@      write(lww,'(a13)') 'CYCLE 1 1 int'
!@@      write(lww,'(I11)') ncyc

! Write points:
      write(lww,'(a7,I11,a6)') 'POINTS ',npntsww,' float'
      bufww3=0.e0
      do i2=1,nt_vtk+1
        do i1=1,nn+1
          bufww1=r_vtk(i1,i2)
          bufww2=t_vtk(i2)
          write(lww,'(2es13.5,es9.1)') bufww1,bufww2,bufww3
        enddo
      enddo

! Write cells:
      write(lww,'(a6,2I11)') 'CELLS ',ncellsww,nclistww
      iww0=4
      do i2=0,nt_vtk-1
        do i1=0,nn-1
          iww1=i1+i2*(nn+1)
          iww2=iww1+1
          iww3=iww2+nn+1
          iww4=iww3-1
          write(lww,'(I1,4I11)') iww0,iww1,iww2,iww3,iww4
        enddo
      enddo

      write(lww,'(a11,I11)') 'CELL_TYPES ',ncellsww
      iww0=9
      do i=1,ncellsww
        write(lww,'(I1)') iww0
      enddo

! Write density:
      write(lww,'(a10,I11)') 'CELL_DATA ',ncellsww
      iww=1
      write(lww,'(a18,I2)') 'SCALARS rho float ',iww
      write(lww,'(a20)') 'LOOKUP_TABLE default'
      do i2=1,nt_vtk
        do i1=1,nn
          bufww1=ro_vtk(i1,i2)
          write(lww,'(es11.4)') bufww1
        enddo
      enddo

! Write pressure:
      iww=1
      write(lww,'(a17,I2)') 'SCALARS pr float ',iww
      write(lww,'(a20)') 'LOOKUP_TABLE default'
      do i2=1,nt_vtk
        do i1=1,nn
          bufww1=pr_vtk(i1,i2)
          write(lww,'(es11.4)') bufww1
        enddo
      enddo

      close(lww)


 9000 return
      end subroutine WMEM_VTK
!__________________________________________________________end WMEM_VTK


!**********************************************************************
!                             TSTEP
!*************************************************************beg TSTEP
      subroutine TSTEP
      use COMDEI, only: czddt,czdt,czdtq,czdtvt,dt,dtcfl,dtmax,dtmin,
     &    dtq,eal,eemin,eet,eit,ep3,ep14,floor,hial,hip3,hip14,
     &    Hzincr,i1geo,iartvis,iflbnd,iifHz,iitemp,j_CFL,k_CFL,
     &    jflgburn,n1,nn,ntbad,ntvbad,pblold,pbrold,pe,pi,qe,qi,qjl,r,
     &    sec,smu1,smu2,Te,Teincr,Ti,Tiincr,Trincr,tmu1,tmu2,u,us,
     &    v,vincr
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     This routine computes the time step DT.

!     Called by:  DELIRA
!     Calls    :  SECSCPU
!======================================================================
! Local variables:
      integer(4) :: j
      real(8) :: adp,del,del1,dtm,dv,rab,rab1,tin,tout,us2
!======================================================================
      tin=SECSCPU()

      dtcfl=dtmax
      dtq=dtmax
      j_CFL=2**30
      k_CFL=9

! Constraints due to boundary pressure:
      if(iflbnd.ne.0) then
        us2=(pblold-pe(1)-pi(1))*v(1)
        if(us2.gt.0.d0) then
          dtcfl=min(dtcfl,.5d0*czdt*(r(2)-r(1))/sqrt(us2))
          j_CFL=0
          k_CFL=1                               ! CFL condition #1
        endif
      endif
      us2=(pbrold-pe(nn)-pi(nn))*v(nn)
      if(us2.gt.0.d0) then
        dtcfl=min(dtcfl,.5d0*czdt*(r(n1)-r(nn))/sqrt(us2))
        j_CFL=nn+1
        k_CFL=2                                 ! CFL condition #2
      endif

! Constraints due to the Courant cond., artif.visc., heating:
      do j=1,nn
        del=r(j+1)-r(j)
        if(iflbnd.eq.0 .and. j.eq.1) del=r(2)/dble(i1geo)
! - Courant condition and linear viscosity:
        rab=czdt*del/(1.d0+smu1+tmu1)
        if(dtcfl*us(j).gt.rab) then
          dtcfl=rab/us(j)
          j_CFL=j                               ! CFL condition #3
          k_CFL=3
        endif

! - quadratic viscosity:
        dv=u(j)-u(j+1)
        if(dv.gt.0.d0 .or. iartvis.ne.1) then
          rab=.25d0*czdt*del
          rab1=abs(dv)*(smu2+tmu2+floor)
          if(rab.lt.dtcfl*rab1) then
            dtcfl=rab/rab1
            j_CFL=j                             ! CFL condition #4
            k_CFL=4
          endif
        endif

! - kinematic cell deformation (added 2018-02-21):
        rab=.5d0*czdtvt*del
        if(dtcfl*abs(dv).gt.rab) then
          dtcfl=rab/(abs(dv)+floor)
          j_CFL=j                               ! CFL condition #5
          k_CFL=5
        endif

! - heating:
        rab=czdtq*(eemin+eet(j)*Te(j)+eit(j)*Ti(j))
        rab1=abs(qe(j))+abs(qi(j))
        if(jflgburn.eq.2) rab1=rab1+abs(eal(j)*hial(j)+ep3(j)*hip3(j)+
     &      ep14(j)*hip14(j))
        if(iifHz.ge.1) rab1=rab1+abs(qjl(j))
        if(dtq*rab1.gt.rab) dtq=rab/(rab1+floor)
      enddo

! Constraints due to eventual pressure jumps (added 2018-02-21):
      do j=1,nn-1
        del=r(j+1)-r(j)
        del1=r(j+2)-r(j+1)
        adp=abs(pe(j+1)+pi(j+1)-pe(j)-pi(j))
        rab=.5d0*czdt**2*(del/v(j)+del1/v(j+1))*min(del,del1)
        if(dtcfl*dtcfl*adp.gt.rab) then
          dtcfl=sqrt(rab/(adp+floor))
          j_CFL=j                               ! CFL condition #6
          k_CFL=6
        endif
      enddo
      dtm=min(dtcfl,dtq)

      rab1=1.d0+czddt
!......................................................................
! If too small a time step is forced by the coditions above, try
!@@      dtmin=1.d-18
!@@      if(Te(1).lt.3.d-3) dtmin=.3d0*dtm
!@@      dtmin=min(dtmin,dt*rab1)
!......................................................................
! Correct the previous time-step value for eventual large increments:
      rab=max(vincr,Teincr,Tiincr,Trincr)
      if(iitemp.le.2) rab=max(vincr,Teincr,Tiincr)
      if(iifHz.ge.1) rab=max(rab,Hzincr)
! - the number of 'bad' and 'very bad' steps:
      if(rab.gt.2.d0*rab1*czdtvt) ntbad=ntbad+1
      if(rab.gt.1.d1*rab1*czdtvt) ntvbad=ntvbad+1
      if(rab.lt.czdtvt/rab1) dt=dt*rab1
      if(rab.gt.czdtvt*rab1) dt=dt*czdtvt/rab
      dt=min(dt,dtm)
      dt=max(dt,dtmin)

 9000 tout=SECSCPU()
      sec(5)=sec(5)+max(0.d0,tout-tin)
      return
      end subroutine TSTEP
!_____________________________________________________________end TSTEP


!**********************************************************************
!                             EBALNC
!************************************************************beg EBALNC
      subroutine EBALNC
      use COMDEI, only: amol,csurf,de_pf,dm,dt,ezal,ezcl,ezdr,ezfus,
     &    ezn2,ezn14,nz,ezp3,ezp14,ifdmfu,ifpfkin,ims_pf,jflgburn,njz,
     &    nn,q_pf,qdriv,qe,qi,qsbh,qsdd,qsdhe,qsdt,tnun2,tnun14,
     &    tnudhe3,v,wn2,wn14,xb,xd,xh,xhe,xt,yi
      implicit none
!======================================================================
!     This routine calculates various quantities needed for the energy
!     balance.

!     Called by: DELIRA
!     Calls    : none
!======================================================================
! Local variables:
      integer(4) :: i,j,jf,ji
      real(8) :: cdt,rab,rab1,rab2,wwi,xqd,xqt,xqhe,xqbh
!======================================================================
! Eventual modification of pf-kinetics heating:
      if(ifpfkin) then
        do j=1,nn
          if(ims_pf(j).ne.2) cycle
          rab=dt*abs(q_pf(j))
          if(rab.lt.abs(de_pf(j))) then
            de_pf(j)=de_pf(j)-rab*sign(1.d0,de_pf(j))
          else
            wwi=1.d0/(yi(j)+1.d0)
            rab1=wwi*q_pf(j)
            de_pf(j)=0.d0
            q_pf(j)=de_pf(j)/dt
            rab1=wwi*q_pf(j)-rab1
            qe(j)=qe(j)+rab1*yi(j)
            qi(j)=qi(j)+rab1
          endif
        enddo
      endif

      cdt=dt*csurf
      DO I=1,nz
! Neutron energy deposition in layer I:
        ezn2(I)=ezn2(I)+cdt*wn2(I)
        ezn14(I)=ezn14(I)+cdt*wn14(I)
        ji=njz(I)
        jf=njz(I+1)-1

        do j=ji,jf
          rab=dm(j)
! Driver energy deposition in layer I:
          ezdr(I)=ezdr(I)+(rab*qdriv(j))*cdt
          if(jflgburn.le.0) cycle
          if(ifdmfu(j).ne.1) cycle
          rab1=rab/(v(j)*Amol(I)**2)
          rab2=rab1*xd(j)
          xqd=xd(j)*qsdd(j)/2.d0
          xqt=xt(j)*qsdt(j)
          xqhe=xhe(j)*qsdhe(j)
          xqbh=xh(j)*xb(j)*qsbh(j)
! Local energy deposition by slow charged fusion products in layer I:
          ezcl(I)=ezcl(I)+(1.76d4*rab2*xqd)*cdt
! Total fusion energy generated in layer I:
          ezfus(I)=ezfus(I)+((1.7d5*xqt+7.04d4*xqd+1.77d5*xqhe)*
     &             rab2+8.38d4*xqbh*rab1)*cdt
! Energy of fast charged fusion products generated in shell I:
          ezal(I)=ezal(I)+((3.4d4*xqt+3.54d4*xqhe)*rab2+
     &            8.38d4*xqbh*rab1)*cdt
          ezp3(I)=ezp3(I)+(2.91d4*xqd*rab2)*cdt
          ezp14(I)=ezp14(I)+(14.16d4*xqhe*rab2)*cdt
! Total number of fusion neutrons generated in shell I:
          tnun2(I)=tnun2(I)+(6.022d20*cdt)*rab2*xqd
          tnun14(I)=tnun14(I)+(6.022d20*cdt)*rab2*xqt
          tnudhe3(I)=tnudhe3(I)+(6.022d20*cdt)*rab2*xqhe
        enddo           ! j-loop
      ENDDO             ! I-loop

 9000 return
      end subroutine EBALNC
!___________________________________________________________end EBALNC


!**********************************************************************
!                             UPSLOI
!************************************************************beg UPSLOI
      subroutine UPSLOI
      use COMDEI, only: a0p,a1p,a3p,aa11,aa12,aa13,aa21,aa22,aa23,
     &    aa31,aa32,aa33,amol,Apro,asbol,b0p,b1p,b3p,bb1,bb2,bb3,
     &    capeb,capib,caprb,csurf,dm,dt,dvdt,eal,eall,
     &    eet,eev,eit,eiv,eouall,eoute,eouti,eoutr,eoutw,eouthz,
     &    erex,erin,ethz,etvi0,etvi1,ep3,ep14,ezjl,Hblold,Hbrold,hebr,
     &    hiall,hiei,hier,hip14n,Hz,Hzbl,Hzbr,Hzincr,Hzmin,Hztil,
     &    i1geo,iartvis,igeo,ifburn,ifdmfu,iflbnd,iifal,iifHz,iifp3,
     &    iifp14,ishlj,jflgburn,lrun,lpro,n1,n1max,n2,nn,nnz,njz,ncyc,
     &    nz,pbl,pblold,pblsol,pblsum,pbr,pbrold,pbrsol,pbrsum,pe,pi,
     &    pinum,plkal,plkall,plke,plkp3,plkp14,plkr,plkw,qal,qall,
     &    qdriv,qe,qi,qp3,qp14,qsbh,qsDD,qsDHe,qsDT,r,scavis,sec,
     &    sig,sigt,smu1,smu2,Te,Teincd,Teincr,tenvis,Tfloor,Ti,Tiincr,
     &    time,tmu1,tmu2,Tpro,Tr,Trex,Trincr,Trlex,u,ucbet,upro,us,
     &    v,vincd,vincr,vn,w1w01,w1w02,w1w03,w1w04,w1w05,w1w06,w1w07,
     &    w1w10,w1w11,w1w12,w1w13,w1w14,w1w15,
     &    xb,xd,xh,xhe,xmol,xt,Zpro
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     This routine recalculates basic dynamic variables from
!     the old time layer ("sloi") TIME to the new one TIME+DT.

!     Called by: DELIRA
!     Calls    : BNDVAL,SECSCPU
!======================================================================
! Local variables:
      integer(4) :: i,ifpro(3),ifvocl,j,j0,jal,jf,ji,jm1,k
      real(8) :: dal,dalp,det,dhtil,dhtilp,div,divm,dt5,f,fe,fep,
     &      fi,fip,fp,fr,frp,g11,g11x,g12,g12x,g13,g13x,g21,g2111,g21x,
     &      g22,g2212,g22x,g23,g31,g3111,g31x,g32,g3212,g33,g3313,g33x,
     &      hieidt,hierdt,htilbl,Hzo,p1ww(nnz+1),p2ww(nnz+1),po,pom,
     &      q1,q2,q3,qddj,qdhej,qdtj,qjlj,qrgr,qrgr1,r11,r12,r13,r21,
     &      r22,r23,r31,r32,r33,r1old,rab,rab1,rab2,rab3,rab4,rabdm,
     &      rabdmm,rabdu,rabomg,rabr1,rabs,rabsf,rabsi,rabsv,rabsvm,
     &      rabt,rabtau,rabtf,rabtv,rabtvm,rbyrm,rbyrp,rpbrs2,rs,rslk,
     &      rsm,rsp,tin,tout,trgr,trgr1,u1old,uo,uu,uup,xdj,zbyaal
!======================================================================
      tin=SECSCPU()

      ifpro(1)=iifal
      ifpro(2)=iifp3
      ifpro(3)=iifp14

!----------------------------------------------------------------------
!     Step 1: Solve the thermonuclear burn equations
!--------------------------------------------------------- begin NEWXDT
! Arrays w1w08,w1w09,w1w12,qall are vacant.
!......................................................................
      if(jflgburn.le.0) goto 110
      qal => qall(1:nn+1)
      qp3 => qall(nn+2:2*nn+2)
      qp14 => qall(2*nn+3:3*nn+3)

      do i=1,nz
        ji=njz(i)
        jf=njz(i+1)-1
        do j=ji,jf
          if(ifdmfu(j).ne.1) cycle
          xdj=xd(j)
          qdtj=qsdt(j)
          qddj=qsdd(j)
          qdhej=qsdhe(j)

! Injection of energy into fast products:
          rab=1.d0/(v(j)*Amol(I)**2)
          qal(j)=rab*(3.4d4*xdj*xt(j)*qdtj+3.54d4*xdj*xhe(j)*
     &           qdhej+8.38d4*xh(j)*xb(j)*qsbh(j))
          qp3(j)=2.91d4*rab*xdj*xdj*qddj/2.d0
          qp14(j)=14.16d4*rab*xdj*xhe(j)*qdhej

! New abundances for DD group:
          rab=dt/(v(j)*Amol(I))
          rab1=rab*xdj*qddj
          xt(j)=(xt(j)+.5d0*rab1*xdj)/(1.d0+rab*xdj*qdtj)
          xhe(j)=(xhe(j)+.5d0*rab1*xdj)/(1.d0+rab*xdj*qdhej)
          xd(j)=xdj*(1.d0-2.d0*rab1-rab*(xt(j)*qdtj+
     &          xhe(j)*qdhej))

! New abundances for BH group:
          rab=(dt*qsbh(j))/(v(j)*Amol(I))
          if(xB(j).lt.xH(j)) then
            rab1=xB(j)
            xB(j)=rab1/(1.d0+rab*xH(j))
            xH(j)=xH(j)+xB(j)-rab1
          else
            rab1=xH(j)
            xH(j)=rab1/(1.d0+rab*xB(j))
            xB(j)=xB(j)+xH(j)-rab1
          endif
        enddo           ! j-loop
      enddo             ! I-loop
 110  continue
      if(ifburn) nullify(qsDT,qsDD,qsDHe)
! Burn equations are solved.
!=========================================================== end NEWXDT

!----------------------------------------------------------------------
!     Step 2: Solve the Euler equation
!---------------------------------------------------------- begin NEWRU
! Arrays w1w08,w1w09,w1w12,w1w13,w1w14,w1w15 are vacant.
!......................................................................
! Boundary conditions, initial assignments:
      dt5=.5d0*dt
      call BNDVAL(time+dt5)
      dm(n1)=0.d0
      ifvocl=0
      r1old=r(1)
      u1old=u(1)

! Lay away for the work at boundaries:
      p1ww(1)=0.d0
      p2ww(1)=0.d0
      if(iflbnd.eq.0) goto 260
      p1ww(1)=u(1)*dt5
      if(iflbnd.eq.1) goto 240
      p2ww(1)=pblsum
      goto 260
 240  p2ww(1)=pbl+asbol*Tr(1)**4/3.d0
      if(iifHz.ge.1) p2ww(1)=p2ww(1)+Hz(1)**2/(8.d0*pinum)
      if(jflgburn.eq.2) p2ww(1)=p2ww(1)+2.d0*(eal(1)+ep3(1)+
     &                          ep14(1))/3.d0
 260  continue

      do i=2,nz
        j=njz(i)
        k=j-1
        p1ww(i)=u(j)*dt5
       po=.5d0*(pe(k)+pe(j)+pi(k)+pi(j)+asbol*(Tr(k)**4+Tr(j)**4)/3.d0)
        if(iifHz.ge.1) po=po+(Hz(k)**2+Hz(j)**2)/(16.d0*pinum)
        if(jflgburn.eq.2) po=po+(eal(k)+eal(j)+ep3(k)+ep3(j)+
     &                       ep14(k)+ep14(j))/3.d0
        p2ww(i)=po
      enddo
      p1ww(nz+1)=u(n1)*dt5
      p2ww(nz+1)=pbrsum

! Check for central void closure (only when IFLBND=1):
      if(iflbnd.eq.0) goto 310
      rab=r(1)+u(1)*dt5
      if(iflbnd.ne.1) goto 300
      if(rab.gt..5d0*r(1)) goto 300
! - central void is closed at this time step:
      ifvocl=1
      rab=.5d0*r(1)

! Radii at half-step:
 300  r(1)=rab
 310  continue
      do j=2,n1
        r(j)=r(j)+u(j)*dt5
      enddo

! Progon coefficients for velocity:  !!!  write over PE and PI  !!!
!      !!!  A0P(J) -> PE(J)=W1W01(J), B0P(J) -> PI(J)=W1W02(J)  !!!
      a0p => w1w01
      b0p => w1w02
      dvdt => w1w02
      scavis => w1w07
      tenvis => w1w12

      rs=0.d0
      rsp=1.d0
      if(igeo.ge.1) rsp=r(1)**igeo
      div=0.d0
      rabdm=0.d0
      rabsv=0.d0
      rabtv=0.d0
      po=0.d0
      if(iflbnd.eq.0) goto 330
! Left boundary pressure for "open halfspace":
      po=pblsum
      if(iflbnd.eq.-1) goto 330
! Left boundary pressure for "closed cavity":
      po=pbl+asbol*Tr(1)**4/3.d0
      if(iifHz.ge.1) po=po+Hz(1)**2/(8.d0*pinum)
      if(jflgburn.eq.2) po=po+2.d0*(eal(1)+ep3(1)+ep14(1))/3.d0
 330  continue

      do j=1,n1
        jm1=max(1,j-1)
        rsm=rs
        rs=rsp
        rsp=1.d0
        if(igeo.ge.1) rsp=r(j+1)**igeo
        divm=div
        div=u(j+1)*rsp-u(j)*rs
        rabdmm=rabdm
        rabdm=dm(j)
        rabsvm=rabsv
        rabtvm=rabtv
        rbyrp=0.d0
        if(iflbnd.ne.-1) rbyrp=r(j)/r(j+1)
! Compute scalar viscosity SCAVIS(J)=(\eta_sca,j)/(\Delta m_j) and
! tensor viscosity TENVIS(J)=(\eta_ten,j)/(\Delta m_j r_j+1^s+2):
        rabsv=0.d0
        rabtv=0.d0
        if(j.eq.n1) goto 490
        rabs=0.d0
        rabt=0.d0
        rabdu=u(j)-u(j+1)
        if(rabdu.le.0.d0 .and. iartvis.eq.1) goto 450
! - add artificial viscosity:
        rabs=(smu1*us(j)+smu2*abs(rabdu))/((1.d0-sig)*rs+sig*rsp)
        if(iflbnd.ne.-1) rabt=(tmu1*us(j)+tmu2*abs(rabdu))*
     &      (sigt+(1.d0-sigt)*(rs/rsp)*rbyrp*rbyrp)
!       !!!  US(J) is free  !!!
! - add physical (ion) viscosity:
 450    if(igeo.ge.1) goto 460
        rabsf=etvi0(j)/3.d0+etvi1(j)
        rabtf=0.d0
        goto 480
 460    if(igeo.ge.2) goto 470
        rabsf=etvi0(j)/3.d0
        rabtf=etvi1(j)
        goto 480
 470    rabsf=0.d0
        rabtf=1.3333333333d0*etvi0(j)
 480    rabsv=(rabs+rabsf/dm(j))/v(j)
        if(iflbnd.ne.-1) rabtv=(rabt+rabtf*(sigt*rsp+
     &      (1.d0-sigt)*rs*(rs/rsp)*rbyrp*rbyrp)/dm(j))/v(j)
!       !!! write over US(J): SCAVIS(J) -> US(J)=W1W07(J)  !!!
!       !!! write over QJL(J): TENVIS(J) -> QJL(J)=W1W12(J)  !!!
 490    scavis(j)=rabsv
        tenvis(j)=rabtv

! Clear out PE(J) and PI(J) and calculate pressure PO:
        eev(j)=eev(j)+pe(j)
        eiv(j)=eiv(j)+pi(j)
        pom=po
        if(j.le.nn) goto 520
        po=pbrsum
        goto 540
 520    rab=Tr(j)**2
        po=pe(j)+pi(j)+(asbol*rab)*rab/3.d0
        if(iifHz.ge.1) po=po+Hz(j)**2/(8.d0*pinum)
        if(jflgburn.eq.2) po=po+2.d0*(eal(j)+ep3(j)+ep14(j))/3.d0

 540    rabtau=rs*(dt/(rabdm+rabdmm))
        if(j.eq.1.and.iflbnd.eq.0) rabtau=0.d0
        rpbrs2=0.d0
        if(iflbnd.eq.-1) goto 570
        if(j.eq.1 .and. iflbnd.eq.0) goto 570
        rpbrs2=(rsp/rs)/rbyrp**2
 570    rbyrm=0.d0
        if(iflbnd.eq.-1) goto 590
        if(j.ge.3) rbyrm=r(j)/r(jm1)
        if(j.eq.2 .and. iflbnd.eq.1) rbyrm=r(j)/r(jm1)
 590    rabomg=1.d0+rabtau*(rabsv*rs+rabsvm*(rs-rsm*a0p(jm1))+
     &         rabtv*rpbrs2+rabtvm*(1.d0-rbyrm*a0p(jm1)))
        a0p(j)=rabtau*(rabsv*rsp+rabtv*rpbrs2*rbyrp)/rabomg
        b0p(j)=(u(j)+rabtau*(2.d0*(pom-po)+rabsv*div-rabsvm*
     &         (divm-rsm*b0p(jm1))+rabtv*rpbrs2*(u(j+1)*rbyrp-u(j))-
     &         rabtvm*(u(j)-rbyrm*(u(jm1)+b0p(jm1)))))/rabomg
      enddo
      nullify(pe,pi,us)

! Reverse progon, new velocities, dV/dt, viscous dissipation:
      rs=1.d0
      if(igeo.ge.1) rs=r(n1)**igeo
      uo=u(n1)
!     revised part 修改了边界更新条件
      u(n1)=b0p(n1)/10
      uu=.5d0*(uo+u(n1))
      u(n2)=u(n1)

      do k=1,nn
        j=n1-k
        rsp=rs
        rs=1.d0
        if(igeo.ge.1) rs=r(j)**igeo
        uo=u(j)
        u(j)=a0p(j)*u(j+1)+b0p(j)
! Check for void closure:
        if(j.ne.1) goto 740
        if(iflbnd.eq.0.or.iflbnd.eq.-1) goto 740
        if(ifvocl.eq.1) goto 730
        rabr1=r(1)+u(1)*dt5
        if(rabr1.lt.1.d-2*abs(r(1))) goto 730
        goto 740
! Correct velocity U(1) for exact void closure:
 730    u(1)=-r(1)/dt5
        ifvocl=1
 740    uup=uu
        uu=.5d0*(uo+u(j))
! Compute dV/dt:
!      !!!  write over B0P(J)=PI(J): DVDT(J) -> B0P(J)=PI(J)  !!!
        dvdt(j)=(uup*rsp-uu*rs)/dm(j)
! Redefine SCAVIS(J) and TENVIS(J):
        scavis(j)=(dvdt(j)*scavis(j))*dm(j)
        rabsi=0.d0
        if(iflbnd.eq.-1) goto 790
        if(iflbnd.eq.1 .or. j.ne.1) goto 770
        rabsi=uup/r(j+1)
        goto 780
 770    rabsi=uup/r(j+1)-uu/r(j)
 780    tenvis(j)=(rabsi*tenvis(j))*rsp*r(j+1)**2
! Viscous dissipation - all into ions(!):
 790    qi(j)=qi(j)+dvdt(j)*scavis(j)+(rabsi/dm(j))*tenvis(j)
      enddo

      nullify(a0p,b0p)

! Add kinetic energy dissipation due to void closure:
      if(iflbnd.eq.1 .and. ifvocl.eq.1) qi(1)=qi(1)+.125d0*u1old**2/dt

! Work at boundaries = outward leak of internal energy:
      plkw(1)=0.d0
      if(iflbnd.eq.0) goto 860
      rs=1.d0
      if(igeo.ge.1) rs=r(1)**igeo
      plkw(1)=csurf*(p1ww(1)/dt+.5d0*u(1))*(p2ww(1)*rs)
      eoutw(1)=eoutw(1)+csurf*(p1ww(1)+dt5*u(1))*(p2ww(1)*rs)
 860  continue

      do I=2,nz+1
        j=njz(I)
        k=j-1
        rs=1.d0
        if(igeo.ge.1) rs=r(j)**igeo
        rab=0.d0
        if(I.eq.nz+1) goto 880
        rab=rs*(scavis(j)+scavis(k))
        if(iflbnd.ne.-1) rab=rab+(tenvis(j)+tenvis(k))/r(j)
 880    plkw(I)=csurf*(p1ww(I)/dt+.5d0*u(j))*(p2ww(I)*rs-.5d0*rab)
       eoutw(I)=eoutw(I)+csurf*(p1ww(I)+dt5*u(j))*(p2ww(I)*rs-.5d0*rab)
      enddo

      nullify(scavis,tenvis)
      vn => w1w07

! Calculate new radii and specific volumes VN(J):
      vincr=0.d0
      vincd=0.d0
      if(iflbnd.eq.0) goto 930
      if(ifvocl.ne.1) goto 940

! Change the type of boundary condition in the case of void closure:
      write(lrun,9110) ncyc,time,r1old,u1old
      write(lpro,9110) ncyc,time,r1old,u1old
      iflbnd=0
 930  r(1)=0.d0
      rsp=0.d0
      u(1)=0.d0
      goto 960

 940  rab=u(1)*dt5
      r(1)=r(1)+rab
      rab1=r(1)+rab
      rsp=rab1
      if(igeo.ge.1) rsp=rab1**i1geo
 960  continue

      do j=1,nn
        rs=rsp
        rab=u(j+1)*dt5
        r(j+1)=r(j+1)+rab
        rab1=r(j+1)+rab
        rsp=rab1
        if(igeo.ge.1) rsp=rab1**i1geo
! Check new radii for monotonousness:
        if(rsp.gt.rs) goto 980
        write(lpro,9001) ncyc,j,dt,r(j),r(j+1)
        write(lrun,9001) ncyc,j,dt,r(j),r(j+1)
        STOP 'STOP in UPSLOI-NEWRU-9001: non-monotonic R(j) !'
!       !!!  write over US: VN(J) -> US(J)=W1W07(J)  !!!
 980    vn(j)=(rsp-rs)/(dble(i1geo)*dm(j))
! Maximum relative change in specific volume:
        rab=vn(j)/v(j)
        if(rab.lt.1.d0) rab=1.d0/rab
        rab=rab-1.d0
        vincr=max(vincr,rab)
        if(rab.gt.vincd .and. qdriv(j).gt.0.d0) vincd=rab
      enddo
      r(n2)=r(n1)
!  the Euler equation is solved.
!  in c/blk /XXX/ vacant are: PE
!============================================================ end NEWRU

!----------------------------------------------------------------------
!     Step 3: Solve diffusn equations for the energy of charged fusion
!             products; add their energy deposition to QE(J), QI(J)
!--------------------------------------------------------- begin NEWEAL
! Arrays w1w01,w1w08,w1w09,w1w12,w1w13,w1w14,w1w15 are vacant.
!......................................................................
      if(jflgburn.le.0) goto 2000
      a1p => w1w01
      b1p => w1w13

      DO k=1,3
        if(ifpro(k).eq.0) cycle
        j0=n1max*(k-1)
!------------------------------------------------------------- begin 11
        if(ifpro(k).eq.1) goto 1200
! Local energy deposition:
        do j=1,nn
          jal=j0+j
          eall(jal)=qall(jal)/hiall(jal)
          rab=Te(j)/Tpro(k)
          qe(j)=qe(j)+qall(jal)/(1.d0+rab)
          qi(j)=qi(j)+rab*qall(jal)/(1.d0+rab)
        enddo
        cycle
!=============================================================== end 11
 1200   continue

!------------------------------------------------------------- begin 12
! Diffusion of the energy of charged products:
! - boundary conditions:
        j=j0+n1
        eall(j)=0.d0
        hiall(j)=hiall(j-1)
        v(n1)=v(nn)
! - progon coefficients:
        fp=0.d0
        po=.125d0*upro(k)**2
        zbyaal=964.9d0*Zpro(k)/Apro(k)
        rab=hiall(j0+1)/v(1)
        if(iifHz.ge.1) rab=rab+.5d0*(zbyaal*Hz(1))**2/rab
        if(k.eq.3) rab=rab+.25d0*hip14n(1)/v(1)
        dalp=po/rab

        do j=1,nn
          jm1=max(1,j-1)
          jal=j0+j
          f=fp
          dal=dalp
          rab=hiall(jal+1)/v(j+1)
          if(iifHz.ge.1) rab=rab+.5d0*(zbyaal*Hz(j+1))**2/rab
          if(k.eq.3) rab=rab+.25d0*hip14n(ishlj(j+1))/v(j+1)
          dalp=po/rab
          rsp=1.d0
          if(igeo.ge.1) rsp=r(j+1)**igeo
          fp=rsp*(dt/(r(j+2)-r(j)))*(dal+dalp)
          pom=vn(j)+dt*hiall(jal)+(fp+f*(1.d0-a1p(jm1)))/dm(j)
          if(j.eq.1 .and. iflbnd.eq.-1) pom=pom+2.d0*dal*
     &      (dt/(r(2)-r(1)))/dm(1)
!     !!!  write over PE: A1P(J) -> PE(J)=W1W01(J)  !!!
          a1p(j)=fp/(pom*dm(j))
!     !!!  write over QSDT: B1P(J) -> QSDT(J)=W1W13(J)  !!!
          b1p(j)=(v(j)*eall(jal)+f*b1p(jm1)/dm(j)+dt*(qall(jal)-
     &           2.d0*eall(jal)*dvdt(j)/3.d0))/pom
        enddo

! Reverse progon, contribution to the heating:
        do jf=1,nn
          j=n1-jf
          jal=j0+j
          eall(jal)=a1p(j)*eall(jal+1)+b1p(j)
          rab=te(j)/tpro(k)
          if(k.eq.3) then
            rab1=eall(jal)*hip14n(ishlj(j))
            rab2=hiall(jal)-hip14n(ishlj(j))
            if(rab2.lt.0.d0) then
              write(*,9040) j,hiall(jal),hip14n(ishlj(j))
              write(lrun,9040) j,hiall(jal),hip14n(ishlj(j))
              STOP 'STOP in UPSLOI-NEWEAL-9040'
            endif
            rab2=eall(jal)*rab2
            rab3=(1.d0+Amol(ishlj(j))/xmol(ishlj(j)))**2*Te(j)/750.d0
            qe(j)=qe(j)+rab1/(1.d0+rab3)+rab2/(1.d0+rab)
            qi(j)=qi(j)+rab1*rab3/(1.d0+rab3)+rab2*rab/(1.d0+rab)
          else
            rab1=eall(jal)*hiall(jal)
            qe(j)=qe(j)+rab1/(1.d0+rab)
            qi(j)=qi(j)+rab1*rab/(1.d0+rab)
          endif
        enddo

! Outward leak of energy from layers - for balance:
        jf=(nnz+1)*(k-1)+1
        plkall(jf)=0.d0
        if(iflbnd.ne.-1) goto 1520
        jal=j0+1
        rab=hiall(jal)/v(1)
        if(iifHz.ge.1) rab=rab+.5d0*(zbyaal*Hz(1))**2/rab
        dal=po/rab
        plkall(jf)=-2.d0*csurf*dal*eall(jal)/(r(2)-r(1))
        eouall(jf)=eouall(jf)-2.d0*csurf*dal*eall(jal)*(dt/(r(2)-r(1)))
 1520 continue

        do i=2,nz+1
          j=njz(i)
          jal=j0+j
          jf=(nnz+1)*(k-1)+i
          rab=hiall(jal)/v(j)
          if(iifHz.ge.1) rab=rab+.5d0*(zbyaal*Hz(j))**2/rab
          dalp=po/rab
          rab=hiall(jal-1)/v(j-1)
          if(iifHz.ge.1) rab=rab+.5d0*(zbyaal*Hz(j-1))**2/rab
          dal=po/rab
          rsp=1.d0
          if(igeo.ge.1) rsp=r(j)**igeo
          plkall(jf)=csurf*rsp*(dal+dalp)*(eall(jal-1)-eall(jal))/
     &               (r(j+1)-r(j-1))
          eouall(jf)=eouall(jf)+csurf*rsp*(dal+dalp)*(eall(jal-1)-
     &               eall(jal))*(dt/(r(j+1)-r(j-1)))
        enddo           ! i-loop
      ENDDO             ! k-loop
!=============================================================== end 12
      nullify(a1p,b1p,qal,qp3,qp14)
 2000 continue
!  diffusion equations for charged products are solved;
!  in c/blk /XXX/ vacant are: PE,QSDT,QSDD,QSDHE,QJL.
!=========================================================== end NEWEAL

!----------------------------------------------------------------------
!     Step 4: Solve the diffusion equation for z-component of magnetic
!             field HZ(J)
!---------------------------------------------------------- begin NEWHZ
! Arrays w1w01,w1w08,w1w09,w1w12,w1w13,w1w14,w1w15,qall are vacant.
!......................................................................
      if(iifHz.le.0) goto 4490
      a3p => w1w01
      b3p => w1w12
      Hztil => w1w13

! Magnetic field at the outer boundary (HZ_tilde, for Joule heating:
      Hztil(n1)=.5d0*(Hzbr+Hbrold)
      Htilbl=.5d0*(Hzbl+Hblold)
! Boundary conditions:
      ethz(n1)=ethz(nn)
      Hz(n1)=Hzbr

! Progon coefficients:
      fp=0.d0
      rab=ucbet**2/(4.d0*pinum)
      do j=1,nn
        jm1=max(1,j-1)
        f=fp
        rsp=1.d0
        if(igeo.ge.1) rsp=r(j+1)**igeo
        fp=rsp*(dt/(r(j+2)-r(j)))*(rab*(ethz(j)+ethz(j+1)))
        pom=vn(j)+(fp+f-f*a3p(jm1))/dm(j)
        if(j.eq.1 .and. iflbnd.eq.-1) pom=pom+2.d0*rab*ethz(1)*
     &      (dt/(r(2)-r(1)))/dm(1)
!     !!!  write over PE: A3P(J) -> PE(J)=W1W01(J)  !!!
        a3p(j)=fp/(pom*dm(j))
        rab1=v(j)*Hz(j)+f*b3p(jm1)/dm(j)
        if(j.eq.1 .and. iflbnd.eq.-1) rab1=rab1+2.d0*rab*ethz(1)*
     &      Hzbl*(dt/(r(2)-r(1)))/dm(1)
!     !!!  write over QJL: B3P(J) -> QJL(J)=W1W12(J)  !!!
        b3p(j)=rab1/pom
      enddo

      Hzincr=0.d0
! Reverse progon:
      do jf=1,nn
        j=n1-jf
        Hzo=Hz(j)
        Hz(j)=a3p(j)*Hz(j+1)+b3p(j)
        if(Hz(j).lt.0.d0) then
          write(lpro,9020) time,ncyc,j,Hz(j)
          write(lrun,9020) time,ncyc,j,Hz(j)
          Hz(j)=0.d0
        endif
        rab=Hzo+2.d0*Hzmin
        Hztil(j)=.5d0*(Hzo+Hz(j))
! Maximum relative change in magnetic field:
        rab=(Hz(j)+2.d0*Hzmin)/rab
        if(rab.lt.1.d0) rab=1.d0/rab
        rab=rab-1.d0
        Hzincr=max(Hzincr,rab)
      enddo

      nullify(a3p,b3p)  ! w1w01,w1w12 are vacant!

      rab4=.5d0*(ucbet/(4.d0*pinum))**2
! Outward leak of magnetic energy -> for balance:
      if(iflbnd.eq.-1) then
        eouthz(1)=eouthz(1)+dt*csurf*(rab4*2.d0*ethz(1))*
     &            (htilbl+Hztil(1))*((Hzbl-Hz(1))/(r(2)-r(1)))
      endif

      do I=2,nz+1
        j=njz(I)
        rs=1.d0
        if(igeo.ge.1) rs=r(j)**igeo
        eouthz(I)=eouthz(I)+dt*csurf*rs*(rab4*(ethz(j-1)+ethz(j)))*
     &           (Hztil(j-1)+Hztil(j))*((Hz(j-1)-Hz(j))/(r(j+1)-r(j-1)))
      enddo

! Compute the Joule heating QJLJ and add to QE(J) and EZJL(I):
      IF(iifHz.eq.2) THEN
        fp=0.d0
        dhtilp=0.d0
        do I=1,nz
          ji=njz(I)
          jf=njz(I+1)-1
          do j=ji,jf
            f=fp
            dhtil=dhtilp
            rsp=1.d0
            if(igeo.ge.1) rsp=r(j+1)**igeo
            fp=rsp*(rab4*(ethz(j)+ethz(j+1)))*((Hz(j)-Hz(j+1))/
     &         (r(j+2)-r(j)))
            dhtilp=Hztil(j)-Hztil(j+1)
            qjlj=f*(dhtil/dm(j))+fp*(dhtilp/dm(j))
            if(j.eq.1 .and. iflbnd.eq.-1) qjlj=qjlj+2.d0*rab4*ethz(1)*
     &        ((Hzbl-Hz(1))/(r(2)-r(1)))*((htilbl-Hztil(1))/dm(1))
            if(qjlj.lt.0.d0) qjlj=0.d0
            qe(j)=qe(j)+qjlj
            ezjl(I)=ezjl(I)+dt*csurf*(qjlj*dm(j))
          enddo     ! j-loop
        enddo       ! I-loop
      ENDIF
      nullify(Hztil)    ! w1w13 is vacant!
 4490 continue
! diffusion equation for magnetic field is solved;
! in c/blk /XXX/ vacant are: PE,QSDT,QSDD,QSDHE,QJL.
!============================================================ end NEWHZ

!----------------------------------------------------------------------
!     Step 5: Solve the energy equations for electrons, ions and
!             radiation
!--------------------------------------------------------- begin NEWTTT
! Arrays w1w01,w1w08,w1w09,w1w12,w1w13,w1w14,w1w15,qall are vacant.
!......................................................................
      do j=1,nn
        qe(j)=qe(j)-eev(j)*dvdt(j)
        qi(j)=qi(j)-eiv(j)*dvdt(j)
      enddo

      aa11 => w1w01
      aa21 => w1w02
      aa12 => w1w03
      aa13 => w1w04
      aa22 => w1w05
      aa23 => w1w06
      aa31 => w1w12
      aa32 => w1w13
      aa33 => w1w14
      bb1 => w1w10
      bb2 => w1w11
      bb3 => w1w15

! Boundary coditions for matrix progon:
      trgr=Tr(nn)
      trgr1=Tr(1)
      rab=1.d0
      if(igeo.ge.1) rab=r(n1)**igeo
      qrgr=.25d0*ucbet*asbol*rab*(dt/dm(nn))
      qrgr1=.25d0*ucbet*asbol*(dt/dm(1))
      capeb(n1)=0.d0
      capib(n1)=0.d0
      caprb(n1)=0.d0

! Matrix progon:
      fep=0.d0
      frp=0.d0
      fip=0.d0
      do j=1,nn
        jm1=max(1,j-1)
        rab=dm(jm1)/dm(j)
        fe=fep*rab
        fr=frp*rab
        fi=fip*rab
        rab1=1.d0
        if(igeo.ge.1) rab1=r(j+1)**igeo
        rab=2.d0*rab1*(dt/(r(j+2)-r(j)))/dm(j)
        fep=rab*capeb(j+1)
        frp=rab*caprb(j+1)
        fip=rab*capib(j+1)
        hieidt=dt*hiei(j)
        hierdt=dt*hier(j)
        g11x=eet(j)+fep+fe-fe*aa11(jm1)
        g11=g11x+hierdt+hieidt
        g12x=-fe*aa12(jm1)
        g12=g12x-hierdt
        g13x=-fe*aa13(jm1)
        g13=g13x-hieidt
        q1=eet(j)*Te(j)+qe(j)*dt+fe*bb1(jm1)
! - external flux for the e-conduction:
        if(j.eq.nn) q1=q1-rab1*hebr*(dt/dm(j))
        g21x=-fr*aa21(jm1)
        g21=g21x-hierdt
        g22x=4.d0*asbol*Tr(j)**3*vn(j)+frp+fr-fr*aa22(jm1)
        if(j.eq.1 .and. iflbnd.eq.-1) g22x=g22x+qrgr1*Tr(1)**3
        if(j.eq.nn) g22x=g22x+qrgr*Tr(nn)**3
        g22=g22x+hierdt
        g23=-fr*aa23(jm1)
        q2=asbol*Tr(j)**4*(3.d0*vn(j)+v(j)-dt*dvdt(j)/3.d0)+fr*bb2(jm1)
        if(j.eq.nn) q2=q2+Trex**2*(qrgr*Trex**2)
        if(j.eq.1 .and. iflbnd.eq.-1) q2=q2+Trlex**2*(qrgr1*Trlex**2)
        g31x=-fi*aa31(jm1)
        g31=g31x-hieidt
        g32=-fi*aa32(jm1)
        g33x=eit(j)+fip+fi-fi*aa33(jm1)
        g33=g33x+hieidt
        q3=eit(j)*Ti(j)+qi(j)*dt+fi*bb3(jm1)

! Compute DET=|G|; modify row 3 by adding row 2:
        g3111=g31x+g11x+hierdt
        g3212=g32+g12
        g3313=g33x+g13x
        g2111=g21x+g11x+hieidt
        g2212=g22x+g12x
        det=g3111*(g12*g23-g13*g22)+g3212*(g13*g21-g11*g23)+
     &  g3313*(g11*g2212-g12*g2111)
        if(abs(det).le.0.d0) then
          write(lpro,9030) time,ncyc,j
          write(lrun,9030) time,ncyc,j
          STOP 'STOP in UPSLOI-NEWTTT-9030: det=0 in matrix-progon !'
        endif

! Invert matrix G: R=G^(-1):
        r11=(g22*g33-g23*g32)/det
        r12=(g13*g32-g12*g33)/det
        r13=(g12*g23-g13*g22)/det
        r21=(g23*g31-g21*g33)/det
        r22=(g11*g3313-g13*g3111)/det
        r23=(g13*g21-g11*g23)/det
        r31=(g21*g32-g22*g31)/det
        r32=(g12*g31-g11*g32)/det
        r33=(g11*g2212-g12*g2111)/det
!  !!!  AA11(J) -> PE(J);   AA12(J) -> EET(J);  AA13(J) -> EEV(J);  !!!
!  !!!  AA21(J) -> PI(J);   AA22(J) -> EIT(J);  AA23(J) -> EIV(J);  !!!
!  !!!  AA31(J) -> QJL(J);  AA32(J) -> QSDT(J); AA33(J) -> QSDD(J); !!!
!  !!!  BB1(J) ->  QE(J);   BB2(J) ->  QI(J);   BB3(J) -> QSDHE(J); !!!
        aa11(j)=r11*fep
        aa12(j)=r12*frp
        aa13(j)=r13*fip
        aa21(j)=r21*fep
        aa22(j)=r22*frp
        aa23(j)=r23*fip
        aa31(j)=r31*fep
        aa32(j)=r32*frp
        aa33(j)=r33*fip
        bb1(j)=r11*q1+r12*q2+r13*q3
        bb2(j)=r21*q1+r22*q2+r23*q3
        bb3(j)=r31*q1+r32*q2+r33*q3
      enddo
      nullify(eet,eev,eit,eiv,dvdt,qe,qi)

! Reverse progon, reassign densities:
      Teincr=0.d0
      Tiincr=0.d0
      Trincr=0.d0
      Teincd=0.d0
      rab4=2.d0*Tfloor
      do jf=1,nn
        j=n1-jf
        rab=max(Te(j),Tfloor)+rab4
        Te(j)=aa11(j)*Te(j+1)+aa12(j)*Tr(j+1)+aa13(j)*Ti(j+1)+bb1(j)
        rab1=max(Te(j),Tfloor)+rab4
        rab=rab/rab1
        if(rab.lt.1.d0) rab=1.d0/rab
        rab=rab-1.d0
        Teincr=max(Teincr,rab)
        if(rab.gt.Teincd .and. qdriv(j).gt.0.d0) Teincd=rab
        rab=max(Tr(j),Tfloor)+rab4
        Tr(j)=aa21(j)*Te(j+1)+aa22(j)*Tr(j+1)+aa23(j)*Ti(j+1)+bb2(j)
        rab1=max(Tr(j),Tfloor)+rab4
        rab=rab/rab1
        if(rab.lt.1.d0) rab=1.d0/rab
        rab=rab-1.d0
        Trincr=max(Trincr,rab)
        rab=max(Ti(j),Tfloor)+rab4
        Ti(j)=aa31(j)*Te(j+1)+aa32(j)*Tr(j+1)+aa33(j)*Ti(j+1)+bb3(j)
        rab1=max(Ti(j),Tfloor)+rab4
        rab=rab/rab1
        if(rab.lt.1.d0) rab=1.d0/rab
        rab=rab-1.d0
        Tiincr=max(Tiincr,rab)
        v(j)=vn(j)
      enddo

      nullify(aa11,aa12,aa13,aa21,aa22,aa23,aa31,aa32,aa33,bb1,bb2,bb3,
     &        vn)

! Outward leak of energy from layers -> for balance:
      plke(1)=0.d0
      plkr(1)=0.d0
      if(iflbnd.eq.-1) then
        plkr(1)=(csurf*.25d0*ucbet*asbol)*(Trlex**4-trgr1**3*tr(1))
        eoutr(1)=eoutr(1)+(csurf*qrgr1*dm(1))*(Trlex**4-trgr1**3*tr(1))
      endif
      do I=2,nz
        j=njz(I)
        rab=1.d0
        if(igeo.ge.1) rab=r(j)**igeo
        rslk=(csurf/(r(j+1)-r(j-1)))*rab
        rs=rslk*dt
        plke(I)=rslk*2.d0*capeb(j)*(Te(j-1)-Te(j))
        plkr(I)=rslk*2.d0*caprb(j)*(Tr(j-1)-Tr(j))
        eoute(I)=eoute(I)+rs*2.d0*capeb(j)*(Te(j-1)-Te(j))
        eouti(I)=eouti(I)+rs*2.d0*capib(j)*(Ti(j-1)-Ti(j))
        eoutr(I)=eoutr(I)+rs*2.d0*caprb(j)*(Tr(j-1)-Tr(j))
      enddo

      eoutr(nz+1)=eoutr(nz+1)+(csurf*qrgr*dm(nn))*(Trgr**3*Tr(nn)-
     &            Trex**4)
      plkr(nz+1)=(csurf*(qrgr/dt)*dm(nn))*(Trgr**3*Tr(nn)-Trex**4)
      erex=erex+csurf*qrgr*dm(nn)*Trex**4
      if(iflbnd.eq.-1) erex=erex+csurf*qrgr1*dm(1)*Trlex**4
      erin=eoutr(1)-eoutr(nz+1)

! Lower limit on temperatures:
      do j=1,nn
        Te(j)=max(Te(j),Tfloor)
        Ti(j)=max(Ti(j),Tfloor)
        Tr(j)=max(Tr(j),Tfloor)
      enddo
! The energy equations are solved.
!=========================================================== end NEWTTT

! Memorize the boundary values used:
      pblold=pbl
      pbrold=pbr
      pblsol=pblsum
      pbrsol=pbrsum
      Hblold=Hzbl
      Hbrold=Hzbr

 9000 tout=SECSCPU()
      sec(8)=sec(8)+max(0.d0,tout-tin)
      return

 9001 format(79('=')/'STOP in UPSLOI-9001: radii are not monotonous, ',
     &'NCYC=',I8/'J=',I6,' DT=',es12.5,'  R(j,j+1)=',2es22.14/
     &79('='))
 9020 format('UPSLOI: HZ_new < 0,  TIME,NCYC,J,HZ_new = ',
     &es12.4,I7,I6,es12.4)
 9030 format(79('=')/'STOP in UPSLOI-9030: det=0 in matrix progon; ',
     &'TIME,NCYC,J=',es12.5,I8,I6/79('='))
 9040 format(79('=')/'STOP in NEWEAL: J=',I6,' HIP14=',es12.4,
     &'  <  HIP14N=',es12.4/79('='))
 9110 format(/'!!!  Central void closure at NCYC=',I8,
     &'  TIME=',es16.8,'  !!!'/'void closure: R_1,old=',es16.8,
     &',  U_1,old=',es16.8)
      end subroutine UPSLOI
!____________________________________________________________end UPSLOI


!**********************************************************************
!                            TIMING
!********************************************************* begin TIMING
      subroutine TIMING(lww)
      use COMDEI, only: lpro,lrun,sec,seccumul,secnam,secstart
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     This routine prints out the timing information accumulated in the
!     array "sec".

!     Called by:
!     Calls    : none
!======================================================================
! Arguments:
      integer(4),intent(in) :: lww

! Local variables:
      integer(4) :: i,nsubww,ncww,mww
      real(8) :: ww0
!======================================================================
      nsubww=8

! Cumulative run time:
      ww0=secscpu()
      seccumul=seccumul+ww0-secstart
      secstart=ww0

      write(lww,9020) seccumul
      write(lww,9030) (secnam(i),sec(i),i=1,nsubww)

 9000 return
 9020 format(/5x,'CUMULATIVE RUN TIME FOR THIS JOB',f14.2,' SECONDS'/)
 9030 format(6x,a8,f13.2,8x,a8,f13.2)
      end subroutine TIMING
!___________________________________________________________ end TIMING


!**********************************************************************
!                             RELCON
!************************************************************beg RELCON
      subroutine RELCON(ishell,vjj,Tejj,Tijj,Trjj,Hzjj,yijj,YIONOU,
     &         CAPEOU,CAPIOU,HIEIOU,ETHZOU,CAPROU,HIEROU,ETV0OU,ETV1OU)
      use COMDEI, only: amol,asbol,ceiling,dfloor,floor,iifopac,iifHz,
     &    iifcapei,iitemp,iifvis,lpro,lrun,imatz0,propmat,sec,smol,
     &    xmol,z2mol,zmol,CLogarEE,CLogarEI
      use COMTA3, only: Asub,Zsub,potofi,bopac,nlager,xlager,wlager,
     &    rwei,pwei
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     This routine calculates the values of the transversal (with
!     respect to the magnetic field) electron heat conduction
!     coefficient CAPEOU, of the ion, CAPIOU, and the radiation, CAPROU,
!     heat conduction coefficients [in 10**20 ergs/(cm s keV)],
!     of the electron-ion, HIEIOU, and the electron-radiation, HIEROU,
!     temperature relaxation coefficients [in 10**22 ergs/(g s keV)],
!     and of the transversal electrical resistivity ETHZOU [10**-8 s];
!     all the output information is transferred via the c/blk /CAHIOU/;
!     YIONOU is the actual ionization degree used to calculate
!     the above mentioned coefficients;
!     the input parameters are: the target layer number ISHELL, the
!     specific volume VJJ [cm**3/g], the electron, TEJJ, the ion, TIJJ,
!     and the radiation, TRJJ, temperatures [in keV], the magnetic
!     z-field HZJJ [in 10**7 Gauss], the ionization degree YIJJ.

!     Called by: KINBUR
!     Calls    : SECSCPU
!======================================================================
! Arguments:
      integer(4),intent(in) :: ishell
      real(8),intent(in) :: Hzjj,Tejj,Tijj,Trjj,vjj,yijj
      real(8),intent(out) :: capeou,capiou,caprou,ethzou,etv0ou,etv1ou,
     &      hieiou,hierou,yionou

! Local variables:
      integer(4) :: iflag,iion,imat,iy,iz2,l,ly,nsubi
      real(8) :: a2m,amu,amuefo,amute,avy2,bcolf,by,ccs,cff,clee,clei,
     &      clii,colfea,colfee,colfen,colfia,colfin,cph,cphot,cphot0,
     &      dee2,dei2,dfrac,dy,diy,efe,efete,efo,efote,efte32,exmute,
     &      fystar,gff,hics,hiff,hiph,Hzj,ome,omr,Planl,pty,rab,rab1,
     &      Rossl,ry2a,rya,sigff,sigmaa,sigph,sigsca,sumpe,sumpr,sumr,
     &      Te32,Tej,tet,tet32,Tfe,Tij,tin,tout,Trj,Trt,vj,ww,xhz2,
     &      y2ae,yae,yion,yrj,ystar,z2
!======================================================================
      tin=SECSCPU()
!----------------------------------------------------------------------
!     Step 1: Zero option, common quantities.
!----------------------------------------------------------------------
      yionou=yijj
      capeou=0.D0
      capiou=0.D0
      ethzou=0.d0
      caprou=0.d0
      hierou=0.d0
      etv0ou=0.d0
      etv1ou=0.d0
      hieiou=ceiling
      if(iifcapei.eq.0 .and. iifvis.eq.0 .and. iifHz.le.1 .and.
     &   iitemp.eq.1) goto 9000

! Common quantities:
      imat=imatz0(ishell)
      nsubi=int(propmat(2,imat)+dfloor)
      z2=propmat(28,imat)
      a2m=Amol(ishell)/xmol(ishell)
      iz2=int(z2+dfloor)

      yion=yijj
      iy=yion+1.d0
      if(iy.lt.1) iy=1
      if(iy.gt.iz2) iy=iz2
      vj=vjj
      Tej=Tejj
      Tij=Tijj
      Trj=Trjj
      Hzj=Hzjj
      Te32=Tej*sqrt(Tej)
      yae=(yion/z2)*zmol(ishell)/Amol(ishell)
      y2ae=(yion/z2)**2*z2mol(ishell)/Amol(ishell)
      rya=yae/vj
      ry2a=y2ae/vj
      efe=.026d0*rya**.666666667d0
      tfe=sqrt(Tej**2+(.666666667d0*efe)**2)
      tet=Tej/efe
      tet32=tet*sqrt(tet)

!----------------------------------------------------------------------
!     Step 2: Radiative conduction CAPROU and e-r relaxation HIEROU.
!----------------------------------------------------------------------
!  IIFOPAC<2 - default values of CAPROU and HIEROU;
!  IIFOPAC=2 - calculate CAPROU and HIEROU from fast DEIRA-2 formulae;
!  IIFOPAC=3 - calculate CAPROU and HIEROU by integrating absorption
!              cross-section (model DEIRA-3);
!  IIFOPAC>3 - default values of CAPROU and HIEROU -> goto 3000;
!......................................................................

      if(iitemp.eq.3) hierou=ceiling
      if(iitemp.le.3) goto 3000
      if(iifopac.eq.2) goto 300
      if(iifopac.eq.3) goto 1000
      goto 3000

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     Step 2.1: Fast formulas from DEIRA-2.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 300  continue
! - Compton:
      CCS=.04D0*TRJ*RYA
      HICS=.94D0*YAE*ASBOL*TRJ**4
! - free-free:
      CFF=RY2A/(TRJ+9.3D0*TET32*TRJ*(TRJ/TEJ))
      HIFF=1.12D5*Y2AE*(TEJ+TRJ)/(1.D0+2.19D0*TET32)
! - free-bound:
      TRT=TEJ/TRJ
      IF(ABS(TRT-1.D0).GT.1.D-4) GOTO 500
      TRJ=TEJ*.9999D0
      TRT=TEJ/TRJ
 500  DY=Z2-YION

      IF(DY.GT.1.D0) GOTO 700
! - hydrogen-like ions:
      PTY=.0136D0*Z2*Z2
      RAB=(EFE-PTY)/TEJ
      DY=0.D0
      IF(RAB.LT.-42.D0) DY=1.D0
      IF(ABS(RAB).LT.42.D0)
     *DY=1.D0/(1.D0+.665D0*TET32*EXP(RAB))
      OME=(TEJ/PTY)**2
      OMR=(TRJ/PTY)**2
      CPH=DY*FI1(OMR,-1.397D0)
      HIPH=DY*((TRT*FI2(OME,-1.397D0)-
     *FI2(OMR,-1.397D0))/(TRT-1.D0))
      GOTO 990

 700  IF(DY.GT.2.D0) GOTO 800
! - helium-like ions:
      PTY=.0136D0*Z2*Z2
      OME=(TEJ/PTY)**2
      OMR=(TRJ/PTY)**2
      RAB=2.D0-DY
      CPH=RAB*FI1(OMR,-1.397D0)
      HIPH=RAB*(TRT*FI2(OME,-1.397D0)-FI2(OMR,-1.397D0))
      PTY=.0136D0*(Z2-.65D0)**2
      OME=(TEJ/PTY)**2
      OMR=(TRJ/PTY)**2
      BY=-1.397D0+13.25D0/(Z2-.4D0)**2
      RAB=2.D0*(DY-1.D0)
      CPH=CPH+RAB*FI1(OMR,BY)
      HIPH=(HIPH+RAB*(TRT*FI2(OME,BY)-FI2(OMR,BY)))
     */(TRT-1.D0)
      GOTO 990

 800  IF(DY.GT.3.D0) GOTO 900
! - lithium-like ions:
      PTY=.0136D0*(Z2-.65D0)**2
      OME=(TEJ/PTY)**2
      OMR=(TRJ/PTY)**2
      BY=-1.397D0+13.25D0/(Z2-.4D0)**2
      RAB=2.D0*(3.D0-DY)
      CPH=RAB*FI1(OMR,BY)
      HIPH=RAB*(TRT*FI2(OME,BY)-FI2(OMR,BY))
      PTY=.0036D0*(Z2-1.77D0)**2
      OME=(TEJ/PTY)**2
      OMR=(TRJ/PTY)**2
      RAB=(1.D0-1.77D0/Z2)**4
      BY=5.53D0*LOG(11.D0/RAB)/RAB
      RAB=2.D0*(DY-2.D0)
      CPH=CPH+RAB*FI1(OMR,BY)
      HIPH=(HIPH+RAB*(TRT*FI2(OME,BY)-FI2(OMR,BY)))
     */(TRT-1.D0)
      GOTO 990

! - highly ionized ions:
 900  LY=YION
      YRJ=LY
      PTY=.0036D0*(YRJ+1.3D0)**2
      IF(Z2-YRJ.GT.10.001D0) PTY=.0063D0*(1.D0+YRJ)*SQRT(1.D0+YRJ)+
     +1.5D-7*(1.D0+YRJ)**4
      OME=(TEJ/PTY)**2
      OMR=(TRJ/PTY)**2
      RAB=(.0272D0*Z2*Z2/PTY)**2/(Z2-YRJ)
      BY=.245D0*RAB*LOG(RAB)
      RAB1=YRJ+1.D0-YION
      CPH=2.D0*FI1(OMR,BY)*RAB1
      HIPH=2.D0*(TRT*FI2(OME,BY)-FI2(OMR,BY))*RAB1
      YRJ=LY+1
      PTY=.0036D0*(YRJ+1.3D0)**2
      IF(Z2-YRJ.GT.10.001D0) PTY=.0063D0*(1.D0+YRJ)*SQRT(1.D0+YRJ)+
     +1.5D-7*(1.D0+YRJ)**4
      OME=(TEJ/PTY)**2
      OMR=(TRJ/PTY)**2
      RAB=(.0272D0*Z2*Z2/PTY)**2/(Z2-YRJ)
      BY=.245D0*RAB*LOG(RAB)
      RAB1=1.D0-RAB1
      CPH=2.D0*FI1(OMR,BY)*RAB1+CPH
      HIPH=(2.D0*(TRT*FI2(OME,BY)-FI2(OMR,BY))*RAB1+HIPH)/(TRT-1.D0)
 990  RAB=Z2**4
      CPH=CPH*1.2D0*RAB/(VJ*A2M*TRJ**2)
      HIPH=HIPH*759.D0*RAB/A2M
      CAPROU=5484.D0*TRJ**2*(TRJ**2/(CCS+CFF+CPH))
      HIEROU=HICS+HIFF+HIPH
      goto 3000

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     Step 2.2: Model DEIRA-3: absorption cross-section is integrated
!               numerically.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 1000 continue
      EFETE=EFE/TEJ
      EFTE32=EFETE*SQRT(EFETE)
      IF(EFETE.LT.2.D1) GOTO 1020
      EXMUTE=1.D30
      AMUTE=EFETE-2.6587D0/EFTE32
      AMU=AMUTE*TEJ
      GOTO 1060
 1020 RAB=1.D0+2.6587D0/EFTE32
      IF(EFETE.GT.1.D-4) GOTO 1040
      EXMUTE=2.D0/RAB
      GOTO 1050
 1040 EXMUTE=(1.D0+EXP(EFETE))/RAB
 1050 AMUTE=LOG(EXMUTE)
      AMU=AMUTE*TEJ
 1060 CONTINUE

 1070 DIY=IY-YION
      IF(IY.NE.IZ2) GOTO 1090
! - a special formula for DIY for hydrogen-like ions:
      RAB=(EFE-0.0136D0*Z2**2)/TEJ
      IF(RAB.LT.5.D1) GOTO 1080
      DIY=0.D0
      GOTO 1085
 1080 RAB=158.54D0*TE32*A2M*VJ*EXP(RAB)
      DIY=2.D0*Z2/(1.D0+Z2+RAB+SQRT((Z2-1.D0)**2+
     +RAB*(RAB+2.D0*(1.D0+Z2))))
 1085 YIONOU=Z2-DIY
 1090 AVY2=DIY*(IY-1)**2+(1.D0-DIY)*IY**2

      IF(ABS(1.D0-TRJ/TEJ).LT.1.D-2) TRJ=.99D0*TEJ

! Compute the necessary integrals:
      CPHOT0=4.1174D0*4.8368D0*Z2**4
      SUMR=0.D0
      SUMPE=0.D0
      SUMPR=0.D0

      DO 1970 L=1,NLAGER
      EFO=XLAGER(L)*TRJ
      IFLAG=0
!--------------------------------------------------------- begin SIGMAA
 1100 CONTINUE
! Calculate SIGMAA(EFO):
      if(nsubi.lt.1 .or. nsubi.gt.5) then
        write(lrun,9010) nsubi
        write(lpro,9010) nsubi
        STOP 'STOP in RELCON-9010: forbidden value of NSUBI!'
      endif
! - photo-absorption:
      SIGPH=0.D0
      IION=IY
      DFRAC=DIY
 1110 RAB=EFO/POTOFI(IION,NSUBI)
      CPHOT=CPHOT0
      IF(IION.NE.IZ2) CPHOT=CPHOT0*2.D0
      IF(RAB.GT.1.D0) GOTO 1150
      SIGPH=SIGPH+DFRAC*(CPHOT/(POTOFI(IION,NSUBI)**3*
     *BOPAC(IION,NSUBI)))*RAB**2
      GOTO 1200
 1150 SIGPH=SIGPH+DFRAC*(CPHOT/POTOFI(IION,NSUBI)**3)/
     *(RAB*(RAB**2+BOPAC(IION,NSUBI)-1.D0))
 1200 IF(IION.EQ.IY+1) GOTO 1300
      IF(IY.EQ.IZ2) GOTO 1300
      IION=IY+1
      DFRAC=1.D0-DIY
      GOTO 1110
 1300 CONTINUE
!@@      SIGPH=0.D0

! - free-free absorption:
      EFOTE=EFO/TEJ
! - the Gaunt-factor:
!@@      GFF=1.D0
!@@      GOTO 1580
      IF(EFO.GE.2.D0*(AMU-.71D0*TEJ)) GOTO 1550
      RAB=2.D0*AMU/EFO
      GFF=.55133D0*LOG(RAB*(1.D0+SQRT(1.D0-1.D0/RAB**2)))
      GOTO 1580
 1550 RAB=.71D0*TEJ/EFO
      GFF=.55133D0*LOG(1.D0+2.D0*RAB*(1.D0+SQRT(1.D0+1.D0/RAB)))
 1580 CONTINUE

! - the logarithm term in cross-section:
      AMUEFO=AMUTE-EFOTE
      IF(AMUTE.LT.4.6D0) GOTO 1660

! -:: AMUTE > 4.6:
      IF(AMUEFO.LT.4.6D0) GOTO 1620
      RAB=EFOTE
      GOTO 1800
 1620 IF(AMUEFO.LT.-4.6D0) GOTO 1640
      RAB=AMUTE-LOG(1.D0+EXP(AMUEFO))
      GOTO 1800
 1640 RAB=AMUTE
      GOTO 1800

 1660 IF(AMUTE.LT.-4.6D0) GOTO 1720
      RAB=LOG((1.D0+EXMUTE)/(1.D0+EXP(AMUEFO)))
      GOTO 1800

! -:: AMUTE < -4.6:
 1720 IF(EFOTE.LT.4.6D0) GOTO 1740
      RAB=EXMUTE
      GOTO 1800
 1740 IF(EFOTE.LT.-4.6D0) GOTO 1760
      RAB=EXMUTE*(1.D0-EXP(-EFOTE))
      GOTO 1800
 1760 RAB=EXMUTE*EFOTE
 1800 SIGFF=1463.7D0*(RAB*TEJ/EFO**3)*AVY2*GFF
!@@      SIGFF=0.D0
      SIGMAA=SIGPH+SIGFF*Z2MOL(ISHELL)/(Z2*Z2*XMOL(ISHELL))
! SIGMAA(EFO) calculated.
!=========================================================== end SIGMAA

      IF(IFLAG.NE.0) GOTO 1900
! Add Thomson scattering: ?
!@@      SIGSCA=0.D0
      SIGSCA=0.6652D0*YION
      SUMR=SUMR+RWEI(L)/(SIGMAA+SIGSCA*ZMOL(ISHELL)/(Z2*XMOL(ISHELL)))
      SUMPR=SUMPR+PWEI(L)*SIGMAA
      EFO=XLAGER(L)*TEJ
      IFLAG=1
      GOTO 1100
 1900 SUMPE=SUMPE+PWEI(L)*SIGMAA
 1970 CONTINUE

      RAB=16.6D0*A2M
      RAB1=RAB*VJ
      ROSSL=RAB1*SUMR
!@@      PLANL=RAB1/SUMPE
      CAPROU=5484.D0*(ROSSL*TRJJ)*TRJJ**2
      HIEROU=1.29D0*TRJJ**4*YION*ZMOL(ISHELL)/(Z2*AMOL(ISHELL))+
     +(4113.D0/RAB)*(TEJ**4*SUMPE-TRJ**4*SUMPR)/(TEJ-TRJ)
      GOTO 3000

!......................................................................
!  If the Rosseland, ROSSL(V,T), and the Planck, PLANL(V,T), mean     .
!  free paths (in units of mm) are provided from some other source,   .
!  CAPROU and HIEROU can be evaluated as:                             .
!  CAPROU=5484.D0*ROSSL(VJ,TRJ)*TRJ**3                                .
!  HIEROU=4113.D0*VJ*TEJ**3/PLANL(VJ,TEJ)                             .
!......................................................................


!----------------------------------------------------------------------
!     Step 3: Coefficients of the electron CAPEOU and ion CAPIOU heat
!             conduction, coefficient of the electron-ion temperature
!             relaxation HIEIOU, electrical resistivity ETHZOU,
!             coefficients of the ion viscosity ETV0OU and ETV1OU.
!----------------------------------------------------------------------
 3000 continue

! Calculate the Coulomb logarithms:
      YSTAR=YION
      IF(YION.LT.1.D0) YSTAR=1.D0
      FYSTAR=1.D0+YION-YSTAR
      DEE2=RYA/TFE
      RAB=315.5D0*TEJ/SQRT(DEE2*(1.D0+27.56D0*TFE))
      RAB=RAB*SQRT(RAB)
      CLEE=RAB/(1.D0+1.D0/(8.5D0*RAB))
      IF(CLEE.GT.1.D-4) CLEE=LOG(1.D0+CLEE)
      CLEE=.6666666667D0*CLEE
!     save Lee value in variable CLogarEE 中
      clogarEE=CLEE
!     ---------------------------------------
      dei2=dee2+(ystar/z2)**2*fystar*z2mol(ishell)/(Amol(ishell)*vj*
     &     tij)
      rab=631.d0*tfe/sqrt(dei2*(ystar**2+27.56d0*tfe))
      clei=propmat(39,imat)*rab/(1.d0+1.d0/(propmat(39,imat)*6.5d0*
     &     rab))
      IF(CLEI.GT.1.D-4) CLEI=LOG(1.D0+CLEI)
 !     save Lee value in variable CLogarEE 中
      clogarEI=CLEI
!     ---------------------------------------
      RAB=315.5D0*TIJ/SQRT(DEI2*(YSTAR**4+1.512D-2*TIJ/A2M))
      CLII=RAB*RAB
      IF(CLII.GT.1.D-4) CLII=LOG(1.D0+CLII)
      CLII=.5D0*CLII

! The e-a collision frequency:
      COLFEA=0.D0
      IF(YION.LT.1.D0) COLFEA=7.987D9*SQRT(TFE)*
     *EXP(-(EFE/1.3D-2)**2)*(1.D0-YION)/(VJ*A2M)

! Electron heat conduction:
      BCOLF=3.394D0
      COLFEE=3.914D5*RYA*CLEE/(TEJ*SQRT(SQRT(TEJ**2+
     +(BCOLF*EFE)**2)))
      BCOLF=0.3402D0
      RAB=SQRT(TEJ**2+(BCOLF*EFE)**2)
      COLFEN=COLFEA+5.536D5*CLEI*YSTAR**2*FYSTAR*Z2MOL(ISHELL)/
     *(RAB*SQRT(RAB)*Z2*Z2*AMOL(ISHELL)*VJ)
      if(iifcapei.eq.0 .or. iifcapei.eq.2) goto 3290
      WW=COLFEE/COLFEN
      XHZ2=0.D0
      IF(IIFHZ.GE.1) XHZ2=(1.759D6*HZJ/COLFEN)**2
      IF(XHZ2.GT.1.D4) GOTO 3240
      RAB=(XHZ2*GA11(WW)+GA12(WW))/
     *(XHZ2*(XHZ2+GA71(WW))+(GA72(WW))**2)
      GOTO 3250
 3240 RAB=(GA11(WW)+GA12(WW)/XHZ2)/
     *(XHZ2+GA71(WW)+(GA72(WW))**2/XHZ2)
 3250 CAPEOU=1.697D5*RYA*TEJ*RAB/COLFEN
 3290 continue

! Ion heat conduction and viscosity:
      COLFIA=0.D0
      IF(YION.LT.1.D0) COLFIA=1.871D7*SQRT(TIJ/A2M)*
     *(1.D0-YION)/(VJ*A2M)
      RAB=Z2MOL(ISHELL)/(VJ*Z2*Z2*AMOL(ISHELL)*SQRT(A2M))
      COLFIN=9166.D0*CLII*YSTAR**4*FYSTAR*RAB/(TIJ*SQRT(TIJ))+COLFIA
      XHZ2=0.D0
      IF(IIFHZ.GE.1) XHZ2=(964.9D0*HZJ*YSTAR/(A2M*COLFIN))**2
      IF(XHZ2.GT.1.D4) GOTO 3340
      RAB=(2.D0*XHZ2+2.645D0)/(XHZ2*(XHZ2+2.7D0)+.677D0)
      RAB1=(4.8D0*XHZ2+2.23D0)/(XHZ2*(16.D0*XHZ2+16.12D0)+2.33D0)
      GOTO 3350
 3340 RAB=(2.D0+2.645D0/XHZ2)/(XHZ2+2.7D0+.677D0/XHZ2)
      RAB1=(4.8D0+2.23D0/XHZ2)/(16.D0*XHZ2+16.12D0+2.33D0/XHZ2)
 3350 if(iifcapei.ge.2) capiou=93.89d0*tij*(rab/colfin)*z2*z2*
     &      smol(ishell)/(vj*Amol(ishell)*sqrt(a2m))
      if(iifvis.eq.1) then
        etv0ou=9.263d0*tij/(vj*a2m*colfin)
        etv1ou=9.649d0*tij*rab1/(vj*a2m*colfin)
      endif

! Electron-ion temperature relaxation:
      if(iitemp.eq.1) goto 3390
      bcolf=0.8271d0
      rab=sqrt(tej**2+(bcolf*efe)**2)
      colfen=colfea+5.536d5*clei*ystar**2*fystar*z2mol(ishell)/
     *(rab*sqrt(rab)*z2*z2*Amol(ishell)*vj)
      hieiou=1.588d-2*yae*colfen/a2m
 3390 continue

! Electrical resistivity:
      if(iifHz.eq.2) then
        bcolf=0.3665d0
        rab=sqrt(tej**2+(bcolf*efe)**2)
        colfen=colfea+5.536d5*clei*ystar**2*fystar*z2mol(ishell)/
     &         (rab*sqrt(rab)*z2*z2*Amol(ishell)*vj)
        ww=colfee/colfen
        xhz2=(1.759d6*Hzj/colfen)**2
        if(xhz2.gt.1.d4) goto 3440
        rab=(xhz2*ga51(ww)+ga52(ww))/(xhz2*(xhz2+ga71(ww))+
     &      (ga72(ww))**2)
        goto 3450
 3440   rab=(ga51(ww)+ga52(ww)/xhz2)/(xhz2+ga71(ww)+(ga72(ww))**2/xhz2)
 3450   ethzou=(6.556d-17*colfen)*(1.d0-rab)/rya
      endif

 9000 tout=SECSCPU()
      sec(2)=sec(2)+max(0.d0,tout-tin)
      return

 9010 format(/79('=')/'STOP in RELCON-9010: forbidden NSUBI=',I9/
     &'For IIFOPAC=3 all materials must have "DEITA3" EOS model #3 !'
     &/79('='))

      contains

! Internal functions:
!      FI1(X,B)=X/(210.D0*X+5.D0*B+.25D0*(B+3.D0)/X)
!      FI2(X,B)=X/(X+.5D0*(B+2.5D0)*(1.D0+1.D0/(12.D0*X)))
!      GA11(W)=3.25D0+2.D0*W
!      GA12(W)=(58.725625D0+W*(375.74D0+W*(427.68D0+129.6D0*W)))/49.D0
!      GA71(W)=(366.625625D0+W*(412.69D0+132.52D0*W))/49.D0
!      GA72(W)=0.31D0+W*(12.08D0+5.76D0*W)/7.D0
!      GA51(W)=(226.895625D0+123.705D0*W)/49.D0
!      GA52(W)=(3.3201D0+W*(34.1064D0+W*(95.7888D0+W*41.472D0)))/49.D0

!**********************************************************************
      function FI1(x,b)
!======================================================================
! Arguments:
      real(8) :: FI1
      real(8),intent(in) :: x,b
!======================================================================
      FI1=x/(210.d0*x+5.d0*b+.25d0*(b+3.d0)/x)
      return
      end function FI1
!______________________________________________________________________

!**********************************************************************
      function FI2(x,b)
!======================================================================
! Arguments:
      real(8) :: FI2
      real(8),intent(in) :: x,b
!======================================================================
      FI2=x/(x+.5d0*(b+2.5d0)*(1.d0+1.d0/(12.d0*x)))
      return
      end function FI2
!______________________________________________________________________

!**********************************************************************
      function GA11(w)
!======================================================================
! Arguments:
      real(8) :: GA11
      real(8),intent(in) :: w
!======================================================================
      GA11=3.25d0+2.d0*w
      return
      end function GA11
!______________________________________________________________________

!**********************************************************************
      function GA12(w)
!======================================================================
! Arguments:
      real(8) :: GA12
      real(8),intent(in) :: w
!======================================================================
      GA12=(58.725625d0+w*(375.74d0+w*(427.68d0+129.6d0*w)))/49.d0
      return
      end function GA12
!______________________________________________________________________

!**********************************************************************
      function GA71(w)
!======================================================================
! Arguments:
      real(8) :: GA71
      real(8),intent(in) :: w
!======================================================================
      GA71=(366.625625d0+w*(412.69d0+132.52d0*w))/49.d0
      return
      end function GA71
!______________________________________________________________________

!**********************************************************************
      function GA72(w)
!======================================================================
! Arguments:
      real(8) :: GA72
      real(8),intent(in) :: w
!======================================================================
      GA72=0.31d0+w*(12.08d0+5.76d0*w)/7.d0
      return
      end function GA72
!______________________________________________________________________

!**********************************************************************
      function GA51(w)
!======================================================================
! Arguments:
      real(8) :: GA51
      real(8),intent(in) :: w
!======================================================================
      GA51=(226.895625d0+123.705d0*w)/49.d0
      return
      end function GA51
!______________________________________________________________________

!**********************************************************************
      function GA52(w)
!======================================================================
! Arguments:
      real(8) :: GA52
      real(8),intent(in) :: w
!======================================================================
      GA52=(3.3201d0+w*(34.1064d0+w*(95.7888d0+w*41.472d0)))/49.d0
      return
      end function GA52
!______________________________________________________________________

      end subroutine RELCON
!____________________________________________________________end RELCON


!**********************************************************************
!                             BNDVAL
!************************************************************beg BNDVAL
      subroutine BNDVAL(tyme)
      use COMDEI, only: asbol,hebr,Hz0,Hzbl,Hzbr,iifHz,pbl,pblsum,pbr,
     &    pbrsum,pinum,Trex,Trlex
      implicit none
!======================================================================
!     This routine computes the values of the boundary pressure,
!     external radiation temperature, and magnetic field at TIME=TYME.

!     Called by: JOBINIT,UPSLOI
!     Calls    :  none
!======================================================================
! Arguments:
      real(8),intent(in) :: tyme
!======================================================================

! Boundary pressure (left and right boundaries):
      pbl=0.d0
      pbr=0.d0

! External electron thermal-conduction flux through the right boundary,
! positive in the outward direction:
      hebr=0.d0

! External X-ray drive temperature:
      Trlex=0.d0
      Trex=0.d0*tyme

! Boundary magnetic z-field:
      Hzbl=Hz0
      Hzbr=Hz0

! Total external pressure:
      pblsum=pbl+asbol*Trlex**4/3.d0
      pbrsum=pbr+asbol*Trex**4/3.d0
      if(iifHz.ge.1) then
        pblsum=pblsum+Hzbl**2/(8.d0*pinum)
        pbrsum=pbrsum+Hzbr**2/(8.d0*pinum)
      endif

 9000 return
      end subroutine BNDVAL
!____________________________________________________________end BNDVAL


!**********************************************************************
!                             BESTO2
!************************************************************beg BESTO2
      subroutine BESTO2(e1g,dg,Teg,Tig,yig,imat,BESTOP)
      use COMDEI, only: dfloor,lrun,lpro,propmat
      use COMTA3, only: Atarg=>Asub,Ztarg=>Zsub,c11,c12,c13,c14,c15,
     &    c16,c17,c1r,A1=>Abeam,Z1=>Zbeam,b1,bet1,sgm1,amu1,ptif1,
     &    dinp1,apin1,pot2,nsh2,ne2,esh2,gb0,b2,bet2,sgm2,amu2
      implicit none
!======================================================================
!     This routine calculates the stopping power in units GeV*mm**2/mg
!     as a function of the ion energy E1G (in GeV),
!     the density DG (in g/cc), the electron, TEG, and the ion, TIG,
!     temperatures (in keV), ionization degree yig, and material
!     number imat.

!     Called by:  DRIVE
!     Calls    :  URSAPB
!======================================================================
! Arguments:
      integer(4),intent(in) :: imat
      real(8),intent(in) :: e1g,dg,Teg,Tig,yig
      real(8),intent(out) :: bestop

! Local variables:
      integer(4) :: ieosww,iq,iy,iz2,k,kpt,ks,ksh,kss,n1,nost,nsubi
      real(8) :: A2,bb1,bb2,d,db1,dcl,deirww,deitww,desh,df,dpirww,
     &      dpitww,dpo,e1,eiww,f1,f2,fww(3),flww(3),dlrww(3),dltww(3),
     &      gb,hm,piww,po,q,r,r1,r2,r3,r4,rmu,ry2,Te,Te2,Tef,Ti,Ti2,
     &      ve2,vi2,vv1,xe,xi,yt,ytn,Z2,zlb

! - formerly in COMMON/BESOUT/ -> eventual detailed output:
      integer(4) :: nibes
      real(8) :: clbe,clfe,clfi,clnu,sbe,sfe,sfi,snu,stpe,stpi,
     &      y2,y300,z1ef
!======================================================================
      ieosww=int(propmat(1,imat)+dfloor)
      nsubi=int(propmat(2,imat)+dfloor)

      e1=e1g/(4.96d-5*a1)
      clbe=0.d0
      sbe=0.d0
      clfe=0.d0
      sfe=0.d0
      clfi=0.d0
      sfi=0.d0
      clnu=0.d0
      snu=0.d0

      if(ieosww.eq.3) then
        if(nsubi.lt.1 .or. nsubi.gt.5) then
          write(lrun,9020) nsubi
          write(lpro,9020) nsubi
          STOP 'STOP in BESTO2-9020: forbidden value of NSUBI!'
        endif
        a2=Atarg(nsubi)
        z2=Ztarg(nsubi)
      else
        a2=propmat(27,imat)
        z2=propmat(28,imat)
      endif

      if(e1.le.1.d-8) goto 999
      r=z2+.5d0
      iz2=r
! - convert to atomic units:
      d=dg/(11.206d0*a2)
      Te=Teg/27.21d-3
      Ti=Tig/27.21d-3
      if(ieosww.eq.3) then
        call URSAPB(nsubi,d,9.5d-4,9.5d-4,1,FWW,FLWW,DLRWW,DLTWW,
     &                  PIWW,DPIRWW,DPITWW,EIWW,DEIRWW,DEITWW)
        y300=fww(1)             ! cold ionization degree
        call URSAPB(nsubi,d,Te,Ti,1,FWW,FLWW,DLRWW,DLTWW,
     &                  PIWW,DPIRWW,DPITWW,EIWW,DEIRWW,DEITWW)
        y2=max(fww(1),1.d-6)    ! hot ionization degree
      else
        y300=1.d0
        y2=max(yig,1.d-6)
      endif
      ry2=(d*y2)**c11
      vv1=e1*(2.d0+c16*e1)/(1.d0+c16*e1)**2
      dcl=2.d0*log(1.d0+c16*e1)-c16*vv1

! Evaluate Z1EF:
      z1ef=1.d0
      yt=1.d0
      if(z1.lt.1.5d0) goto 67
      z1ef=z1/(1.d0+(.62d0*z1**c11/sqrt(vv1))**1.7d0)**.58824d0
      ytn=c1r
      if(ytn.lt.1.d-8) ytn=.5d0*z1
      if(ytn.gt.z1-1.d-8) ytn=.5d0*z1
      hm=.2d0*z1
      nibes=0
      r=c12*ry2
      r1=amu1*te**2
      rmu=r+Te*(Te/(Te+.4d0*r)-1.5d0*log(1.d0+2.5d0*Te/r))
! - iterations between labels 40 and 67:
 40   nibes=nibes+1
      yt=ytn
      r=y2*d/yt
      r4=r**bet1
      if(r*1.d-8-1.d0) 41,43,43
 41   r2=r**sgm1
      if(r2-1.d-16) 42,44,44
 42   if(r1-1.d-16) 43,44,44
 43   r3=1.d0
      goto 45
 44   r3=r2/(r2+r1)
 45   bb1=b1*r4*r3
      db1=bb1*(bet1+sgm1*(1.d0-r3))/yt
      if(yt-z1+1.d0) 48,50,50
 48   k=yt
      k=k+1
      po=apin1(4,k)+yt*(apin1(3,k)+yt*(apin1(2,k)+yt*apin1(1,k)))
      dpo=apin1(3,k)+2.d0*yt*apin1(2,k)+3.d0*yt**2*apin1(1,k)
      goto 51
 50   r3=ptif1/(z1-yt)**dinp1
      po=r3*yt
      dpo=r3*(1.d0+dinp1*yt/(z1-yt))
 51   f2=rmu+po-bb1
      df=dpo+db1
      if(abs(f2)-abs(.01d0*yt*df)) 66,66,52
 66   if(abs(f2)-abs(1.d-8+.1d0*po)) 67,67,52
 52   if(yt-1.d-6) 53,53,54
 53   if(f2) 54,67,67
 54   if(z1-yt-1.d-6) 55,55,56
 55   if(f2) 67,67,56
 56   if(nibes.eq.1) f1=f2/abs(f2)
      if(f1*f2) 57,67,58
 57   hm=.3d0*hm
      if(hm-.003d0) 67,67,58
 58   if(abs(f2)-abs(df*hm)) 59,60,60
 59   ytn=yt-f2/df
      goto 61
 60   ytn=yt-f2*hm/abs(f2)
 61   if(ytn-1.d-8) 62,62,63
 62   ytn=.5d0*(yt+1.d-8)
      goto 65
 63   if(z1-ytn-1.d-8) 64,64,65
 64   ytn=.5d0*(yt+z1-1.d-8)
 65   f1=f2/abs(f2)
      if(nibes.lt.100) goto 40
      write(lrun,9010) d,te,y2,yt,ytn,f2,f1,hm
      write(lpro,9010) d,te,y2,yt,ytn,f2,f1,hm
      yt=0.d0
 67   c1r=yt
      if(z1ef.lt.yt) z1ef=yt

! Bound-electron stopping power:
      k=1
      ksh=0
      kpt=0
 110  if(k.eq.nsubi) goto 120
      ksh=ksh+nsh2(k)
      r=Ztarg(k)+.5d0
      n1=r
      kpt=kpt+n1
      k=k+1
      goto 110
 120  q=z2-y2
      if(q.lt.1.d-8) q=1.d-8
      iq=q
      if(iq.ge.iz2) iq=iz2-1
      nost=0
      kss=ksh
 130  kss=kss+1
      nost=nost+ne2(kss)
      if(nost.lt.iq+1) goto 130
      nost=nost-ne2(kss)
      ks=kss
      if(nost.eq.iq) ks=kss-1
      iy=iz2-iq+kpt
      r=d**sgm2(nsubi)
      r1=amu2(nsubi)*Te*Te
      if(r-1.d-16) 140,150,150
 140  if(r1-1.d-16) 144,150,150
 144  r=1.d0
      goto 160
 150  r=r/(r+r1)
 160  bb2=b2(nsubi)*r*d**bet2(nsubi)
      r=q-iq
      if(iq.eq.0) goto 170
      desh=r*(pot2(iy)-esh2(kss)-bb2)+(1.d0-r)*
     &     (pot2(iy+1)-esh2(ks)-bb2)
      if(iz2.eq.1) goto 166
      if(y2-y300) 162,162,164
 162  gb=gb0(nsubi)
      goto 180
 164  gb=gb0(nsubi)+(1.105d0-gb0(nsubi))*(y2-y300)/(z2-y300-1.d0)
      goto 180
 166  gb=1.105d0
      goto 180
 170  desh=pot2(iy)-esh2(kss)-bb2
      gb=1.105d0
 180  if(desh.lt.0.d0) desh=0.d0
      k=kss
      r=2.d0*vv1/(gb*(esh2(k)+desh))
      r=r/sqrt(1.d0+c14*z1ef**2/vv1)
      zlb=(q-nost)*H(r)
 186  k=k-1
      if(k.eq.ksh) goto 190
      r=2.d0*vv1/(gb*(esh2(k)+desh))
      r=r/sqrt(1.d0+c14*z1ef**2/vv1)
      zlb=zlb+ne2(k)*H(r)
      goto 186
 190  clbe=zlb/q
      zlb=zlb+q*dcl
      sbe=4.d0*c13*z1ef**2*zlb/(a1*vv1)

! Free-electron stoppig power:
      r=911.5d0*A2*vv1
      if(r-1.d8*Ti) 202,204,204
 202  Ti2=Ti
      goto 210
 204  Ti2=1.d-8*r
 210  xi=sqrt(r/Ti2)
      Tef=sqrt(Te*Te+(1.26d0*c13*ry2)**2)
      r=.5d0*vv1
      if(r-1.d8*tef) 212,214,214
 212  Te2=Tef
      goto 220
 214  Te2=1.d-8*r
 220  xe=sqrt(r/te2)
      ve2=2.d0*Te2*ETA(xe)
      r=4.d0*c13*d*y2*(1.d0/ve2+y2/(2.d0*ti2*eta(xi)))
      r=1.d0/r
      r1=(.75d0*z1ef/c13)**c11/ry2
      if(r.lt.r1) r=r1
      r1=4.d0*r*ve2/(1.d0+c14*z1ef**2/ve2)
      r1=sqrt(r1)
      clfe=log(1.d0+r1/(1.d0+.5d0/sqrt(r1)))
      sfe=4.d0*c13*z1ef**2*(xe**3/(xe**3+1.33d0))/(a1*vv1)
      sfe=sfe*y2*(clfe+dcl)
      if(c17.gt.1823.d0) goto 999

C: ion+nuclear stopping power:
      r2=1.d0/(1823.d0*a2)
      vi2=2.d0*r2*Ti2*ETA(xi)
      r3=(2.d0*1823.d0*a2*a1/(a2+a1))**2*vi2
      r1=z1-z1ef
      if(r1.lt.1.d0) r1=1.d0
      r4=(z1/r1**c11)**2
      r1=z2-y2
      if(r1.lt.1.d0) r1=1.d0
      r4=.26d0/(r4+(z2/r1**c11)**2)
      r1=sqrt(r3*r4/(1.d0+c14*(z1*z2)**2/vi2))
      clnu=log(1.d0+r1/(1.d0+.35d0/sqrt(r1)))
      r2=4.d0*c13*r2*(xi**3/(xi**3+1.33d0))/(a1*vv1)
      r2=r2*sqrt(1.d0+a2/a1)
      snu=r2*(z1*z2)**2*clnu
      r1=(1.d0+c14*(z1ef*y2)**2/vi2)/r3
      if(r1.lt.r4) r1=r4
      r1=sqrt(r/r1)
      clfi=log(1.d0+r1/(1.d0+.5d0/sqrt(r1)))
      sfi=r2*(z1ef*y2)**2*clfi
 999  BESTOP=4.589d-2*a1*(sbe+sfe+sfi+snu)/a2
      return
 9010 format(' -> Diverg.in BESTO2,Z1-ION.:',1P8G9.3)
 9020 format(/79('=')/'STOP in BESTO2-9020: the value NSUBI=',I9,
     &' is forbidden!'/79('='))

      contains

! Internal functions:
!      H(X)=LOG(1.D0+X/(1.D0 +3.5D0/SQRT(X)))
!      ETA(X)=.353D0+X**2*((X**3+2.34D0)/(X**3+11.D0))
!**********************************************************************
      function H(x)
!======================================================================
! Arguments:
      real(8) :: H
      real(8),intent(in) :: x
!======================================================================
      H=log(1.d0+x/(1.d0 +3.5d0/sqrt(x)))
      return
      end function H
!______________________________________________________________________

!**********************************************************************
      function ETA(x)
!======================================================================
! Arguments:
      real(8) :: ETA
      real(8),intent(in) :: x
!======================================================================
      ETA=.353d0+x**2*((x**3+2.34d0)/(x**3+11.d0))
      return
      end function ETA
!______________________________________________________________________

      end subroutine BESTO2
!____________________________________________________________end BESTO2


!**********************************************************************
!                             EOSDRV
!************************************************************beg EOSDRV
      subroutine EOSDRV(imat,vj,Tej,Tij,ifMSeos,YOU,PEOU,PIOU,EEOU,
     &                  EEVOU,EETOU,EIOU,EIVOU,EITOU,US2OU)
      use COMDEI, only: amol,dfloor,eemin,floor,omdfloo,p_cri,propmat,
     &    T_cri,undef,v_cri
      use GWEOS_rd, only: PE_GWR,PES_EQGWR
      implicit none
!======================================================================
!     This is the driver routine for calculating EOS (without
!     contribution of radiation) for a single point (v,Te,Ti).

! INPUT:
!     imat = material number (1 =< imat =< nnmatrls);
!     vj   = specific volume in cm**3/g;
!     Tej  = electron temperature in keV;
!     Tij  = ion temperature in keV;
!     ifMSeos= logical flag for choosing between the MS and EQ EOS;

! OUTPUT:
!     YOU       = ionization degree;
!     PEOU(PIOU)= electron (ion) pressure in 10**14 erg/cm**3;
!     EEOU(EIOU)= electron (ion) specific energy in 10**14 erg/g;
!     EEVOU     = d(Ee)/dV;
!     EIVOU     = d(Ei)/dV;
!     EETOU     = d(Ee)/dTe;
!     EITOU     = d(Ei)/dTi;
!     USOU2     = square of the sound speed (in 10**7 cm/s) due to
!                 matter pressure only.

!     Called by:  UPDEOS
!     Calls    :  FEREOS,URSAPB,PE_GWR,PES_EQGWR
!======================================================================
! Argumets:
      integer(4),intent(in) :: imat
      real(8),intent(in) :: Tej,Tij,vj
      real(8),intent(out) :: you,peou,piou,eeou,eevou,eetou,eiou,eivou,
     &      eitou,us2ou
      logical, intent(in) :: ifMSeos

! Local variables:
      integer(4) :: ieosmod,nsubi
      real(8),parameter :: aferfit=.4d0
      real(8) :: Aeos,anu,anu1,c02,cKe,cKi,cs2,cve,cvi,dedtet,
     &      deilr,deilt,dlfar(3),dlfat(3),dpdr,dpdtet,dpilr,dpilt,
     &      ec,egw,eiap,eu,fapp(3),flapp(3),game,gami,GGame,GGami,
     &      gw_n,hiww,pc,pgw,piap,rab,rab1,rbngas,rbnliq,rlbngas,
     &      ro0,roa,roj,rrho,Tea,tet,Tia,va,vu,ww0,ww1,wwc2,wwe,wwi,
     &      wwr,ya,Zeos
      real(8) :: pefou,eefou,dpeltf,deeltf,dpelvf,deelvf,pifou,eifou,
     &      dpiltf,deiltf,dpilvf,deilvf
      real(8) :: cs2a(1),dedteta(1),dpdra(1),dpdteta(1),egwa(1),
     &      pgwa(1),rrhoa(1),rrhola(1),sgwa(1)
      logical :: ifnoMxw
!======================================================================
!......................................................................
!  Allowed values of IEOSMOD:
!     1 - analytic -> polytropic (ideal) gas
!     2 - analytic -> Fermi-electrons + Maxwellian ions
!     3 - tabular "DEITA3"
!     4 - analytic -> Gruneisen
!     5 - analytic -> generalized van der Waals (GWEOS)
!     7 - tabular "GLT"
!......................................................................

      roj=1.d0/vj
      ieosmod=int(propmat(1,imat)+dfloor)

! Jump to appropriate EOS:
      SELECT CASE(ieosmod)

!----------------------------------------------------------------------
      CASE(1) ! Polytropic EOS.
!----------------------------------------------------------------------
!     Polytropic EOS is defined as:
!           pe=Ke*rho*Te,           pi=Ki*rho*Ti,
!           ee=Ke*Te/(game-1),      ei=Ki*Ti/(gami-1).
!     It has 4 free parameters: Ke,Ki,game,gami.
!......................................................................
! User-defined EOS parameters:
      cKe=propmat(2,imat)
      cKi=propmat(3,imat)
      game=propmat(4,imat)
      gami=propmat(5,imat)

! EOS proper:
      you=cKe/cKi
      peou=cKe*Tej*roj
      piou=cKi*Tij*roj
      eetou=cKe/(game-1.d0)
      eitou=cKi/(gami-1.d0)
      eeou=eetou*Tej
      eiou=eitou*Tij
      eevou=0.d0
      eivou=0.d0
! Sound speed squared due to matter pressure only:
      us2ou=vj*(peou*game+piou*gami)

!----------------------------------------------------------------------
      CASE(2) ! Fermi EOS.
!----------------------------------------------------------------------
!     Fermi EOS is defined as the sum of the ideal Fermi-gas of free
!     non-relativistic electrons and of the ideal Maxwellian gas of
!     ions.
!     This EOS has two free parameters: the mean atomic mass Aeos,
!     and a constant ionization degree ya.
!......................................................................

! User-defined EOS parameters:
      ya=propmat(2,imat)
      Aeos=propmat(25,imat)

! Atomic units:
 210  vu=1.d0/(11.206d0*Aeos)
      eu=2.942d0*vu
      roa=vu*roj
      va=1.d0/roa
      Tea=Tej/.02721d0
      Tia=Tij/.02721d0

      call FEREOS(ya,va,Tea,Tia,aferfit,PEFOU,EEFOU,DPELTF,
     &    DEELTF,DPELVF,DEELVF,PIFOU,EIFOU,DPILTF,DEILTF,DPILVF,DEILVF)

      you=ya
      peou=pefou*2.942d0
      piou=pifou*2.942d0
      eeou=eefou*eu
      eiou=eifou*eu
      eevou=deelvf*2.942d0*roa
      eivou=deilvf*2.942d0*roa
      eitou=deiltf*eu/Tij
      rab=deeltf*eu/Tej
! A lower limit on the electron heat capacity:
      rab1=eemin/tej
      if(rab1.gt.vu) rab1=vu
      if(rab.lt.rab1) rab=rab1
      eetou=rab
! Sound speed squared due to matter pressure only:
      us2ou=vj*(-dpelvf-dpilvf+(eu*dpeltf**2/(tej*eetou)+
     &      dpiltf**2/deiltf)*va)*2.942d0

!----------------------------------------------------------------------
      CASE(3) ! Tabular "DEITA3" EOS.
!----------------------------------------------------------------------
! User-defined EOS parameters:
      Aeos=propmat(25,imat)
      Zeos=propmat(26,imat)
      nsubi=int(propmat(2,imat)+dfloor)

! Atomic units:
      vu=1.d0/(11.206d0*Aeos)
      eu=2.942d0*vu

! EOS proper:
      roa=vu*roj
      Tea=tej/.02721d0
      Tia=tij/.02721d0
      call URSAPB(nsubi,roa,Tea,Tia,3,FAPP,FLAPP,DLFAR,DLFAT,
     &                  PIAP,DPILR,DPILT,EIAP,DEILR,DEILT)
      you=min(fapp(1),Zeos)
      peou=fapp(2)*2.942d0
      piou=piap*2.942d0
      eeou=fapp(3)*eu
      eiou=eiap*eu
      eevou=-dlfar(3)*2.942d0*roa
      eivou=-deilr*2.942d0*roa
      eitou=deilt*eu/Tij
      rab=dlfat(3)*eu/Tej
! Lower limit on the electron heat capacity:
      rab1=eemin/tej
      if(rab1.gt.vu) rab1=vu
      if(rab.lt.rab1) rab=rab1
      eetou=rab

! Sound speed squared due to matter pressure only:
      us2ou=vj*(dlfar(2)+dpilr+(eu*dlfat(2)**2/(Tej*eetou)+
     &      dpilt**2/deilt)/roa)*2.942d0
!us      usou=sqrt(max(0.d0,us2ou))


!----------------------------------------------------------------------
      CASE(4) ! Linear EOS of the Mie-Gruneisen type.
!----------------------------------------------------------------------
!     This EOS is defined as:
!        pe = pc(rho) + G_e*cve*rho*Te,
!        ee = ec(rho) + cve*Te,
!        pi = G_i*cvi*rho*Ti = G_i*rho*ei,
!        ei = cvi*Ti,
!        pc = rho_0*c_0^2*[(rho/rho_0)^(nu+1) - 1]/(nu+1),
!        ec = c_0^2*[(rho/rho_0)^nu/nu + rho_0/rho]/(nu+1) - c_0^2/nu.
!     It has 7 free parameters: rh_0, c_0^2, nu, G_e, G_i, c_ve, c_vi.
!......................................................................

! User-defined EOS parameters:
      ro0=propmat(2,imat)
      c02=propmat(3,imat)
      anu=propmat(4,imat)
      GGame=propmat(5,imat)
      GGami=propmat(6,imat)
      cve=propmat(7,imat)
      cvi=propmat(8,imat)

! Intermediate quantities:
      anu1=1.d0/anu                       ! 1/nu
      wwc2=c02/(anu+1.d0)                 ! c_0^2/(nu+1)
      wwe=GGame*cve                       ! G_e*c_ve
      wwi=GGami*cvi                       ! G_i*c_vi
      ww0=ro0*vj                          ! rho_0/rho
      wwr=1.d0/ww0                        ! rho/rho_0
      ww1=wwr**anu                        ! (rho/rho_0)**nu
      pc=ro0*wwc2*(ww1*wwr-1.d0)          ! p_cold
      ec=wwc2*(anu1*ww1+ww0)-c02*anu1     ! e_cold

! EOS proper:
      you=wwe/wwi
      peou=pc+wwe*roj*Tej
      piou=wwi*roj*Tij
      eeou=ec+cve*Tej
      eiou=cvi*Tij
      eetou=cve
      eitou=cvi
      eevou=-pc
      eivou=0.d0
      us2ou=c02*ww1+(GGame+1.d0)*wwe*Tej+(GGami+1.d0)*wwi*Tij

!----------------------------------------------------------------------
      CASE(5) ! Generalized van-der-Waals EOS (GWEOS).
!----------------------------------------------------------------------
! Initialization:
      rbnliq=undef
      rbngas=undef
      rlbngas=undef

! User-defined EOS parameters:
      gw_n=propmat(2,imat)
      cve=propmat(3,imat)
      wwc2=p_cri(imat)*v_cri(imat)

      ya=propmat(7,imat)
      wwi=1.d0/(ya+1.d0)

! GWEOS in reduced (dimensionless) form:
      tet=Tej/T_cri(imat)
      rrho=v_cri(imat)/vj
      if(ifMSeos) then
! - apply GWEOS-MS:
        call PE_GWR(rrho,tet,gw_n,cve,PGW,EGW,CS2,DPDR,DPDTET,DEDTET)
      else
! - apply GWEOS-EQ:
        rrhoa(1)=rrho
        rrhola(1)=log(rrho)
        call PES_EQGWR(rrhoa,rrhola,tet,gw_n,cve,1,PGWA,EGWA,SGWA,
     &      CS2A,DPDRA,DPDTETA,DEDTETA,RBNLIQ,RBNGAS,RLBNGAS)
        pgw=pgwa(1)
        egw=egwa(1)
        cs2=cs2a(1)
        dpdr=dpdra(1)
        dpdtet=dpdteta(1)
        dedtet=dedteta(1)
      endif

! Convert into the dimensional form:
      you=ya
      piou=p_cri(imat)*wwi*pgw
      peou=ya*piou
      eiou=wwc2*wwi*egw
      eeou=ya*eiou
      us2ou=wwc2*cs2
      eitou=wwc2*wwi*dedtet/T_cri(imat)
      eetou=ya*eitou
      eivou=p_cri(imat)*wwi*(tet*dpdtet-pgw)
      eevou=ya*eivou

!----------------------------------------------------------------------
      CASE DEFAULT
!----------------------------------------------------------------------

      STOP 'STOP in EOSDRV: unacceptable value of IEOSMOD'

      END SELECT


 9000 return
      end subroutine EOSDRV
!____________________________________________________________end EOSDRV


!**********************************************************************
!                       URSAPB
!************************************************************beg URSAPB
      subroutine URSAPB(nsub,d,Te,Ti,job,FAPP,FLAPP,DLFAR,DLFAT,
     &                  PIAP,DPILR,DPILT,EIAP,DEILR,DEILT)
      use COMDEI, only: sec
      use COMTA3, only: Asbs=>Asub,Zsbs=>Zsub,p00,e00,nroo,roill,
     &    hlroo,ntemm,temill,hltemm,farr,qurs
      use MATKIT, only: SECSCPU
      implicit none
!======================================================================
!     This routine computes the two-temperature EOS by interpolation
!     from the "DEITA3" tables, generated by the "TABIN3" package from
!     the Basko source EOS.

!     All the input and output quantities are in atomic units (a.u.)!

! INPUT:
!     NSUB   = sequential material number in the "DEITA3" table
!              (according to c/blk /URSAZ/);
!     d      = density in a.u.;
!     Te(Ti) = electron(ion) temperature in a.u..
!     job    = integer flag for job options; calculated are:
! for job=1  -> YI=FAPP(1) (ionization degree) and its derivatives;
!        =2  -> YI,PE=FAPP(2),PI,EI and their derivatives;
!        =3  -> complete set;
!        =4  -> PE,PI,EI    and their derivatives;
!        =5  -> EE,PI,EI    and their derivatives;
!        =6  -> PE,EE,PI,EI and their derivatives;
!        >6  -> PI,EI       and their derivatives;

! OUTPUT:
!     FAPP(1)  = yi -> ionization degree;
!     FAPP(2)  = pe -> electron pressure;
!     FAPP(3)  = ee -> electron specific internal energy;
!     FLAPP(1) = ln(yi);
!     FLAPP(2) = ln(pe+p00);
!     FLAPP(3) = ln(ee+e00);
!     DLFAR(1) = d(yi)/d[ln(rho)];
!     DLFAR(2) = d(pe)/d[ln(rho)];
!     DLFAR(3) = d(ee)/d[ln(rho)];
!     DLFAT(1) = d(yi)/d[ln(Te)];
!     DLFAT(2) = d(pe)/d[ln(Te)];
!     DLFAT(3) = d(ee)/d[ln(Te)];
!     PIAP     = pi -> ion pressure;
!     DPILR    = d(pi)/d[ln(rho)];
!     DPILT    = d(pi)/d[ln(Ti)];
!     EIAP     = ei -> ion specific internal energy;
!     DEILR    = d(ei)/d[ln(rho)];
!     DEILT    = d(ei)/d[ln(Ti)].

!     Called by: EOSDRV,BESTO2
!     Calls    : none
!======================================================================
! Arguments:
      integer(4),intent(in) :: nsub,job
      real(8),intent(in) :: d,Te,Ti
      real(8),intent(out) :: FAPP(3),FLAPP(3),DLFAR(3),DLFAT(3),
     &      PIAP,DPILR,DPILT,EIAP,DEILR,DEILT

! Local variables:
      integer(4) :: i,itab,j,k,l,m,m1,m2,ma,mb,mq,ms,nro,nte2,nte3,ntem
      real(8) :: a01,a02,a10,a11,a12,a20,a21,dg,dql,etad,etad1,etau,
     &      etau1,f00,f01,f10,f11,g1,g3,hlro,hlt1,hlt2,hlt3,hltem,
     &      q,ql,r,r1,r2,roil,temi1,temi2,temi3,tin,tout,xld,xx,xy
!======================================================================
      tin=SECSCPU()

      j=nsub
      roil=roill(j)
      hlro=hlroo(j)
      nro=nroo(j)
      nte2=ntemm(1,j)
      nte3=ntemm(2,j)
      ntem=ntemm(3,j)
      temi1=temill(1,j)
      temi2=temill(2,j)
      temi3=temill(3,j)
      hlt1=hltemm(1,j)
      hlt2=hltemm(2,j)
      hlt3=hltemm(3,j)

      ms=0
      mq=1
      if(j.gt.1) then
        do k=1,j-1
          mq=mq+4*nroo(k)
          ms=ms+3*nroo(k)*ntemm(3,k)
        enddo
      endif

      itab=0
      xld=log(d)
      xx=(xld-roil)/hlro
      i=xx
      if(job.ge.7) goto 400
      if(i.lt.1) then
        i=0
        itab=1
        goto 20
      endif
      if(i.gt.nro-3) then
        i=nro-2
        r=i
        xx=xx-r
        itab=1
        goto 20
      endif
      r=i
      xx=xx-r

 20   xy=log(Te)
      etad=1.d0
      etau=1.d0
      if(xy.gt.temi2) goto 3
      hltem=hlt1
      xy=(xy-temi1)/hltem
      k=xy
      if(k.le.0) k=0
      r=k
      xy=xy-r
      if(k.eq.nte2-2) etau=hlt2/hlt1
      if(k.lt.1) itab=1
      goto 7

 3    if(xy.gt.temi3) goto 4
      hltem=hlt2
      xy=(xy-temi2)/hltem
      k=xy
      r=k
      xy=xy-r
      k=k+nte2-1
      if(k.eq.nte2-1) etad=hlt1/hlt2
      if(k.eq.nte3-2) etau=hlt3/hlt2
      goto 7

 4    hltem=hlt3
      xy=(xy-temi3)/hltem
      k=xy
      if(k.ge.ntem-nte3-1) k=ntem-nte3-1
      r=k
      xy=xy-r
      k=k+nte3-1
      if(k.eq.nte3-1) etad=hlt2/hlt3
      IF(K.GT.NTEM-3) ITAB=1

 7    m1=1
      if(job.ge.4) m1=2
      if(job.eq.5) m1=3
      m2=m1+job-1
      if(job.ge.4) m2=m1
      if(job.ge.6) m2=m1+1

      do l=m1,m2
        ma=ms+((l-1)*nro+i)*ntem+k+1
        mb=ma+ntem
        f00=farr(ma)
        f01=farr(ma+1)
        f10=farr(mb)
        f11=farr(mb+1)
        if(itab.eq.0) goto 71
        a20=0.d0
        a21=0.d0
        a02=0.d0
        a12=0.d0
        goto 72

 71     ma=ma-ntem
        mb=mb+ntem
        a20=.25d0*(farr(mb)+farr(ma)-f00-f10)
        a21=.25d0*(farr(mb+1)+farr(ma+1)-f01-f11)-a20
        etau1=1.d0/(1.d0+etau)
        etad1=1.d0/(1.d0+etad)
        ma=ma+ntem-1
        mb=mb-ntem-1
        a02=.5d0*(((farr(ma+3)-f01)/etau+etau*(f01-f00))*etau1-
     &      ((f00-farr(ma))/etad+etad*(f01-f00))*etad1)
        a12=.5d0*(((farr(mb+3)-f11)/etau+etau*(f11-f10))*etau1-
     &      ((f10-farr(mb))/etad+etad*(f11-f10))*etad1)-a02
 72     a11=f00+f11-f01-f10-a21-a12
        a01=f01-f00-a02
        a10=f10-f00-a20
        flapp(l)=f00+xx*(a10+xy*(a11+xy*a12)+xx*(a20+a21*xy))+
     &           xy*(a01+a02*xy)
        dlfar(l)=(a10+2.d0*xx*(a20+a21*xy)+xy*(a11+a12*xy))/hlro
        dlfat(l)=(a01+2.d0*xy*(a02+a12*xx)+xx*(a11+a21*xx))/hltem
        r=0.d0
        if(l.eq.2) r=p00(j)
        if(l.eq.3) r=e00(j)
        r1=exp(flapp(l))
        fapp(l)=r1-r
        dlfar(l)=dlfar(l)*r1
        dlfat(l)=dlfat(l)*r1
      enddo

 400  if(job.eq.1) goto 9000
      if(xld.gt.roil) goto 403
      m=mq
      r1=roil
      goto 405
 403  r1=roil+(nro-1)*hlro
      if(xld.lt.r1) goto 407
      m=mq+4*(nro-2)
 405  ql=(qurs(m)*r1*(3.d0*xld-2.d0*r1)+qurs(m+1)*(2.d0*xld-r1))*r1+
     &    qurs(m+2)*xld+qurs(m+3)
      dg=0.d0
      dql=(3.d0*r1*qurs(m)+2.d0*qurs(m+1))*r1+qurs(m+2)
      goto 411
 407  m=mq+4*i
      ql=((qurs(m)*xld+qurs(m+1))*xld+qurs(m+2))*xld+qurs(m+3)
      dql=(3.d0*qurs(m)*xld+2.d0*qurs(m+1))*xld+qurs(m+2)
      dg=3.d0*qurs(m)*xld+qurs(m+1)
 411  q=exp(ql)
      g1=1.5d0*dql
      g3=g1+1.d0
      r1=ti+q
      r2=d*ti/r1
      piap=r2*(ti+g3*q)
      dpilr=r2*(ti*(ti+(1.d0+2.d0*g1*g1/3.d0)*q)/r1+(g3+3.d0*dg)*q)
      dpilt=r2*(ti*(ti+(1.d0-g1)*q)/r1+g3*q)
 444  eiap=1.5d0*ti*(r1+q)/r1
      deilr=g1*q*(ti/r1)**2
      deilt=1.5d0*ti*(1.d0+(q/r1)**2)

 9000 tout=SECSCPU()
      sec(4)=sec(4)+max(0.d0,tout-tin)
      return
      end subroutine URSAPB
!____________________________________________________________end URSAPB


!**********************************************************************
!                             FEREOS
!************************************************************beg FEREOS
      subroutine FEREOS(ya,va,Tea,Tia,aferfit,PEFOU,EEFOU,DPELTF,
     &    DEELTF,DPELVF,DEELVF,PIFOU,EIFOU,DPILTF,DEILTF,DPILVF,DEILVF)
      implicit none
!======================================================================
!     This routine computes the equation of state of a mixture of
!     the Boltzmann gas of ions and the Fermi gas of electrons (without
!     contribution from radiation) by using the interpolation of Fermi
!     integrals by Basko.
!     All input and output quantities are in atomic units (a.u.).
!
! INPUT:
!     ya       = ionization degree (number of free electrons per atom);
!     va       = specific volume;
!     Tea(Tia) = electron (ion) temperature;
!     aferfit  = fitting parameter; 0.4 yields error |dp/p| <1.5%.

! OUTPUT:
!     PEFOU  - electron pressure;
!     EEFOU  - electron energy;
!     DPELTF - dP_e/dlnT_e;
!     DPELVF - dP_e/dlnV;
!     DEELTF - dE_e/dlnT_e;
!     DEELVF - dE_e/dlnV;
!     PIFOU  - electron pressure;
!     EIFOU  - electron energy;
!     DPILTF - dP_i/dlnT_i;
!     DPILVF - dP_i/dlnV;
!     DEILTF - dE_i/dlnT_e;
!     DEILVF - dE_i/dlnV;
!     FEFOU  - electron free energy;
!     AMUFOU - electron chemical potential;

! Atomic units:
!     density     -> 11.206*A g/cc;
!     temperature -> e^2/a_0 = 27.21 eV;
!     pressure    -> e^2/a_0^4 = 294.2 Mbar;
!     internal energy -> 27.21 eV/atom = (26.26/A)*10^12 erg/g.

!     Called by:  EOSFER
!     Calls    :  none
!======================================================================
! Arguments:
      real(8),intent(in) :: ya,va,Tea,Tia,aferfit
      real(8),intent(out) :: pefou,eefou,dpeltf,deeltf,dpelvf,deelvf,
     &      pifou,eifou,dpiltf,deiltf,dpilvf,deilvf

! Local variables:
      real(8),parameter :: floorloc=1.d-100,pinum=3.141592653589793d0,
     &      third=1.d0/3.d0,twoth=2.d0/3.d0,
     &      ccefe=.5d0*(3.d0*pinum**2)**twoth
      real(8) :: ane,efe,p0,pfe,rho,rl15,Te,tete,Ti,v,ww0,ww1,ww2,y
!======================================================================

! Initial assignments:
      y=max(floorloc,ya)
      v=max(floorloc,va)
      rho=1.d0/v
      Te=max(0.d0,Tea)
      Ti=max(0.d0,Tia)

!----------------------------------------------------------------------
!     Step 1: Ion part.
!----------------------------------------------------------------------
      pifou=Ti*rho
      eifou=1.5d0*Ti
      dpiltf=pifou
      dpilvf=-pifou
      deiltf=eifou
      deilvf=0.d0

!----------------------------------------------------------------------
!     Step 2: Electron part.
!----------------------------------------------------------------------
      ane=y*rho
      efe=ccefe*ane**twoth
      pfe=ane*efe
      tete=Te/efe
      ww0=1.d0/(tete+aferfit)
      ww1=ww0*tete
      ww2=ww1*ww0
      p0=ane*Te
      pefou=.4d0*pfe+p0*tete*ww0
      eefou=1.5d0*pefou*v
      dpeltf=p0*ww2*(tete+2.d0*aferfit)
      dpelvf=-twoth*pfe-p0*ww2*(tete+third)
      deeltf=1.5d0*v*dpeltf
      deelvf=eefou+1.5d0*v*dpelvf

!@@  If chemical potential AMUFOU and free energy FEFOU are not needed,
!@@  the three lines below may be commented out:
!@@      rl15=1.5d0*log(1.d0+tete/aferfit)
!@@      fefou=y*(.6d0*efe-Te*rl15)
!@@      amufou=efe+Te*(ww1-rl15)

 9000 return
      end subroutine FEREOS
!____________________________________________________________end FEREOS


!**********************************************************************
!                             TFEREOS
!**********************************************************************
      subroutine TFEREOS(ya,va,pa,aferfit,TFOU)
      implicit none
!======================================================================
!     This routine computes the temperature T = Te = Ti for given
!     total pressure pa (Boltzmann ions + Fermi electrons) in a.u.;
!     works in combination with FEREOS.
!     All the input and output quantities are in atomic units (a.u.).
!
! INPUT:
!     ya      = ionization degree (number of free electrons per atom);
!     va      = specific volume;
!     pa      = total pressure;
!     aferfit = fitting parameter; 0.4 yields error |dp/p| <1.5%.

! OUTPUT:
!     TFOU  - temperature T = Te = Ti.

! Atomic units:
!     density     -> 11.206*A g/cc;
!     temperature -> e^2/a_0 = 27.21 eV;
!     pressure    -> e^2/a_0^4 = 294.2 Mbar;
!     internal energy -> 27.21 eV/atom = (26.26/A)*10^12 erg/g.

!     Called by:  ...
!     Calls    :  none
!======================================================================
! Arguments:
      real(8),intent(in) :: ya,va,pa,aferfit
      real(8),intent(out) :: TFOU

! Local variables:
      real(8),parameter :: floorloc=1.d-100,pinum=3.141592653589793d0,
     &      third=1.d0/3.d0,twoth=2.d0/3.d0,
     &      ccefe=.5d0*(3.d0*pinum**2)**twoth
      real(8) :: efe,h,rab,rab0,rab1,rho,v,y
!======================================================================

! Initial assignments:
      y=max(floorloc,ya)
      v=max(floorloc,va)
      rho=1.d0/v
      efe=ccefe*(y*rho)**twoth
      h=pa*v-0.4d0*y*efe
      if(h.lt.0.d0) then
        write(*,9020) ya,va,pa,h*rho
        STOP 'STOP in TFEREOS-9020:  P < P_Fermi !'
      elseif(h.eq.0.d0) then
        TFOU=0.d0
        goto 9000
      endif

      rab0=aferfit*efe
      rab=h-rab0
      rab1=sqrt(rab**2+4.d0*rab0*(y+1.d0)*h)
      if(rab.lt.0.d0) then
        TFOU=2.d0*rab0*h/(rab1-rab)
      else
        TFOU=.5d0*(rab1+rab)/(y+1.d0)
      endif

 9000 return
 9020 format(79('=')/'P < P_Fermi for Y,V,P,P_th=',4es12.4/79('='))
      end subroutine TFEREOS
!___________________________________________________________end TFEREOS


!**********************************************************************
!                            GZERA
!********************************************************* begin GZERA
      subroutine GZERA(F,xl,xr,eps,nfcallmax,imethd,XOUT,FOUT,NFCALLS)
      implicit none
!======================================================================
!     This routine calculates the root x = xout of equation  F(x)=0
!     on the interval xl < x < xr by the bisection method, combined
!     with the chord method where f(xp)*f(x2) < 0.

!     Absolute accuracy of the result x is eps*max(abs(xl),abs(xr)).
!     The number of F calls is limited to nfcallmax.

! INPUT:
!     xl,xr  = the left and right boundaries (xl < xr) of the interval
!             (xl,xr) where the root of F(x) is to be found;
!     eps    = relative accuracy of the solution, i.e. the absolute
!             error of root x is < eps*max(abs(xl),abs(xr));
!     nfcallmax = maximum allowed number of calls of function F(x);
!     imethd = integer flag of the method used:
!            =< 0 -> the bisection method;
!            >= 1 -> the bisection + the chord method;

! OUTPUT:
!     xout   = the numerical solution of equation F(xout)=0;
!     fout   = F(xout);
!     nfcalls= actual number of calls of F(x).

!     Calls: F(x)
!======================================================================
! Arguments and result:
      integer(4), intent(in) :: nfcallmax,imethd
      integer(4), intent(out) :: nfcalls
      real(8), intent(in) :: xl,xr,eps
      real(8), intent(out) :: xout,fout

! Local variables:
      integer(4) :: imww,k2stuck,kflat,kgood,kflip,kpstuck,ncww
      real(8), parameter :: dfloor=1.d-15,floor=1.d-200
      real(8) :: af2,aff,afp,aww,bww,dxl,dxr,f0,f1,f2,f2sgn,ff,ffsgn,
     &      fp,fpp,h,hmin,hmmin,x,x0,x1,x2,xi2,xi22,xp,ww0,ww1,ww2,ww3,
     &      wwf1,wwf2,wwf2t

! Interfaces:
      interface
        function f(x)
          real(8) :: f
          real(8), intent(in) :: x
        end function f
      end interface
!======================================================================
      imww=imethd
 2    xp=.5d0*(xr+xl)
      h=.24d0*(xr-xl)
      hmin=eps*max(abs(xl),abs(xr))+floor
      hmmin=.03d0*hmin
      dxl=xl+dfloor*abs(xl)+floor
      dxr=xr-dfloor*abs(xr)-floor

      fp=f(xp)
      ncww=1
      if(fp.eq.0.d0) then
        xout=xp
        goto 90
      endif
      kflip=0
      kflat=0
      kgood=0

! Begin main loop:
 10   x=xp+h
      if(abs(h).le.dfloor*abs(xp)+floor) then
        xout=xp
        goto 90
      endif

! Eventual overstepping (or reaching) one of the ends of [xl,xr]:
      IF(x.ge.xr) THEN
        if(xp.ge.dxr) then
          xout=xp
          goto 90
        endif
        x=.5d0*(xr+xp)
      ELSEIF(x.le.xl) THEN
        if(xp.le.dxl) then
          xout=xp
          goto 90
        endif
        x=.5d0*(xl+xp)
      ENDIF

! New function value:
      ff=f(x)
      ncww=ncww+1
      if(ff.eq.0.d0) then
        xout=x
        goto 90
      endif
      ffsgn=ff/abs(ff)

      IF(fp*ffsgn.gt.0.d0) THEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     CASE 1: the same sign of ff and fp
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if(abs(ff).lt.abs(fp)) then
! - current step was made in the right direction:
          fpp=fp
          xp=x
          fp=ff
          kflip=0
          kflat=0
          kgood=kgood+1
          goto 80

        elseif(ff.eq.fp) then
          xp=x
          kflip=0
          kflat=kflat+1
          goto 80

        else
! - current step was made in the wrong direction:
          if(kgood.ge.1) then
            if(kflat.ge.1) then
! - - exit because a local flat maximum was reached:
              xout=xp
              goto 90
            endif
            h=.3d0*h
            if(abs(ff).gt.abs(fpp)) h=-h
            kgood=0
            kflip=0
            kflat=0
            goto 80
          endif

          if(kflip.eq.0) then
            kflip=1
            kgood=kgood+1
            h=-h
            fpp=ff
          else
            kflip=0
            kgood=0
            h=.3d0*h
            if(abs(ff).gt.abs(fpp)) h=-h
          endif
          kflat=0
          goto 80
        endif

      ELSE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     CASE 2: opposite signs of ff and fp -> the root is bracketed ->
!             -> either the bisection or the chord method
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        IF(imww.le.0) THEN

! The bisection method:
          if(abs(h).lt.hmin) then
            xout=x
            goto 90
          endif

          h=.3d0*h
          if(abs(ff).lt.abs(fp)) then
            xp=x
            fp=ff
            h=-h
          endif
          kflip=0
          kflat=0
          kgood=0
          goto 80

        ELSE

! The chord method:
          x2=x
          f2=ff
          kpstuck=0
          k2stuck=0
 40       x=xp+(fp/(fp-f2))*(x2-xp)
          ff=f(x)
          ncww=ncww+1
          ww1=abs(x2-xp)
          if(ff.eq.0.d0 .or. ww1.lt.hmin .or. ncww.gt.nfcallmax) then
            xout=x
            goto 90
          endif
          aff=abs(ff)
          ffsgn=ff/aff
          ww3=aff+aff

          if(fp*ffsgn.gt.0.d0) then
            ww0=abs(x-xp)
            afp=abs(fp)
            xp=x
            fp=ff
            if(ww0.lt.hmmin .or. (kpstuck.ge.1.and. ww3.gt.afp)) then
! - - switch back to the bisection method:
              h=.3d0*(x2-xp)
              goto 10
            endif
            k2stuck=0
            if(ww3.gt.afp) then
              kpstuck=kpstuck+1
            else
              kpstuck=0
            endif
          else
            ww0=abs(x-x2)
            af2=abs(f2)
            if(ww0.lt.hmmin .or. (k2stuck.ge.1.and. ww3.gt.af2)) then
! - - switch back to the bisection method:
              h=.3d0*(xp-x)
              xp=x
              fp=ff
              goto 10
            endif
            x2=x
            f2=ff
            kpstuck=0
            if(ww3.gt.af2) then
              k2stuck=k2stuck+1
            else
              k2stuck=0
            endif
          endif
          goto 40

        ENDIF
      ENDIF

! Check for abnormal termination:
 80   if(abs(h).lt.hmmin .or. ncww.gt.nfcallmax) then
        if(abs(ff).lt.abs(fp)) then
          xout=x
        else
          xout=xp
        endif
        goto 90
      endif
      goto 10

 90   fout=f(xout)
      nfcalls=ncww+1

 9000 return
      end subroutine GZERA
!___________________________________________________________ end GZERA


!**********************************************************************
!                       DM0PRO
!************************************************************beg DM0PRO
      subroutine DM0PRO(r0,r1,I,ngauss,DM0PR)
      use MATKIT, only: GQUAD
      implicit none
!======================================================================
!     This routine calculates the "reduced" mass DM0PR in the interval
!     R0 < r < R1 by integrating the initial density profile over the
!     layer I.

!     Called by:  JOBINIT,FZER0PR
!     Calls    :  GQUAD
!======================================================================
! Arguments:
      integer(4),intent(in) :: I,ngauss
      real(8),intent(in) :: r0,r1
      real(8),intent(out) :: DM0PR
!======================================================================
      DM0PR=GQUAD(FDM0PR,r0,r1,ngauss)
      return

      contains

!**********************************************************************
!                             FDM0PR
!************************************************************beg FDM0PR
      function FDM0PR(rr)
      use COMDEI, only: igeo
      implicit none
!======================================================================
!     This routine defines the function to be integrated when
!     the "reduced" mass interval DM is calculated in DM0PR.

!     Called by:  GQUAD
!     Calls    :  RO0PR
!======================================================================
! Arguments:
      real(8),intent(in) :: rr
      real(8) :: fdm0pr

! Local variables:
      real(8) :: rab_
!======================================================================
      rab_=1.d0
      if(igeo.ge.1) rab_=rr**igeo
      fdm0pr=rab_*RO0PR(rr,I)
      return
      end function FDM0PR
!____________________________________________________________end FDM0PR


!**********************************************************************
!                             RO0PR
!**********************************************************************
      function RO0PR(rrab,I)
      use COMDEI, only: bro0pr,roz0,roz01,rz0
      implicit none
!======================================================================
!     This routine defines the initial density profile as a function of
!     radius RRAB in target layer I.

!     Called by:  FDM0PR
!     Calls    :  none
!======================================================================
! Arguments:
      integer(4),intent(in) :: I
      real(8),intent(in) :: rrab
      real(8) :: ro0pr
!======================================================================
! Initial density profile in layer I:
      ro0pr=roz0(I)+(roz01(I)-roz0(I))*((rrab-rz0(I))/
     &(rz0(I+1)-rz0(I)))**bro0pr(I)
      return
      end function RO0PR
!_____________________________________________________________end RO0PR

      end subroutine DM0PRO
!____________________________________________________________end DM0PRO
