!                     ___________________________
!                    |                           |
!                    | Code: DELIRA              |
!                    | Version: 2021.03          |
!                    | File: fd0_comod_i03.f     |
!                    | Last modified: 2020-02-23 |
!======================================================================
!**********************    MODULES in this file   ********************
!======================================================================
! Module COMDEI
! Module COMTA3
! Module MATKIT :: contains:
!     function ZN(n,hs,h1)
!     function ZERA0(F,xl,xr,x0)
!     function GQUAD(f,a,b,n)
!     function ELLICK(x)
!     function SECSCPU()

! Module GWEOS_rd :: contains:
!     subroutine PE_GWR(rrho,tet,gw_n,cv_gw,P,E,CS2,DPDR,DPDTET,DEDTET)
!     subroutine S_GWR(rrho,rrhol,tet,gw_n,cv_gw,S)
!     subroutine SPND_T_NGWR(tet,gw_n,cv_gw,RSPLIQ,RSPGAS,F_SPLIQ,
!    &                       F_SPGAS,NFLIQ,NFGAS)
!     function QZN(n,hs,h1,IERR)
!     subroutine GZERA(F,xl,xr,eps,nfcallmax,imethd,XOUT,FOUT,NFCALL)
!----------------------------------------------------------------------
!           ROUTINES invoking other routines from this module:
!----------------------------------------------------------------------
!     subroutine BIND_NGWR(tet,gw_n,cv_gw,RBNLIQ,RBNGAS,RLBNGAS,F_BN_T,
!    &                     NFBN_T)
!     :: contains: subroutine GZER_bi(F,xl,xr,eps,nfcallmax,imethd,
!    &                                XOUT,FOUT,NFCALLS)
!                  function F_BN_T_GWR(x_)
!     subroutine PES_EQGWR(rrho,rrhol,tet,gw_n,cv_gw,nco,P,E,S,CS2,
!    &      DPDR,DPDTET,DEDTET,RBNLIQ,RBNGAS,RLBNGAS)
!----------------------------------------------------------------------
!           In particular, the following ROUTINES call GZERA:
!----------------------------------------------------------------------
!     subroutine PTS_EQGWR(rrho,e,gw_n,cv_gw,P,TET,EOUT,S,CS2,DPDR,
!    &            DPDTET,DEDTET,RBNLIQ,RBNGAS,RLBNGAS,F_PTSEQ,NF_PTSEQ)
!     :: contains: function FE_EQ(x_)

!**********************************************************************
!**********************************************************************
!                            COMDEI
!*********************************************************** beg COMDEI
      module COMDEI
      implicit none

!======================================================================
!                             PARAMETERS
!======================================================================
! MEMORY for hydro, materials:
      integer(4),parameter :: nnz=12,nnmatrls=30
! NNZ is the maximum allowed number of target layers (shells).
! NNMATRLS is the maximum allowed number of materials.

!----------------------------------------------------------------------
! R/W units:
      integer(4),parameter :: linpt=15,lrun=16,lpro=17,lhist=18,
     &      lpr1=23,lpr2=24,lpr3=25,lvirtl=28,lvirtl1=29
      character(128),parameter :: pathoutpt='output'

!----------------------------------------------------------------------
! Math parameters:
      integer(4),parameter :: iundef=-123456789
      real(8),parameter :: undef=-123456789.d0, floor=1.d-80,
     &      ceiling=1.d0/floor, dfloor=1.d-13, efloor=3.d-8,
     &      omdfloo=1.d0-dfloor, opdfloo=1.d0+dfloor,
     &      pinum=3.1415926535898d0, pinum4=4.d0*pinum,
     &      third=1.d0/3.d0, sixth=.5d0*third, twelfth=.5d0*sixth,
     &      dfuzz=1.d-15

!----------------------------------------------------------------------
! Physical constants:
      real(8),parameter :: asbol=1.372d0,ucbet=2.998d3,
     &      apro(3)=(/4.d0,1.d0,1.d0/),zpro(3)=(/2.d0,1.d0,1.d0/),
     &      upro(3)=(/130.3d0,240.5d0,530.2d0/),
     &      Tpro(3)=(/20.d0,60.d0,300.d0/)

!----------------------------------------------------------------------
! Subroutine names for timing:
      character(8),parameter :: secnam(10)=(/
     &      'KINBUR  ','RELCON  ','UPDEOS  ','URSAPB  ','STEP    ',
     &      'DRIVE   ','PFKIN   ','UPSLOI  ','XXX     ','YYY     '/)

!======================================================================
!           PRINCIPAL CONTROL PARAMETERS in NAMELIST/INPUT/
!======================================================================
!----------------------------------------------------------------------
!     JOB-INVARIANT, i.e. SAVED for continuation and not allowed to be
!     changed at restart:
!----------------------------------------------------------------------
! Geometry and mesh:
      integer(4),save :: igeo=iundef,nmesh(1:nnz)=iundef,nz=1,nzeven=1
      real(8),save :: qzeven=1.d0

! Activated physics:
      integer(4),save :: iifcapei=3,iifHz=0,iiflas=0,iitemp=4,iifvis=1
      logical,save :: ifburn=.false.

! Boundary conditions, initial state:
      integer(4),save :: iflbnd=0,ifmfu0(1:nnz)=0,imatz0(1:nnz)=iundef
      real(8),save :: bro0pr(1:nnz)=1.d0,Hz0=0.d0,roz0(1:nnz)=undef,
     &      roz01(1:nnz)=undef,rz0(1:nnz+1)=undef,Te0(1:nnz)=0.d0,
     &      Ti0(1:nnz)=0.d0,Tr0(1:nnz)=0.d0,uz0(1:nnz+1)=0.d0,
     &      xB0(1:nnz)=0.d0,xD0(1:nnz)=0.d0,xH0(1:nnz)=0.d0,
     &      xHe0(1:nnz)=0.d0,xT0(1:nnz)=0.d0

! Run control:
      integer(4),save :: nrun=0,nt_vtk=0

!----------------------------------------------------------------------
!     JOB-VARIABLE, i.e. NOT SAVED for continuation and allowed to be
!     changed at restart:
!----------------------------------------------------------------------
! Activated physics:
      integer(4),save :: iifal=0,iifn14=0,iifn2=0,iifp14=0,iifp3=0,
     &      iifopac=2

! EOS, material properties:
      integer(4),save :: imatadd(nnmatrls)=iundef
      real(8),save :: propmat(100,nnmatrls)=undef,tau_pf=4.d0

! Target drive:
      real(8),save :: eionb=0.d0,tdrcal=ceiling,tdrfin=0.d0,wdriv=0.d0
!  driver focal：
      real(8),save :: R1B=0.0d0,R2B=1.0d0,P1OWER=0.0d0,TP=0.0d0

! Accuracy and time step control:
      real(8),save :: czddt=.05d0,czdriv=.03d0,czdt=.5d0,czdtq=.05d0,
     &      czdtvt=.05d0,czTkin=.03d0,czvkin=.03d0,
     &      cz_pf=.02d0,dt0=1.d-8,dtmax=1.d0,dtmin=1.d-18,
     &      flle=.5d0,flli=.5d0,fllr=.5d0,eemin=1.d-4,Hzmin=floor,
     &      Tburn0=.1d0,Tfloor=3.d-7,xbfloo=1.d-9

! Artificial viscosity:
      integer(4),save :: iartvis=1
      real(8),save :: sig=1.d0,sigt=.1d0,smu1=.1d0,smu2=0.d0,
     &      tmu1=0.d0,tmu2=2.d0

! Run- and printout-control:
      integer(4),save :: istart=0,kalpri=1,njpri=1,nddump=2**30,
     &      ncycfin=2**30,ncycprii(100)=2**30,ndrlin=2**30
      real(8),save :: tfin=0.d0,tprinii(100)=ceiling


!======================================================================
!              OTHER VARIABLES SAVED FOR CONTINUATION
!======================================================================
! Time-dependent variables (except for invariant nn):
      integer(4),save :: nn,ncyc,ncycpri,ncycdpri=2**30,ndump,nprin,
     &      ntbad,ntvbad,ntflle,ntflli,ntfllr,ndriv,jflgburn,jrorp,
     &      it_vtk=0
      real(8),save :: time,dt,dtprin,tprin,dthist,thist,rorafu,rorfu,
     &      rorfum,rorp,Tvaccd,Teincd,Hzincr,Teincr,Tiincr,Trincr,
     &      vincd,vincr,sec(10)=0.d0,seccumul=0.d0,tw_vtk=0.d0
      logical :: ifpfkin=.false.

!----------------------------------------------------------------------
! Boundary conditions:
      real(8),save :: pbl,pbr,Trlex,Trex,Hzbl,Hzbr,pblsum,pbrsum,
     &      pblold,pbrold,Hblold,Hbrold,pblsol,pbrsol,hebr

!----------------------------------------------------------------------
! Balance quantities:
      real(8),save :: ez0(1:nnz),erex,erin
      real(8),save :: ezdr(1:nnz),ezjl(1:nnz),ezfus(1:nnz),ezcl(1:nnz),
     &      ezn14(1:nnz),ezn2(1:nnz),ezal(1:nnz),ezp3(1:nnz),
     &      ezp14(1:nnz),eoutw(1:nnz+1),eoute(1:nnz+1),eouti(1:nnz+1),
     &      eoutr(1:nnz+1),eouthz(1:nnz+1),tnun2(1:nnz),tnun14(1:nnz),
     &      tnudhe3(1:nnz)
      real(8),target,save :: eouall(1:3*nnz+3)
! - global pointers associated once and for all with the target EOUALL:
      real(8),pointer :: eoutal(:),eoutp3(:),eoup14(:)      ! => eouall

!----------------------------------------------------------------------
! Arrays for pfe-kinetics:
      integer(4),allocatable,save :: ims_pf(:)
      real(8),allocatable,save :: de_pf(:),de0_pf(:),q_pf(:),q0_pf(:)

! Arrays for vtk output:
      real(8),allocatable,save :: r_vtk(:,:),t_vtk(:),ro_vtk(:,:),
     &      pr_vtk(:,:)

!----------------------------------------------------------------------
! Principal field variables:
      integer(4),allocatable,save :: ifdmfu(:),matnum(:)
      real(8),allocatable,save :: dm(:),r(:),u(:),v(:),Te(:),Ti(:),
     &      Tr(:),Hz(:),xD(:),xT(:),xHe(:),xH(:),xB(:)
      real(8),allocatable,target,save :: eall(:)
! - global pointers associated once and for all with a single target:
      real(8),pointer :: eal(:),ep3(:),ep14(:)  ! => eall

!======================================================================
!              OTHER VARIABLES NOT SAVED FOR CONTINUATION
!======================================================================
! Secondary control parameters and indicators:
      integer(4),allocatable,save :: ishlj(:)
      integer(4),save :: i1geo,n1,n2,nfu,njz(1:nnz+2),n1max,j_CFL,
     &      jtau140,k_CFL,itimpri,itimprmax,imatlist(nnmatrls),nmatrls,
     &      ndeb1,ndeb2

      real(8),save :: dtcfl,dtq,csurf,amsfu,tau140,secstart,
     &      amz(1:nnz),amol(1:nnz),xmol(1:nnz),
     &      zmol(1:nnz),z2mol(1:nnz),smol(1:nnz),
     &      hip14n(1:nnz),wn14(1:nnz+1),wn2(1:nnz+1)

      character(128) :: jobtit='???'

!----------------------------------------------------------------------
! EOS and material properties:
      real(8),save :: p_cri(nnmatrls)=undef,T_cri(nnmatrls)=undef,
     &      v_cri(nnmatrls)=undef

!----------------------------------------------------------------------
! Rates of energy transfer (powers) across layer interfaces:
      real(8),save :: plkw(1:nnz+1),plke(1:nnz+1),plkr(1:nnz+1)
      real(8),target,save :: plkall(1:3*nnz+3)
! - global pointers associated once and for all with the target PLKALL:
      real(8),pointer :: plkal(:),plkp3(:),plkp14(:)  ! => plkall

!----------------------------------------------------------------------
! Recalculated arrays:
      real(8),allocatable,save :: cape(:),capeb(:),capi(:),capib(:),
     &      capr(:),caprb(:),ethz(:),etvi0(:),etvi1(:),hiei(:),
     &      hier(:),Hzold(:),qdriv(:),qsBH(:),Teold(:),vold(:),yi(:),
     &      peden(:)  !=> peden femi electron force -> pe(\rho,tr=0)
      real(8),allocatable,target,save :: hiall(:),qall(:)
      logical,allocatable,save :: ifupeos(:)
! - global pointers associated once and for all with a single target:
      real(8),pointer :: hial(:),hip3(:),hip14(:)     ! => hiall
! - local pointers associated only temporarily:
      real(8),pointer :: qal(:),qp3(:),qp14(:)        ! => qall
! - The Coulomb logarithms are approximated as
      real(8) :: CLogarEE=0.0d0,CLogarEI=0.0d0
!----------------------------------------------------------------------
! Working memory, where each target array can be assigned to different
! pointers in different parts of the code:
      real(8),allocatable,target,save :: w1w01(:),w1w02(:),w1w03(:),
     &      w1w04(:),w1w05(:),w1w06(:),w1w07(:),w1w08(:),w1w09(:),
     &      w1w10(:),w1w11(:),w1w12(:),w1w13(:),w1w14(:),w1w15(:)

!----------------------------------------------------------------------
! Local pointers associated only temporarily with global working arrays
      real(8),pointer ::
     &      pe(:),a0p(:),a1p(:),a3p(:),aa11(:), ! => w1w01
     &      pi(:),aa21(:),b0p(:),dvdt(:),       ! => w1w02
     &      eet(:),aa12(:),                     ! => w1w03
     &      eev(:),aa13(:),                     ! => w1w04
     &      eit(:),aa22(:),                     ! => w1w05
     &      eiv(:),aa23(:),                     ! => w1w06
     &      us(:),scavis(:),vn(:),              ! => w1w07
     &      ee(:),                              ! => w1w08
     &      ei(:),                              ! => w1w09
     &      qe(:),bb1(:),                       ! => w1w10
     &      qi(:),bb2(:),                       ! => w1w11
     &      qjl(:),tenvis(:),b3p(:),aa31(:),    ! => w1w12
     &      qsDT(:),b1p(:),Hztil(:),aa32(:),    ! => w1w13
     &      qsDD(:),aa33(:),                    ! => w1w14
     &      qsDHe(:),bb3(:)                     ! => w1w15

      end module COMDEI
!___________________________________________________________ end COMDEI


!**********************************************************************
!                            COMTA3
!********************************************************* begin COMTA3
      module COMTA3
      implicit none
! Common blocks for the "DEITA3" tabular EOS model #3:
! - common blocks for the equation of state, filled from file 'EOSTA3':
      integer(4),save :: nlager,nwwww,nroo(6),nTemm(3,6)
      real(8),save :: Asub(5),Zsub(5),potofi(100,5),bopac(100,5),
     &      xlager(12),wlager(12),rwei(12),pwei(12),p00(5),e00(5),
     &      roill(5),hlroo(5),Temill(3,5),hlTemm(3,5),qurs(1000)
      real(4),save :: farr(30000)

! Here one should pay attention to ensure
! 3*(NROO(1)*NTEMM(3,1)+...+NROO(5)*NTEMM(3,5)) < 30000, and
! 4*(NROO(1)+...+NROO(5)) < 1000

! - common blocks for the ion beam, filled from file 'BEMTA3':
      integer(4),save :: nsh2(6),ne2(100)                     ! /BEAM2/
      real(8),save :: c11,c12,c13,c14,c15,c16,c17,c1r,        !/BECNST/
     &Abeam,Zbeam,b1,bet1,sgm1,amu1,ptif1,dinp1,apin1(4,100), ! /BEAM1/
     &pot2(350),esh2(100),gb0(5),b2(5),bet2(5),sgm2(5),amu2(5)! /BEAM2/

      end module COMTA3
!___________________________________________________________ end COMTA3


!**********************************************************************
!                       module MATKIT
!************************************************************beg MATKIT
      module MATKIT
      implicit none
      private
      public :: ZN,ZERA0,GQUAD,ELLICK,SECSCPU

      integer(4), parameter :: nglist(1:12)=(/4,8,12,16,20,24,32,40,48,
     &            64,80,96/)
      real(8), parameter :: xg(222)=(/3.39981043584856D-1,
     &8.61136311594053D-1,1.8343464249565D-1,
     &5.25532409916329D-1,7.96666477413627D-1,9.60289856497536D-1,
     &1.25233408511469D-1,3.67831498998180D-1,5.87317954286617D-1,
     &7.69902674194305D-1,9.04117256370475D-1,9.81560634246719D-1,
     &9.501250983763744D-2,2.816035507792589D-1,4.580167776572274D-1,
     &6.178762444026437D-1,7.554044083550030D-1,8.656312023878317D-1,
     &9.445750230732326D-1,9.894009349916499D-1,7.652652113349733D-2,
     &2.277858511416451D-1,3.737060887154196D-1,5.108670019508271D-1,
     &6.360536807265150D-1,7.463319064601508D-1,8.391169718222188D-1,
     &9.122344282513259D-1,9.639719272779138D-1,9.931285991850949D-1,
     &6.405689286260563D-2,1.911188674736163D-1,3.150426796961634D-1,
     &4.337935076260451D-1,5.454214713888395D-1,6.480936519369756D-1,
     &7.401241915785544D-1,8.200019859739029D-1,8.864155270044010D-1,
     &9.382745520027328D-1,9.747285559713095D-1,9.951872199970214D-1,
     &4.830766568773832D-2,1.444719615827965D-1,2.392873622521371D-1,
     &3.318686022821276D-1,4.213512761306353D-1,5.068999089322294D-1,
     &5.877157572407623D-1,6.630442669302152D-1,7.321821187402897D-1,
     &7.944837959679424D-1,8.493676137325699D-1,8.963211557660521D-1,
     &9.349060759377397D-1,9.647622555875064D-1,
     &9.856115115452683D-1,9.972638618494816D-1,
     &3.877241750605082D-2,1.160840706752552D-1,1.926975807013711D-1,
     &2.681521850072537D-1,3.419940908257585D-1,4.137792043716050D-1,
     &4.830758016861787D-1,5.494671250951282D-1,6.125538896679802D-1,
     &6.719566846141795D-1,7.273182551899271D-1,7.783056514265194D-1,
     &8.246122308333117D-1,8.659595032122595D-1,9.020988069688743D-1,
     &9.328128082786765D-1,9.579168192137917D-1,9.772599499837743D-1,
     &9.907262386994570D-1,9.982377097105592D-1,3.238017096286936D-2,
     &9.700469920946270D-2,1.612223560688917D-1,2.247637903946891D-1,
     &2.873624873554556D-1,3.487558862921607D-1,4.086864819907167D-1,
     &4.669029047509584D-1,5.231609747222330D-1,5.772247260839727D-1,
     &6.288673967765136D-1,6.778723796326639D-1,7.240341309238147D-1,
     &7.671590325157403D-1,8.070662040294426D-1,8.435882616243935D-1,
     &8.765720202742479D-1,9.058791367155697D-1,9.313866907065543D-1,
     &9.529877031604309D-1,9.705915925462473D-1,9.841245837228269D-1,
     &9.935301722663508D-1,9.987710072524261D-1,2.435029266342443D-2,
     &7.299312178779904D-2,1.214628192961206D-1,1.696444204239928D-1,
     &2.174236437400071D-1,2.646871622087674D-1,3.113228719902110D-1,
     &3.572201583376681D-1,4.022701579639916D-1,4.463660172534641D-1,
     &4.894031457070530D-1,5.312794640198945D-1,
     &5.718956462026340D-1,6.111553551723933D-1,6.489654712546573D-1,
     &6.852363130542332D-1,7.198818501716108D-1,7.528199072605319D-1,
     &7.839723589433414D-1,8.132653151227976D-1,8.406292962525804D-1,
     &8.659993981540928D-1,8.893154459951141D-1,9.105221370785028D-1,
     &9.295691721319396D-1,9.464113748584028D-1,9.610087996520537D-1,
     &9.733268277899110D-1,9.833362538846260D-1,9.910133714767443D-1,
     &9.963401167719553D-1,9.993050417357721D-1,1.951138325679400D-2,
     &5.850443715242067D-2,9.740839844158460D-2,1.361640228091439D-1,
     &1.747122918326468D-1,2.129945028576661D-1,2.509523583922721D-1,
     &2.885280548845119D-1,3.256643707477019D-1,3.623047534994873D-1,
     &3.983934058819692D-1,4.338753708317561D-1,4.686966151705445D-1,
     &5.028041118887850D-1,5.361459208971319D-1,5.686712681227098D-1,
     &6.003306228297517D-1,6.310757730468720D-1,6.608598989861198D-1,
     &6.896376443420276D-1,7.173651853620999D-1,7.440002975835973D-1,
     &7.695024201350414D-1,7.938327175046054D-1,8.169541386814635D-1,
     &8.388314735802553D-1,8.594314066631111D-1,8.787225676782138D-1,
     &8.966755794387707D-1,9.132631025717577D-1,9.284598771724458D-1,
     &9.422427613098727D-1,9.545907663436349D-1,9.654850890437993D-1,
     &9.749091405857278D-1,9.828485727386291D-1,
     &9.892913024997555D-1,9.942275409656883D-1,9.976498643982377D-1,
     &9.995538226516306D-1,1.627674484960297D-2,4.881298513604973D-2,
     &8.129749546442556D-2,1.136958501106659D-1,1.459737146548969D-1,
     &1.780968823676186D-1,2.100313104605672D-1,2.417431561638400D-1,
     &2.731988125910491D-1,3.043649443544964D-1,3.352085228926254D-1,
     &3.656968614723136D-1,3.957976498289086D-1,4.254789884073005D-1,
     &4.547094221677430D-1,4.834579739205964D-1,5.116941771546677D-1,
     &5.393881083243574D-1,5.665104185613972D-1,5.930323647775721D-1,
     &6.189258401254686D-1,6.441634037849671D-1,6.687183100439162D-1,
     &6.925645366421716D-1,7.156768123489676D-1,7.380306437444001D-1,
     &7.596023411766475D-1,7.803690438674332D-1,8.003087441391408D-1,
     &8.194003107379317D-1,8.376235112281871D-1,8.549590334346015D-1,
     &8.713885059092965D-1,8.868945174024204D-1,9.014606353158523D-1,
     &9.150714231208981D-1,9.277124567223087D-1,9.393703397527552D-1,
     &9.500327177844376D-1,9.596882914487425D-1,9.683268284632642D-1,
     &9.759391745851365D-1,9.825172635630147D-1,9.880541263296238D-1,
     &9.925439003237626D-1,9.959818429872093D-1,9.983643758631817D-1,
     &9.996895038832308D-1/)
      real(8), parameter :: wg(222)=(/6.52145154862546D-1,
     &3.47854845137454D-1,
     &3.62683783378362D-1,3.13706645877887D-1,2.22381034453374D-1,
     &1.01228536290376D-1,2.49147045813403D-1,2.33492536538355D-1,
     &2.03167426723066D-1,1.60078328543346D-1,1.06939325995318D-1,
     &4.717533638651200D-2,1.894506104550685D-1,1.826034150449236D-1,
     &1.691565193950025D-1,1.495959888165767D-1,1.246289712555339D-1,
     &9.515851168249278D-2,6.225352393864789D-2,2.715245941175409D-2,
     &1.527533871307258D-1,1.491729864726037D-1,1.420961093183821D-1,
     &1.316886384491766D-1,1.181945319615184D-1,1.019301198172404D-1,
     &8.327674157670475D-2,6.267204833410906D-2,4.060142980038694D-2,
     &1.761400713915212D-2,1.279381953467522D-1,1.258374563468283D-1,
     &1.216704729278034D-1,1.155056680537256D-1,1.074442701159656D-1,
     &9.761865210411389D-2,8.619016153195328D-2,7.334648141108031D-2,
     &5.929858491543678D-2,4.427743881741981D-2,2.853138862893366D-2,
     &1.234122979998720D-2,9.654008851472780D-2,9.563872007927486D-2,
     &9.384439908080457D-2,9.117387869576388D-2,8.765209300440381D-2,
     &8.331192422694676D-2,7.819389578707031D-2,7.234579410884851D-2,
     &6.582222277636185D-2,5.868409347853555D-2,5.099805926237618D-2,
     &4.283589802222668D-2,3.427386291302143D-2,2.539206530926206D-2,
     &1.627439473090567D-2,7.0186100094700097D-3,
     &7.750594797842481D-2,7.703981816424797D-2,7.611036190062624D-2,
     &7.472316905796826D-2,7.288658239580406D-2,7.061164739128678D-2,
     &6.791204581523390D-2,6.480401345660104D-2,6.130624249292894D-2,
     &5.743976909939155D-2,5.322784698393682D-2,4.869580763507223D-2,
     &4.387090818567327D-2,3.878216797447202D-2,3.346019528254785D-2,
     &2.793700698002340D-2,2.224584919416696D-2,1.642105838190789D-2,
     &1.049828453115281D-2,4.521277098533191D-3,6.473769681268392D-2,
     &6.446616443595008D-2,6.392423858464819D-2,6.311419228625403D-2,
     &6.203942315989266D-2,6.070443916589388D-2,5.911483969839564D-2,
     &5.727729210040322D-2,5.519950369998416D-2,5.289018948519367D-2,
     &5.035903555385447D-2,4.761665849249047D-2,4.467456085669428D-2,
     &4.154508294346475D-2,3.824135106583071D-2,3.477722256477044D-2,
     &3.116722783279809D-2,2.742650970835695D-2,2.357076083932438D-2,
     &1.961616045735553D-2,1.557931572294385D-2,1.147723457923454D-2,
     &7.327553901276262D-3,3.153346052305839D-3,4.869095700913972D-2,
     &4.857546744150343D-2,4.834476223480296D-2,4.799938859645831D-2,
     &4.754016571483031D-2,4.696818281621002D-2,4.628479658131442D-2,
     &4.549162792741814D-2,4.459055816375656D-2,4.358372452932345D-2,
     &4.247351512365359D-2,4.126256324262353D-2,
     &3.995374113272034D-2,3.855015317861563D-2,3.705512854024005D-2,
     &3.547221325688238D-2,3.380516183714161D-2,3.205792835485155D-2,
     &3.023465707240248D-2,2.833967261425948D-2,2.637746971505466D-2,
     &2.435270256871087D-2,2.227017380838325D-2,2.013482315353021D-2,
     &1.795171577569734D-2,1.572603047602472D-2,1.346304789671864D-2,
     &1.116813946013113D-2,8.846759826363948D-3,6.504457968978363D-3,
     &4.147033260562468D-3,1.783280721696433D-3,3.901781365630665D-2,
     &3.895839596276953D-2,3.883965105905197D-2,3.866175977407646D-2,
     &3.842499300695942D-2,3.812971131447764D-2,3.777636436200140D-2,
     &3.736549023873049D-2,3.689771463827601D-2,3.637374990583598D-2,
     &3.579439395341605D-2,3.516052904474759D-2,3.447312045175393D-2,
     &3.373321498461152D-2,3.294193939764540D-2,3.210049867348777D-2,
     &3.121017418811470D-2,3.027232175955798D-2,2.928836958326785D-2,
     &2.825981605727686D-2,2.718822750048638D-2,2.607523576756512D-2,
     &2.492253576411549D-2,2.373188286593010D-2,2.250509024633246D-2,
     &2.124402611578201D-2,1.995061087814200D-2,1.862681420829903D-2,
     &1.727465205626931D-2,1.589618358372569D-2,1.449350804050908D-2,
     &1.306876159240134D-2,1.162411412079783D-2,1.016176604110306D-2,
     &8.683945269260858D-3,7.192904768117313D-3,
     &5.690922451403199D-3,4.180313124694895D-3,2.663533589512682D-3,
     &1.144950003186942D-3,3.255061449236317D-2,3.251611871386884D-2,
     &3.244716371406427D-2,3.234382256857593D-2,3.220620479403025D-2,
     &3.203445623199266D-2,3.182875889441101D-2,3.158933077072717D-2,
     &3.131642559686136D-2,3.101033258631384D-2,3.067137612366915D-2,
     &3.029991542082759D-2,2.989634413632839D-2,2.946108995816791D-2,
     &2.899461415055524D-2,2.849741106508539D-2,2.797000761684833D-2,
     &2.741296272602924D-2,2.682686672559176D-2,2.621234073567241D-2,
     &2.557003600534936D-2,2.490063322248361D-2,2.420484179236469D-2,
     &2.348339908592622D-2,2.273706965832937D-2,2.196664443874435D-2,
     &2.117293989219130D-2,2.035679715433332D-2,1.951908114014502D-2,
     &1.866067962741147D-2,1.778250231604526D-2,1.688547986424517D-2,
     &1.597056290256229D-2,1.503872102699494D-2,1.409094177231486D-2,
     &1.312822956696157D-2,1.215160467108832D-2,1.116210209983850D-2,
     &1.016077053500842D-2,9.148671230783387D-3,8.126876925698759D-3,
     &7.096470791153865D-3,6.058545504235962D-3,5.014202742927518D-3,
     &3.964554338444687D-3,2.910731817934946D-3,1.853960788946922D-3,
     &7.967920655520124D-4/)

      contains

!**********************************************************************
!                            ZN
!************************************************************* begin ZN
      function ZN(n,hs,h1)
      implicit none
!======================================================================
!     This routine calculates the denominator of a geometric
!     progression given the number of terms N, the sum HS and
!     the 1-st term H1; sets Q1=1.d0  for the increasing, and
!     Q1=0.d0 for the decreasing progressions.
!======================================================================
! Arguments and result:
      integer(4), intent(in) :: n
      real(8), intent(in) :: hs,h1
      real(8) :: ZN

! Local variables:
      real(8) :: dq,f,q,q1,rab,rabn
!======================================================================

      if(n.lt.2) STOP 'STOP in ZN: n.le.1'
      rab=hs/h1
      rabn=n
      q=0.5d0
      dq=0.5d0

      if(rab.lt.1.d0) then
        STOP 'STOP in ZN: hs/h1 < 1'
      elseif(rab.eq.1.d0) then
        zn=0.d0
        goto 9000
      elseif(rab.lt.rabn) then
        goto 100
      elseif(rab.eq.rabn) then
        zn=1.d0
        goto 9000
      else
        goto 200
      endif

! Iterations for zn < 1:
 100  if(dq.lt.1.d-14) goto 130
      f=(1.d0-q**n)/(1.d0-q)-rab
      if(f.eq.0.d0) goto 130
      dq=0.5d0*dq
      q=q-sign(dq,f)
      goto 100
 130  zn=q
      goto 9000

! Iterations for zn > 1:
 200  if(dq.lt.1.d-14) goto 230
      q1=1.d0/q
      if(n*log(q1).gt.7.d2) then
        f=(exp(7.d2)-1.d0)/(q1-1.d0)-rab
      else
        f=(q1**n-1.d0)/(q1-1.d0)-rab
      endif
      if(f.eq.0.d0) goto 230
      dq=0.5d0*dq
      q=q+sign(dq,f)
      goto 200
 230  zn=1.d0/q

 9000 return
      end function ZN
!_______________________________________________________________ end ZN


!**********************************************************************
!                            ZERA0
!***********************************************************begin ZERA0
      function ZERA0(F,xl,xr,x0)
      implicit none
!======================================================================
!     This routine calculates the root x = ZERA0 of equation  F(x)=0
!     on the interval xl < x < xr by the bisection method.

!     xl < x0 < xr is the initial approximation;
!     absolute accuracy of the result x is better than 1.d-14*xl.

!     Calls: F(x)
!======================================================================
! Arguments and result:
      real(8), intent(in) :: xl,xr,x0
      real(8) :: ZERA0

! Local variables:
      integer(4),parameter :: nrcmax=300
      integer(4) :: kf,kfl,kfr,nrcal
      real(8) :: f2,f2sgn,fp,h,hmin,hmmin,x,xp

! Interfaces:
      interface
        function f(x)
          real(8) :: f
          real(8), intent(in) :: x
        end function f
      end interface
!======================================================================

      x=x0
      h=.2d0*(xr-xl)
      hmin=1.d-15*max(abs(xl),abs(xr))
      hmmin=.1d0*hmin
      fp=f(x)
      if(fp.eq.0.d0) goto 77
      nrcal=1
      kf=0
      kfr=0
      kfl=0
      xp=x

 10   x=x+h
      if(x.lt.xr) goto 14
      kfr=kfr+1
      x=.5d0*(xr+xp)
      goto 19
 14   kfr=0
      if(x.gt.xl) goto 19
      kfl=kfl+1
      x=.5d0*(xl+xp)
      goto 20
 19   kfl=0

 20   f2=f(x)
      nrcal=nrcal+1
      if(f2.eq.0.d0) goto 77
      f2sgn=f2/abs(f2)
      if(fp*f2sgn.gt.0.d0) goto 30
      if(abs(h).lt.hmin) goto 77
      h=-.3d0*h
      goto 40

 30   if(abs(fp).gt.abs(f2)) goto 40
      if(kf.eq.1) goto 33
      kf=1
      x=xp
      h=-h
      goto 10

 33   kf=0
      x=xp
      h=.3d0*h
      goto 50

 40   xp=x
      kf=0
      fp=f2

! Check for abnormal termination:
 50   if(abs(h).lt.hmmin) goto 77
      if(nrcal.gt.nrcmax) goto 66
!!!      if(xr-x.lt.hmmin.and.kfr.gt.2) goto 66
!!!      if(x-xl.lt.hmmin.and.kfl.gt.2) goto 66
      goto 10

 66   write(*,9010) nrcal,xl,x,xr,h,fp,f2
 77   zera0=x

 9000 return
 9010  format('::: F(x).ne.0 in ZERA: NRCAL=',I6/' XL,X,XR=',3es15.8/
     *' H=',es15.8,' FP,F2=',2es15.8)
      end function ZERA0
!_____________________________________________________________end ZERA0


!**********************************************************************
!                             GQUAD
!***********************************************************begin GQUAD
      function GQUAD(f,a,b,n)
      implicit none
!======================================================================
!     This function computes Gaussian quadrature of \int_a^b  f(x) dx.
!
!     Called by: ...
!     Calls    : f(x)
!======================================================================
! Arguments and result:
      integer(4), intent(in) :: n
      real(8), intent(in) :: a,b
      real(8) :: GQUAD

! Local variables:
      integer(4) :: j,k,ni
      real(8) :: xa,xb,r,s

! Interfaces:
      interface
        function f(x)
          real(8) :: f
          real(8), intent(in) :: x
        end function f
      end interface
!======================================================================

      ni=0
      do k=1,12
        if(n.eq.nglist(k)) goto 20
        ni=ni+nglist(k)/2
      enddo
      write(*,9010) n,nglist
      STOP 'STOP in GQUAD: requested N not available'

 20   xa=.5d0*(a+b)
      xb=.5d0*(b-a)
      s=0.d0
      j=n/2

      do k=1,j
        ni=ni+1
        r=f(xa+xb*xg(ni))+f(xa-xb*xg(ni))
        s=s+r*wg(ni)
      enddo

      GQUAD=s*xb

      return
 9010 format(/'STOP in GQUAD: the requested number of nodes N=',I6,
     &' is not available!'/'Available are: N=',12I3/)
      end function GQUAD
!____________________________________________________________ end GQUAD


!**********************************************************************
!                             ELLICK
!************************************************************beg ELLICK
      function ELLICK(x)
      implicit none
!======================================================================
!     This routine calculates the full first elliptic integral.

!     Called by:  KINBUR
!     Calls    :  none
!======================================================================
! Arguments:
      real(8),intent(in) :: x
      real(8) :: ellick

! Local variables:
      real(8) :: z
!======================================================================

      z=min(1.d0,1.d0-x)
      z=max(z,1.d-12)
      ellick=1.3862944d0+z*(.1119723d0+7.25296d-2*z)+
     +(.5d0+z*(.1213478d0+2.88729d-2*z))*log(1.d0/z)
      return
      end function ELLICK
!____________________________________________________________end ELLICK


!**********************************************************************
!                            SECSCPU
!******************************************************** begin SECSCPU
      function SECSCPU()
      implicit none
!======================================================================
!     This function computes the number of astronomical seconds from
!     the moment Year 2016, January 1, 00h:00sec by local clock.

!     Called by: many
!     Calls    : DATE_AND_TIME [intrinsic]
!======================================================================
! Result:
      real(8) :: SECSCPU

! Local variables:
      integer(4), parameter :: ndprem(1:12)=(/0,31,59,90,120,151,181,
     &            212,243,273,304,334/)
      integer(4) :: itimww(8),iyww,ndayspre,ndextra
      character(10) :: chww(3)
!======================================================================

      call DATE_AND_TIME(chww(1),chww(2),chww(3),itimww)

      iyww=itimww(1)-2016
      ndextra=-1
      if(mod(iyww,4).eq.0 .and. itimww(2).gt.2) ndextra=0
      ndayspre=365*iyww+ndprem(itimww(2))+itimww(3)+ndextra+(iyww+3)/4
      secscpu=86400.d0*ndayspre+3600.d0*itimww(5)+60.d0*itimww(6)
     &        +dble(itimww(7))+1.d-3*itimww(8)
      return
      end function SECSCPU
!__________________________________________________________ end SECSCPU

      end module MATKIT
!___________________________________________________________ end MATKIT


!**********************************************************************
!                       module GWEOS_rd
!********************************************************* beg GWEOS_rd
      module GWEOS_rd
      implicit none
      private
      public :: PE_GWR,S_GWR,SPND_T_NGWR,QZN,GZERA,BIND_NGWR,PES_EQGWR,
     &      PTS_EQGWR
      public :: ncorit_eqgwr
      integer(4), save :: ncorit_eqgwr=0
      real(8), parameter :: dfloor=1.d-13,floor=1.d-150,
     &      tetminbi=1.d-5,undef=-123456789.d0

      contains

!**********************************************************************
!                            PE_GWR
!********************************************************* begin PE_GWR
      subroutine PE_GWR(rrho,tet,gw_n,cv_gw,P,E,CS2,DPDR,DPDTET,DEDTET)
!======================================================================
!     This routine computes pressure P and energy E (plus their
!     derivatives) in the GWEOS-MS as functions of the reduced density
!     rrho and temperature tet. The density, temperature and pressure
!     are normalized to their critical values rho_cr, P_cr, T_cr.
!     The internal energy E and the square of the sound speed cs2 are
!     normalized to P_cr/rho_cr.

!     All the input and output quantities are dimensionless, i.e. in
!     the reduced form (or, equivalently, normalized).

! INPUT:
!     rrho  = rho/rho_cr -> normalized (reduced) density;
!     tet   = T/T_cr -> normalized (reduced) temperature;
!     gw_n  = user-defined power exponent;
!     cv_gw = user-defined heat capacity by constant volume per atom.

! OUTPUT:
!     p     = P/P_cr -> normalized pressure;
!     e     = epsilon*rho_cr/P_cr -> normalized mass-specific internal
!             energy;
!     cs2   = (rho_cr/P_cr)*(dP/drho)_s -> normalized square of the
!             isentropic sound speed;
!     dpdr  = (dp/drrho)_tet=constant;
!     dpdtet= (dp/dtet)_v=constant;
!     dedtet= (de/dtet)_v=constant -> normalized heat capacity by
!             constant volume;

! NOTES:
! 1. Must be wn > 1, cv > 0, rrho < cap, tet > 0.

!     Called by: ...
!     Calls    : none
!======================================================================
! Arguments:
      real(8), intent(in) :: cv_gw,gw_n,rrho,tet
      real(8), intent(out) :: e,p,cs2,dpdr,dpdtet,dedtet

! Local variables:
      real(8) :: alph,cap,cv,v00,wn
      real(8) :: tww,rww,ww0,ww1,ww2,ww3,ww4
!======================================================================
      rww=max(0.d0,rrho)
      tww=max(floor,tet)
      wn=max(1.d0+dfloor,gw_n)
      cv=max(floor,cv_gw)
      cap=(wn+1.d0)/(wn-1.d0)
      v00=1.d0/cap
      alph=cap-v00

      ww0=max(TINY(1.d0),rww)
      ww0=(wn-1.d0)*log(ww0)
      ww1=cap*exp(ww0)        ! = cap*rrho^(n-1)
      ww2=1.d0/(max(floor,1.d0-rww*v00))  ! = 1/(1-rrho/cap)
      ww3=alph*ww2                        ! = alpha/(1-rrho/cap)
      dpdtet=ww3*rww
      p=rww*(ww3*tww-ww1)
      dedtet=alph*cv
      e=dedtet*tww-.5d0*(cap-1.d0)*ww1
      ww4=ww3*ww2*tww
      dpdr=ww4-wn*ww1
      cs2=dpdr+ww4/cv

 9000 return
      end subroutine PE_GWR
!___________________________________________________________ end PE_GWR


!**********************************************************************
!                            S_GWR
!********************************************************** begin S_GWR
      subroutine S_GWR(rrho,rrhol,tet,gw_n,cv_gw,S)
!======================================================================
!     This routine computes the specific entropy S in the GWEOS-MS as
!     a function of the reduced density rrho and temperature tet.
!     The density and temperature are normalized to their critical
!     values rho_cr and T_cr.
!     The specific entropy is normalized to P_cr/(rho_cr*T_cr).

!     All the input and output quantities are dimensionless, i.e. in
!     the reduced form (or, equivalently, normalized).

! INPUT:
!     rrho  = rho/rho_cr -> normalized (reduced) density;
!     rrhol = ln(rho/rho_cr) -> ln of normalized (reduced) density;
!     tet   = T/T_cr -> normalized (reduced) temperature;
!     gw_n  = power exponent;
!     cv_gw = heat capacity by constant volume per one atom.

! OUTPUT:
!     s     = S*rho_cr*T_cr/P_cr -> normalized specific entropy.

! NOTES:
! 1. Must be wn > 1, cv > 0, rrho < cap,  tet > 0.

!     Called by: ...
!     Calls    : none
!======================================================================
! Arguments:
      real(8), intent(in) :: cv_gw,gw_n,rrho,rrhol,tet
      real(8), intent(out) :: s

! Local variables:
      real(8) :: alph,cap,cv,v00,wn
      real(8) :: tww,ww0
!======================================================================
      tww=max(TINY(1.d0),tet)
      wn=max(1.d0+dfloor,gw_n)
      cv=max(floor,cv_gw)
      cap=(wn+1.d0)/(wn-1.d0)
      v00=1.d0/cap
      alph=cap-v00

      ww0=max(TINY(1.d0),1.d0-rrho*v00)   ! = 1-rrho/cap
      s=alph*(cv*(1.d0+log(tww))+log(ww0)-rrhol)

 9000 return
      end subroutine S_GWR
!____________________________________________________________ end S_GWR


!**********************************************************************
!                            SPND_T_NGWR
!**************************************************** begin SPND_T_NGWR
      subroutine SPND_T_NGWR(tet,gw_n,cv_gw,RSPLIQ,RSPGAS,F_SPLIQ,
     &                       F_SPGAS,NFLIQ,NFGAS)
!======================================================================
!     This routine computes the densities (in units of rho_cr) of the
!     liquid, RSPLIQ, and of the gas, RSPGAS, branches of the spinodal
!     as functions of temperature tet (in units T_cr) for the
!     generalized van-der-Waals EOS in its reduced form, where density,
!     temperature and pressure are normalized to their critical
!     values rho_cr, P_cr, T_cr.
!     Numerical method: Newton iterations.

!     All the input and output quantities are dimensionless, i.e. in
!     the reduced form (or, equivalently, normalized).

! INPUT:
!     tet   = T/T_cr -> normalized temperature;
!     gw_n  = power exponent;
!     cv_gw = heat capacity by constant volume per one atom.

! OUTPUT:
!     rspliq  = normalized density on the liquid spinodal branch;
!     rspgas  = normalized density on the gaseous spinodal branch;
!     f_spliq = function value that must be zero on liquid branch;
!     f_spgas = function value that must be zero on gaseous branch;
!     nfliq   = number of Newton iterations on the liquid branch;
!     nfgas   = number of Newton iterations on the gaseous branch.

! NOTES:
! 1. Must be wn>1, cv>0, tet>0.

!     Called by: ...
!     Calls    : none
!======================================================================
! Arguments:
      integer(4), intent(out) :: nfliq,nfgas
      real(8), intent(in) :: cv_gw,gw_n,tet
      real(8), intent(out) :: rspliq,rspgas,f_spliq,f_spgas

! Local variables:
      integer(4), parameter :: nitmaxww=50
      integer(4) :: nitww
      real(8), parameter :: efl_sp=1.d-11,eps8=1.d-15
      real(8) :: a1mv00,alph,cap,cv,v00,wn
      real(8) :: Agww,Alww,Fpdww,Fpww,Fww,tww,ww0,ww1,ww2,ww3,xpww,xww
!======================================================================
      tww=max(floor,tet)
      if(tww.ge.1.d0) then
        rspliq=1.d0
        rspgas=1.d0
        goto 9000
      endif

!----------------------------------------------------------------------
!     Step 1: Initialize.
!----------------------------------------------------------------------
      wn=max(1.d0+dfloor,gw_n)
      cv=max(floor,cv_gw)
      cap=(wn+1.d0)/(wn-1.d0)
      v00=1.d0/cap
      alph=cap-v00
      a1mv00=1.d0-v00
      Alww=sqrt(cap**(wn-1.d0)/tww)
      Agww=1.d0/(a1mv00**2*tww)**(.5d0*(cap-1.d0))

!----------------------------------------------------------------------
!     Step 2: Compute the liquid branch (Newton iterations).
!----------------------------------------------------------------------
      nitww=0
      ww3=.5d0*(1.d0-wn)
      xpww=1.d0/(Alww-v00)
      ww0=1.d0-a1mv00*xpww
      ww1=ww0**ww3
      Fpww=Alww*xpww-ww1
 22   nitww=nitww+1
      if(nitww.gt.nitmaxww) then
        xww=xpww
        Fww=Fpww
        goto 26
      endif
      Fpdww=Alww-ww1/(cap*ww0)
      xww=xpww-Fpww/(Fpdww+sign(floor,Fpdww))
      xww=max(0.d0,min(1.d0,xww))

      ww0=1.d0-a1mv00*xww
      ww1=ww0**ww3
      Fww=Alww*xww-ww1
      if(abs(Fww).le.eps8*ww1) goto 26
      if(abs(xww-xpww).le.eps8*(abs(xww)+abs(xpww))) goto 26

      if(Fww.gt.1.d2*eps8*ww1) then
        write(*,9008) tet,xww,Fww
        stop 'STOP in SPND_T_NGWR-liq.branch: F_spl>>0'
      elseif(Fww.gt.0.d0) then
        if(nitww.eq.1) then
          write(*,9004) tet,xww,Fww
        endif
! - linear interpolation between two points:
        ww2=Fpww-Fww
        xww=(xww*Fpww-xpww*Fww)/(ww2+sign(floor,ww2))
        Fww=Alww*xww-(1.d0-a1mv00*xww)**ww3
        nitww=nitww+1
        goto 26
      endif

      xpww=xww
      Fpww=Fww
      goto 22

 26   f_spliq=Fww
      rspliq=cap*(1.d0-a1mv00*xww)
      nfliq=nitww

      if(nfliq.ge.nitmaxww .or. abs(f_spliq).gt.efl_sp) then
!@@        write(*,9010) tet,nfliq,f_spliq
        write(*,9011) tet,nfliq,f_spliq
        stop 'STOP in SPND_T_NGWR-9011: no convergence for RSPLIQ'
      endif

!----------------------------------------------------------------------
!     Step 3: Compute the gaseous branch (Newton iterations).
!----------------------------------------------------------------------
      nitww=0
      ww3=1.d0-cap
      xpww=1.d0/(Agww-1.d0+v00)
      ww0=1.d0-v00*xpww
      ww1=ww0**ww3
      Fpww=Agww*xpww-ww1
 32   nitww=nitww+1
      if(nitww.gt.nitmaxww) then
        xww=xpww
        Fww=Fpww
        goto 36
      endif
      Fpdww=Agww-a1mv00*ww1/ww0
      xww=xpww-Fpww/(Fpdww+sign(floor,Fpdww))
      xww=max(0.d0,min(1.d0,xww))

      ww0=1.d0-v00*xww
      ww1=ww0**ww3
      Fww=Agww*xww-ww1
      if(abs(Fww).le.eps8*ww1) goto 36
      if(abs(xww-xpww).le.eps8*(abs(xww)+abs(xpww))) goto 36

      if(Fww.gt.1.d2*eps8*ww1) then
        write(*,9018) tet,xww,Fww
        stop 'STOP in SPND_T_NGWR-gas.branch: F_spg>>0'
      elseif(Fww.gt.0.d0) then
        if(nitww.eq.1) then
          write(*,9014) tet,xww,Fww
        endif
! - linear interpolation between two points:
        ww2=Fpww-Fww
        xww=(xww*Fpww-xpww*Fww)/(ww2+sign(floor,ww2))
        Fww=Agww*xww-(1.d0-v00*xww)**ww3
        nitww=nitww+1
        goto 36
      endif

      xpww=xww
      Fpww=Fww
      goto 32

 36   rspgas=xww
      f_spgas=Fww
      nfgas=nitww

      if(nfgas.ge.nitmaxww .or. abs(f_spgas).gt.efl_sp) then
!@@        write(*,9020) tet,nfgas,f_spgas
        write(*,9021) tet,nfgas,f_spgas
        stop 'STOP in SPND_T_NGWR-9021: no convergence for RSPGAS'
      endif

 9000 return
 9004 format('WARN from SPND_T_NGWR: F_spl>0 at nit=1: tet=',es11.4,
     &' xww,Fww=',es11.4,es8.1)
 9008 format(/'STOP in SPND_T_NGWR-liq.branch: F_spl>0: tet=',es15.8/
     &10x,'xww,Fww=',es15.8,es10.2)
 9010 format('WARN from SPND_T_NGWR: no conv.for r_spl, T=',es11.4,
     &' nFl=',I4,' F=',es9.2)
 9011 format(/'STOP in SPND_T_NGWR: no conv.for r_spl, T=',es13.6,
     &' nFl=',I4,' F=',es9.2)
 9014 format('WARN from SPND_T_NGWR: F_spg>0 at nit=1: tet=',es11.4,
     &' xww,Fww=',es11.4,es8.1)
 9018 format('STOP in SPND_T_NGWR-gas.branch: F_spg>0: tet=',es15.8/
     &10x,'xww,Fww=',es15.8,es10.2)
 9020 format('WARN from SPND_T_NGWR: no conv.for r_spg, T=',es11.4,
     &' nFg=',I4,' F=',es9.2)
 9021 format(/'STOP in SPND_T_NGWR: no conv.for r_spg, T=',es13.6,
     &' nFg=',I4,' F=',es9.2)
      end subroutine SPND_T_NGWR
!______________________________________________________ end SPND_T_NGWR


!**********************************************************************
!                            QZN
!************************************************************ begin QZN
      function QZN(n,hs,h1,IERR)
      implicit none
!======================================================================
!     This routine calculates the denominator of a geometric
!     progression given the number of terms N, the sum HS and
!     the 1-st term H1; sets Q1=1.d0  for the increasing, and
!     Q1=0.d0 for the decreasing progressions.

!     Called by: ...
!     Calls    : none
!======================================================================
! Arguments and result:
      integer(4), intent(in) :: n
      integer(4), intent(out) :: ierr
      real(8), intent(in) :: hs,h1
      real(8) :: qzn

! Local variables:
      real(8) :: dq,f,q,q1,rab,rabn
!======================================================================
      ierr=0
      if(n.lt.2) then
        ierr=1
        goto 9000
      endif
      rab=hs/h1
      rabn=n
      q=0.5d0
      dq=0.5d0

      if(rab.lt.1.d0) then
        ierr=2
        goto 9000
      elseif(rab.eq.1.d0) then
        qzn=0.d0
        goto 9000
      elseif(rab.lt.rabn) then
        goto 100
      elseif(rab.eq.rabn) then
        qzn=1.d0
        goto 9000
      else
        goto 200
      endif

! Iterations for qzn < 1:
 100  if(dq.lt.1.d-14) goto 130
      f=(1.d0-q**n)/(1.d0-q)-rab
      if(f.eq.0.d0) goto 130
      dq=0.5d0*dq
      q=q-sign(dq,f)
      goto 100
 130  qzn=q
      goto 9000

! Iterations for qzn > 1:
 200  if(dq.lt.1.d-14) goto 230
      q1=1.d0/q
      if(n*log(q1).gt.7.d2) then
        f=(exp(7.d2)-1.d0)/(q1-1.d0)-rab
      else
        f=(q1**n-1.d0)/(q1-1.d0)-rab
      endif
      if(f.eq.0.d0) goto 230
      dq=0.5d0*dq
      q=q+sign(dq,f)
      goto 200
 230  qzn=1.d0/q

 9000 return
      end function QZN
!______________________________________________________________ end QZN


!**********************************************************************
!                            GZERA
!********************************************************** begin GZERA
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
!____________________________________________________________ end GZERA


!**********************************************************************
!                            BIND_NGWR
!***************************************************** begin BIND_NGWR
      subroutine BIND_NGWR(tet,gw_n,cv_gw,RBNLIQ,RBNGAS,RLBNGAS,F_BN_T,
     &                     NFBN_T)
!======================================================================
!     This routine computes the densities (in units of rho_cr) of the
!     liquid, RBNLIQ, and of the gas, RBNGAS, branches of the binodal
!     as functions of the temperature tet (in units T_cr) for the
!     generalized van-der-Waals EOS in its reduced form, where the
!     density,temperature and pressure are normalized to their critical
!     values rho_cr, P_cr, T_cr.

!     All the input and output quantities are dimensionless, i.e. in
!     the reduced form (or, equivalently, normalized).

! INPUT:
!     tet   = T/T_cr -> normalized temperature;
!     gw_n  = power exponent;
!     cv_gw = heat capacity by constant volume per one atom.

! OUTPUT:
!     rbnliq  = normalized density of the liquid spinodal branch;
!     rbngas  = normalized density of the gaseous spinodal branch;
!     rlbngas = ln(rbngas);
!     f_bn_t = function value that must be zero at the binodal;
!     nfbn_t = number of calls of F_BN_T_GWR;

! NOTES:
! 1. Must be wn>1, cv>0, tet>0.

!     Called by: ...
!     Calls    : GZER_bi,SPND_T_NGWR,F_BN_T_GWR
!======================================================================
! Arguments:
      integer(4), intent(out) :: nfbn_t
      real(8), intent(in) :: cv_gw,gw_n,tet
      real(8), intent(out) :: rbnliq,rbngas,rlbngas,f_bn_t

! Local variables:
      integer(4) :: imethd,nfcmaxww,nfcall,nfgaww,nfliww
      real(8), parameter :: efl_bn=1.d-9,eps_bn=1.d-14
      real(8) :: a1mv00,alph,cap,captown,cv,v00,wn
      real(8) :: cc2Fpww,ccFbnww,f_spgww,f_splww,o1rgww_,pgv00ww,
     &      psp_l_ww,rbnlww,rg1mww_,rgn1ww_,rgww_,rsp_g_ww,rsp_l_ww,
     &      tww,vlbngww,ww0,ww1,ww2,xmiww,xmaww
!======================================================================
      tww=max(floor,tet)
      if(tww.ge.1.d0) then
        rbnliq=1.d0
        rbngas=1.d0
        rlbngas=0.d0
        goto 900
      endif

!----------------------------------------------------------------------
!     Step 1: Initialize.
!----------------------------------------------------------------------
      imethd=1
      nfcmaxww=200
      wn=max(1.d0+dfloor,gw_n)
      cv=max(floor,cv_gw)
      cap=(wn+1.d0)/(wn-1.d0)
      captown=cap**wn
      v00=1.d0/cap
      alph=cap-v00
      a1mv00=1.d0-v00
      ccFbnww=.5d0*cap/a1mv00

!----------------------------------------------------------------------
!     Step 2: Compute the spinodal.
!----------------------------------------------------------------------
      call SPND_T_NGWR(tww,gw_n,cv_gw,RSP_L_WW,RSP_G_WW,F_SPLWW,
     &                 F_SPGWW,NFLIWW,NFGAWW)

! - compute the spinodal pressure on the liquid branch:
      psp_l_ww=alph*tww*rsp_l_ww/(1.d0-rsp_l_ww*v00)-cap*rsp_l_ww**wn

!----------------------------------------------------------------------
!     Step 3: Compute the binodal.
!----------------------------------------------------------------------
      xmiww=-log(rsp_g_ww)
      xmaww=log(tww*v00)+.5d0*captown/(tww*(1.d0+v00))
      imethd=1

      call GZER_bi(F_BN_T_GWR,xmiww,xmaww,eps_bn,nfcmaxww,imethd,
     &           VLBNGWW,F_BN_T,NFBN_T)

      if(nfbn_t.ge.nfcmaxww .or. abs(f_bn_t).gt.efl_bn) then
        write(*,9010) tet,nfbn_t,f_bn_t
      endif

      RBNLIQ=rbnlww
      RLBNGAS=-vlbngww
      RBNGAS=exp(-vlbngww)

  900 return
 9010 format('WARN from BIND_NGWR:no conv.in GZER_bi, T=',es15.8
     &,' nF=',I4,' F=',es8.1)

      contains


!**********************************************************************
!                            F_BN_T_GWR
!***************************************************** begin F_BN_T_GWR
      function F_BN_T_GWR(x_)
!======================================================================
!     This routine computes the function to be zeroed to compute the
!     binodal.
!     Newton iterations are used when computing the liquid-branch
!     density RBNLWW.

!     All the input and output quantities are dimensionless, i.e. in
!     the reduced form (or, equivalently, normalized).

! INPUT:
!     x_ = ln(v_g) -> normalized (reduced) spec.volume on gas branch;

! OUTPUT:
!     F_BN_T_GWR= function to be zeroed.

! NOTES:
!     1. Must be v_g >= v_sp_g = 1/rsp_g_ww.

!     Called by: BIND_NGWR
!     Calls    : none
!======================================================================
! Arguments:
      real(8), intent(in) :: x_
      real(8) :: F_BN_T_GWR

! Local variables:
      integer(4), parameter :: nfpmax_=50
      integer(4) :: nfpww_
      real(8), parameter :: dfloor_=1.d-15,epsp_=1.d-15
      real(8) :: Alww_,Fpdww_,Fpww_,Fww_,pgww_,rtown_,rwww_,ww0_,
     &      ww1_,xpww_,xww_
!======================================================================
!----------------------------------------------------------------------
!     Step 1: Initialize.
!----------------------------------------------------------------------
      rgww_=exp(-x_)                ! rrho_g
      rgn1ww_=exp(-x_*(wn-1.d0))    ! rrho_g^(n-1)
      rg1mww_=1.d0-rgww_*v00        ! 1-rrho_g/cap
      o1rgww_=1.d0/rg1mww_          ! 1/(1-rrho_g/cap)
      cc2Fpww=.125d0*(3.d0-rgww_)/tww
      Alww_=(1.d0+v00)*tww
! - pressure on the gas branch:
      pgww_=alph*tww*rgww_/(1.d0-rgww_*v00)-cap*rgww_*rgn1ww_
      pgv00ww=pgww_*v00

!----------------------------------------------------------------------
!     Step 2: Compute the liquid-branch density RBNLWW from equal
!             pressure on the gas branch.
!----------------------------------------------------------------------
      IF(pgww_.le.psp_l_ww*(1.d0+dfloor_)) THEN
        rbnlww=rsp_l_ww
        nfpww_=1
        Fww_=1.d0
      ELSE
! Newton iterations:
        nfpww_=0
        xpww_=(captown+pgv00ww)/Alww_
        rwww_=xpww_/(1.d0-v00+v00*xpww_)
        rtown_=rwww_**wn
        ncorit_eqgwr=min(999999999,ncorit_eqgwr+1)
        Fpww_=Alww_*xpww_-rtown_-pgv00ww
 22     nfpww_=nfpww_+1
        if(nfpww_.gt.nfpmax_) then
          xww_=xpww_
          Fww_=Fpww_
!@@        write(*,9020) tww,xww_,Fww_,nfpww_
          write(*,9021) tww,xww_,Fww_,nfpww_
          stop 'STOP in F_BN_T_GWR-9021: no conv.in Newton for RBNLWW'
          goto 26
        endif
        Fpdww_=Alww_-wn*a1mv00*rtown_*rwww_/xpww_**2
        xww_=xpww_-Fpww_/(Fpdww_+sign(floor,Fpdww_))
        xww_=max(1.d0,xww_)
        rwww_=xww_/(1.d0-v00+v00*xww_)
        rtown_=rwww_**wn
        ncorit_eqgwr=min(999999999,ncorit_eqgwr+1)
        Fww_=Alww_*xww_-rtown_-pgv00ww
        ww0_=epsp_*(rtown_+abs(pgv00ww))
        if(abs(Fww_).le.ww0_) goto 26
        if(abs(xww_-xpww_).le.epsp_*(abs(xww_)+abs(xpww_))) goto 26

        if(Fww_.lt.-1.d2*ww0_) then
          write(*,9008) tww,xww_,Fww_
          stop 'STOP in F_BN_T_GWR: Fww_<<0'
        elseif(Fww_.lt.0.d0) then
          if(nfpww_.eq.1) then
            write(*,9004) tww,xww_,Fww_
          endif
! - linear interpolation between two points:
          ww1_=Fpww_-Fww_
          xww_=(xww_*Fpww_-xpww_*Fww_)/(ww1_+sign(floor,ww1_))
          Fww_=Alww_*xww_-(xww_/(1.d0-v00+v00*xww_))**wn-pgv00ww
          ncorit_eqgwr=min(999999999,ncorit_eqgwr+1)
          nfpww_=nfpww_+1
          goto 26
        endif

        xpww_=xww_
        Fpww_=Fww_
        goto 22

 26     rbnlww=xww_/(1.d0-v00+v00*xww_)
      ENDIF

!----------------------------------------------------------------------
!     Step 3: Compute the function for binodal.
!----------------------------------------------------------------------
      ww0_=1.d0/(1.d0-rbnlww*v00)
      F_BN_T_GWR=tww*(x_+log(rbnlww*rg1mww_*ww0_)+ww0_-o1rgww_)+
     &           ccFbnww*(rgn1ww_-rbnlww**(wn-1.d0))
      ncorit_eqgwr=min(999999999,ncorit_eqgwr+1)

 9000 return
 9004 format('WARN from F_BN_T_GWR: Fww_<0 at nit=1: tet=',es11.4,
     &' xww_,Fww_=',es11.4,es8.1)
 9008 format(/'STOP in F_BN_T_GWR: Fww_<<0: tet=',es15.8/
     &10x,'xww_,Fww_=',es15.8,es10.2)
 9020 format('WARN from F_BN_T_GWR-9020: no conv.in Newton for RBNLWW:'
     &,'tet=',es13.6/10x,'xww_=',es22.14,' Fww_=',es10.2,' nfpww_=',I4)
 9021 format(/'STOP in F_BN_T_GWR-9021: no conv.in Newton for RBNLWW:',
     &'tet=',es13.6/10x,'xww_=',es22.14,' Fww_=',es10.2,' nfpww_=',I4)
      end function F_BN_T_GWR
!_______________________________________________________ end F_BN_T_GWR


!**********************************************************************
!                            GZER_bi
!******************************************************** begin GZER_bi
      subroutine GZER_bi(F,xl,xr,eps,nfcallmax,imethd,XOUT,FOUT,
     &      NFCALLS)
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
      real(8), parameter :: dfloor_=1.d-15,floor_=1.d-200
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
      hmin=eps*max(abs(xl),abs(xr))+floor_
      hmmin=.03d0*hmin
      dxl=xl+dfloor_*abs(xl)+floor_
      dxr=xr-dfloor_*abs(xr)-floor_

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
      if(abs(h).le.dfloor_*abs(xp)+floor_) then
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
      end subroutine GZER_bi
!__________________________________________________________ end GZER_bi

      end subroutine BIND_NGWR
!_______________________________________________________ end BIND_NGWR


!**********************************************************************
!                            PES_EQGWR
!****************************************************** begin PES_EQGWR
      subroutine PES_EQGWR(rrho,rrhol,tet,gw_n,cv_gw,nco,P,E,S,CS2,
     &      DPDR,DPDTET,DEDTET,RBNLIQ,RBNGAS,RLBNGAS)
!======================================================================
!     This routine computes the arrays of pressure p(:), internal
!     energy e(:) (plus their derivatives), and entropy s(:) along
!     the isotherm T/T_cr=tet for the equilibrium (EQ-GWEOS) version
!     (i.e. with the Maxwell construction under the binodal) of the
!     generalized van-der-Waals EOS in its reduced form, where the
!     density, temperature and pressure are normalized to their
!     critical values rho_cr, P_cr, T_cr. The internal energy e and the
!     square of the sound speed cs2 are normalized to P_cr/rho_cr.
!     Given are the nco density points rrho(1:nco) along the given
!     isotherm tet.

!     All the input and output quantities are dimensionless, i.e. in
!     the reduced form (or, equivalently, normalized).

! INPUT:
!     rrho(1:nco)  = rho/rho_cr -> normalized (reduced) densities;
!     rrhol(1:nco) = ln(rho/rho_cr) -> logarithms of reduced densities;
!     tet   = T/T_cr -> normalized (reduced) temperature;
!     gw_n  = user-defined power exponent;
!     cv_gw = user-defined heat capacity by constant volume per atom.

! OUTPUT:
!     p(1:nco)     = P/P_cr -> normalized pressures;
!     e(1:nco)     = epsilon*rho_cr/P_cr -> normalized mass-specific
!                    internal energies;
!     s(1:nco)     = S*rho_cr*T_cr/P_cr -> normalized specific entropy;
!     cs2(1:nco)   = (rho_cr/P_cr)*(dP/drho)_s -> normalized square of
!                    the isentropic sound speed;
!     dpdr(1:nco)  = (dp/drrho)_tet=constant;
!     dpdtet(1:nco)= (dp/dtet)_v=constant;
!     dedtet(1:nco)= (de/dtet)_v=constant -> normalized heat capacity
!                    by constant volume;
!     rbnliq       = liquid-branch density on the binodal for given tet
!     rbngas       = gas-branch density on the binodal for given tet;
!     rlbngas      = ln(rbngas);

! NOTES:
! 1. Must be wn > 1, cv > 0, rrho < cap, tet > 0.
! 2. When tet >= 1, the output values rbnliq=rbngas=rlbngas=UNDEF are
!    assigned.

!     Called by: ...
!     Calls    : BIND_NGWR
!======================================================================
! Arguments:
      integer(4), intent(in) :: nco
      real(8), intent(in) :: cv_gw,gw_n,rrho(nco),rrhol(nco),tet
      real(8), intent(out) :: p(nco),e(nco),s(nco),cs2(nco),dpdr(nco),
     &      dpdtet(nco),dedtet(nco),rbnliq,rbngas,rlbngas

! Local variables:
      integer(4) :: k,nfbn_t
      real(8) :: alph,cap,cv,v00,wn
      real(8) :: anug,anul,ccev,dpdtorg,dlvgdt,dlvldt,f_bn_t,
     &      o1mrgorl,rgas,rgn1,rgor,rgorl,rlgas,rln1,rliq,rww,
     &      sigg,sigl,tww,ww0,ww1,ww1g,ww1l,ww2,ww3,ww4
      logical :: ifnobn
!======================================================================
!----------------------------------------------------------------------
!     Step 1: Initial assignments.
!----------------------------------------------------------------------
      tww=max(floor,tet)
      ifnobn=.true.
      wn=max(1.d0+dfloor,gw_n)
      cv=max(floor,cv_gw)
      cap=(wn+1.d0)/(wn-1.d0)
      v00=1.d0/cap
      alph=cap-v00

      rbnliq=undef
      rbngas=undef
      rlbngas=undef

!----------------------------------------------------------------------
!     Step 2: MAIN LOOP.
!----------------------------------------------------------------------
      DO k=1,nco
        rww=max(0.d0,rrho(k))
        if(tww.ge.1.d0) goto 1000
        if(ifnobn) then
          call BIND_NGWR(tww,gw_n,cv_gw,RLIQ,RGAS,RLGAS,F_BN_T,NFBN_T)
          ifnobn=.false.
          rbnliq=rliq
          rbngas=rgas
          rlbngas=rlgas
        endif
        if(rww.ge.rliq .or. rrhol(k).le.rlgas) goto 1000

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!          EQ-EOS (Maxwell's construction) under the binodal
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ccev=.5d0*cap*(cap-1.d0)
        rgn1=exp((wn-1.d0)*rlgas)   ! = rho_g^(n-1)
        rln1=rliq**(wn-1.d0)        ! = rho_l^(n-1)
        rgorl=rgas/rliq             ! = rh_g/rho_l
        o1mrgorl=1.d0/(1.d0-rgorl)  ! = 1/(1-rho_g/rho_l)
        rgor=exp(rlgas-rrhol(k))    ! = rho_g/rho
        anul=(1.d0-rgor)*o1mrgorl
        anug=(rgor-rgorl)*o1mrgorl

        ww0=alph*tww
        ww1g=max(floor,1.d0-rgas*v00)
        ww1l=max(floor,1.d0-rliq*v00)
        p(k)=rgas*(ww0/ww1g-cap*rgn1)
        e(k)=ww0*cv-ccev*(anul*rln1+anug*rgn1)
        s(k)=alph*(cv*(1.d0+log(tww))+anul*
     &       log(max(floor,1.d0/rliq-v00))+anug*(log(ww1g)-rlgas))
        if(tww.lt..999999d0) then
          dpdtorg=alph*o1mrgorl*(-rlgas+log(rliq*ww1g/ww1l))
        else
          dpdtorg=(cap+1.d0)/rgas
        endif
        dpdtet(k)=rgas*dpdtorg
        dpdr(k)=0.d0

        ww2=(rgn1-rln1)*o1mrgorl
        sigl=anul*(wn*rln1-rgn1+ww2)
        sigg=anug*((wn-1.d0)*rgn1+ww2)

        ww1=ww1l**2
        ww3=wn*cap*rln1*ww1-ww0
        dlvldt=(dpdtorg*rgorl*ww1-alph*ww1l)/(ww3+sign(floor,ww3))
        ww1=ww1g**2
        ww3=wn*cap*rgn1*ww1-ww0
        dlvgdt=(dpdtorg*ww1-alph*ww1g)/(ww3+sign(floor,ww3))
        ww4=alph*cv+ccev*(sigl*dlvldt+sigg*dlvgdt)
        dedtet(k)=ww4
        cs2(k)=tet*(dpdtorg*rgor)**2/(max(floor,ww4))

        cycle

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                 MS-EOS (metastable) above the binodal
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 1000   ww1=cap*exp((wn-1.d0)*rrhol(k))   ! = cap*rrho^(n-1)
        ww2=1.d0/(max(floor,1.d0-rww*v00))! = 1/(1-rrho/cap)
        ww3=alph*ww2                      ! = alpha/(1-rrho/cap)
        dpdtet(k)=ww3*rww
        p(k)=rww*(ww3*tww-ww1)
        dedtet(k)=alph*cv
        e(k)=dedtet(k)*tww-.5d0*(cap-1.d0)*ww1
        ww4=ww3*ww2*tww
        dpdr(k)=ww4-wn*ww1
        cs2(k)=dpdr(k)+ww4/cv
        ww0=max(TINY(1.d0),1.d0-rww*v00)  ! = 1-rrho/cap
        s(k)=alph*(cv*(1.d0+log(tww))+log(ww0)-rrhol(k))
      ENDDO

 9000 return
      end subroutine PES_EQGWR
!________________________________________________________ end PES_EQGWR


!**********************************************************************
!                            PTS_EQGWR
!****************************************************** begin PTS_EQGWR
      subroutine PTS_EQGWR(rrho,e,gw_n,cv_gw,P,TET,EOUT,S,CS2,DPDR,
     &            DPDTET,DEDTET,RBNLIQ,RBNGAS,RLBNGAS,F_PTSEQ,NF_PTSEQ)
!======================================================================
!     This routine computes the temperature TET, pressure P (plus its
!     derivatives), and entropy S as functions of the reduced density
!     rrho and internal energy e in the EQ-GWEOS.

!     All the input and output quantities are dimensionless, i.e. in
!     the reduced form (or, equivalently, normalized).

! INPUT:
!     rrho  = rho/rho_cr -> normalized (reduced) density;
!     e     = epsilon*rho_cr/P_cr -> normalized mass-specific internal
!             energy;
!     gw_n  = user-defined power exponent;
!     cv_gw = user-defined heat capacity by constant volume per atom.

! OUTPUT:
!     p     = P/P_cr -> normalized pressure;
!     tet   = T/T_cr -> normalized (reduced) temperature;
!     eout  = epsilon*rho_cr/P_cr -> normalized mass-specific internal
!             energy (output value);
!     s     = S*rho_cr*T_cr/P_cr -> normalized specific entropy;
!     cs2   = (rho_cr/P_cr)*(dP/drho)_s -> normalized square of the
!              isentropic sound speed;
!     dpdr  = (dp/drrho)_tet=constant;
!     dpdtet= (dp/dtet)_v=constant;
!     dedtet= (de/dtet)_v=constant -> normalized heat capacity at
!              constant volume;
!     rbnliq    = liquid-branch density on the binodal for final tet;
!     rbngas    = gas-branch density on the binodal for fnal tet;
!     rlbngas   = ln(rbngas);
!     f_ptseq   = last value of the function FE_EQ that is to be zeroed
!                 when calculating TET;
!     nf_ptseq  = number of iterations (i.e. number of calls of
!                 function FE_EQ) when calculating TET;

! NOTES:
! 1. Must be wn > 1, cv > 0, rrho < cap, tet > 0.
! 2. When tet >= 1, the output values rbnliq=rbngas=rlbngas=UNDEF are
!    assigned.

!     Called by: ...
!     Calls    : BIND_NGWR,PES_EQGWR,GZERA,FE_EQ
!======================================================================
! Arguments:
      integer(4), intent(out) :: nf_ptseq
      real(8), intent(in) :: rrho,e,cv_gw,gw_n
      real(8), intent(out) :: tet,p,eout,s,cs2,dpdr,dpdtet,dedtet,
     &      rbnliq,rbngas,rlbngas,f_ptseq

! Local variables:
      integer(4) :: imethd,nf_bn_t,nfmaxww
      real(8), parameter :: eps_pts=1.d-14
      real(8) :: alph,cap,cv,v00,wn
      real(8) :: caprn1,cs2ww,dedtww,dpdrww,dpdtww,eouww,eww,f_bn_t,
     &      pww,rbnliqww,rbngasww,rlbngasww,rlww,rww,sww,tet_mi,
     &      tet_ms,tww,ww0,ww1,ww2,ww3,ww4
      real(8) :: acs2ww(1),adedtww(1),adpdrww(1),adpdtww(1),aeww(1),
     &      apww(1),arww(1),arlww(1),asww(1)
!======================================================================
!----------------------------------------------------------------------
!     Step 1: Initial assignments.
!----------------------------------------------------------------------
      wn=max(1.d0+dfloor,gw_n)
      cv=max(floor,cv_gw)
      cap=(wn+1.d0)/(wn-1.d0)
      v00=1.d0/cap
      alph=cap-v00

      imethd=1
      nfmaxww=200
      rbnliq=undef
      rbngas=undef
      rlbngas=undef

      rww=max(floor,min(cap*(1.d0-dfloor),rrho))
      rlww=log(rww)
      f_ptseq=0.d0
      nf_ptseq=0

!----------------------------------------------------------------------
!     Step 2: Sort out the MS-EQ cases.
!----------------------------------------------------------------------
! Compute the MS temperature tet_ms (we will always have tet>tet_ms):
      caprn1=cap*exp((wn-1.d0)*rlww)      ! = cap*rrho^(n-1)
      tet_ms=(e+.5d0*(cap-1.d0)*caprn1)/(alph*cv)
      tet_ms=max(floor,tet_ms)
      if(tet_ms.ge.1.d0) then
        tww=tet_ms
        goto 500
      endif

      IF(tet_ms.gt.tetminbi) THEN
        call BIND_NGWR(tet_ms,gw_n,cv_gw,RBNLIQww,RBNGASww,RLBNGASww,
     &        F_BN_T,NF_BN_T)
        if(rlww.gt.rlbngasww .and. rww.lt.rbnliqww) goto 400
        tww=tet_ms
        goto 500
      ENDIF

      arww=rww
      arlww=rlww
      call PES_EQGWR(arww,arlww,tetminbi,gw_n,cv_gw,1,aPww,aEww,aSww,
     &      aCS2ww,aDPDRww,aDPDTww,aDEDTww,RBNLIQww,RBNGASww,RLBNGASww)
      if(e.gt.aeww(1)) goto 400

!----------------------------------------------------------------------
!     Step 3: Low-temperature case: rationale -> we do not want to
!             compute the binodal for tet < tetminbi !
!----------------------------------------------------------------------
      tet=tetminbi
      p=apww(1)
      eout=aeww(1)
      s=asww(1)
      cs2=acs2ww(1)
      dpdr=adpdrww(1)
      dpdtet=adpdtww(1)
      dedtet=adedtww(1)
      rbnliq=rbnliqww
      rbngas=rbngasww
      rlbngas=rlbngasww
      goto 900

!----------------------------------------------------------------------
!     Step 4: Iterations for EQ-EOS.
!----------------------------------------------------------------------
 400  tet_mi=max(tet_ms,tetminbi)
      eww=e
      call GZERA(FE_EQ,tet_mi,1.d0,eps_pts,nfmaxww,imethd,TWW,
     &              F_PTSEQ,NF_PTSEQ)

      tet=tww
      p=pww
      eout=eouww
      s=sww
      cs2=cs2ww
      dpdr=dpdrww
      dpdtet=dpdtww
      dedtet=dedtww
      rbnliq=rbnliqww
      rbngas=rbngasww
      rlbngas=rlbngasww
      goto 900

!----------------------------------------------------------------------
!     Step 5: MS-EOS (metastable) above the binodal.
!----------------------------------------------------------------------
 500  tet=tww
      ww2=1.d0/(max(floor,1.d0-rww*v00))! = 1/(1-rrho/cap)
      ww3=alph*ww2                      ! = alpha/(1-rrho/cap)
      dpdtet=ww3*rww
      p=rww*(ww3*tww-caprn1)
      dedtet=alph*cv
      eout=dedtet*tww-.5d0*(cap-1.d0)*caprn1
      ww4=ww3*ww2*tww
      dpdr=ww4-wn*caprn1
      cs2=dpdr+ww4/cv
      ww0=max(TINY(1.d0),1.d0-rww*v00)  ! = 1-rrho/cap
      s=alph*(cv*(1.d0+log(tww))+log(ww0)-rlww)

  900 return

      contains


!**********************************************************************
!                            FE_EQ
!******************************************************* begin FE_EQ
      function FE_EQ(x_)
!======================================================================
!     This routine computes the function to be zeroed when computing
!     the TET value for given energy eww.

!     All the input and output quantities are dimensionless, i.e. in
!     the reduced form (or, equivalently, normalized).

! INPUT:
!     x_ = tet -> normalized (reduced) temperature;

! OUTPUT:
!     FE_EQ= function to be zeroed.

!     Called by: PTS_EQGWR
!     Calls    : PES_EQGWR
!======================================================================
! Arguments:
      real(8), intent(in) :: x_
      real(8) :: FE_EQ

! Local variables:
      real(8) :: arww_(1),arlww_(1),aP_(1),aE_(1),aS_(1),acs2_(1),
     &      adpdr_(1),adpdt_(1),adedt_(1)
!======================================================================
      arww_=rww
      arlww_=rlww
      call PES_EQGWR(arww_,arlww_,x_,gw_n,cv_gw,1,aP_,aE_,aS_,aCS2_,
     &      aDPDR_,aDPDT_,aDEDT_,RBNLIQWW,RBNGASWW,RLBNGASWW)
      FE_EQ=eww-aE_(1)

      pww=aP_(1)
      eouww=aE_(1)
      sww=aS_(1)
      cs2ww=acs2_(1)
      dpdrww=aDPDR_(1)
      dpdtww=aDPDT_(1)
      dedtww=aDEDT_(1)

 9000 return
      end function FE_EQ
!_________________________________________________________ end FE_EQ

      end subroutine PTS_EQGWR
!________________________________________________________ end PTS_EQGWR

      end module GWEOS_rd
!_________________________________________________________ end GWEOS_rd
