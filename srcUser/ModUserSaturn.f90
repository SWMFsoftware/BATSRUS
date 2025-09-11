!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModUser

  use BATL_lib, ONLY: &
       test_start, test_stop, iTest, jTest, kTest, iBlockTest, iProc
  use ModVarIndexes, ONLY: rho_, Ux_, Uy_, Uz_,p_,Bx_, By_, Bz_, Energy_, &
       rhoUx_,rhoUy_,rhoUz_
  use ModSize, ONLY: nI,nJ,nK
  use ModUserEmpty,               &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_calc_sources_expl,          &
       IMPLEMENTED3 => user_set_boundary_cells,         &
       IMPLEMENTED4 => user_set_face_boundary

  include 'user_module.h' ! list of public methods

  character (len=*), parameter :: NameUserFile = "ModUserSaturn.f90"
  character (len=*), parameter :: &
       NameUserModule = 'Saturn Magnetosphere, Hansen, Nov, 2006'

  real :: MassLoadingRate
  logical :: UseMassLoading, AccelerateMassLoading

contains
  !============================================================================

  subroutine user_calc_sources_expl(iBlock)

    use ModAdvance, ONLY: Source_VC

    integer, intent(in) :: iBlock

    real, dimension(1:nI,1:nJ,1:nK):: &
         Srho_C,SrhoUx_C,SrhoUy_C,SrhoUz_C,SBx_C,SBy_C,SBz_C,Sp_C,SE_C

    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_calc_sources_expl'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest, iBlock)

    ! Set the source arrays for this block to zero
    Srho_C   = 0.0
    SrhoUx_C = 0.0
    SrhoUy_C = 0.0
    SrhoUz_C = 0.0
    SBx_C    = 0.0
    SBy_C    = 0.0
    SBz_C    = 0.0
    SP_C     = 0.0
    SE_C     = 0.0

    call user_sources(iBlock)

    Source_VC(rho_   ,:,:,:) = Srho_C   + Source_VC(rho_   ,:,:,:)
    Source_VC(rhoUx_ ,:,:,:) = SrhoUx_C + Source_VC(rhoUx_ ,:,:,:)
    Source_VC(rhoUy_ ,:,:,:) = SrhoUy_C + Source_VC(rhoUy_ ,:,:,:)
    Source_VC(rhoUz_ ,:,:,:) = SrhoUz_C + Source_VC(rhoUz_ ,:,:,:)
    Source_VC(Bx_    ,:,:,:) = SBx_C    + Source_VC(Bx_    ,:,:,:)
    Source_VC(By_    ,:,:,:) = SBy_C    + Source_VC(By_    ,:,:,:)
    Source_VC(Bz_    ,:,:,:) = SBz_C    + Source_VC(Bz_    ,:,:,:)
    Source_VC(P_     ,:,:,:) = SP_C     + Source_VC(P_     ,:,:,:)
    Source_VC(Energy_,:,:,:) = SE_C     + Source_VC(Energy_,:,:,:)

    call test_stop(NameSub, DoTest, iBlock)
  contains
    !==========================================================================

    subroutine user_sources(iBlock)

      use ModVarIndexes
      use ModAdvance, ONLY: State_VGB
      use ModGeometry, ONLY: Xyz_DGB, r_GB, rMin_B
      use ModConst
      use ModPlanetConst
      use ModPhysics
      use CON_axes

      integer, intent(in) :: iBlock

      ! Variables required by this user subroutine
      integer :: i,j,k

      ! Variable meanings:
      !   Srho_C: Source terms for the continuity equation
      !   SE_C,SP_C: Source terms for the energy (conservative) and pressure
      !          (primative) equations
      !   SrhoUx_C,SrhoUy_C,SrhoUz_C: Source terms for the momentum equation
      !   SBx_C,SBy_C,SBz_C: Source terms for the magnetic field equations

      ! User declared local variables go here

      real :: Ux,Uy,Uz,Usq,Unx,Uny,Unz,Un,Unsq,Urelsq
      real :: Rxy,R0,R1,Dtorus
      real :: Hr1,Hr2,Hneutral,Helectron
      real :: mn, N0,nN, nElec
      real :: rhodot0,rhodot,rhodotnorm,CXNorm
      real :: accelerationfactor, sourcefactor
      real :: alpha_rec,CXsource,Tfrac
      real :: Telectron, EVelectron, LogEVelectron,eta,alphaE
      real :: LTerm,Part1,Part2
      real :: rSaturn, rTitan_Orbit,omegaTitan_orbit

      ! arrays for computing the tilt of the equatorial plane the therefore the
      ! tilt of the mass loading

      real, dimension(3) :: xGSE,xSMG,vGSE,vSMG
      real, dimension(3,3) :: SMG_GSE_mat

      logical:: DoTest
      character(len=*), parameter:: NameSub = 'user_sources'
      !------------------------------------------------------------------------
      call test_start(NameSub, DoTest, iBlock)

      rSaturn = rPlanet_I(Saturn_)
      rTitan_Orbit  = rOrbitPlanet_I(Titan_)
      omegaTitan_orbit = 2.0*cPi/OrbitalPeriodPlanet_I(Titan_)

      if(UseMassLoading)then
         ! First rotate the coordinate system. The mass loading functions are
         ! centered at the rotational equator. However, BATSRUS/GM solves in GSE
         ! so the z=0 plane is not the equatorial plane when doing the "real"
         ! saturn. The fuctions here are coded assuming that the z=0 plane is the
         ! same as the equatorial plane. This is the SMG frame (see CON_axes).
         ! So, I need to transform x,y,z from GSE to SMG using CON_planet.

         SMG_GSE_mat = transform_matrix(0.0, 'GSE','SMG')

         if(rMin_B(iBlock) > 45.0) RETURN

         do k = 1, nK
            do j = 1, nJ
               do i = 1, nI
                  ! Do the rotations. Note that we want to compute the positions
                  ! in the SMG coordinate system. However, we need to compute
                  ! the velocities in the GSE coordinate system where GM/BATSRUS
                  ! works. This makes life a little complicated.

                  xGSE(1) = Xyz_DGB(x_,i,j,k,iBlock)
                  xGSE(2) = Xyz_DGB(y_,i,j,k,iBlock)
                  xGSE(3) = Xyz_DGB(z_,i,j,k,iBlock)
                  xSMG = matmul(SMG_GSE_mat,xGSE)

                  ! First calculate the mass loading source terms for the inner
                  ! torus of H2O and products. Although the rates used fall of
                  ! at infinity and could be calculated everywhere, we will only
                  ! calculate in the region where the contribution is not
                  ! negligible.

                  ! Note that velocities, distances and scale heights are
                  ! normalized (unitless). Density is outlined below.

                  if (((xSMG(3) < 2.0) .and.    &
                       (xSMG(3) > -2.0)) .and.  &
                       (r_GB(i,j,k,iBlock) < 14.0) .and.    &
                       (r_GB(i,j,k,iBlock) > 3.0)) then

                     Ux  = State_VGB(rhoUx_,i,j,k,iBlock)/State_VGB(rho_,i,j,k,iBlock)
                     Uy  = State_VGB(rhoUy_,i,j,k,iBlock)/State_VGB(rho_,i,j,k,iBlock)
                     Uz  = State_VGB(rhoUz_,i,j,k,iBlock)/State_VGB(rho_,i,j,k,iBlock)
                     Usq = sqrt(Ux**2 + Uy**2 + Uz**2)

                     ! Compute the cylindrical radius for later use
                     Rxy = sqrt(xSMG(1)**2 + xSMG(2)**2)

                     ! The neutral velocity is taken to be the orbital velocity
                     ! Note that Gbody/r**2 is a unitless acceleration. So
                     ! Gbody/r is a uniless u**2.  Gbody is negative so that
                     ! gravity pulls inward.  Take abs() here to get magnitude
                     ! of the orbital velocity.

                     ! Compute in the SMG frame and then rotate back to the GSE
                     ! frame where we really want the velocities.
                     unsq = abs(Gbody)/Rxy
                     un = sqrt(unsq)
                     unx = -un*xSMG(2)/Rxy
                     uny =  un*xSMG(1)/Rxy
                     unz =  0.0

                     vSMG(1) = unx
                     vSMG(2) = uny
                     vSMG(3) = unz
                     vGSE = matmul(vSMG,SMG_GSE_mat)

                     unx = vGSE(1)
                     uny = vGSE(2)
                     unz = vGSE(3)
                     urelsq = (unx-Ux)**2 + (uny-Uy)**2 + (unz-Uz)**2

                     ! Now code the model taken from Richardson, 90,98. See my
                     ! dissertation on p. 125-129.
                     !
                     ! In the dissertation rhodot0 = 7.2e-4 amu/cm^3/s. This
                     ! rate corresponds to a total production rate of 8.6E26 s^-1
                     ! analytically. Using an average mass of 16.6 amu per ion
                     ! the correponding mass production rate is 1.43e28 amu/s.
                     ! Note that rhodot0 is in amu/cm^3/s so it ALREADY TAKES
                     ! INTO ACCOUNT THE 16.6 AMU/ION. It is already a MASS
                     ! DENSITY not simply a number density!
                     !
                     ! Since Richardson gives a rate of about 1.3e27 s^-1 we
                     ! multiply by a factor of 2 to get a number close to his.
                     ! This means that we typically use
                     ! rhodot0 = 2.0* 7.2e-4 amu/cm^3/s. Using this rate and
                     ! integrating on different size grids gives the
                     ! the following rates
                     !
                     ! dx		ndot		rhodot
                     !
                     ! 1/16	1.44E27		2.40E28
                     ! 1/8		1.16E27		1.92E28
                     ! 0.195	8.55E26		1.42E28
                     ! 1/4		6.45E26		1.07E28
                     !
                     ! So, to repeat. Below, rhodot0 and rhodot are in units of
                     ! amu/cm^3/s and already take into account the average mass
                     ! of an ion that is being added. You do not need to
                     ! multiply 16.6 to get the rhodot.
                     !
                     ! NOTE: here we introduce a multiplication factor which we
                     ! will use to control the mass addition rate.
                     ! A factor of 1.0 corresponds to a rate of
                     ! rhodot0 = 7.2e-4 amu/cm^3/s. A factor of 2.0 obviously
                     ! corresponds to twice this and therefore corresponds
                     ! to the rates in the table above!
                     !
                     !-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
                     !-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
                     !-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

                     ! nominal rate of ~0.9E27 on a typical grid (3/16=0.195)
                     ! this corresponds to the above numbers for a 3/16 grid)
                     ! sourcefactor = 2.0

                     ! Rate of ~0.3*1e28 on a typical grid (3/16=0.195)
                     ! This number is taken from the most recent Richardson
                     ! paper which I just reviewed (Nov, 2004).  The paper
                     ! gives 1e28 as the neutral source rate of which .3 is
                     ! converted to ions.  The rest escapes the system as
                     ! neutrals.   The 3.333 comes from .3E28/.9E27
                     ! sourcefactor = 2.0*3.333333333

                     ! Rate of ~1e28 on a typical grid  (3/16=0.195)
                     ! This number is the current neutral source rate being
                     ! used by Richardson as well as the Cassini people. As such
                     ! this is the largest mass loading rate you might expect.
                     ! The 11.1 comes from 1e28/.9e27
                     ! sourcefactor = 2.0*11.1

                     ! Rate which can be adjusted by the user from the input file
                     ! The user inputs the MassLoadingRate in #ofparticles/cm^3.
                     ! We still assume that the average mass is 16.6. Now, so
                     ! that we don't have to change anything below, here we
                     ! caluclate the sourcefacter from the MassLoadingRate. As
                     ! pointed out above a sourcefactor=2.0 corresponds to a
                     ! rate of ~0.9E27.
                     ! So, to calcuate the sourcefactor we use
                     !    sourcefactor_A/2.0  =  MassLoadingRate/0.9E27
                     ! so
                     !    sourcefactor_A = 2.0*MassLoadingRate/0.9e27
                     ! or
                     !    sourcefactor_A = 2.22222E-27 * MassLoadingRate
                     !
                     ! Just to test.  If MassLoadingRate = 1.8E27 then
                     !
                     !    sourcefactor_A = 4.0
                     !
                     ! which is twice the value for a rate of 0.9E27! Good.
                     !
                     sourcefactor = 2.222222E-27 * MassLoadingRate

                     !-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
                     !-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
                     !-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

                     ! Variables preceeded by H are scale heights

                     ! If we are using accelerated mass loading then calculate
                     ! the factor by which we should increase the rates

                     ! Set up values for this mass loading model
                     Hneutral  = .45
                     Helectron = .6 + .2*(Rxy-3.0)

                     rhodot0 = sourcefactor*7.2E-4

                     R0 = 5.35
                     Hr1 = 1.2
                     Hr2 = 2.25 + 0.075*(Rxy-R0)

                     if (Rxy < R0) then
                        rhodot =  rhodot0*exp(-(Rxy-R0)**2/Hr1**2)
                     else
                        rhodot =  rhodot0*exp(-(Rxy-R0)**2/Hr2**2)
                     end if

                     rhodot = rhodot*exp(-(xSMG(3)**2)/Hneutral**2)&
                          *exp(-(xSMG(3)**2)/Helectron**2)

                     ! Get the CX friction source terms
                     !
                     ! The charge exchange friction is also calculated from
                     ! Richardson in my dissertation.  The charge exchange
                     ! friction depends on the neutral density and the plasma
                     ! density.  We have coded eta so that the neutral density
                     ! dependence is built in.  So, if the neutral density is
                     ! doubled the eta should be doubled.  Since above we change
                     ! the mass loading rate by a source factor which is general
                     ! because of a neutral rate, we will use the same factor
                     ! here to control the eta scaling.

                     R0 = 4.6
                     n0 = 0.4*1200
                     Hr1 = .8
                     Hr2 = 1.2 + 0.035*Rxy

                     if (Rxy < R0) then
                        Cxsource =  exp((Rxy-R0)/Hr1)
                     else
                        CXsource =  exp(-(Rxy-R0)/Hr2)
                     end if

                     CXsource = 0.6*((-0.0330 + 0.1636*Rxy)*1E-8)              &
                          *n0*CXsource*exp(-(xSMG(3)**2)/ &
                          Hneutral**2)

                     CXsource = sourcefactor*CXsource

                     ! Get the electron recombination terms. First get the
                     ! electron density. This is gotten by first converting from
                     ! unitless rho to amr/cm^3 using No2Io_V(UnitRho_). Then, by
                     ! dividing by the average amu per particle. In that case we
                     ! use 16.6 for O, OH, H2O. Note that this term does not
                     ! depend on the neutrals in any way so we do not need to
                     ! use the sourcefactor to scale as we raise and lower the
                     ! mass loading rate.

                     nElec = State_VGB(rho_,i,j,k,iBlock)*No2Io_V(UnitRho_)/16.6

                     ! We now need the electron temperature to calculate some
                     ! of the ionization rates. The electron temperature that
                     ! comes out of this calculation have units of K (kelvin).
                     ! This is an electron temperature calculation that takes
                     ! Te = 1/15 Tp (Tp = Ti+Te). The EVelectron is the electron
                     ! temperature converted to an energy in electron volts (eV).
                     ! This is used in a formula to get the recombination rate
                     ! in s.

                     Tfrac = 1.0/15.0

                     Telectron = (16.6*cProtonMass/cBoltzmann)*&
                          (State_VGB(p_,i,j,k,iBlock)*     &
                          No2Si_V(UnitP_))/(State_VGB(rho_,i,j,k,iBlock)*&
                          No2Si_V(UnitRho_))* Tfrac
                     EVelectron = cBoltzmann/cElectronCharge*Telectron
                     LogEVelectron = Log(EVelectron)

                     Alpha_rec = exp(-17.6168-1.3982*LogEVelectron+0.1439* &
                          LogEVelectron**2)*nElec
                     if (Telectron < 1.0) Alpha_rec = 2.234E-8*nElec
                     if (Telectron > 100) Alpha_rec = 7.552E-10*nElec

                     ! Get everything normalized - note that rhodot is in
                     ! units amu/cm^3/s already (the mass is taken into account
                     ! above. To get a unitless rhodot we simply need to multiply
                     ! by the mass of a proton times the 1.0e6 (to get 1/m^3)
                     ! and then divide by (dimensions(rho)/dimensions(t)) which
                     ! is No2Si_V(UnitRho_)/No2Si_V(UnitT_) to get the
                     ! normalized (dimensionless) form.

                     rhodotNorm =  No2Si_V(UnitT_)/No2Si_V(UnitRho_)
                     CXNorm = No2Si_V(UnitT_)

                     rhodot = cProtonMass*1.0e6*rhodot*rhodotNorm
                     CXsource = CXsource*CXNorm
                     Alpha_rec = Alpha_rec*CXNorm

                     if (AccelerateMassLoading) then
                        accelerationFactor = 10.0
                        rhodot   = accelerationFactor*rhodot
                        CXsource = accelerationFactor*CXsource
                     end if

                     !                   ! testing only
                     !                   Alpha_rec = 0.0
                     !                   CXsource = 0.0

                     ! Load the source terms into the right hand side
                     Srho_C(i,j,k)   = Srho_C(i,j,k) &
                          + (rhodot - Alpha_rec*State_VGB(rho_,i,j,k,iBlock))
                     SrhoUx_C(i,j,k) = SrhoUx_C(i,j,k) &
                          + (rhodot + CXsource*State_VGB(rho_,i,j,k,iBlock))*unx&
                          - (CXsource + Alpha_rec)*State_VGB(rhoUx_,i,j,k,iBlock)
                     SrhoUy_C(i,j,k) = SrhoUy_C(i,j,k) &
                          + (rhodot + CXsource*State_VGB(rho_,i,j,k,iBlock))*uny&
                          - (CXsource + Alpha_rec)*State_VGB(rhoUy_,i,j,k,iBlock)
                     SrhoUz_C(i,j,k) = SrhoUz_C(i,j,k) &
                          - (CXsource + Alpha_rec)*State_VGB(rhoUz_,i,j,k,iBlock)
                     SE_C(i,j,k)     = SE_C(i,j,k)       &
                          + 0.5*(rhodot + CXsource*State_VGB(rho_,i,j,k,iBlock))&
                          *unsq &
                          - (CXsource + Alpha_rec)* &
                          (0.5*usq*State_VGB(rho_,i,j,k,iBlock)&
                          + 1.5*State_VGB(p_,i,j,k,iBlock))   &
                          + 1.5*CXsource*State_VGB(p_,i,j,k,iBlock)*Tfrac
                     SP_C(i,j,k)     = SP_C(i,j,k) &
                          + 0.5*(rhodot + CXsource*State_VGB(rho_,i,j,k,iBlock))&
                          *urelsq &
                          - 1.5*CXsource*State_VGB(p_,i,j,k,iBlock)*(1.0-Tfrac)&
                          - 1.5*Alpha_rec*State_VGB(p_,i,j,k,iBlock)

                     ! Output to look at rates
                     if (DoTest .and. iBlock==iBlockTest  .and. &
                          i==iTest .and. j==jTest .and. k==kTest ) then
                        write(*,*) '----Inner Torus Mass Loading Rates----'
                        write(*,'(a,3(1X,E13.5))') 'X, Y, Z:', &
                             Xyz_DGB(X_,i,j,k,iBlock), Xyz_DGB(Y_,i,j,k,iBlock),&
                             Xyz_DGB(Z_,i,j,k,iBlock)
                        write(*,'(a,5(1X,i6))')    'i,j,k,iBlock,iProc:', &
                             i,j,k,iBlock,iProc
                        write(*,'(a,3(1X,E13.5))') &
                             'Telectron, EVelectron, LogEVelectron:', &
                             Telectron, EVelectron, LogEVelectron
                        write(*,'(a,3(1X,E13.5))') &
                             'rhodot, CXsource, Alpha_rec (unnormalized):', &
                             rhodot/rhodotNorm, CXsource/CXNorm,Alpha_rec/CXNorm
                        write(*,'(a,4(1X,E13.5))') &
                             'rho(amu/cc),nElec(1/cc),un,usq(km/s):', &
                             State_VGB(rho_,i,j,k,iBlock)*No2Io_V(UnitRho_), &
                             nElec, un*No2Io_V(UnitU_), usq*No2Io_V(UnitU_)
                        write(*,'(a,3(1X,E13.5))') &
                             'rhodot, CXsource, Alpha_rec (normalized):', &
                             rhodot, CXsource,Alpha_rec
                        write(*,'(a,4(1X,E13.5))') &
                             'rho, nElec, un, usq (normalized):', &
                             State_VGB(rho_,i,j,k,iBlock), &
                             nElec/No2Io_V(UnitRho_), un, usq
                        write(*,*) '--------------------------------------------'
                     end if

                  end if ! inner torus - location test

                  ! Calculate the source terms for the neutral nitrogen torus
                  ! centered around Titan.

                  ! Calculate the radial distance from the center of the torus,
                  ! if inside, compute the mass loading, if outside do nothing.

                  Dtorus = sqrt( (sqrt(xSMG(1)**2 + xSMG(2)**2) - &
                       Rtitan_orbit/Rsaturn)**2 + xSMG(3)**2)

                  if (Dtorus < 10.0) then

                     usq = (State_VGB(rhoUx_,i,j,k,iBlock)**2 +   &
                          State_VGB(rhoUy_,i,j,k,iBlock)**2 +   &
                          State_VGB(rhoUz_,i,j,k,iBlock)**2 ) / &
                          (State_VGB(rho_,i,j,k,iBlock)**2)

                     ! compute the cylindrical radius for later use
                     Rxy = sqrt(xSMG(1)**2 + xSMG(2)**2)

                     ! The neutral torus is taken to rotate ridgidly with Titan
                     unsq = (Rxy**2)*((OMEGAtitan_orbit*No2Si_V(UnitT_))**2)
                     un = sqrt(unsq)

                     rhodotNorm =  No2Si_V(UnitT_)/No2Si_V(UnitRho_)

                     ! See my dissertation to find the functional form of what
                     ! is being used below and how it was derived. This is
                     ! calculated somewhat differently than the icy satellites.
                     ! Here we clearly calculate the nuetral density  and then
                     ! get rhodot (amu/cm^3/s) my using all the correct factors
                     ! including the average mass of the ion (14.0 nitrogen).
                     ! note that Dtorus**2/2.0 = Dtorus**2/(sqrt(2)**2) with a
                     ! resulting scale height of sqrt(2). The resulting mass
                     ! loading rates for different grid sizes are:
                     !
                     ! dx		ndot		rhodot
                     !
                     ! 1/16	5.46E25		7.64E26
                     ! 1/8		5.21E25		7.29E26
                     ! 0.195	4.87E25		6.82E26
                     ! 1/4		4.61E25		6.45E26
                     !
                     !

                     mn = 14.0
                     nN = 10.*exp(-Dtorus**2/2.0)
                     rhodot = (1.0E+6*cProtonMass*(14.0*nN/(3.0E7)))*rhodotNorm
                     eta = 2.14287E-4

                     ! This is an electron temperature calculation that takes
                     ! Te = 1/2 Tp = 1/2 (Ti+Te).

                     Tfrac = 1.0/2.0;

                     Telectron = (mn*cProtonMass/cBoltzmann)*&
                          (State_VGB(p_,i,j,k,iBlock)* &
                          No2Si_V(UnitP_))/(State_VGB(rho_,i,j,k,iBlock)*&
                          No2Si_V(UnitRho_))* Tfrac
                     EVelectron = cBoltzmann/cElectronCharge*Telectron
                     LogEVelectron = Log(EVelectron)

                     ! Define alpha
                     if (EVelectron < 200.) then
                        alphaE = 7.E-7*sqrt(300./EVelectron)
                     else
                        alphaE = 2.342*7.E-7*   &
                             EVelectron**(0.2553-0.1633*log10(EVelectron))
                     end if

                     ! Note that the term in the second bracket is the
                     ! electron density calculated by assuming the average
                     ! mass of the implanted ions. Futher note that the 0.1
                     ! is the number density in cm^-3.

                     Lterm = alphaE*(No2Si_V(UnitT_))*   &
                          (No2Io_V(UnitRho_)*State_VGB(rho_,i,j,k,iBlock)/mn)

                     part1 = rhodot + rhodot*eta*State_VGB(rho_,i,j,k,iBlock)
                     part2 = rhodot*eta + Lterm

                     ! Load the source terms into the right hand side
                     Srho_C(i,j,k)   = Srho_C(i,j,k)   + &
                          (rhodot - Lterm*State_VGB(rho_,i,j,k,iBlock))
                     SrhoUx_C(i,j,k) = SrhoUx_C(i,j,k) + &
                          (part1*(-OMEGAtitan_orbit*No2Si_V(UnitT_)*  &
                          Xyz_DGB(Y_,i,j,k,iBlock)) -  &
                          part2*State_VGB(rhoUx_,i,j,k,iBlock))
                     SrhoUy_C(i,j,k) = SrhoUy_C(i,j,k) + &
                          (part1*(OMEGAtitan_orbit*No2Si_V(UnitT_)*   &
                          Xyz_DGB(X_,i,j,k,iBlock)) -   &
                          part2*State_VGB(rhoUy_,i,j,k,iBlock))
                     SrhoUz_C(i,j,k) = SrhoUz_C(i,j,k) + &
                          (part1*0.0 - part2*State_VGB(rhoUz_,i,j,k,iBlock))
                     SE_C(i,j,k)     = SE_C(i,j,k)     + (part1*0.5*unsq -   &
                          part2*((0.5*State_VGB(rho_,i,j,k,iBlock)*usq)+ &
                          ((3./2.)*State_VGB(p_,i,j,k,iBlock))))

                  end if    ! end calculation of source terms inside the torus.

               end do     ! end the i loop
            end do     ! end the j loop
         end do     ! end the k loop

      end if     ! usemassloading test

      call test_stop(NameSub, DoTest, iBlock)
    end subroutine user_sources
    !==========================================================================
  end subroutine user_calc_sources_expl
  !============================================================================

  subroutine user_read_inputs
    use ModMain
    use ModReadParam
    use ModIO, ONLY: write_prefix, write_myname, iUnitOut

    integer:: i
    character (len=100) :: NameCommand
    logical:: DoTest
    character(len=*), parameter:: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    call test_start(NameSub, DoTest)

    if(iProc==0.and.lVerbose > 0)then
       call write_prefix; write(iUnitOut,*)'User read_input SATURN starts'
    endif
    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case("#MASSLOADING")
          call read_var('UseMassLoading',UseMassLoading)
  	  call read_var('MassLoadingRate (#/s)', MassLoadingRate)
          call read_var('DoAccelerateMassLoading',AccelerateMassLoading)

       case('#USERINPUTEND')
          if(iProc==0.and.lVerbose > 0)then
             call write_prefix; write(iUnitOut,*)'User read_input SATURN ends'
          endif
          EXIT
       case default
          if(iProc==0) then
             call write_myname; write(*,*) &
                  'ERROR: Invalid user defined #COMMAND in user_read_inputs. '
             write(*,*) '--Check user_read_inputs for errors'
             write(*,*) '--Check to make sure a #USERINPUTEND command was used'
             write(*,*) '  *Unrecognized command was: '//NameCommand
             call stop_mpi('ERROR: Correct PARAM.in or user_read_inputs!')
          end if
       end select
    end do
    call test_stop(NameSub, DoTest)
  end subroutine user_read_inputs
  !============================================================================

  subroutine user_set_boundary_cells(iBlock)
    use ModGeometry, ONLY: ExtraBc_, Xyz_DGB, xMaxBox
    use ModBoundaryGeometry, ONLY: iBoundary_GB
    integer, intent(in):: iBlock
    !--------------------------------------------------------------------------
    where(Xyz_DGB(x_,:,:,:,iBlock) > xMaxBox) &
         iBoundary_GB(:,:,:,iBlock) = ExtraBc_

  end subroutine user_set_boundary_cells
  !============================================================================

  subroutine user_set_face_boundary(FBC)

    use ModMain, ONLY: FaceBCType
    use ModSize, ONLY: x_
    use ModVarIndexes, ONLY: nVar, Bx_, Bz_
    use ModSolarwind, ONLY: get_solar_wind_point
    use ModB0, ONLY: B0_DX

    type(FaceBCType), intent(inout):: FBC

    character(len=*), parameter:: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------
    associate( VarsGhostFace_V => FBC%VarsGhostFace_V, &
         iFace => FBC%iFace, jFace => FBC%jFace, kFace => FBC%kFace, &
         TimeBc => FBC%TimeBc )

      call get_solar_wind_point(TimeBc, FBC%FaceCoords_D(x_), VarsGhostFace_V)
      VarsGhostFace_V(Bx_:Bz_) = VarsGhostFace_V(Bx_:Bz_) - &
           B0_DX(:,iFace,jFace,kFace)

    end associate
  end subroutine user_set_face_boundary
  !============================================================================

end module ModUser
!==============================================================================
