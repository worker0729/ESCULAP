! file WakeTrajInitBeam.f90
! determination of the initial parameters values of the injected beam for the WakeTraj program
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 14/03/2017

!> subroutine of initialization of the main parameters
subroutine InitBeam()
  use MdConstant
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  implicit none
  integer :: iBeam
!
  namelist/ParExternalBeam/ BeamDuration, BeamCharge_nc, BeamChargeTR, BeamChargeDTR, BeamDeltaX, BeamDeltaX_cm, &
                          & BeamDeltaY, BeamDeltaY_cm, BeamDuration, BeamDuration_fs, BeamEpsMultiply, BeamGamMoy,   &
                          & BeamGamWidth, BeamPhi_mrad, BeamPosition, BeamPosition_cm, BeamTeta_mrad, BeamWidthX,    &
                          &  BeamWidthX_cm, BeamWidthY, BeamWidthY_cm,BeamWidthTetaX_mrad, BeamWidthTetaY_mrad, BeamZfocX,  &
                          & BeamZfocY, BeamZfocX_cm, BeamZfocY_cm, MultiplyBeamPart, NcaseBeamInjection, NtBeamPartRead, NumBeam
   
!*  default values
  withBeamLoading = .False.
  NumBeam = 1
  NcaseBeamInjection = 0
  NtBeamPart = 0
  MultiplyBeamPart = 1
  BeamDeltaX = 1.d20
  BeamDeltaX_cm = 0.d0
  BeamDeltaY = 1.d20
  BeamDeltaY_cm = 0.d0
  BeamDuration = 0.d0
  BeamDuration_fs = 0.d0
  BeamEpsMultiply = 5.d-3
  BeamPosition = 1.d20
  BeamCharge_nc = 0.0
  BeamChargeTR = 0.d0
  BeamChargeDTR = 0.d0
  BeamGamMoy = 0.d0
  BeamGamWidth = 0.d0
  BeamPhi_mrad = 0.d0
  BeamTeta_mrad = 0.d0
  BeamWidthX = 1.d20
  BeamWidthX_cm = 0.d0
  BeamWidthY = 1.d20
  BeamWidthY_cm = 0.d0
  BeamWidthTetaX_mrad = 0.d0
  BeamWidthTetaY_mrad = 0.d0
  BeamZfocX = 1.d20
  BeamZfocY = 1.d20
  BeamZfocX_cm = TimeOn_cm
  BeamZfocY_cm = TimeOn_cm

!* reading the namelist, the default value is used when the variable name is not included in the input data
  read(10,ParExternalBeam)
! Check Particle Beam Values
  if(NumBeam /= 1) then
    write(*,*) 'withBeamParticle = True, presently only NumBeam = 1  is allowed'
    stop
  endif
  if (withBeamLoading) then
    if(BeamCharge_nc < 1.d-10) then
      write(*,*) 'withBeamLoading = True, then BeamCharge_nc should be > 0'
      stop
    endif
  endif
  if((NcaseBeamInjection < 0) .or. (NcaseBeamInjection > 1)) then
    write(*,*) ' withBeamParticle = True, one should have -1<NcaseBeamInjection<0'
    stop
  endif

  if(NcaseBeamInjection == 0) then
! specific cas of a Gaussian Beam  
! for a gaussian beam, duration should be specified
    if((BeamDuration < 1.d-10).and.(BeamDuration_fs < 1.d-10)) then
      write(*,*) ' withBeamParticle=True; for a Gaussian Beam, its duration should be specified '
      stop
    endif
! and also the average value of gamma
    if(BeamGamMoy < 1.d-10) then
      write(*,*) ' withBeamParticle=True; for a Gaussian Beam, average value of gamma should be specified '
      stop
    endif
! look if the parameters are given in reduced units or not    
    if(BeamDuration < 1.d-10) then
     BeamDuration = BeamDuration_fs * OmegaP * 1d-15
    else
      BeamDuration_fs = 1.d15 * BeamDuration / OmegaP
    endif
    if(BeamZfocX > 1.d10) then
      BeamZfocX = BeamZfocX_cm * PlasmaWNum_cm1
    else
      BeamZfocX_cm = BeamZfocX / PlasmaWNum_cm1
    endif
    if(BeamZfocY > 1.d10) then
      BeamZfocY = BeamZfocY_cm * PlasmaWNum_cm1
    else
      BeamZfocY_cm = BeamZfocY / PlasmaWNum_cm1
    endif
    if(BeamWidthX > 1.d10) then
      BeamWidthX = BeamWidthX_cm * PlasmaWNum_cm1
    else
      BeamWidthX_cm = BeamWidthX / PlasmaWNum_cm1
    endif
    if(BeamWidthY > 1.d10) then
      BeamWidthY = BeamWidthY_cm * PlasmaWNum_cm1
    else
      BeamWidthY_cm = BeamWidthY / PlasmaWNum_cm1
    endif
     
    
! 
! initial number of beam particles. For a Gaussian Beam = NtBeamPartRead
  NtBeamPartInit = NtBeamPartRead 
  else
! reading of an external data file for the beam
! initial number of particles  is now : 
    NtBeamPartInit = NtBeamPartRead * MultiplyBeamPart
  endif
!
  if(BeamPosition > 1.d10)   BeamPosition = BeamPosition_cm * PlasmaWNum_cm1
  if(BeamDeltaX > 1.d10) then
    BeamDeltaX = BeamDeltaX_cm * PlasmaWNum_cm1
  else
    BeamDeltaX_cm = BeamDeltaX / PlasmaWNum_cm1
  endif
  if(BeamDeltaY > 1.d10) then
    BeamDeltaY = BeamDeltaY_cm * PlasmaWNum_cm1
  else
    BeamDeltaY_cm = BeamDeltaY / PlasmaWNum_cm1
  endif
! initial number of beam particles
  NtBeamPartInit = NtBeamPartRead * MultiplyBeamPart
  
! Allocation et initialization des fichiers
  Allocate(T1BeamPartOutTest(NtBeamPartInit)); T1BeamPartOutTest=0
  Allocate(T1BeamPartIdent(NtBeamPartInit))
  Allocate(T1BeamGam(NtBeamPartInit)); T1BeamGam = 1.d0
  allocate(T1uOneBeamPart(6)); T1uOneBeamPart = 0.d0
  allocate(T1uPOneBeamPart(6)); T1uPOneBeamPart = 0.d0
  allocate(T2uBeam1(6,NtBeamPartInit)); T2uBeam1 = 0.d0
  allocate(T2uBeam3(6,NtBeamPartInit)); T2uBeam3 = 0.d0
  allocate(T1uBeamMoy(6), T1uBeamWidth(6), T1uBeamXYZUxyz(3), T2BeamEmit(2,4))
  T1uBeamMoy = 0.d0; T1uBeamWidth = 0.d0; T1uBeamXYZUxyz = 0.d0; T2BeamEmit = 0.d0
  BeamTeta = 1.d3 * BeamTeta_mrad
  BeamPhi = 1.d3 * BeamPhi_mrad
  
end subroutine InitBeam
