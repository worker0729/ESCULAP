! file WakeTrajInitLaser.f90
! determination of the initial parameters values of the laser for the WakeTraj program
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 14/03/2017

!> subroutine of initialization of the main parameters
subroutine InitLaser()
  use MdConstant
  use MdLaser
  use MdNumeric
  implicit none
!  
  namelist/ParLaser/ LaserAmax, EpsLaserDiffrac, LaserEnergy_J, LaserMaxIntensity_Wcm2,  LaserDuration, &
                      & LaserDuration_fs, LaserGridPositionInit, LaserGridPosition_duration, TimeGaussianLaser, &
                      &  SpaceGaussianLaser, LaserWaist0, LaserWaist0_cm
!
! default values
  LaserAmax = 0.d0
  EpsLaserDiffrac = 0.5d0
  LaserDuration = 0.d0
  LaserDuration_fs = 0.d0
  LaserWLength_cm = 0.8d-4
  LaserGridPositionInit = 1.d20
  LaserGridPosition_duration = 0.d0
  LaserEnergy_J = 0.d0
  LaserMaxIntensity_Wcm2 = 0.d0
  LaserWaist0 = 0.d0
  LaserWaist0_cm = 0.d0
  FullGaussianLaser = .True.
  TimeGaussianLaser = .true.
  SpaceGaussianLaser = .True.
  !
  !* reading the namelist, the default value is used when the variable name is not include in the input data
  read(10,ParLaser)
!* check Laser values  
  if((TimeGaussianLaser).and.(SpaceGaussianLaser)) FullGaussianLaser = .True.
  if((LaserWaist0 < 1.d-10).and. (LaserWaist0_cm < 1.d-10)) then
    write(*,*) ' withlaser = true, then either LaserWaist0 or LaserWaist0_cm should be > 0 '
    stop
  endif
  if((.not. FullGaussianLaser) .and. (LaserEnergy_J < 1.d-10)) then
    write(*,*) ' FullGaussianLaser = false, then LaserEnergy_J should be > 0 '
    stop
  endif
  if(TimeGaussianLaser) then
    if((LaserDuration < 1.d-10) .and. (LaserDuration_fs < 1.d-10)) then
      write(*,*) 'FullGaussianLaser is true, then either LaserDuration or LaserDuration_cm should be > 0'
      stop
    endif
  endif
  if((LaserGridPositionInit > 1.d10) .and. (LaserGridPosition_duration < 1.d-10)) then
    write(*,*) ' withlaser=.true., either LaserGridPosition or LaserGridPosition_duration shoud be specified'
    stop
  endif 
  
end subroutine InitLaser  
 