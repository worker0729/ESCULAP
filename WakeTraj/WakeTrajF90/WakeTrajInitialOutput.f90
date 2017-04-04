! file WakeTrajInitOutput.f90
! print the initial value of the main parameters
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 23/03/2017
!> subroutine printing the initial values of parameters
subroutine InitOutput()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdOutput
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
! Numerical parameters
  write(10,*) ' ***********  Initial Numerical Parameters ***********'
  write(10,'(''      CircRz      '',i4)') CircRz
  write(10,'(''      IDIMR       '',I4)') IDIMR
  write(10,'(''      IDIMZ       '',I4)') IDIMZ
  write(10,'(''        IR1       '',I4)') iR1
  write(10,'(''        IR2       '',I4)') iR2
  write(10,'(''       IRMX       '',I4)') IRMX
  write(10,'(''       IZMX       '',I4)') IZMX
  write(10,'(''        np1       '',I4)') nP1
  write(10,'(''        np2       '',I4)') nP2
  write(10,'(''        np3       '',I4)') nP3
  write(10,'(''MaxNumTimeStep    '',I8)') MaxNumTimeStep
  write(10,'('' ProjOrderR       '',i4)') ProjOrderR
  write(10,'('' ProjOrderZ       '',i4)') ProjOrderZ
  write(10,'(''TypeOfCalculation '',i4)') TypeOfCalculation
  write(10,'(''    DeltaT        '',e12.4)') deltaT
  write(10,'(''    DeltaT_cm     '',e12.4)') deltaT_cm
  write(10,'(''    DeltaT_Rayleigh '',e12.4)') deltaT_Rayleigh
  write(10,'(''         dR        '',e12.4)') dR
  write(10,'(''         dR_cm     '',e12.4)') dR / PlasmaWNum_cm1
  write(10,'(''         dZ        '',e12.4)') dZ
  write(10,'(''         dZ_cm     '',e12.4)') dZ / PlasmaWNum_cm1
  write(10,'(''       EpsCFL      '',e12.4)') EpsCFL
  write(10,'(''       EpsRK       '',e12.4)') EpsRK
  write(10,'(''      RadiusGrid   '',e12.4)') RadiusGrid
  write(10,'(''   RadiusGrid_cm   '',e12.4)') RadiusGrid_cm
  write(10,'(''  RadiusGridCoef   '',e12.4)') RadiusGridCoef
  write(10,'(''       TimeOn      '',e12.4)') TimeOn
  write(10,'(''       TimeOn_cm   '',e12.4)') TimeOn_cm
  write(10,'('' TimeOn_Rayleigh   '',e12.4)') TimeOn_Rayleigh
  write(10,'(''       TimeEnd     '',e12.4)') TimeEnd
  write(10,'(''       TimeEnd_cm  '',e12.4)') TimeEnd_cm
  write(10,'(''  TimeEnd_Rayleigh '',e12.4)') TimeEnd_Rayleigh
  write(10,'(''   ZLengthGrid     '',e12.4)') ZLengthGrid
  write(10,'(''   ZLengthGrid_cm  '',e12.4)') ZLengthGrid_cm
  write(10,'(''     withLaser     '',A)') withLaser
  write(10,'('' withBeamParticle  '',A)') withBeamParticle
  write(10,'('' withPlasma        '',A)') withPlasma
  write(10,'('' withTestParticle  '',A)') withTestParticle
!
! Laser Parameter
  if(withLaser) then
    write(10,*) ' *************Initial Laser Parameters ************** '
    write(10,'(''    LaserAmax             '',e12.4)') LaserAmax
    write(10,'('' CriticalDensity_cm3 '',e12.4)') CriticalDensity_cm3
    write(10,'('' EpsLaserDiffrac    '',e12.4)') EpsLaserDiffrac
    write(10,'('' EpsRayleigh        '',e12.4)') EpsRayleigh
    write(10,'('' LaserDuration      '',e12.4)') LaserDuration
    write(10,'('' LaserDUration_fs   '',e12.4)') LaserDuration_fs
    write(10,'('' LaserEnergy_J      '',e12.4)') LaserEnergy_J
    write(10,'(''LaserGridPosition    '',e12.4)') LaserGridPositionInit
    write(10,'(''LaserGridPosition_duration '',e12.4)') LaserGridPosition_duration
    write(10,'(''LaserGridPosition_cm  '',e12.4)') LaserGridPositionInit / PlasmaWNum_cm1
    write(10,'(''LaserMaxIntensity_Wcm2 '',e12.4)') LaserMaxIntensity_Wcm2
    write(10,'(''LaserWLength_cm        '',e12.4)') LaserWLength_cm
    write(10,'(''LaserWNum_cm1          '',e12.4)') LaserWNum_cm1
    write(10,'(''  OmegaL               '',e12.4)') OmegaL
    write(10,'(''  Rayleigh_cm           '',e12.4)') Rayleigh_cm
    write(10,'(''  LaserWaist0           '',e12.4)') LaserWaist0
    write(10,'(''  LaserWaist0_cm        '',e12.4)') LaserWaist0_cm
    write(10,'(''  FullGaussianLaser     '',A)') FullGaussianLaser
    write(10,'('' TimeGaussianLaser      '',A)') TimeGaussianLaser
    write(10,'('' SpaceGaussianLaser     '',A)') SpaceGaussianLaser
  endif
!
!  Beam Particles Parameters
  if(withBeamParticle) Then
    write(10,'('' NumBeam                '',I4)') NumBeam
    write(10,'('' NcaseBeamInjection     '',I4)') NcaseBeamInjection
    write(10,'('' NtBeamPart             '',i4)') NtBeamPart
    write(10,'('' NtBeamPartRead         '',i4)') NtBeamPartRead
    write(10,'('' MultiplyBeamPart       '',i4)') MultiplyBeamPart
    write(10,'('' BeamPhi                '',e12.4)') BeamPhi
    write(10,'('' BeamTeta               '',e12.4)') BeamTeta
    write(10,'(''  BeamDuration          '',e12.4)') BeamDuration
    write(10,'('' BeamDuration_fs       '',e12.4)') BeamDuration_fs
    write(10,'('' BeamGamMoy            '',e12.4)') BeamGamMoy
    write(10,'('' BeamGamWidth          '',e12.4)') BeamGamWidth
    write(10,'('' BeamPosition          '',e12.4)') BeamPosition
    write(10,'('' BeamPosition_cm       '',e12.4)') BeamPosition_cm
    write(10,'('' BeamCharge_nc         '',e12.4)') BeamCharge_nc
    write(10,'('' BeamChargeTR          '',e12.4)') BeamChargeTR
    write(10,'('' BeamChargeDTR         '',e12.4)') BeamChargeDTR
    write(10,'('' BeamDenWt0            '',e12.4)') BeamDenWt0
    write(10,'('' withBeamLoading       '',A)') withBeamLoading
  endif
! Plasma Parameters
  if(withPlasma) then
    write(10,'('' FormeDensityProfile   '',i4)') FormedensityProfile
    write(10,'('' Dens0Plasma_cm3       '',e12.4)') Dens0Plasma_cm3
    write(10,'('' EpsPlasmaLaser        '',e12.4)') EpsPlasmaLaser
    write(10,'('' OmegaP                '',e12.4)') OmegaP
    write(10,'('' PlasmaWLength_cm      '',e12.4)') PlasmaWLength_cm
    write(10,'('' PlasmaWNum_cm1        '',e12.4)') PlasmaWNum_cm1
  endif
! Plasma TestParticle
  if(withTestParticle) then
    write(10,'('' MaxNumTP              '',I4)') MaxNumTP
    write(10,'('' TStartInjecTP         '',e12.4)') TStartInjecTP
    write(10,'('' TStartInjecTP_cm      '',e12.4)') TStartInjecTP_cm
    write(10,'('' TStopInjecTP          '',e12.4)') TStopInjecTP
    write(10,'('' TStopInjecTP_cm       '',e12.4)') TStopInjecTP_cm
  endif
! Output Parameters
  write(10,'(''  DeltaSort             '',e12.4)') DeltaTSort
  write(10,'(''  DeltaSort_cm          '',e12.4)') DeltaTSort_cm
  write(10,'('' BeamNumPrint           '',e12.4)') BeamNumPrint
!  
end subroutine InitOutput  
