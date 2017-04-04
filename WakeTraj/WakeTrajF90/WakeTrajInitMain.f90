! file WakeTrajInitMain.f90
! Step I. determination of the initial parameters values for the WakeTraj program
! called by WakeTrajMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 29/03/2017
!> subroutine of initialization of the main parameters
subroutine InitMain()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdOutput
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
! local variables  
  integer :: i, j , k, ir, iz
  real*8 :: loctime
! define the namelist used as general input parameters
  namelist/ParNumeric/ CircRz, DeltaT, DeltaT_cm, DeltaT_Rayleigh, EpsCFL, EpsRK, IDIMR, IDIMZ, iR1, iR2, np1,np2,np3,  &
                    & MaxNumTimeStep, ProjOrderR, ProjOrderZ, RadiusGridCoef, TypeOfCalculation, RadiusGridCoef, &
                    & TimeOn_Rayleigh, TimeOn_cm, TimeEnd_Rayleigh, TimeEnd_cm, withLaser, withBeamParticle, withPlasma, &
                    & withTestParticle, ZLengthGrid, ZLengthGrid_cm
  namelist/ParOutput/ BeamNumPrint, DeltaTSort, DeltaTSOrt_cm, LPrintPlasmaField, LPrintLaserField, LPrintBeamPartField, &
                    & LPrintBeamPartT2u, LPrintTestPartField, LPrintTotalField  
!
!*

!  open data input and output file, as a general rule files are open and closed in the same subroutine, 10 is for input and 11 for output
  OPEN (10,file='WakeTraj_input.par')
!********* 1 reading of general parameters  
!***** 1.1 Physical and Mathematical Constants
  Pi = acos(-1.d0)
  VacMu0 = 4.d-7 * Pi
  VacEps0 = 1.d4 / (VacMu0 * c_cm_s**2)
!
!***** 1.2 Numerical parameters
!* default values
  CircRz = 0
  DeltaT = 1.d20
  DeltaT_cm = 1.d20
  DeltaT_Rayleigh = 1.d20
  IDIMR = 500
  IDIMZ = 500
  IRMX = 0
  IZMX = 0
  IR1 = IDIMR / 3.d0
  IR2 = IDIMR * 2.d0 / 3.d0
  MaxNumTimestep = 1000000
  np1 = 10
  np2 = 5
  np3 = 2
  ProjOrderR = 2
  ProjOrderZ = 2
  RadiusGridCoef = 5.d0
  TypeOfCalculation = 1
  TimeOn_Rayleigh = 1.d20
  TimeOn_cm = 1.d20
  TimeEnd_Rayleigh = 1.d20
  TimeEnd_cm = 1.d20
  ZLengthGrid = 0.d0
  ZlengthGrid_cm = 0.d0
  withLaser = .false.
  withPlasma = .false.
  withBeamParticle = .False.
  withTestParticle = .False.

! reading the namelist, the default value is used when the variable name is not included in the namelist 
  read(10,ParNumeric)
! check Numeric
  if ((abs(TimeOn_Rayleigh)> 1.d10) .and. (abs(TimeOn_cm) > 1.d10) ) Then
    write(*,*) ' error in TimeOn input values; one of TimeOn_Rayleigh, TimeOn_cm values should be specified'
    stop
  endif
  if ((abs(TimeEnd_Rayleigh) > 1.d10) .and. (abs(TimeEnd_cm) > 1.d10) ) then
    write(*,*) ' error in Trmax input values; one of the TimeEnd_Rayleigh, TimeEnd_cm values should be specified'
    stop
  endif
  if (TypeOfCalculation /= 1) then
    write(*,*) 'Presently only the value TypeCalculation = 1 is allowed '
    stop
  endif
  if (RadiusGridCoef < 1.d-10) then
    write(*,*) 'RadiusGridCoef should be > 0'
    stop
  endif
  if((ZLengthGrid < 1.d-10) .and.(ZLengthGrid_cm < 1.d-10)) then
    write(*,*) 'error in Numeric input; one of ZlengthGrid or ZlengthGrid_cm should be > 0)'
    stop
  endif
  IZMX = max(IZMX, IDIMZ + 2)
  IRMX = max(IRMX, IDIMR + 2)
!
!*
!**** 1.3 Laser and Plasma parameters
 if(withLaser) then
      call InitLaser
  else
! if there is no laser, we fix wave length and the waist so that the Rayleigh length will be equal to one cm
    LaserWLength_cm = 0.8d-4; LaserWaist0_cm = sqrt(LaserWLength_cm / Pi)
  endif
!
    OmegaL = 2.d0 * Pi * c_cm_s / LaserWLength_cm
    CriticalDensity_cm3 = 1.11485e13 / LaserWLength_cm**2
    Rayleigh_cm = Pi * LaserWaist0_cm**2 / LaserWLength_cm

  if(withPlasma) call InitPlasma
       

!*  Calculate laser-plasma parameters
  if(withPlasma) then
    OmegaP = sqrt(1.d-12 * Dens0Plasma_cm3 * ChargeElec_nc**2 / (Masse_Elec_kg * VacEps0))
    PlasmaWNum_cm1 = OmegaP / c_cm_s
    PlasmaWlength_cm = 2.d0 * Pi / PlasmaWNum_cm1
  else
! without plasma, we fix the density so that PlasmaWNum_cm1 = 1 cm^-1
    PlasmaWNum_cm1 = 1.d0
    OmegaP = PlasmaWNum_cm1 * c_cm_s
    Dens0Plasma_cm3 = 1.d12 * Masse_Elec_kg * VacEps0 * OmegaP**2 / ChargeElec_nc**2
  endif
!
  EpsPlasmaLaser = OmegaP / OmegaL
  if(laserDuration < 1.d-10) then
    laserDuration = LaserDuration_fs * 1.d-15 * OmegaP
  else
    LaserDuration_fs = laserDuration / OmegaP
  endif
!  
  if(laserGridPositionInit > 1.d10) then
    laserGridPositionInit = LaserGridPosition_duration * laserDuration
  else
    LaserGridPosition_duration = laserGridPositionInit / laserDuration
  endif
!
!*
!***  1.4 Particle Beams
  if(withBeamParticle) call InitBeam()
!*
!***  1.5 Test Particles
  if(withTestParticle) call InitTestPart()
!*
!***  1.6 Control Output
!* Default values
  DeltaTSort =1.d20
  DeltaTSort_cm = 1.d20
  LPrintPlasmaField = .False.
  LPrintLaserField = .False.
  LPrintBeamPartField = .False.
  LPrintBeamPartT2u = .False.
  LPrintTestPartField = .False.
  LPrintTotalField = .False.
  BeamNumPrint = NtBeamPartInit
!*** read the namelist
!*** Check  
  read(10,ParOutput)
  if((DeltaTSort > 1.d10).and.(DeltaTSort_cm> 1.d20)) then
    write(*,*) 'one of DeltaTSort, DeltaTSort_cm should be specified'
    stop
  endif
 
! basic input is finished
    close(10)
!*   
!********* 2 initialization of general parameters
   
  if(LaserWaist0 < 1.d-10) then !< LaserWaist0 is needed to determine the grid radius
    LaserWaist0 = LaserWaist0_cm * PlasmaWNum_cm1
  else
    LaserWaist0_cm = LaserWaist0 / PlasmaWNum_cm1
  endif
  if(TypeOfCalculation == 1) then !< determination of LaserAmax  
    if((LaserAmax < 1.d-10).and.(LaserMaxIntensity_Wcm2 < 1.d-10)) then !< LaserAmax and Int Max are determined by the energy
      LaserMaxIntensity_Wcm2 = 1.d15 * LaserEnergy_J / ((0.5*Pi)**1.5 * LaserWaist0_cm**2 * LaserDuration_fs)
      LaserAmax = 0.855d-5 * LaserWLength_cm * sqrt(LaserMaxIntensity_Wcm2)
    else
      if(LaserAmax < 1.d-10) then
        LaserAmax = 0.855d-5 * LaserWLength_cm * sqrt(LaserMaxIntensity_Wcm2)
      else
        LaserMaxIntensity_Wcm2 = (LaserAmax / (0.855d-5 * LaserWLength_cm))**2
      endif
      LaserEnergy_J = 1.d-15 * LaserMaxIntensity_Wcm2 * ((0.5*Pi)**1.5 * LaserWaist0_cm**2 * LaserDuration_fs)
    endif
  endif
      
  
  RadiusGrid = LaserWaist0 * RadiusGridCoef
  RadiusGrid_cm = RadiusGrid /  PlasmaWNum_cm1
  dR = RadiusGrid / IDIMR
  UnSdR = 1.d0 / dR
  dRS2 = 0.5d0 * DR
  dR2 = dR * dR
  UnSdR2 = 1.d0 / dR2
  if(ZLengthGrid < 1.d-10) then
    ZLengthGrid = ZLengthGrid_cm * PlasmaWNum_cm1
  else
    ZLengthGrid_cm = ZLengthGrid / PlasmaWNum_cm1
  endif
  dZ = ZLengthGrid / IDIMZ
  dZs2 = 0.5d0 * dZ
  dZ2 = dZ*dZ
  allocate(T1GridRadi(-1:IRMX))
  allocate(T1GridRadj(-1:IRMX))
  do ir = -1, IRMX
    T1GridRadi(ir) = ir * dR
    T1GridRadj(ir) = (ir + 0.5d0) * dR
  enddo
  allocate(T1Gridzi(-1:IZMX))
  allocate(T1Gridzj(-1:IZMX))
  do iz = -1 , IZMX
    T1Gridzi(iz) = iz * dZ
    T1Gridzj(iz) = (iz + 0.5d0) * dZ
  enddo
!**
  if(TimeOn_Rayleigh > 1.d10) then
    TimeOn_Rayleigh = Timeon_cm / Rayleigh_cm
  else
    Timeon_cm = TimeOn_Rayleigh * Rayleigh_cm
  endif
  TimeOn = TimeOn_cm * PlasmaWNum_cm1
!*  
  if(TimeEnd_Rayleigh > 1.d10) then
    TimeEnd_Rayleigh = TimeEnd_cm / Rayleigh_cm
  else
    TimeEnd_cm = TimeEnd_Rayleigh * Rayleigh_cm
  endif
  TimeEnd = TimeEnd_cm * PlasmaWNum_cm1
  
!**
  if(DeltaTSort > 1.d10 ) then
    DeltaTSort = DeltaTSort_cm * PlasmaWNum_cm1
  else
    DeltaTSort_cm = DeltaTSort / PlasmaWNum_cm1
  endif
!*
  if(DeltaT > 1.d10) then
    if(DeltaT_cm > 1.d10) then
      if(DeltaT_Rayleigh < 1.d10)  DeltaT = DeltaT_Rayleigh * Rayleigh_cm * PlasmaWNum_cm1
    else
      DeltaT = DeltaT_cm * PlasmaWNum_cm1
    endif
  endif
  DeltaT = min(DeltaT, DeltaTSort)
  DeltaT_cm = DeltaT / PlasmaWNum_cm1
  DeltaT_Rayleigh = DeltaT_cm / Rayleigh_cm
!
    
        
!********** 3 Output the values of general parameters
! initialization of the computing time
  call CPU_TIME(loctime)
  CpuTime_mn = loctime / 60.d0
! outputs  
  open (10,file='WakeTRaj_output.out')
  write(10,*) '********   WakeTraj Calculation **********'
  call GETDAT(i,j,k)
  write(10,'(''year 20'',I2,'' month '',I2,'' day '',I2)') i,j,k
  call InitOutput()
  close (10) 
  
end subroutine InitMain
  