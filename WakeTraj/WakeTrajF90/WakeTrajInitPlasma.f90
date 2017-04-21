! file WakeTrajInitLaser.f90
! determination of the initial parameters values of the laser for the WakeTraj program
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 13/04/2017

!> subroutine of initialization of the main parameters
subroutine InitPlasma()
  use MdConstant
  use MdNumeric
  use MdPlasma
  implicit none
! 
  namelist /ParPlasma/ Dens0Plasma_cm3, MaxNumTimestepWakeAC
! default values
  Dens0Plasma_cm3 = 0.d0
  MaxNumTimestepWakeAC = 0
!* reading the namelist, the default value is used when the variable name is not include in the input data
  read(10,ParPlasma)
! check some plasma values  
  if(Dens0Plasma_cm3 < 1.d-10) then
    write(*,*) ' withplasma = true, then Dens0Plasma should be > 0 '
    stop
  endif
  if((TypeOfCalculation == 2).and.(MaxNumTimestepWakeAC == 0)) then
    write(*,*) ' Error, TypeOfCalculation = 2 and MaxNumTimestepWakeAC = 0 '
    stop
  endif
! allocate the tables
  allocate(T2PlasmaElecRad1(-1:IZMX,-1:IRMX)); T2PlasmaElecRad1 = 0.d0
  allocate(T2PlasmaElecRad3(-1:IZMX,-1:IRMX)); T2PlasmaElecRad3 = 0.d0
  allocate(T2PlasmaElecZ1(-1:IZMX,-1:IRMX)); T2PlasmaElecZ1 = 0.d0
  allocate(T2PlasmaElecZ3(-1:IZMX,-1:IRMX)); T2PlasmaElecZ3 = 0.d0
  if (TypeOfCalculation == 2) then 
    allocate(T2PsiAC(-1:idimR+7, -2:idimZ+5)); T2PsiAC = 0.d0
    allocate(T2PlasmaPot3(-1:IZMX,-1:IRMX)); T2PlasmaPot3 = 0.d0
  endif

end subroutine InitPlasma 
