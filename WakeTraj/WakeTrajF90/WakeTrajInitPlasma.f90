! file WakeTrajInitLaser.f90
! determination of the initial parameters values of the laser for the WakeTraj program
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 14/03/2017

!> subroutine of initialization of the main parameters
subroutine InitPlasma()
  use MdConstant
  use MdNumeric
  use MdPlasma
  implicit none
! 
  namelist /ParPlasma/ Dens0Plasma_cm3
! default values
  Dens0Plasma_cm3 = 0.d0  
!* reading the namelist, the default value is used when the variable name is not include in the input data
  read(10,ParPlasma)
! check some plasma values  
      if(withPlasma .and. (Dens0Plasma_cm3 < 1.d-10)) then
      write(*,*) ' withplasma = true, then Dens0Plasma should be > 0 '
      stop
    endif
! allocate the tables
    allocate(T2PlasmaElecRad1(-1:IZMX,-1:IRMX)); T2PlasmaElecRad1 = 0.d0
    allocate(T2PlasmaElecRad3(-1:IZMX,-1:IRMX)); T2PlasmaElecRad3 = 0.d0
    allocate(T2PlasmaElecZ1(-1:IZMX,-1:IRMX)); T2PlasmaElecZ1 = 0.d0
    allocate(T2PlasmaElecZ3(-1:IZMX,-1:IRMX)); T2PlasmaElecZ3 = 0.d0

end subroutine InitPlasma 
