! file WakeTrajInitTestPart.f90
! determination of the initial parameters values of the test Particle calculation for the WakeTraj program
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 14/03/2017
!> subroutine of initialization of the main parameters
subroutine InitTestPart()
  use MdConstant
  use MdNumeric
  use MdTestParticle
  implicit none
 namelist/ParTestParticle/ MaxNumTP, TStartInjecTP, TStartInjecTP_cm, PonderForceTP, TStopInjecTP, &
                          & TStopInjecTP_cm   

!
!*  default values
  withTestParticle = .False.
  MaxNumTP = 0
  PonderforceTP = .True.
  TestInjecTP = .False.
  TStartInjecTP = -1.d10
  TStopInjecTP = 1.d10
  TStartInjecTP_cm = -1.d10
  TStopInjecTP_cm = 1.d10
!* reading the namelist, the default value is used when the variable name is not include in the input data
  read(10,ParTestParticle)
!* Check input Test Particles values
  if(withTestParticle) then
    if(MaxNumTP == 0) then
      write(*,*) ' withTesParticle = True; MaxNumTP should be > 0'
      stop
    endif
    if((TStartInjecTP < -1d9) .and.(TStartInjecTP_cm < -1.d9)) then
      write(*,*) ' withTesParticle = True; either TStartInjecTP or TStartInjecTP_cm should be specified '
      stop
    endif
    if((TStopInjecTP > 1d9) .and.(TStopInjecTP_cm > 1.d9)) then
      write(*,*) ' withTesParticle = True; either TStopInjecTP or TStopInjecTP_cm should be specified '
      stop
    endif
  endif
end subroutine InitTestPart