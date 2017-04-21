! file WakeTrajMain.f90
! calculation of electrons trajectories in a plasma Wake
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! note that time is often written as a length in cm, the related time is in fact the one needed to travel one cm with the speed of light, which is the speed of the moving window  
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 13/04/2017
! ** 13/04/2017: implementation of TypeOfCalculation=2 for which the field is calculated from T2Psi values of a WakeAC calculation
!> main program
program WakeTrajMain
  use MdNumeric
  implicit none
!
  call InitMain() !< initialization of the main parameters
  call InitialStep()  !< set the initial state of the fields and of the electrons trajectories
  call TimeEvolution()  !< Time Evolution of the fields and of the electrons trajectories
  call FinalAnalysis()  !< final analysis of the results
end program WakeTrajMain  