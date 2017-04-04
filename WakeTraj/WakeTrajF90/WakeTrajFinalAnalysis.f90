! file WakeTrajFinalAnalysis.f90
! performed eventual post-processing analysis at the end of 
! called by WakeTrajMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 10/03/2017
!> subroutine yielding the evolution of fields and particles during the propagation
subroutine FinalAnalysis()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
!
    real*8 :: locxav, locyav, loczav, locsum
    integer :: ibeam
  
  locxav = 0.d0
  locsum = 0.d0
!
! start of the loop over the beam particles
  do ibeam = 1 , ntBeamPart
    if(T1BeamPartOutTest(ibeam) == 1) cycle
    locsum = locsum+ 1.d0
    locxav = locxav + T2uBeam3(4,ibeam)*cos(T2uBeam3(5,ibeam))
  enddo
!  normalized :
    locxav = locxav / locsum
! output
    open(10,file='WakeTrajEnd.out')
end subroutine FinalAnalysis  
