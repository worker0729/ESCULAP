! file WakeTrajBeamPartLoading.f90
! calculation of the density of charge and current of the Beam Particles 
! called by TimeEvolution, only in case of withBeamLoading = True  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 21/03/2017
!> subroutine calculating the density of charge and of current of the beam particles
subroutine BeamPartLoading()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
!
end subroutine BeamPartLoading  
