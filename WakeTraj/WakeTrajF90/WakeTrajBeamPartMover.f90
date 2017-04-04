! file WakeTrajBeamPartMover.f90
! Move the Beam Particles from time 1 to time 3 
! called by TimeEvolution, only in case of withBeamParticle = True  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 22/03/2017
!> subroutine calculating the evolution of the Beam Particles from time 1 up to time 3
subroutine BeamPartMover()
  use MdConstant
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  use MdRungeKutta
  implicit none
!
  integer :: ibeam
  real*8 :: locdtR, locdtR2, locdtz, locdtTeta, locGam, locdtRK
!
  do ibeam = 1 , ntBeamPart
    if(T1BeamPartOutTest(ibeam) == 1) cycle
    locGam = T1BeamGam(ibeam)
    T1uOneBeamPart(:) = T2ubeam1(:,ibeam)
! determination of the timestep
    if(CircRz == 0) then     
! condition in the radial direction : displacement smaller than dr
      locdtR = EpsCFL * dr / (abs(T1uOneBeamPart(1))/locGam + 1.d-10)
! condition in the longitudinal direction : displacement smaller  than dz
      locdtz = EpsCFL / (1.d0 - T1uOneBeamPart(3)/locGam + 1.d-10)
! condition for teta : variation has to be small compare to Pi
      locdtTeta = EpsRK * Pi * T1uOneBeamPart(4)**2  /(abs(T1uOneBeamPart(2))/locGam+1.d-10)
! condition for the radial direction : variation has to be small very compare to r
      locdtR2 = 0.1d0 * EpsRK * T1uOneBeamPart(4) / (abs(T1uOneBeamPart(1))/locGam + 1.d-10)     
    endif
! minimum value of the RK timestep
    locdtRK = min(locdtR,locdtR2, locdtz,locdtteta)
    NStepRK = int(DeltaT/locdtRK +0.5d0)
    NStepRK = max(1,NStepRK)
!   perform the Runge-Kutta calculation
    Call RungeKutta()
    T2ubeam3(:,ibeam) = T1uOneBeamPart(:)
  enddo
    
  
end subroutine BeamPartMover  
