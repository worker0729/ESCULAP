! file WakeTrajRK.f90
! Solve Runge_Kutta equations  time 1 to time 3 
! called by TimeEvolution, only in case of withBeamParticle = True  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 29/03/2017
!> subroutine solving the Runge-Kutta equations
subroutine RungeKutta
  use MdConstant
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  use MdRungeKutta
  implicit none

!
  integer :: iRK
  
!
  deltatRK = DeltaT / NStepRK
  timeRK = time1
! loop on RK
  do iRK = 1 , NStepRK
! Runge Kutta 4
    T1uRK(:) = T1uOneBeamPart(:)
     DeltatRK4 = 0.5d0 * deltatRK 
    Call BeamDerive()
    T1PuRK = deltatRK * T1uPOneBeamPart(:) / 6.d0
    T1uOneBeamPart(:) = T1uRK(:) + DeltatRK4  * T1uPOneBeamPart(:)
    timeRK = TimeRK + 0.5*DeltatRK
    Call BeamDerive()
    T1PuRK = T1PuRK+deltatRK * T1uPOneBeamPart(:) / 3.d0
    T1uOneBeamPart(:) = T1uRK(:) + DeltatRK4  * T1uPOneBeamPart(:)
    DeltatRK4  = deltatRK
    call BeamDerive()
    T1PuRK = T1PuRK+deltatRK * T1uPOneBeamPart(:) / 3.d0
    T1uOneBeamPart(:) = T1uRK(:) + DeltatRK4 * T1uPOneBeamPart(:)
    timeRK = TimeRK + 0.5d0 * DeltatRK
    call BeamDerive()
    T1PuRK = T1PuRK+deltatRK * T1uPOneBeamPart(:) / 6.d0
    T1uOneBeamPart(:) = T1uRK(:) + T1PuRK(:)
! stop the calculation if the particle is outside the grid 
      if(T1uOneBeamPart(4) > RadiusGrid -dr) exit
      if(T1uOneBeamPart(6) > ZLengthGrid - dz) exit
      if(T1uOneBeamPart(6) < dz) exit
      if(T1uOneBeamPart(4)< 0.d0) then
        write(*,*) ' rad < 0 '
        T1uOneBeamPart(4) = 0.5d0 * (T1uOneBeamPart(4) + T1uRK(4))
        T1uOneBeamPart(1) = T1uOneBeamPart(1) * 0.5d0
    endif          
  enddo
!  
end subroutine RungeKutta  
