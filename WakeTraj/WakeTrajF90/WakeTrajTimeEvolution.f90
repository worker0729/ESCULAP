! file WakeTrajTimeEvolution.f90
! determination of the evolution of fileds and particles during time (time is in fact identified by the position of the moving window) 
! called by WakeTrajMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 13/04/2017
!* 13/04/2017 : implementation of typeofcalculation=2  
!> subroutine calculating the evolution of fields and particles during the propagation
subroutine TimeEvolution()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
!
  iTimeStep = -1
!
  do while (time < timeEnd) !< start of main loop
    iTimeStep = iTimeStep + 1
    if( (iTimeSTep/10)*10 == iTimeStep) then
        write(*,'(''** istep ** '',i9,'' ** position  ** '',e12.4,'' cm'')') itimeStep, time_cm
    endif
! 1. analyze the present position state (time = time3)
    call Analyze
    
! 2.  Copy state 3 to state 1 and initialize state 3
    call Copy3To1
    
! 3. calculate the Fields at time 3, which can depend on the type of Calculation
  select case (TypeOfCalculation)

    case(1) !< linear theory for a Gaussian beam
!  main laser properties
      LaserWaist = LaserWaist0 * sqrt(1.d0 + (time_cm/Rayleigh_cm)**2)
      AmpLaser = LaserAmax * LaserWaist0 / LaserWaist
      Call LinearGaussianField()  !<determination of the field at the given time

    case (2)  !< electric field calculated from WakeAC values of Psi potential
      Call FieldFromWakeAC()
      
    case default
      write(*,*) ' error, presently only TypeOfCalculation=1, 2 are allowed '
      stop
  end select

! 4. Calculate the evolution of the Beam Particles properties from time1 to time3
    if(withBeamParticle) call BeamPartMover()  !< move the particles
    if(withBeamLoading) call BeamPartLoading  !< calculate the density of charge and of current
    
    
  enddo !< end of main loop
  
end subroutine TimeEvolution  
