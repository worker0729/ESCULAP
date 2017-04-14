! file WakeTrajAnalyze.f90
! analyze the state at time3 
! called by TimeEvolution  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 24/03/2017

!> subroutine analyzing the state at time 3
subroutine Analyze()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdOutput
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
  integer :: ibeam, locNBeamPart
!
! 1 check if Beam Particles are outside the physical grid
  if(withBeamParticle) then
    locNBeamPart = 0
    do ibeam = 1 , NtBeamPart
      if(T1BeamPartOutTest(ibeam) == 1) cycle
      if(T2uBeam3(4,ibeam) > RadiusGrid -dr) then
        T1BeamPartOutTest(ibeam) = 1
        cycle
      endif
      if(T2uBeam3(6,ibeam) > ZLengthGrid -dz) then
        T1BeamPartOutTest(ibeam) = 1
        cycle
      endif
      if(T2uBeam3(6,ibeam) < dz) then
        T1BeamPartOutTest(ibeam) = 1
        cycle
      endif
      locNBeamPart = locNBeamPart + 1
    enddo
  endif
! 2 save new values of  Gamma for the Beam Particles
  if(withBeamParticle) then
    do iBeam = 1 , NtBeamPart
      if(T1BeamPartOutTest(ibeam) == 1) cycle
      T1BeamGam(iBeam) = sqrt(1.d0 + T2uBeam3(1,iBeam)**2 + (T2uBeam3(2,iBeam)/T2uBeam3(4,iBeam))**2 + T2uBeam3(3,iBeam)**2)
    enddo
  endif

! 2 Look whether we have to do some output
  if(TimeSort < 0.5*DeltaT) then
    TimeSort = DeltaTSort + 0.499d0*deltaT
    NumPrint = NumPrint+1
    write(*,*) ' Time_cm',time_cm, ' NumPrint ',Numprint
    if(withBeamParticle) write(*,*) ' Number of beam particles ',locNBeamPart
    if(LPrintPlasmaField) call PrintPlasmaField()
    if(LPrintLaserField) call PrintLaserField()
    if(LPrintBeamPartField) call PrintBeamPartField()
    if(LPrintBeamPartT2u) call PrintBeamPartT2u()
    if(LPrintTestPartField) call PrintTestPartField()
    if(LPrintTotalField) call PrintTotalField()
  endif
  TimeSort = TimeSort - deltaT
end subroutine Analyze
