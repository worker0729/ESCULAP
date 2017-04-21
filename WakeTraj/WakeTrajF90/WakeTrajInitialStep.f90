! file WakeTrajInitalStep.f90
! Step II: determine the values of all involved variables at the initial step
! called by WakeTrajMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 13/04/2017
!** 13/04/2017  : case TypeOfCalculation = 2 is added
!> subroutine that determine the initial step of the calculation
subroutine InitialStep()
  use MdConstant
  use MdOutput
  use MdLaser
  use MdPlasma
  use MdNumeric
  implicit none
!*
!   1.0 Numeric parameters
! initial time is time3, time1 = time3 - deltaT
  Time3 = TimeOn_cm *  PlasmaWNum_cm1
  Time1 = Time3 - DeltaT
  Time = Time3
  Time_cm = TimeOn_cm
  Time_Rayleigh = Time_cm / Rayleigh_cm
! if printing is on then we will print the initial state
  Timesort = 0.d0
  NumPrint = -1
! if plasma is present we determine the initial density
  if(withplasma) call DensityProfile()

! initialisation will depends on the type of calculation
  select case (TypeOfCalculation)

    case(1) !< linear theory for a Gaussian beam
!  main laser properties
!  Laser intensities and energy
      LaserWaist = LaserWaist0 * sqrt(1.d0 + (time_cm/Rayleigh_cm)**2)
      AmpLaser = LaserAmax * LaserWaist0 / LaserWaist
      Call LinearGaussianField()  !<determination of the field at the given time
      if(withBeamParticle) call BeamInjection !< injection of the beam particles
    
    case(2) !< field is calculated from WakeAC
      iTimeStep = -1 !< initial time should be the same than in the WakeAC calculation
      Call FieldFromWakeAC()  !< determination of the field at the given time
      if(withBeamParticle) call BeamInjection !< injection of the beam particles
      
    case default
      write(*,*) ' error, presently only TypeOfCalculation=1 is allowed '
      write(*,*) ' however, input value of TypeOfCalculation = ',TypeOfCalculation
      stop
  end select
! number of time steps
      NumTimestep = (TimeEnd - TimeOn) / DeltaT
      if(NumTimestep > MaxNumTimestep) then
        write(*,*) 'Warning, NumTimestep = ', NumTimestep, 'is larger than  MaxNumTimestep'
        NumTimestep = MaxNumTimestep
        TimeEnd = NumTimestep * DeltaT
        TimeEnd_cm = TimeEnd / PlasmaWNum_cm1
        TimeEnd_Rayleigh = TimeEnd_cm / Rayleigh_cm
        write(*,'(''TimeEnd '',E16.8,'' TimeEnd_cm '',e16.8,'' TimeEnd_Rayleigh '',e16.8)') TimeEnd, TimeEnd_cm, TimeEnd_Rayleigh
      endif
      
  
end subroutine InitialStep  
