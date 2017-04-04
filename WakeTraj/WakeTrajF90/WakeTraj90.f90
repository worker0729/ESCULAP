! file WakeTrajModules.f90
! this file contains all the modules needed for the WakeTraj program
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 29/03/2017
!> Modules for the WakeTraj program
!*
!
!*
!> module of atomic values for the plasma
module MdAtomicPlasma
  integer :: IZ_ATM
  integer :: NumberSpecies  !< number of Plasma atom species
  integer :: NbreRanIoni    !< Propbability of ionization is calculated from NbreRanIoni random numbers  
  integer :: nelec
  integer :: netat
  integer :: nion
  real*8 :: ataux

  Real*8 :: AverageAtomicNumber    !< average atomic number, taking into account proportion of each species
  real*8 :: aeff          !< used in calculting ionizatio probability
  complex*8 :: a_ioniz       !< used for calculating ionisation energy
  complex*8 :: E_ioniz       !< ionisation energy
  real*8 :: Rlim
  integer, allocatable, dimension(:) :: T1AtomicNumber  !< atomic number of each species of the gas  
  real*8, allocatable, dimension(:) :: T1AtomicMass_me !< atomic mass of each species of the gas divided by the mass of electron
  real*8, allocatable, dimension(:) :: T1FracSpecies  !< proportion of species densities
  real*8, allocatable, dimension(:) :: T1NumSpecies !< Number species of each atom
  REAL*8, allocatable, dimension(:,:) :: T2pot    !< ionisation potential of each level of each species
  REAL*8, allocatable, dimension(:,:) :: T2Augst !< correction term for tunnel ionization
  real*8, allocatable, dimension(:,:) :: T2nQuant !< principal quantum number of each level
  real*8, allocatable, dimension(:,:) :: T2lQuant !< azymuthal quantum number
end module MdAtomicPlasma
!
!*
!> module of atomic values for the test particle
module MdAtomicTP
  Real*8 :: AtomicNumberTP    !< average atomic number, taking into account proportion of each species
  real*8 :: aeffTP          !< used in calculting ionizatio probability
  complex*8 :: a_ionizTP       !< used for calculating ionisation energy
  complex*8 :: E_ionizTP       !< ionisation energy
  real*8 :: AtomicMassTP_me !< atomic mass of each species of the gas divided by the mass of electron
  real*8 :: FracGazTP  !< Fraction of gas ofr the test particles
  REAL*8 :: T1potTP    !< ionisation potential of each level of Test Particles
  REAL*8 :: T1AugstTP !< correction term for tunnel ionization for Test Particles
  real*8  :: T1nQuantTP !< principal quantum number of each level for test particles
  real*8 :: T1lQuant !< azymuthal quantum number for test particles
  integer :: StateInitTP !< input, initial ionization state of the test particle
  integer :: StateFinalTP !< input, maximum ionization state considered
end module MdAtomicTP
!
!*
!> Physical and Mathematical Constants
module MdConstant
  real*8 :: c_cm_s = 2.99792458d+10             !< celerity of light in units of cm / s
  real*8 :: C_Em8 = 2.99792458d+00           !< celerity of light in units of 10^(-8) m / s
  real*8 :: ChargeElec_nc = -1.60217653d-10  !< electron charge in nano Coulomb
  real*8 :: Masse_Elec_kg = 9.10938260d-31  !< electron mass in kg
  real*8 :: Mc2_eV = 5.10998921d+05          !< rest energy of the electron in eV
  real*8 :: Mc2_keV = 5.10998921d+02          !< rest energy of the electron in keV
  real*8 :: Mc2_MeV = 5.10998921d-01         !< rest energy of the electron in MeV
  real*8 :: Omega_fs = 4.13413733d+01       ! atomic frequency in fs^-1
  real*8 :: PStar = 8.70995d09
  real*8 :: Pi              !< pi = -acos(-1.d0)
  real*8 :: Rydb_eV  = 1.36056923d+01        !< Rydberg energy in eV
  real*8 :: UnitMassSme = 1.82288848d+03   !< unit of atomic mass divide by mass of electron  
  real*8 :: VacEps0 !< vaccuum permittivity
  real*8 :: VacMu0  !< vacuum pereability
  complex*8 :: czero = (0.d0,0.d0)      ! complex (0,0)
end module MdConstant
!
!*
!> Parameter of the laser and of the plasma
module MdLaser
  real*8 :: LaserAmax !< input, maximum value of the normalized vector potential amplitude in the focal plane, if on input = 0, calculated from other parameters
  real*8 :: AmpLaser  !< value of the normalized vector potential at a given time
  real*8 :: CriticalDensity_cm3 !< critical density in cm^-3
  real*8 :: EpsLaserDiffrac !< input, used to calculate DeltaT from laser diffraction
  real*8 :: EpsRayleigh !< input, used to calculate DeltaT from the Rayleigh length
  real*8 :: LaserDuration !< input, 1/e^2 duration of the laser intensity in reduced units
  real*8 :: LaserDuration_fs !< input, 1/e^2 duration of the laser intensity in fs
  real*8 :: LaserEnergy_J !< input, laser energy in Joule, is zero on input, calculated
  real*8 :: LaserGam3  !< Lorentz Factor of the group velocity of the laser, at time 3
  real*8 :: LaserGam1  !< Lorentz Factor of the group velocity of the laser, at time 1
  real*8 :: LaserGridPositionInit !< input, Longitudinal Position of the maximum of the laser amplitude in the moving window
  real*8 :: LaserGridPosition3 !< Longitudinal Position of the maximum of the laser amplitude in the moving window at time3
  real*8 :: LaserGridPosition1 !< Longitudinal Position of the maximum of the laser amplitude in the moving window at time1
  real*8 :: LaserGridPosition_duration  !< input, longitudinal position of the maximum of the laser amplitude in the moving window in unit of its duration
  real*8 :: LaserMaxIntensity_Wcm2  ! input, Maximum laser intensity in the focal plane, if on input = 0, calculated from other parameters
  real*8 :: LaserRelatBeta3 !< relative velocity divided by c in the reference fram of the numerical grid, at time 3
  real*8 :: LaserRelatBeta1 !< relative velocity divided by c in the reference fram of the numerical grid, at time 1
  real*8 :: LaserWLength_cm  !< input, laser wavelength in cm
  real*8 :: LaserWNum_cm1 !< LaserWNum_cm1 = 2 * Pi / LaserWLength_cm
  real*8 :: OmegaL  !< angular frequency of the laser in rad/s
  real*8 :: Rayleigh_cm   !< Rayleigh Length in cm
  real*8 :: LaserWaist0    !< input, waist of the laser in reduced units in the focal plane
  real*8 :: LaserWaist0_cm !< input, waist of the laser in cm in the focal plane
  real*8 :: LaserWaist  !< waist of the laser at the given time
  logical :: FullGaussianLaser !< whether the laser is Gaussian in time and in space (true) or not
  logical :: TimeGaussianLaser !< input, whether the laser intensity is Gaussian in time
  logical :: SpaceGaussianLaser !< input, whether the laser amplitude is Gaussian in Space
end module MdLaser
!
!*
!> General Numeric parameters
module MdNumeric
  integer :: CircRz !< input, max number of angular modes. If 0 : cylindrical symmetry
  Integer :: IDIMR   !< input, determine the size of the transverse grid (physical one)
  Integer :: IDIMZ  !< input, size of the physical grid in the longitudinal direction
  integer :: iR1  !< input, number of radial cells with np1 trajectories
  integer :: iR2  !< input, number of radial cells with np2 trajectories, should be > IR1  
  Integer :: IRMX     !< size of the numerical grid in the transverse dimension. IRMX > IDIMR. IRMX -IDIMR can be used as an numerical boundary condition
  Integer :: IZMX     !< size of the numerical grid in the longitudinal dimension
  integer :: np1  !< number of trajectories per cell for ir < IR1
  integer :: np2  !< number of trajectories per cell for IR1 < ir < IR2
  integer :: np3  !< number of trajectories per cell for ir > IR2
  integer :: NumTimestep  !< expected number of timestep
  integer :: iTimeStep    !< index of time Step 
  integer :: MaxNumTimestep !< input, maximum number of timestep
  integer :: ProjOrderR !< input, projection order in the r grid
  integer :: ProjOrderZ !< input, projection order in the z grid
  integer :: TypeOfCalculation  !< input, set the type of calculation, 1 : linear response of a given LG laser field, 2 : post-processing WakeAC fields, 3: make WakeAC-type calculation
  real*8 :: CPuTime_mn !< CPU_Time in mn since the begining of the calculation
  real*8 :: DeltaT !< input, this is the global value of time step in reduced units
  real*8 :: DeltaT_cm !< input, value of DeltaT in cm
  real*8 :: DeltaT_Rayleigh !< input, value of DeltaT in units of Rayleigh length
  real*8 :: dR  !< transverse size of a numerical cell in reduced units
  real*8 :: DRs2  !< = 0.5d0 * DR
  real*8 :: DR2 !< = DR**2
  real*8 :: DZ !< longitudinal size of a numerical cell in reduced units
  real*8 :: dZs2 !< = 0.5 * dZ
  real*8 :: dZ2 !< = dZ*dZ
  real*8 :: EpsCFL !< input, used to calculate the value of DeltaT from Courant Friedrichs Lewy Condition
  real*8 :: EpsRK !< input, used to calculate the time step of a Runge-Kutta integration
  real*8 :: RadiusGrid  !< Radius of the grid in reduced units
  real*8 :: RadiusGrid_cm !< Radius of the grid in cm
  real*8 :: RadiusGridCoef !< input, coefficient used to determined the radius of the grid * RadiusGrid =  RadiusGridCoef * LaserWaist0
  real*8 :: TimeOn_Rayleigh !< input, stating time of the calculation in Rayleigh length, in fact it is more a starting position, remenber that z=0 corresponds to the focal plane of the laser
  real*8 :: TimeOn_cm !< input, = TimeOn in cm, if TimeOn is 0, then TimeOn is calculated from TimeOn_cm, if TimeOn_cm is zero, then it is defined from TimeOn
  real*8 :: TimeOn  !< timeOn in reduced units
  real*8 :: TimeEnd_Rayleigh !< input : final time of the calculation in Rayleigh length, same remark as for TimeOn
  real*8 :: TimeEnd_cm !< input, final time of the calculation in cm, same remark as for TimeOn_cm
  real*8 :: TimeEnd !< TimeEnd in reduced units
  real*8 :: Time  !< current time, in reduced units
  real*8 :: time_cm !< curent position of the moving window in cm
  real*8 :: Time_Rayleigh !< current time in units of the Raeileigh length
  real*8 :: Time1 !< time at position 1, in reduced units
  real*8 :: Time3 !< Time at position 3, in reduced units
  real*8 :: UnSdR !< = 1/DR
  real*8 :: UnSdR2 !< = 1/dR2
  real*8 :: ZLengthGrid !< input, length of the grid in reduced units
  real*8 :: ZLengthGrid_cm  !< input, length of the grid in cm
  Logical :: OnTimeIteration !< whether to continue (True) or not (False) the time iteration
  logical :: withLaser    !< input, determines whether there is a laser beam (True) or not (False)
  Logical :: withBeamParticle   !< input whether there is one or more injected particle beam (True) or not (False)
  logical :: withPlasma   !< input, determines whether there is a plasma (True) or not (False)
  Logical :: withTestParticle !< input, whether testParticle are generated (True) or not (False)  
  Character*10, dimension(10) :: T1Titles !< used for output
! tables
  real*8, allocatable, dimension(:) :: T1GridRadi  !< positions of the radial vertices
  real*8,allocatable, dimension(:) :: T1GridRadj  !< positions of the center of the radial edges   
  real*8, allocatable, dimension(:) :: T1Gridzi !< Positions of the longitudinal vertices
  real*8, allocatable, dimension(:) :: T1Gridzj !< Positions of the center of the longitudinal edges
  
end module MdNumeric
!
!*
!> Parameters for injected particles beam
module MdBeamParticle
  integer :: NumBeam !< input, number of injected beams, if only one, it is supposed to have the mass of electron but can have positive or negative charge
  integer :: NcaseBeamInjection !< input, used for injected beam in the subroutine BeamInjection, = 0:gaussian distribution; =1 read External_BeamPart.dat (ASTRA format)
  integer :: NtBeamPart !< number of injected particle at a given time 
  integer :: NtBeamPartRead !< input, used in case of injected particles, number of particles to be read
  integer :: NtBeamPartInit    !< =NtBeamPartRead * MultiplyBeamPart, number of injecteed particles
  integer :: MultiplyBeamPart !< input, used for injected beam, when NcaseBeamInjection=1, number of particle for each read particle
  real*8 :: BeamDeltaX  !< input, delta x of the particles beam compare to the laser beam, in reduced units
  real*8 :: BeamDeltaX_cm !< input, delta x of the particles beam compare to the laser beam in cm 
  real*8 :: BeamDeltaY  !< input, delta y of the particles beam compare to the laser beam, in reduced units
  real*8 :: BeamDeltaY_cm !< input, delta y of the particles beam compare to the laser beam in cm
  real*8 :: BeamEpsMultiply !< input, dispersion in terms of the width for calculating new beam position in case of MultiplyBeamPart>1
  real*8 :: BeamPhi  !< = BeamPhi_mrad * 1e3
  real*8 :: BeamTeta  !< = BeamTeta_mrad * 1.e3
  real*8 :: BeamPhi_mrad   !< input angle (mrad)in the transverse plane, between the beam and the laser axis
  real*8 :: BeamTeta_mrad  !< input, angle (mrad)between the plasma wave axis and the beam axis in radian.
  REAL*8 :: BeamDuration !< input, used only for injected beam and multiplyBeam > 1, used to select the width in time in reduced units of the injected beam
  REAL*8 :: BeamDuration_fs !< input, used only for injected beam and multiplyBeam > 1, used to select the width in time in fs of the injected beam
  real*8 :: BeamZfocX !< input, z position in reduced units of the focal point in x (alpha_x = 0)
  real*8 :: BeamZfocY !< input, z position in reduced units of the focal point in y (alpha_y = 0)
  real*8 :: BeamZfocX_cm  !< input, z position in cm of the focal point in x (alpha_x = 0)
  real*8 :: BeamZfocY_cm  !< input, z position in cm of the focal point in y (alpha_y = 0)
  Real*8 :: BeamGamMoy !< input, average value of the Lorentz factor for the beam particles
  Real*8 :: BeamGamWidth !< input, width of the distribution in the Lorentz factor valor for the beam particles
  REAL*8 :: BeamPosition !< input, yield the reference position in reduced units of the injected beam
  real*8 :: BeamPosition_cm !< input,  yield the reference position in cm of the injected beam
  REAL*8 :: BeamCharge_nc   !< input, charge of the inject external beam (beam_line=.true) in nC, 
  Real*8 :: BeamChargeTR  !< input, used only in case of WithBeamloadin= True, to fix the evolution of beam charge (to avoid too strong field variation)
  Real*8 :: BeamChargeDTR !< input, used only in case of WithBeamloadin= True, to fix the evolution of beam charge (to avoid too strong field variation)
  REAL*8 :: BeamDenWt0   !< max number of electron per macroparticle
  REAL*8 :: BeamDenWt   !< number of electron per macroparticle at the considered position
  real*8 :: BeamWidthTetaX_mrad !< rms value of the divergence in X in mrad
  real*8 :: BeamWidthTetaY_mrad !< rms value of the divergence in X in mrad
  real*8 :: BeamWidthX  !< input, width in x of the initial ditribution in reduced units
  real*8 :: BeamWidthX_cm !< input, width in y of the initial distribution in cm
  real*8 :: BeamWidthY  !< input, width in y of the initial distribution in reduced units
  real*8 :: BeamWidthY_cm !< input, width in y of the initial distribution in y 
  Logical :: withBeamLoading !< input, whether the fieds of the particle beams are taken into account or not 
!** Tables
  integer, allocatable, dimension(:) :: T1BeamPartOutTest !< (NtBeamPartInit), check whether the particle is inside the grid (0) or outside (1)
  integer, allocatable, dimension(:)  :: T1BeamPartIdent  !< (NtBeamPartInit),number used to identified each beam particle
  real*8, allocatable, dimension(:) ::  T1BeamGam !< (NtBeamPartInit), Lorentz gamma factor of the beam particles
  real*8, allocatable, dimension(:) ::  T1uOneBeamPart  !< (6) phase space of one beam particle at a given time: 1=ux,2=uy,3=uz, 4=x, 5=y, 6=z
  real*8, allocatable, dimension(:) ::  T1uPOneBeamPart  !< (6) derivative of T1uOneBeamPart
  real*8, allocatable, dimension(:,:) :: T2uBeam1  !< (6,NtBeamPart),phase space of the beam particle at time1: 1=ux,2=uy,3=uz, 4=x, 5=y, 6=z
  real*8, allocatable, dimension(:,:) :: T2uBeam3  !< (6,NtBeamPart),phase space of the beam particle at time3: 1=ux,2=uy,3=uz, 4=x, 5=y, 6=z
  real*8, allocatable, dimension(:) :: T1uBeamWidth !< (6) Sigma values of T2uBeam
  real*8, allocatable, dimension(:) :: T1uBeamMoy !< (6) averages values of T2uBeam, on input, positions in cm, momentum in mc
  real*8, allocatable, dimension(:) :: T1uBeamXYZUxyz   !< (3), average value of xUx, yuY, zuz
  real*8,  allocatable, dimension(:,:) :: T2BeamEmit     !< (2,4), emittance, and alpha, beta and gama value of the beamparticles distribution
end module MdBeamParticle
!
!*
!> Parameter of the plasma
module MdPlasma
  integer :: FormeDensityProfile !< input in WakeTrajDensProfile.dat, determine the type of density profile
  real*8 :: DensPlasma !< relative plasma density
  real*8 :: Dens0Plasma_cm3  !< input, reference density of the plasma electrons in cm3, density on a specific point = Dens0Plasma_cm3 * DensPlasma
  real*8 :: EpsPlasmaLaser !< ratio of OmegaP / OmegaL
  real*8 :: OmegaP  !< angular frequency of the plasma (at relative density = 1) in rad/s
  real*8 :: PlasmaWlength_cm  !< Plasma wave length
  real*8 :: PlasmaWNum_cm1 !< Plasma nwave number in cm^-1
!** Tables
  real*8, allocatable, dimension(:,:) :: T2PlasmaElecRad1 !< (-1:IZMX,-1:IRMX) radial electric field at time 1
  real*8, allocatable, dimension(:,:) :: T2PlasmaElecrad3 !< (-1:IZMX,-1:IRMX) radial electric field at time 3
  real*8, allocatable, dimension(:,:) :: T2PlasmaElecZ1   !< (-1:IZMX,-1:IRMX) longitudinal electric field at time 1
  real*8, allocatable, dimension(:,:) :: T2PlasmaElecZ3   !< (-1:IZMX,-1:IRMX) longitudinal electric field at time 3
end module MdPlasma

!
!*
!> Parameters for plasma test Particles
module MdTestParticle
  integer :: MaxNumTP !< input, maximum number of test particle
  integer :: NumTotTP  !< total number of test Particles at the considered time
  integer :: NumMacroCellRTP  !< number of macro test particle per cell in the radial dimension
  integer :: NumMacroCellZTP  !< number of macro test particle per cell in the longitudinal dimension
  real*8 :: TStartInjecTP !< input, time in reduced units for starting of injected particle
  real*8 :: TStartInjecTP_cm  !< input, time in cm for starting of injected particle
  real*8 :: TStopInjecTP !< input, time in reduced units for starting of injected particle
  real*8 :: TStopInjecTP_cm  !< input, time in cm for starting of injected particle
  Logical :: PonderForceTP  !< input, whether to use (.True.) or ont (False) the ponderomotive approximation to calculate the interaction with the laser
  Logical :: TestInjecTP !< test whether  we have to calculate injection of new test particle (True) or not (False)

end module MdTestParticle
!
!*
!> Parameter for output
module MdOutput
  real*8 :: DeltaTSort  !< input time step for output in reduced units
  real*8 :: DeltaTSort_cm  !< input, timestep for output in cm
  real*8 :: Timesort  !< time for output in reduced output
  Logical :: LPrintPlasmaField  !< input, print (True) or not (False) the plasma field
  Logical :: LPrintLaserField !< input, print (true) or not (False) the laser field
  logical :: LPrintBeamPartField  !< input, print (true) or not (false) the Beam Particle field
  logical :: LPrintBeamPartT2u  !< input, print(True) or not (False) the Beam Particle Phase Space
  logical :: LPrintTestPartField  !< input, print (true) or not (false) the test particle field
  logical :: LPrintTotalField !< input, print (true) or not (false) the total field
  integer :: NumPrint  !< number used to identify the outputTables
  integer :: BeamNumPrint  !< input, Number of printed particles 
  
end module MdOutput
!
!*
!>  Parameter for Runge-Kutta Calculation
module MdRungeKutta
  integer :: NStepRK  !< number of RK steps in one Deltat calculation
  real*8, dimension(6) :: T1uRK, T1PuRK !< intermediate tables used in RK calculation
  real*8 :: DeltatRK  !< = DeltaT / NStepRK, used in RK calculation
  real*8 :: TimeRK  !< time during the RK calculation
  real*8 :: DeltatRK4 !< intermediate values of RK timestep
end module MdRungeKutta
! file WakeTrajMain.f90
! calculation of electrons trajectories in a plasma Wake
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! note that time is often written as a length in cm, the related time is in fact the one needed to travel one cm with the speed of light, which is the speed of the moving window  
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 08/03/2017
!> main program
program WakeTrajMain
  use MdNumeric
  implicit none
!
  call InitMain() !< initialization of the main parameters
  call InitialStep()  !< set the initial state of the fields and of the electrons trajectories
  call TimeEvolution()  !< Time Evolution of the fields and of the electrons trajectories
  call FinalAnalysis()  !< final analysis of the results
end program WakeTrajMain  ! file WakeTrajAnalyze.f90
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
    TimeSort = DeltaTSort + 0.5*deltaT
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
! file WakeTrajBeamDerive.f90
! Calculate the derivative of a Beam Particle Coordinate  
! called by RungeKutta  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 29/03/2017
!> subroutine calculating T1uPOneBeamPart
subroutine BeamDerive()
  use MdConstant
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  use MdRungeKutta  
  implicit none
!
  real*8 :: loctime
  real*8, save ::  LocProjREz(-1:1)=0.d0, LocProjREr(-1:1)=0.d0, LocProjZEz(-1:1)=0.d0, LocProjZEr(-1:1)=0.d0 !< projectors
  real*8 :: locdzEz, locdzEr, locdrEz, locdrEr
  real*8 :: locEz, locEr, locGam, locNothing
  integer :: indrEz, indzEz, indrEr, indzEr !< central index for Ez and Er in the r and z grid
  integer :: ifieldz, ifieldr, izz,izr, irz,irr
  integer :: iminr, iminz
!
  loctime = (TimeRK - time1) / DeltaT
  if(CircRz == 0) then
! determination of the central index  for projection of Ez and Er at the particle position
    select case (ProjOrderR)
      case(1) !< linear interpolation : reference node is given by the integer value of the position
        indrEz = int(T1uOneBeamPart(4)/dr)  !< relative to T1GridRadi
        indrEr = int(T1uOneBeamPart(4)/dr + 0.5d0) - 1  !<relative to T1GridRadj
      case(2) !< second order : reference node is given by the nearest integer value of the position
        indrEz = int(T1uOneBeamPart(4)/dr + 0.5d0) !< relative to T1GridRadi
        indrEr = int(T1uOneBeamPart(4)/dr) !<relative to T1GridRadj
      case default
        write(*,*) ' error in ProjOrderR, only values 1 ,2 are allowed'
        write(*,*) ' ProjOrderR = ', ProjOrderR
        stop
      end select
!
    select case (ProjOrderZ)
      case(1) !< linear interpolation : reference node is given by the integer value of the position
        indzEz = int(T1uOneBeamPart(6)/dz + 0.5d0) - 1 !<relative to T1GridZj
        indzEr = int(T1uOneBeamPart(6)/dz) !<relative to T1GridZi
      case(2) !< second order : reference node is given by the nearest integer value of the position
       indzEz = int(T1uOneBeamPart(6)/dz)  !<relative to T1GridZj
        indzEr = int(T1uOneBeamPart(6)/dz + 0.5d0) !<relative to T1GridZi
      case default
        write(*,*) ' error in ProjOrderZ, only values 1 ,2 are allowed'
        write(*,*) ' ProjOrderZ = ', ProjOrderZ
        stop
      end select
!  determination of delta  for projection of Ez and Er at the particle position   
        locdrEz = (T1uOneBeamPart(4)-T1GridRadi(indrEz)) / dr
        locdrEr = (T1uOneBeamPart(4)-T1GridRadj(indrEr)) / dr
        locdzEz = (T1uOneBeamPart(6)-T1Gridzj(indzEz)) / dz
        locdzEr = (T1uOneBeamPart(6)-T1Gridzi(indzEr)) / dz
!        
    select case (ProjOrderR)
      case(1) !< linear interpolation : reference node is given by the integer value of the position
        locProjRez(0) = 1.d0 - locdrEz; locProjRez(1) = locdrEz
        locProjRer(0) = 1.d0 - locdrEr; locProjRer(1) = locdrEr
      case(2) !< second order : reference node is given by the nearest integer value of the position
        locProjRez(0) = 0.75d0 - locdrEz**2
        locProjRez(-1) = 0.5d0*(locdrEz*(locdrEz-1.d0)+0.25d0); locProjRez(1) = 0.5d0*(locdrEz*(locdrEz+1.d0)+0.25d0)
        locProjRer(0) = 0.75d0 - locdrEr**2
        locProjRer(-1) = 0.5d0*(locdrEr*(locdrEr-1.d0)+0.25d0); locProjRer(1) = 0.5d0*(locdrEr*(locdrEr+1.d0)+0.25d0)
       case default
        stop
      end select
!
    select case (ProjOrderZ)
      case(1) !< linear interpolation : reference node is given by the integer value of the position
        locProjZez(0) = 1.d0 - locdzEz; locProjZez(1) = locdzEz
        locProjZer(0) = 1.d0 - locdzEr; locProjZer(1) = locdzEr
      case(2) !< second order : reference node is given by the nearest integer value of the position
        locProjZez(0) = 0.75d0 - locdzEz**2
        locProjZez(-1) = 0.5d0*(locdzEz*(locdzEz-1.d0)+0.25d0); locProjZez(1) = 0.5d0*(locdzEz*(locdzEz+1.d0)+0.25d0)
        locProjZer(0) = 0.75d0 - locdzEr**2
        locProjZer(-1) = 0.5d0*(locdzEr*(locdzEr-1.d0)+0.25d0); locProjZer(1) = 0.5d0*(locdzEr*(locdzEr+1.d0)+0.25d0)
    end select
!
! determination of the longitudinal and the radial field at the particle position
    locEz = 0.d0; locEr = 0.d0
    iminr = -1; if(ProjOrderR ==1) iminr = 0
    iminz = -1; if(ProjOrderZ ==1) iminz = 0
    do ifieldz = iminz,1
      izz =  indzEz + ifieldz; izr = indzEr + ifieldz    
      do ifieldR = iminr,1
        irr = indrEr + ifieldr; irz = indrEz + ifieldr
        locEz = locEz + locProjZez(ifieldz)*locProjRez(ifieldr) * &
              & (T2PlasmaElecZ1(izz,irz) + loctime*(T2PlasmaElecZ3(izz,irz)-T2PlasmaElecZ1(izz,irz)))
        locEr = locEr + locProjZer(ifieldz)*locProjRer(ifieldr) * &
              & (T2PlasmaElecRad1(izr,irr) + loctime*(T2PlasmaElecRad3(izr,irr)-T2PlasmaElecRad1(izr,irr)))
      enddo
    enddo
!
! determination of the derivative
    locGam = sqrt(1.d0 + T1uOneBeamPart(1)**2 + (T1uOneBeamPart(2)/T1uOneBeamPart(4))**2 + T1uOneBeamPart(3)**2)
    T1uPOneBeamPart(1) = -locEr + T1uOneBeamPart(2)**2 / (locGam*T1uOneBeamPart(4)**3)
    T1uPOneBeamPart(2) = 0.d0 !< kinetic momentum is constant for CircRz
    T1uPOneBeamPart(3) = -locEz 
    T1uPOneBeamPart(4) = T1uOneBeamPart(1) / locGam
    T1uPOneBeamPart(4) = max (T1uPOneBeamPart(4),-0.5d0 * T1uRK(4) / DeltatRK4) !< delta_r / r < 1/2
     
    T1uPOneBeamPart(5) = T1uOneBeamPart(2)/ (locGam * T1uOneBeamPart(4)**2)
    T1uPOneBeamPart(6) = 1.d0 - T1uOneBeamPart(3) / locGam

  endif !< end of CircRZ = 0
end subroutine BeamDerive
       
        
        ! file WakeTrajBeamInjection.f90
! determination of the initial Beam parameters 
! called by InitialStep  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 20/03/2017

!> subroutine of initialization of the main parameters
subroutine BeamInjection()
  use MdConstant
  use MdBeamParticle
  use MdNumeric
  use MdOutput
  use MdPlasma
  implicit none
  integer :: iBeam, jBeam, jPhaseSpace, kBeam
  integer :: initAlea !< initialization of a normal distribution
  real*8 :: locGam, locRan
  real*8 :: locx0, locy0, locz0, locpx0, locpy0, locpz0
  real*8 :: locx, locy, locz, locpx, locpy, locpz
  real*8 :: locRPerp, locUPerp, locLz, locTeta
  
!
  select case(NcaseBeamInjection)  
  case (0) !< we assume Gaussian Distribution for all of the 6 phase space coordinates
      initAlea = 0
      call GaussAlea(locran,initAlea)
      initAleA = 1
      DO iBeam = 1,NtBeamPartInit
! determine Gamma      
        call GaussAlea(locRan,initAlea)
        locGam = BeamGamMoy*(1.d0 + BeamGamWidth * locRan)
! angle for px
        call GaussAlea(locRan,initAlea)
        locpx = sin(1.d-3 * locRan * BeamWidthTetaX_mrad)
! angle for py
        call GaussAlea(locRan,initAlea)
        locpy = sin(1.d-3 * locRan * BeamWidthTetaY_mrad)
! value of uz, and ux, uy
        T2uBeam3(3,ibeam) = sqrt( (locGam**2 -1.d0) / (1.d0 + locpx**2 + locpy ** 2))
        T2uBeam3(1,ibeam) = T2uBeam3(3,ibeam) * locpx
        T2uBeam3(2,ibeam) = T2uBeam3(3,ibeam) * locpy
! value of x and y in their focal plane
        call GaussAlea(locRan,initAlea)
        locx = BeamWidthX * locRan
        call GaussAlea(locRan,initAlea)
        locy = BeamWidthY * locRan
! change x and y due to displacement between focal plane and TimeOn
        T2uBeam3(4,ibeam) = locx + (BeamZfocX - TimeOn) * T2uBeam3(1,ibeam) / locGam
        T2uBeam3(5,ibeam) = locy + (BeamZfocY - TimeOn) * T2uBeam3(2,ibeam) / locGam
! value of z
        call GaussAlea(locRan,initAlea)
        T2uBeam3(6,ibeam) = BeamPosition + BeamDuration * locRan
      enddo
!      
  case (1)  !< read input_data table, Format ASTRA
      open(111,file='External_BeamPart.dat')
      read(111,*) !< first line is used for describing the data
      read(111,*) locx0, locy0, locz0, locpx0, locpy0, locpz0 !< first particle is the reference one
      ibeam=1
      T2uBeam3(4,1) = 1.d-15
      T2uBeam3(5,1) = 1.d-15
      T2uBeam3(6,1) = BeamPosition
      T2uBeam3(3,1) = locpz0 / Mc2_eV
      T2uBeam3(1,1) = 1d-6 * locpz0 / Mc2_eV
      T2uBeam3(2,1) = 1d-6 * locpz0 / Mc2_eV
      locgam = sqrt(1.d0 + T2uBeam3(1,ibeam)**2 + T2uBeam3(2,ibeam)**2 + T2uBeam3(3,ibeam)**2)
      do iBeam = 2 , NtBeamPartRead
        read(111,*) locx, locy, locz, locpx, locpy, locpz 
        T2uBeam3(1,iBeam) = locpx / Mc2_eV
        T2uBeam3(2,iBeam) = locpy / Mc2_eV
        T2uBeam3(3,iBeam) = (locpz + locpz0) / Mc2_eV
!       calculate the time to go from  locz to the reference point
        locgam = sqrt( 1.d0 + T2uBeam3(1,iBeam)**2 + T2uBeam3(2,iBeam)**2 +T2uBeam3(3,iBeam)**2)
        locz =  - locz * 1d2 * locgam  / (c_cm_s * T2uBeam3(3,iBeam))
!       calculate the new position in x and in y in cm
        locx = 1.d2*locx + locz * T2uBeam3(1,iBeam)  * c_cm_s / locgam
        locy = 1.d2*locy + locz * T2uBeam3(2,iBeam)  * c_cm_s / locgam
        locx0 = locx  * PlasmaWNum_cm1 !< tranform cm to kp^-1
        locy0 = locy  * PlasmaWNum_cm1 !< tranform cm to kp^-1
        locz0 = beamPosition + locz * OmegaP  !< transform s to OmegaP^-1
        T2uBeam3(4,iBeam) = locx0  
        T2uBeam3(5,iBeam) = locy0
        T2uBeam3(6,iBeam) = locz0   
      enddo
      iBeam = NtBeamPartRead
!     add MultiplyBeams
      if (MultiplyBeamPart > 1 ) then
!       first calculate the average values and the width
        T1uBeamMoy = 0.d0
        T1uBeamWidth = 0.d0
        do jPhaseSpace= 1 , 6
          do ibeam = 1 , NtBeamPartRead
            T1uBeamMoy(jPhaseSpace) = T1uBeamMoy(jPhaseSpace) +  T2uBeam3(jPhaseSpace,iBeam)
            T1uBeamWidth(jPhaseSpace) = T1uBeamWidth(jPhaseSpace) +  T2uBeam3(jPhaseSpace,iBeam)**2
          enddo
        enddo
        T1uBeamMoy = T1uBeamMoy / dble(NtBeamPartRead)
        T1uBeamWidth = T1uBeamWidth / dble(NtBeamPartRead)
        do jPhaseSpace = 1 ,6
          T1uBeamWidth(jPhaseSpace) = sqrt(T1uBeamWidth(jPhaseSpace) - T1uBeamMoy(jPhaseSpace)**2) 
        enddo
        
!       next add trajectory around the average one        
        do  jPhaseSpace= 1 , 6
          initAlea = 0
          do kBeam = 1, MultiplyBeamPart
            do jBeam = 1,NtBeamPartInit
              ibeam = jbeam + kBeam * NtBeamPartInit
              call GaussAlea(locRan,InitAlea)
              T2uBeam3(jPhaseSpace,ibeam) = T2uBeam3(jPhaseSpace,jbeam) + T1uBeamWidth(jPhaseSpace)*BeamEpsMultiply * locRan 
            enddo
          enddo
        enddo
      endif
      close (111)
!
      case default
        write(*,*) 'Error in the value of NcaseBeamInjection '
        write(*,*) ' NcaseBeamInjection = ', NcaseBeamInjection
        stop
  end select
!   withdraw particles outside the grid 
  ntBeamPart = ntBeamPartInit 
  do iBeam = 1 , ntBeamPartInit
    locx = T2uBeam3(4,iBeam); locy = T2uBeam3(5,iBeam); locz = T2uBeam3(6,iBeam)
    locRperp = sqrt(locx**2.0 + locy**2.0)
      if((LocRperp > RadiusGrid-dr).or.(locz < dz) .or. (locz > ZlengthGrid-dz)) then
        write(*,*) 'particle out of the grid ',' iBeam ',iBeam,' z ',locz,' RPERP ', locRperp
        ntBeamPart = ntBeamPart - 1
        do jBeam = ibeam, ntBeamPart 
         T2uBeam3(:,jBeam)= T2uBeam3(:,jBeam+1)
        enddo
      endif
  enddo
! transverse displacement
  do ibeam = 1 , ntBeamPart
    T2uBeam3(4,ibeam) = T2uBeam3(4,iBeam) + BeamDeltaX
    T2uBeam3(5,ibeam) = T2uBeam3(5,iBeam) + BeamDeltaY
  enddo
! rotation of the Beam Frame
! rotation is performed around the average position
  T1uBeamMoy(4:6) = 0.d0
  do jPhaseSpace= 4 , 6
    do ibeam = 1 , NtBeamPartRead
      T1uBeamMoy(jPhaseSpace) = T1uBeamMoy(jPhaseSpace) +  T2uBeam3(jPhaseSpace,iBeam)
    enddo
  enddo
  T1uBeamMoy(4:6) = T1uBeamMoy(4:6) / dble(NtBeamPartRead)
  do ibeam = 1 , ntBeamPart
    locpx = T2uBeam3(1,iBeam); locpy = T2uBeam3(2,iBeam); locpz = T2uBeam3(3,iBeam)
    locx = T2uBeam3(4,iBeam)-T1uBeamMoy(4); locy = T2uBeam3(5,iBeam) - T1uBeamMoy(5); locz = T2uBeam3(6,iBeam) - T1uBeamMoy(6)
    T2uBeam3(1,iBeam) = -locpy * sin(BeamPhi) + cos(BeamPhi) * (locpx * cos(BeamTeta) + locpz * sin(BeamTeta))
    T2uBeam3(2,iBeam) = +locpy * cos(BeamPhi) + sin(BeamPhi) *  (locpx * cos(BeamTeta) + locpz * sin(BeamTeta))
    T2uBeam3(3,iBeam) = locpz * cos(BeamTeta) - locpx * sin(BeamTeta)
    T2uBeam3(4,iBeam) = T1uBeamMoy(4) - locy * sin(BeamPhi) + cos(BeamPhi) * (locx * cos(BeamTeta) + locz * sin(BeamTeta))
    T2uBeam3(5,iBeam) = T1uBeamMoy(5) + locy * cos(BeamPhi) + sin(BeamPhi) * (locx * cos(BeamTeta) + locz * sin(BeamTeta))
    T2uBeam3(6,iBeam) = T1uBeamMoy(6) + locz * cos(BeamTeta) - locx * sin(BeamTeta)
  enddo
    
    
! print initial phase space of the injected Beam Particles
  if(LPrintBeamPartT2u) then
    open(112,file='check_ExternalBeam.out')    
    write(*,*) ' ntBeamPart_init ', ntBeamPartInit,' ntBeamPart ',ntBeamPart
    do iBeam = 1 , ntBeamPart
      locx = T2uBeam3(4,iBeam); locy = T2uBeam3(5,iBeam)
      LocRperp=sqrt(locx**2.0 + locy**2.0)
      locGam = sqrt(1.d0 + T2uBeam3(1,iBeam)**2 + T2uBeam3(2,iBeam)**2 + T2uBeam3(3,iBeam)**2)
      LocUperp = sqrt(T2uBeam3(1,iBeam)**2 + T2uBeam3(2,iBeam)**2)
      write(112,'(I6,100e16.8)') iBeam, (T2uBeam3(jPhaseSpace,iBeam),jPhaseSpace=1,6), &
                              & locRperp, LocRPerp / RadiusGrid, locGam, locUperp            
    enddo
    close(112)
  endif
!  if CircRZ = 0, new T2u variables are : u_rad, l_z, u_z, rad, teta, z
  if(CircRZ == 0) then
    do ibeam = 1 , ntBeamPart
      locx = T2uBeam3(4,iBeam); locy = T2uBeam3(5,iBeam)
      locpx = T2uBeam3(1,iBeam); locpy = T2uBeam3(2,iBeam)
      LocRperp = sqrt(locx**2.0 + locy**2.0)
      if (locRperp > 1.d-1 * dr) then
        LocUperp = (locx * locpx + locy * locpy) / locRPerp
        locLz = locx * locpy - locy * locpx
        locteta = -acos(locx / locRperp)
        if(locy < 0.d0) locteta = -locTeta
      else  !< for small value of r, we assume that u_rad =0, and locRperp = 1.d-1 * r,locTeta = random 
        locRperp = 1.d-1 * dr
        call RANDOM_NUMBER(locTeta)
        locTeta = 2.d0*Pi*(locTeta - 0.5d0)
        locx = locRPerp * cos(locTeta)
        locy = locRPerp * sin(locTeta)
        locUperp = sqrt(locpx**2 + locpy**2)
        locpx = locUperp * cos(locteta + 0.5d0 * Pi)
        locpy = locUPerp * sin(locTeta + 0.5d0 * Pi)
        locLz = locx * locpy - locy * locpx
        locUperp = 0.d0
      endif
      T2ubeam3(1,iBeam) = locUperp; T2ubeam3(2,iBeam) = locLz
      T2uBeam3(4,iBeam) = locRperp; T2uBeam3(5,ibeam) = locTeta
    enddo
  endif
! identification of the Beam Particles
  do ibeam = 1, NtBeamPart
    T1BeamPartIdent(ibeam) = ibeam
  enddo  
end subroutine BeamInjection  
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
! file WakeTrajCopy3to1.f90
! Copy variable values from time3 to time1 and extrapolate time3 values  
! called by TimeEvolution  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 21/03/2017
!> subroutine calculating the evolution of fields and particles during the propagation
subroutine Copy3to1()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
!
! 1. Numeric values
  time1 = time3
  time3 = time3 + DeltaT
  time = time3
  time_cm = time / PlasmaWNum_cm1
  Time_Rayleigh = time_cm / Rayleigh_cm
!
! 2. Plasma
  
  if(withPlasma) then
! copy fields  
    T2PlasmaElecrad3(:,:) = 2.d0*T2PlasmaElecrad3(:,:) - T2PlasmaElecrad1(:,:)  !< extrapolation of 3
    T2PlasmaElecrad1(:,:) = 0.5d0 * (T2PlasmaElecrad3(:,:) + T2PlasmaElecrad1(:,:)) !< calculation of 1
    T2PlasmaElecZ3(:,:) = 2.d0*T2PlasmaElecZ3(:,:) - T2PlasmaElecZ1(:,:)  !< extrapolation of 3
    T2PlasmaElecZ1(:,:) = 0.5d0 * (T2PlasmaElecZ3(:,:) + T2PlasmaElecZ1(:,:)) !< calculation of 1
! calculate the new density
    call DensityProfile
  endif
!   

! 3. Laser
  if(withLaser) then
! copy 3 to 1  
    LaserGam1 = LaserGam3
    LaserGridPosition1 = LaserGridPosition3
    LaserRelatBeta1 = LaserRelatBeta3
  endif
!
! 4. Beam Particles
  if(withBeamParticle) then
    T2uBeam1(:,:) = T2uBeam3(:,:)
  endif
!
! 5. Test Particles
  if(withTestParticle) then
  endif
!
end subroutine Copy3to1
  ! file WakeTrajDensityProfile.f90
! determine the values relative density at the given time
! called by InitialStep  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 16/03/2017
!> subroutine that determine the initial step of the calculation
subroutine DensityProfile()
  use MdConstant
  use MdOutput
  use MdLaser
  use MdPlasma
  use MdNumeric
  implicit none
!*
  integer, save :: Iflag = 0
  integer :: i , j, ii
  integer, save :: LocNPar
  real*8, save, allocatable, dimension(:,:) :: T2LocParamDensProfile !< Spline interpolation
  real*8 :: loca, locb, locc, locdd, loct, locRien
  real*8, save :: locTargetCenter
  real*8, save :: locfond1, locfond2, locfond3
  real*8, save  :: Tlinear1, Tlinear2, TLinear3, Tlinear4
  
!* 
  if(IFlag == 0) then  !< at the first call one needs to initialize the parameters from the file  WakeTrajDensityProfile.dat
    open(10,file='WakeTrajDensityProfile.dat')
    read(10,*) FormeDensityProfile
    select case(FormeDensityProfile)
      case(0) !< uniform target
        locfond1 = 1.d0
      case(1) !< uniform before T1, linear between T1 and T2, uniform between T2 and  T3, linear between T3 and T4, unform above T4
        read(10,*) locfond1, locfond2, locfond3
        read(10,*) Tlinear1, Tlinear2, TLinear3, Tlinear4
      case(2) !< third order spline-like interpolation
        read(10,*) locNPar   !< number of parameter for the Spline
        allocate(T2LocParamDensProfile(1:locNPar,5))
        do i = 1, locNPar
          read(10,*) (T2LocParamDensProfile(i,j), j=1,5)
        enddo
      case default
        write(*,*) 'error in WakeTrajDensityProfile.dat, FormeDensityProfile  = ',FormeDensityProfile
        write(*,*) 'allowed values are 0, 1, 2'
      end select
      close (10)  
      Iflag = 1
    endif
!  
    loct = Time_cm  !< here all lengths are in cm
    select case(FormeDensityProfile)
      case(0) !< uniform
        DensPlasma = locfond1

      case(1)   ! linear profile
 		    if(loct.lt.Tlinear1) DensPlasma = locfond1
		    if(loct.ge.Tlinear1.and.loct.lt.Tlinear2) DensPlasma =    locfond1 + (locfond2-locfond1)* &
                                                          & (loct-Tlinear1)/(Tlinear2-Tlinear1)  
		    if(loct.ge.Tlinear2.and.loct.le.Tlinear3) DensPlasma = locfond2 
		    if(loct.gt.Tlinear3) DensPlasma =  locfond3 + (locfond2-locfond3)*  &
                                      & ((Tlinear4-loct)/(Tlinear4-Tlinear3))
		    if(loct.ge.Tlinear4) DensPlasma = locfond3      

      case(2) ! third order spline-like profile
        ii = 0
        do i = locNpar, 1, -1
          if(loct > T2LocParamDensProfile(i,1)) then
            ii = i
            exit
          endif
        enddo
        if(ii == 0) then! fond initial
          DensPlasma = T2LocParamDensProfile(1,2)
        endif
        if((ii > 0) .and. (ii < locNpar)) then
          locrien = (loct - T2LocParamDensProfile(ii,1)) / (T2LocParamDensProfile(ii+1,1) - T2LocParamDensProfile(ii,1))
          DensPlasma = T2LocParamDensProfile(ii,2) + locrien*(T2LocParamDensProfile(ii,3)+ &
                      & locrien*(T2LocParamDensProfile(ii,4)+ locrien*T2LocParamDensProfile(ii,5)))
        endif
        if(ii == locNpar) then
          DensPlasma = T2LocParamDensProfile(locNpar,2)
        endif 
        case default
          write(*,*) 'error FormeDensityProfile  '
          write(*,*) ' FormeDensityProfile ', FormeDensityProfile
          stop
      end select
end subroutine DensityProfile  
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
! file WakeTrajInitBeam.f90
! determination of the initial parameters values of the injected beam for the WakeTraj program
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 14/03/2017

!> subroutine of initialization of the main parameters
subroutine InitBeam()
  use MdConstant
  use MdNumeric
  use MdBeamParticle
  use MdPlasma
  implicit none
  integer :: iBeam
!
  namelist/ParExternalBeam/ BeamDuration, BeamCharge_nc, BeamChargeTR, BeamChargeDTR, BeamDeltaX, BeamDeltaX_cm, &
                          & BeamDeltaY, BeamDeltaY_cm, BeamDuration, BeamDuration_fs, BeamEpsMultiply, BeamGamMoy,   &
                          & BeamGamWidth, BeamPhi_mrad, BeamPosition, BeamPosition_cm, BeamTeta_mrad, BeamWidthX,    &
                          &  BeamWidthX_cm, BeamWidthY, BeamWidthY_cm,BeamWidthTetaX_mrad, BeamWidthTetaY_mrad, BeamZfocX,  &
                          & BeamZfocY, BeamZfocX_cm, BeamZfocY_cm, MultiplyBeamPart, NcaseBeamInjection, NtBeamPartRead, NumBeam
   
!*  default values
  withBeamLoading = .False.
  NumBeam = 1
  NcaseBeamInjection = 0
  NtBeamPart = 0
  MultiplyBeamPart = 1
  BeamDeltaX = 1.d20
  BeamDeltaX_cm = 0.d0
  BeamDeltaY = 1.d20
  BeamDeltaY_cm = 0.d0
  BeamDuration = 0.d0
  BeamDuration_fs = 0.d0
  BeamEpsMultiply = 5.d-3
  BeamPosition = 1.d20
  BeamCharge_nc = 0.0
  BeamChargeTR = 0.d0
  BeamChargeDTR = 0.d0
  BeamGamMoy = 0.d0
  BeamGamWidth = 0.d0
  BeamPhi_mrad = 0.d0
  BeamTeta_mrad = 0.d0
  BeamWidthX = 1.d20
  BeamWidthX_cm = 0.d0
  BeamWidthY = 1.d20
  BeamWidthY_cm = 0.d0
  BeamWidthTetaX_mrad = 0.d0
  BeamWidthTetaY_mrad = 0.d0
  BeamZfocX = 1.d20
  BeamZfocY = 1.d20
  BeamZfocX_cm = TimeOn_cm
  BeamZfocY_cm = TimeOn_cm

!* reading the namelist, the default value is used when the variable name is not included in the input data
  read(10,ParExternalBeam)
! Check Particle Beam Values
  if(NumBeam /= 1) then
    write(*,*) 'withBeamParticle = True, presently only NumBeam = 1  is allowed'
    stop
  endif
  if (withBeamLoading) then
    if(BeamCharge_nc < 1.d-10) then
      write(*,*) 'withBeamLoading = True, then BeamCharge_nc should be > 0'
      stop
    endif
  endif
  if((NcaseBeamInjection < 0) .or. (NcaseBeamInjection > 1)) then
    write(*,*) ' withBeamParticle = True, one should have -1<NcaseBeamInjection<0'
    stop
  endif

  if(NcaseBeamInjection == 0) then
! specific cas of a Gaussian Beam  
! for a gaussian beam, duration should be specified
    if((BeamDuration < 1.d-10).and.(BeamDuration_fs < 1.d-10)) then
      write(*,*) ' withBeamParticle=True; for a Gaussian Beam, its duration should be specified '
      stop
    endif
! and also the average value of gamma
    if(BeamGamMoy < 1.d-10) then
      write(*,*) ' withBeamParticle=True; for a Gaussian Beam, average value of gamma should be specified '
      stop
    endif
! look if the parameters are given in reduced units or not    
    if(BeamDuration < 1.d-10) then
     BeamDuration = BeamDuration_fs * OmegaP * 1d-15
    else
      BeamDuration_fs = 1.d15 * BeamDuration / OmegaP
    endif
    if(BeamZfocX > 1.d10) then
      BeamZfocX = BeamZfocX_cm * PlasmaWNum_cm1
    else
      BeamZfocX_cm = BeamZfocX / PlasmaWNum_cm1
    endif
    if(BeamZfocY > 1.d10) then
      BeamZfocY = BeamZfocY_cm * PlasmaWNum_cm1
    else
      BeamZfocY_cm = BeamZfocY / PlasmaWNum_cm1
    endif
    if(BeamWidthX > 1.d10) then
      BeamWidthX = BeamWidthX_cm * PlasmaWNum_cm1
    else
      BeamWidthX_cm = BeamWidthX / PlasmaWNum_cm1
    endif
    if(BeamWidthY > 1.d10) then
      BeamWidthY = BeamWidthY_cm * PlasmaWNum_cm1
    else
      BeamWidthY_cm = BeamWidthY / PlasmaWNum_cm1
    endif
     
    
! 
! initial number of beam particles. For a Gaussian Beam = NtBeamPartRead
  NtBeamPartInit = NtBeamPartRead 
  else
! reading of an external data file for the beam
! initial number of particles  is now : 
    NtBeamPartInit = NtBeamPartRead * MultiplyBeamPart
  endif
!
  if(BeamPosition > 1.d10)   BeamPosition = BeamPosition_cm * PlasmaWNum_cm1
  if(BeamDeltaX > 1.d10) then
    BeamDeltaX = BeamDeltaX_cm * PlasmaWNum_cm1
  else
    BeamDeltaX_cm = BeamDeltaX / PlasmaWNum_cm1
  endif
  if(BeamDeltaY > 1.d10) then
    BeamDeltaY = BeamDeltaY_cm * PlasmaWNum_cm1
  else
    BeamDeltaY_cm = BeamDeltaY / PlasmaWNum_cm1
  endif
! initial number of beam particles
  NtBeamPartInit = NtBeamPartRead * MultiplyBeamPart
  
! Allocation et initialization des fichiers
  Allocate(T1BeamPartOutTest(NtBeamPartInit)); T1BeamPartOutTest=0
  Allocate(T1BeamPartIdent(NtBeamPartInit))
  Allocate(T1BeamGam(NtBeamPartInit)); T1BeamGam = 1.d0
  allocate(T1uOneBeamPart(6)); T1uOneBeamPart = 0.d0
  allocate(T1uPOneBeamPart(6)); T1uPOneBeamPart = 0.d0
  allocate(T2uBeam1(6,NtBeamPartInit)); T2uBeam1 = 0.d0
  allocate(T2uBeam3(6,NtBeamPartInit)); T2uBeam3 = 0.d0
  allocate(T1uBeamMoy(6), T1uBeamWidth(6), T1uBeamXYZUxyz(3), T2BeamEmit(2,4))
  T1uBeamMoy = 0.d0; T1uBeamWidth = 0.d0; T1uBeamXYZUxyz = 0.d0; T2BeamEmit = 0.d0
  BeamTeta = 1.d3 * BeamTeta_mrad
  BeamPhi = 1.d3 * BeamPhi_mrad
  
end subroutine InitBeam
! file WakeTrajInitOutput.f90
! print the initial value of the main parameters
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 23/03/2017
!> subroutine printing the initial values of parameters
subroutine InitOutput()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdOutput
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
! Numerical parameters
  write(10,*) ' ***********  Initial Numerical Parameters ***********'
  write(10,'(''      CircRz      '',i4)') CircRz
  write(10,'(''      IDIMR       '',I4)') IDIMR
  write(10,'(''      IDIMZ       '',I4)') IDIMZ
  write(10,'(''        IR1       '',I4)') iR1
  write(10,'(''        IR2       '',I4)') iR2
  write(10,'(''       IRMX       '',I4)') IRMX
  write(10,'(''       IZMX       '',I4)') IZMX
  write(10,'(''        np1       '',I4)') nP1
  write(10,'(''        np2       '',I4)') nP2
  write(10,'(''        np3       '',I4)') nP3
  write(10,'(''MaxNumTimeStep    '',I8)') MaxNumTimeStep
  write(10,'('' ProjOrderR       '',i4)') ProjOrderR
  write(10,'('' ProjOrderZ       '',i4)') ProjOrderZ
  write(10,'(''TypeOfCalculation '',i4)') TypeOfCalculation
  write(10,'(''    DeltaT        '',e12.4)') deltaT
  write(10,'(''    DeltaT_cm     '',e12.4)') deltaT_cm
  write(10,'(''    DeltaT_Rayleigh '',e12.4)') deltaT_Rayleigh
  write(10,'(''         dR        '',e12.4)') dR
  write(10,'(''         dR_cm     '',e12.4)') dR / PlasmaWNum_cm1
  write(10,'(''         dZ        '',e12.4)') dZ
  write(10,'(''         dZ_cm     '',e12.4)') dZ / PlasmaWNum_cm1
  write(10,'(''       EpsCFL      '',e12.4)') EpsCFL
  write(10,'(''       EpsRK       '',e12.4)') EpsRK
  write(10,'(''      RadiusGrid   '',e12.4)') RadiusGrid
  write(10,'(''   RadiusGrid_cm   '',e12.4)') RadiusGrid_cm
  write(10,'(''  RadiusGridCoef   '',e12.4)') RadiusGridCoef
  write(10,'(''       TimeOn      '',e12.4)') TimeOn
  write(10,'(''       TimeOn_cm   '',e12.4)') TimeOn_cm
  write(10,'('' TimeOn_Rayleigh   '',e12.4)') TimeOn_Rayleigh
  write(10,'(''       TimeEnd     '',e12.4)') TimeEnd
  write(10,'(''       TimeEnd_cm  '',e12.4)') TimeEnd_cm
  write(10,'(''  TimeEnd_Rayleigh '',e12.4)') TimeEnd_Rayleigh
  write(10,'(''   ZLengthGrid     '',e12.4)') ZLengthGrid
  write(10,'(''   ZLengthGrid_cm  '',e12.4)') ZLengthGrid_cm
  write(10,'(''     withLaser     '',A)') withLaser
  write(10,'('' withBeamParticle  '',A)') withBeamParticle
  write(10,'('' withPlasma        '',A)') withPlasma
  write(10,'('' withTestParticle  '',A)') withTestParticle
!
! Laser Parameter
  if(withLaser) then
    write(10,*) ' *************Initial Laser Parameters ************** '
    write(10,'(''    LaserAmax             '',e12.4)') LaserAmax
    write(10,'('' CriticalDensity_cm3 '',e12.4)') CriticalDensity_cm3
    write(10,'('' EpsLaserDiffrac    '',e12.4)') EpsLaserDiffrac
    write(10,'('' EpsRayleigh        '',e12.4)') EpsRayleigh
    write(10,'('' LaserDuration      '',e12.4)') LaserDuration
    write(10,'('' LaserDUration_fs   '',e12.4)') LaserDuration_fs
    write(10,'('' LaserEnergy_J      '',e12.4)') LaserEnergy_J
    write(10,'(''LaserGridPosition    '',e12.4)') LaserGridPositionInit
    write(10,'(''LaserGridPosition_duration '',e12.4)') LaserGridPosition_duration
    write(10,'(''LaserGridPosition_cm  '',e12.4)') LaserGridPositionInit / PlasmaWNum_cm1
    write(10,'(''LaserMaxIntensity_Wcm2 '',e12.4)') LaserMaxIntensity_Wcm2
    write(10,'(''LaserWLength_cm        '',e12.4)') LaserWLength_cm
    write(10,'(''LaserWNum_cm1          '',e12.4)') LaserWNum_cm1
    write(10,'(''  OmegaL               '',e12.4)') OmegaL
    write(10,'(''  Rayleigh_cm           '',e12.4)') Rayleigh_cm
    write(10,'(''  LaserWaist0           '',e12.4)') LaserWaist0
    write(10,'(''  LaserWaist0_cm        '',e12.4)') LaserWaist0_cm
    write(10,'(''  FullGaussianLaser     '',A)') FullGaussianLaser
    write(10,'('' TimeGaussianLaser      '',A)') TimeGaussianLaser
    write(10,'('' SpaceGaussianLaser     '',A)') SpaceGaussianLaser
  endif
!
!  Beam Particles Parameters
  if(withBeamParticle) Then
    write(10,'('' NumBeam                '',I4)') NumBeam
    write(10,'('' NcaseBeamInjection     '',I4)') NcaseBeamInjection
    write(10,'('' NtBeamPart             '',i4)') NtBeamPart
    write(10,'('' NtBeamPartRead         '',i4)') NtBeamPartRead
    write(10,'('' MultiplyBeamPart       '',i4)') MultiplyBeamPart
    write(10,'('' BeamPhi                '',e12.4)') BeamPhi
    write(10,'('' BeamTeta               '',e12.4)') BeamTeta
    write(10,'(''  BeamDuration          '',e12.4)') BeamDuration
    write(10,'('' BeamDuration_fs       '',e12.4)') BeamDuration_fs
    write(10,'('' BeamGamMoy            '',e12.4)') BeamGamMoy
    write(10,'('' BeamGamWidth          '',e12.4)') BeamGamWidth
    write(10,'('' BeamPosition          '',e12.4)') BeamPosition
    write(10,'('' BeamPosition_cm       '',e12.4)') BeamPosition_cm
    write(10,'('' BeamCharge_nc         '',e12.4)') BeamCharge_nc
    write(10,'('' BeamChargeTR          '',e12.4)') BeamChargeTR
    write(10,'('' BeamChargeDTR         '',e12.4)') BeamChargeDTR
    write(10,'('' BeamDenWt0            '',e12.4)') BeamDenWt0
    write(10,'('' withBeamLoading       '',A)') withBeamLoading
  endif
! Plasma Parameters
  if(withPlasma) then
    write(10,'('' FormeDensityProfile   '',i4)') FormedensityProfile
    write(10,'('' Dens0Plasma_cm3       '',e12.4)') Dens0Plasma_cm3
    write(10,'('' EpsPlasmaLaser        '',e12.4)') EpsPlasmaLaser
    write(10,'('' OmegaP                '',e12.4)') OmegaP
    write(10,'('' PlasmaWLength_cm      '',e12.4)') PlasmaWLength_cm
    write(10,'('' PlasmaWNum_cm1        '',e12.4)') PlasmaWNum_cm1
  endif
! Plasma TestParticle
  if(withTestParticle) then
    write(10,'('' MaxNumTP              '',I4)') MaxNumTP
    write(10,'('' TStartInjecTP         '',e12.4)') TStartInjecTP
    write(10,'('' TStartInjecTP_cm      '',e12.4)') TStartInjecTP_cm
    write(10,'('' TStopInjecTP          '',e12.4)') TStopInjecTP
    write(10,'('' TStopInjecTP_cm       '',e12.4)') TStopInjecTP_cm
  endif
! Output Parameters
  write(10,'(''  DeltaSort             '',e12.4)') DeltaTSort
  write(10,'(''  DeltaSort_cm          '',e12.4)') DeltaTSort_cm
  write(10,'('' BeamNumPrint           '',e12.4)') BeamNumPrint
!  
end subroutine InitOutput  
! file WakeTrajInitalStep.f90
! Step II: determine the values of all involved variables at the initial step
! called by WakeTrajMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 16/03/2017
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
! determination of DeltaT;  here only from the Rayleigh length
      DeltaT = EpsRayleigh * Rayleigh_cm * PlasmaWNum_cm1 
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
! file WakeTrajInitLaser.f90
! determination of the initial parameters values of the laser for the WakeTraj program
! called by InitMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 14/03/2017

!> subroutine of initialization of the main parameters
subroutine InitLaser()
  use MdConstant
  use MdLaser
  use MdNumeric
  implicit none
!  
  namelist/ParLaser/ LaserAmax, EpsLaserDiffrac, EpsRayleigh, LaserEnergy_J, LaserMaxIntensity_Wcm2,  LaserDuration, &
                      & LaserDuration_fs, LaserGridPositionInit, LaserGridPosition_duration, TimeGaussianLaser, &
                      &  SpaceGaussianLaser, LaserWaist0, LaserWaist0_cm
!
! default values
  LaserAmax = 0.d0
  EpsLaserDiffrac = 0.5d0
  EpsRayleigh = 1.d-2
  LaserDuration = 0.d0
  LaserDuration_fs = 0.d0
  LaserWLength_cm = 0.8d-4
  LaserGridPositionInit = 1.d20
  LaserGridPosition_duration = 0.d0
  LaserEnergy_J = 0.d0
  LaserMaxIntensity_Wcm2 = 0.d0
  LaserWaist0 = 0.d0
  LaserWaist0_cm = 0.d0
  FullGaussianLaser = .True.
  TimeGaussianLaser = .true.
  SpaceGaussianLaser = .True.
  !
  !* reading the namelist, the default value is used when the variable name is not include in the input data
  read(10,ParLaser)
!* check Laser values  
  if((TimeGaussianLaser).and.(SpaceGaussianLaser)) FullGaussianLaser = .True.
  if((LaserWaist0 < 1.d-10).and. (LaserWaist0_cm < 1.d-10)) then
    write(*,*) ' withlaser = true, then either LaserWaist0 or LaserWaist0_cm should be > 0 '
    stop
  endif
  if((.not. FullGaussianLaser) .and. (LaserEnergy_J < 1.d-10)) then
    write(*,*) ' FullGaussianLaser = false, then LaserEnergy_J should be > 0 '
    stop
  endif
  if(TimeGaussianLaser) then
    if((LaserDuration < 1.d-10) .and. (LaserDuration_fs < 1.d-10)) then
      write(*,*) 'FullGaussianLaser is true, then either LaserDuration or LaserDuration_cm should be > 0'
      stop
    endif
  endif
  if((LaserGridPositionInit > 1.d10) .and. (LaserGridPosition_duration < 1.d-10)) then
    write(*,*) ' withlaser=.true., either LaserGridPosition or LaserGridPosition_duration shoud be specified'
    stop
  endif 
  
end subroutine InitLaser  
 ! file WakeTrajInitMain.f90
! Step I. determination of the initial parameters values for the WakeTraj program
! called by WakeTrajMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 29/03/2017
!> subroutine of initialization of the main parameters
subroutine InitMain()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdOutput
  use MdBeamParticle
  use MdPlasma
  use MdTestParticle
  implicit none
! local variables  
  integer :: i, j , k, ir, iz
  real*8 :: loctime
! define the namelist used as general input parameters
  namelist/ParNumeric/ CircRz, DeltaT, DeltaT_cm, DeltaT_Rayleigh, EpsCFL, EpsRK, IDIMR, IDIMZ, iR1, iR2, np1,np2,np3,  &
                    & MaxNumTimeStep, ProjOrderR, ProjOrderZ, RadiusGridCoef, TypeOfCalculation, RadiusGridCoef, &
                    & TimeOn_Rayleigh, TimeOn_cm, TimeEnd_Rayleigh, TimeEnd_cm, withLaser, withBeamParticle, withPlasma, &
                    & withTestParticle, ZLengthGrid, ZLengthGrid_cm
  namelist/ParOutput/ BeamNumPrint, DeltaTSort, DeltaTSOrt_cm, LPrintPlasmaField, LPrintLaserField, LPrintBeamPartField, &
                    & LPrintBeamPartT2u, LPrintTestPartField, LPrintTotalField  
!
!*

!  open data input and output file, as a general rule files are open and closed in the same subroutine, 10 is for input and 11 for output
  OPEN (10,file='WakeTraj_input.par')
!********* 1 reading of general parameters  
!***** 1.1 Physical and Mathematical Constants
  Pi = acos(-1.d0)
  VacMu0 = 4.d-7 * Pi
  VacEps0 = 1.d4 / (VacMu0 * c_cm_s**2)
!
!***** 1.2 Numerical parameters
!* default values
  CircRz = 0
  DeltaT = 1.d20
  DeltaT_cm = 1.d20
  DeltaT_Rayleigh = 1.d20
  IDIMR = 500
  IDIMZ = 500
  IRMX = 0
  IZMX = 0
  IR1 = IDIMR / 3.d0
  IR2 = IDIMR * 2.d0 / 3.d0
  MaxNumTimestep = 1000000
  np1 = 10
  np2 = 5
  np3 = 2
  ProjOrderR = 2
  ProjOrderZ = 2
  RadiusGridCoef = 5.d0
  TypeOfCalculation = 1
  TimeOn_Rayleigh = 1.d20
  TimeOn_cm = 1.d20
  TimeEnd_Rayleigh = 1.d20
  TimeEnd_cm = 1.d20
  ZLengthGrid = 0.d0
  ZlengthGrid_cm = 0.d0
  withLaser = .false.
  withPlasma = .false.
  withBeamParticle = .False.
  withTestParticle = .False.

! reading the namelist, the default value is used when the variable name is not included in the namelist 
  read(10,ParNumeric)
! check Numeric
  if ((abs(TimeOn_Rayleigh)> 1.d10) .and. (abs(TimeOn_cm) > 1.d10) ) Then
    write(*,*) ' error in TimeOn input values; one of TimeOn_Rayleigh, TimeOn_cm values should be specified'
    stop
  endif
  if ((abs(TimeEnd_Rayleigh) > 1.d10) .and. (abs(TimeEnd_cm) > 1.d10) ) then
    write(*,*) ' error in Trmax input values; one of the TimeEnd_Rayleigh, TimeEnd_cm values should be specified'
    stop
  endif
  if (TypeOfCalculation /= 1) then
    write(*,*) 'Presently only the value TypeCalculation = 1 is allowed '
    stop
  endif
  if (RadiusGridCoef < 1.d-10) then
    write(*,*) 'RadiusGridCoef should be > 0'
    stop
  endif
  if((ZLengthGrid < 1.d-10) .and.(ZLengthGrid_cm < 1.d-10)) then
    write(*,*) 'error in Numeric input; one of ZlengthGrid or ZlengthGrid_cm should be > 0)'
    stop
  endif
  IZMX = max(IZMX, IDIMZ + 2)
  IRMX = max(IRMX, IDIMR + 2)
!
!*
!**** 1.3 Laser and Plasma parameters
 if(withLaser) then
      call InitLaser
  else
! if there is no laser, we fix wave length and the waist so that the Rayleigh length will be equal to one cm
    LaserWLength_cm = 0.8d-4; LaserWaist0_cm = sqrt(LaserWLength_cm / Pi)
  endif
!
    OmegaL = 2.d0 * Pi * c_cm_s / LaserWLength_cm
    CriticalDensity_cm3 = 1.11485e13 / LaserWLength_cm**2
    Rayleigh_cm = Pi * LaserWaist0_cm**2 / LaserWLength_cm

  if(withPlasma) call InitPlasma
       

!*  Calculate laser-plasma parameters
  if(withPlasma) then
    OmegaP = sqrt(1.d-12 * Dens0Plasma_cm3 * ChargeElec_nc**2 / (Masse_Elec_kg * VacEps0))
    PlasmaWNum_cm1 = OmegaP / c_cm_s
    PlasmaWlength_cm = 2.d0 * Pi / PlasmaWNum_cm1
  else
! without plasma, we fix the density so that PlasmaWNum_cm1 = 1 cm^-1
    PlasmaWNum_cm1 = 1.d0
    OmegaP = PlasmaWNum_cm1 * c_cm_s
    Dens0Plasma_cm3 = 1.d12 * Masse_Elec_kg * VacEps0 * OmegaP**2 / ChargeElec_nc**2
  endif
!
  EpsPlasmaLaser = OmegaP / OmegaL
  if(laserDuration < 1.d-10) then
    laserDuration = LaserDuration_fs * 1.d-15 * OmegaP
  else
    LaserDuration_fs = laserDuration / OmegaP
  endif
!  
  if(laserGridPositionInit > 1.d10) then
    laserGridPositionInit = LaserGridPosition_duration * laserDuration
  else
    LaserGridPosition_duration = laserGridPositionInit / laserDuration
  endif
!
!*
!***  1.4 Particle Beams
  if(withBeamParticle) call InitBeam()
!*
!***  1.5 Test Particles
  if(withTestParticle) call InitTestPart()
!*
!***  1.6 Control Output
!* Default values
  DeltaTSort =1.d20
  DeltaTSort_cm = 1.d20
  LPrintPlasmaField = .False.
  LPrintLaserField = .False.
  LPrintBeamPartField = .False.
  LPrintBeamPartT2u = .False.
  LPrintTestPartField = .False.
  LPrintTotalField = .False.
  BeamNumPrint = NtBeamPartInit
!*** read the namelist
!*** Check  
  read(10,ParOutput)
  if((DeltaTSort > 1.d10).and.(DeltaTSort_cm> 1.d20)) then
    write(*,*) 'one of DeltaTSort, DeltaTSort_cm should be specified'
    stop
  endif
 
! basic input is finished
    close(10)
!*   
!********* 2 initialization of general parameters
   
  if(LaserWaist0 < 1.d-10) then !< LaserWaist0 is needed to determine the grid radius
    LaserWaist0 = LaserWaist0_cm * PlasmaWNum_cm1
  else
    LaserWaist0_cm = LaserWaist0 / PlasmaWNum_cm1
  endif
  if(TypeOfCalculation == 1) then !< determination of LaserAmax  
    if((LaserAmax < 1.d-10).and.(LaserMaxIntensity_Wcm2 < 1.d-10)) then !< LaserAmax and Int Max are determined by the energy
      LaserMaxIntensity_Wcm2 = 1.d15 * LaserEnergy_J / ((0.5*Pi)**1.5 * LaserWaist0_cm**2 * LaserDuration_fs)
      LaserAmax = 0.855d-5 * LaserWLength_cm * sqrt(LaserMaxIntensity_Wcm2)
    else
      if(LaserAmax < 1.d-10) then
        LaserAmax = 0.855d-5 * LaserWLength_cm * sqrt(LaserMaxIntensity_Wcm2)
      else
        LaserMaxIntensity_Wcm2 = (LaserAmax / (0.855d-5 * LaserWLength_cm))**2
      endif
      LaserEnergy_J = 1.d-15 * LaserMaxIntensity_Wcm2 * ((0.5*Pi)**1.5 * LaserWaist0_cm**2 * LaserDuration_fs)
    endif
  endif
      
  
  RadiusGrid = LaserWaist0 * RadiusGridCoef
  RadiusGrid_cm = RadiusGrid /  PlasmaWNum_cm1
  dR = RadiusGrid / IDIMR
  UnSdR = 1.d0 / dR
  dRS2 = 0.5d0 * DR
  dR2 = dR * dR
  UnSdR2 = 1.d0 / dR2
  if(ZLengthGrid < 1.d-10) then
    ZLengthGrid = ZLengthGrid_cm * PlasmaWNum_cm1
  else
    ZLengthGrid_cm = ZLengthGrid / PlasmaWNum_cm1
  endif
  dZ = ZLengthGrid / IDIMZ
  dZs2 = 0.5d0 * dZ
  dZ2 = dZ*dZ
  allocate(T1GridRadi(-1:IRMX))
  allocate(T1GridRadj(-1:IRMX))
  do ir = -1, IRMX
    T1GridRadi(ir) = ir * dR
    T1GridRadj(ir) = (ir + 0.5d0) * dR
  enddo
  allocate(T1Gridzi(-1:IZMX))
  allocate(T1Gridzj(-1:IZMX))
  do iz = -1 , IZMX
    T1Gridzi(iz) = iz * dZ
    T1Gridzj(iz) = (iz + 0.5d0) * dZ
  enddo
!**
  if(TimeOn_Rayleigh > 1.d10) then
    TimeOn_Rayleigh = Timeon_cm / Rayleigh_cm
  else
    Timeon_cm = TimeOn_Rayleigh * Rayleigh_cm
  endif
  TimeOn = TimeOn_cm * PlasmaWNum_cm1
!*  
  if(TimeEnd_Rayleigh > 1.d10) then
    TimeEnd_Rayleigh = TimeEnd_cm / Rayleigh_cm
  else
    TimeEnd_cm = TimeEnd_Rayleigh * Rayleigh_cm
  endif
  TimeEnd = TimeEnd_cm * PlasmaWNum_cm1
  
!**
  if(DeltaTSort > 1.d10 ) then
    DeltaTSort = DeltaTSort_cm * PlasmaWNum_cm1
  else
    DeltaTSort_cm = DeltaTSort / PlasmaWNum_cm1
  endif
!*
  if(DeltaT > 1.d10) then
    if(DeltaT_cm > 1.d10) then
      if(DeltaT_Rayleigh < 1.d10)  DeltaT = DeltaT_Rayleigh * Rayleigh_cm * PlasmaWNum_cm1
    else
      DeltaT = DeltaT_cm * PlasmaWNum_cm1
    endif
  endif
  DeltaT = min(DeltaT, DeltaTSort)
  DeltaT_cm = DeltaT / PlasmaWNum_cm1
  DeltaT_Rayleigh = DeltaT_cm / Rayleigh_cm
!
    
        
!********** 3 Output the values of general parameters
! initialization of the computing time
  call CPU_TIME(loctime)
  CpuTime_mn = loctime / 60.d0
! outputs  
  open (10,file='WakeTRaj_output.out')
  write(10,*) '********   WakeTraj Calculation **********'
  call GETDAT(i,j,k)
  write(10,'(''year 20'',I2,'' month '',I2,'' day '',I2)') i,j,k
  call InitOutput()
  close (10) 
  
end subroutine InitMain
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
end subroutine InitTestPart! file WakeTrajLinearGaussianField.f90
! Calculate through linear response theory the plasma_wave field generated by a Gaussian beam
! called by InitialStep, TimeEvolution  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 16/03/2017
!> subroutine calculation the linear plasma field generated by a Gaussian laser at time 3
subroutine LinearGaussianField()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdPlasma
  implicit none
!
  integer, save :: IFlag = 0
  integer :: ir,iz
  real*8 ::  locdL, locX, loceta, locpsi0, locKp, locRad, locRad2, locdistz, locEz, locErad        !< see Equ. (1.15) of the documentation
!*
! Some initialization
  LaserGam3 = sqrt(CriticalDensity_cm3 / (DensPlasma* Dens0Plasma_cm3))
  if(LaserGam3 < 1.d3) then
    LaserRelatBeta3 = sqrt(1.d0 - 1.d0 / LaserGam3**2)
  else
    LaserRelatBeta3 = 1.d0 - (0.5d0 / LaserGam3**2) * (1.d0-0.25d0 / LaserGam3**2)
  endif
  if (Iflag == 0) then
    LaserGam1 = LaserGam3
    LaserGridPosition3 = LaserGridPositionInit
    Iflag = 1
  else
    LaserGridPosition3 = LaserGridPosition1 + (1.d0-0.5d0*(LaserRelatBeta3 + LaserRelatBeta1)) * &
                        & DeltaT
  endif

! plasma quantities depends on the reduced density
  
  locKp = sqrt(DensPlasma)
  locX = 0.5d0 * locKp * LaserDuration
  loceta = sqrt(2.d0 * Pi) * locX * exp(-0.5d0 * locX**2)
  AmpLaser = LaserAmax  / sqrt(1.d0 + time_Rayleigh**2)
  locPsi0 = 0.25d0 * locEta * AmpLaser **2
! longitudinal field is on the radial grid but halfway axial  
  do ir = 0 , IDIMR
    locRad = T1GridRadi(ir) / LaserWaist
    locRad2 = locRad**2
    locEz = locPsi0 * exp(-2.0d0 * locRad2)     
    do iz = 0 , IDIMZ   
      locdistz =T1Gridzi(iz) + dZs2 - LaserGridPosition3
      if(locdistz > 0.d0) then
        T2PlasmaElecZ3(iz,ir) = locEz * cos(locdistz)
      else
        T2PlasmaElecZ3(iz,ir) = 0.d0
      endif
    enddo
  enddo
! Ez is symetric in R
  T2PlasmaElecZ3(:,-1) = T2PlasmaElecZ3(:,0)
! radial field is on the axial grid and halfway of the radial one
  do ir = 0 , IDIMR
    locRad = (T1GridRadi(ir)+dRs2) / LaserWaist
    locRad2 = locRad**2
    locERad = locPsi0 * exp(-2.0d0 * locRad2) * 4.d0 * locRad / LaserWaist     
    do iz = 0 , IDIMZ   
      locdistz =T1Gridzi(iz)  - LaserGridPosition3
      if  (locdistz > 0.d0) then
        T2PlasmaElecRad3(iz,ir) = locERad * sin(locdistz)
      else
        T2PlasmaElecRad3(iz,ir) = 0.d0
      endif
    enddo
  enddo
! Er is anti-symetric in R 
  T2PlasmaElecRad3(:,-1) = -T2PlasmaElecRad3(:,0)
end subroutine LinearGaussianField  ! file WakeTrajRK.f90
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
! file WakeTrajTimeEvolution.f90
! determination of the evolution of fileds and particles during time (time is in fact identified by the position of the moving window) 
! called by WakeTrajMain  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 20/03/2017
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
    case default
      write(*,*) ' error, presently only TypeOfCalculation=1 is allowed '
      stop
  end select

! 4. Calculate the evolution of the Beam Particles properties from time1 to time3
    if(withBeamParticle) call BeamPartMover()  !< move the particles
    if(withBeamLoading) call BeamPartLoading  !< calculate the density of charge and of current
    
    
  enddo !< end of main loop
  
end subroutine TimeEvolution  
! file WakeTrajUtilities.f90
! this file contains utilities subroutines and functions for the WakeTraj program
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 20/03/2017
!*
!> calculate a random number following normal distribution (gaussian)
  SUBROUTINE GaussAlea(random,init)
	IMPLICIT NONE
  integer, intent(in) :: init !< init is used for initialization. init=0 will start a new distribution
 	REAL*8, INTENT(OUT) :: random
	REAL*8 :: locrsq, locv1, locv2
	REAL*8, SAVE :: locg
	LOGICAL, SAVE :: gaus_stored=.false.
  if( init == 0) then
    gaus_stored = .false.
    return
  endif
	if (gaus_stored) then
		random = locg
		gaus_stored=.false.
	else
		do
			call RANDOM_NUMBER(locv1)             
			call RANDOM_NUMBER(locv2)               
			locv1 = 2.d0*locv1 - 1.d0                
			locv2 = 2.d0 * locv2 - 1.d0
			locrsq = locv1**2 + locv2**2
			if (locrsq > 0.d0 .and. locrsq < 1.0) exit
		end do
		locrsq = sqrt(-2.d0*log(locrsq)/locrsq)
		random = locv1 * locrsq
		locg = locv2 * locrsq
		gaus_stored = .true.
	end if
  END SUBROUTINE GaussAlea
!
!*
!>  Subroutine to print Plasma fields
subroutine  PrintPlasmaField()
  use MdConstant
  use MdNumeric
  use MdOutput
  use MdPlasma
  implicit none
  integer :: iz, ir
  character*15 :: LocNameFich
!
!* Longitudinal field
  write(LocNameFich,'(''PlElecZ'',I4.4,''.out'')') NumPrint
  open(111,file=LocNameFich)
  do ir = 0,IDIMR
    write(111,'(5000e16.8)') (T2PlasmaElecZ3(iz,ir),iz=0,IDIMZ)
  enddo
  close (111)
!*  Transverse Field
  write(LocNameFich,'(''PlElecR'',I4.4,''.out'')') NumPrint
  open(111,file=LocNameFich)
  do ir = 0,IDIMR
    write(111,'(5000e16.8)') (T2PlasmaElecRad3(iz,ir),iz=0,IDIMZ)
  enddo
  close (111)
!
end subroutine PrintPlasmaField
!
!*
!>  Subroutine to print the laser field
subroutine  PrintLaserField()
  use MdConstant
  use MdLaser
  use MdNumeric
  use MdOutput
  implicit none
!
end subroutine PrintLaserField
!
!*
!> subroutine to print the Beam Particles Fields
subroutine PrintBeamPartField
  use MdConstant
  use MdBeamParticle
  use MdNumeric
  use MdOutput
  implicit none
!
end subroutine PrintBeamPartField
!
!*
!> subroutine to print phase space properties of the Beam Particles
subroutine PrintBeamPartT2u
  use MdConstant
  use MdBeamParticle
  use MdNumeric
  use MdPlasma
  use MdOutput
  implicit none
  character*15 :: LocNameFich
  integer :: ibeam, iz, iprintB, iprintE, iphase
  real*8 :: locRan, locRPerp, locUPerp

!
!* 
  write(LocNameFich,'(''BeamGam'',I4.4,''.out'')') NumPrint
  open(111,file=LocNameFich)
  if(CircRz == 0) then
    write(111,'('' Num Gam'',i4.4,'' pr  lz  pz rad teta z p_pert zel elec'',i4.4)') NumPrint, NumPrint
!
    iprintE = -1; iprintB = NtBeamPart/BeamNumPrint; iprintB = max(1,iprintB)
    do ibeam = 1 , NtBeamPart, iprintB
      if(T1BeamPartOutTest(ibeam)==1) cycle
      iprintE = iprintE + 1
      locUPerp = 1e3 * T2uBeam3(1,ibeam) / T2uBeam3(3,ibeam)
      if(iprintE <= idimz) then
        write(111,'(i6,50e16.8)') T1BeamPartIdent(ibeam), T1BeamGam(ibeam), (T2uBeam3(iphase,ibeam),iphase=1,6),  locUPerp, &
                          & iprintE*dz, T2PlasmaElecZ3(iprintE,0)
      else
        write(111,'(i6,50e16.8)') T1BeamPartIdent(ibeam), T1BeamGam(ibeam), (T2uBeam3(iphase,ibeam),iphase=1,6),  locUPerp
      endif
    enddo
  endif
  close (111)
!
end subroutine PrintBeamPartT2u
!
!*
!> subroutine to print Test Particles Field
subroutine PrintTestPartField
  use MdConstant
  use MdNumeric
  use MdPlasma
  use MdOutput
  use MdTestParticle
  implicit none
  character*15 :: LocNameFich
!
end subroutine PrintTestPartField
!
!*
!>  subroutine to print total field
subroutine PrintTotalField
  use MdConstant
  use MdNumeric
  use MdPlasma
  use MdOutput
  implicit none
  character*15 :: LocNameFich
!
end subroutine PrintTotalField



