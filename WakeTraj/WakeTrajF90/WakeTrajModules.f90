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
