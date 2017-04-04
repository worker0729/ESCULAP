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
  