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
       
        
        