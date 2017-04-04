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
