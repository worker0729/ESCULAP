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



