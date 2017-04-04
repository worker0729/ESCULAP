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
  