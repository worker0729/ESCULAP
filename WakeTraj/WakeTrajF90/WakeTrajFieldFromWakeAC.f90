! file WakeTrajFieldFromWakeAC.f90
! Calculate  the plasma_wave field from the potential filed as calculated by WakeAC
! called by InitialStep, TimeEvolution  
! see documentaion in WakeTrajDoc
! description of main variables is given in WakeTrajModule.90
! author Gilles Maynard CNRS/LPGP/ITFIP
! version 13/04/2017
!> subroutine calculation of the plasma field  at time 3 as 
subroutine FieldFromWakeAC()
  use MdNumeric
  use MdPlasma
  implicit none
!
  character*21 :: locNameFich
  integer :: ir, iz
  if(iTimeStep < MaxNumTimestepWakeAC) then
! reading T2Psi tables
    write(locNameFich,'(''.\WakeAC\PsiPo'',i3.3,''.bin'')') iTimeStep +1
    write(*,*) ' T2PsiFich ', locNameFich
    open(111,file=locNameFich,form='unformatted')
    read(111) T2PsiAC
    do ir = -1, IRMX
      do iz = -1, IZMX
        T2PlasmaPot3(iz,ir) = T2PsiAC(ir,iz)
      enddo
    enddo
    T2PlasmaPot3(IDIMZ+1,:) = 2.d0*T2PlasmaPot3(IDIMZ,:) - T2PlasmaPot3(IDIMZ-1,:)
    T2PlasmaPot3(:,IDIMR+1) = 2.d0*T2PlasmaPot3(:,IDIMR) - T2PlasmaPot3(:,IDIMR-1)
! longitudinal field is on the radial grid but halfway axial
    do ir = 0 , IDIMR
      do iz = 0, IDIMZ
        T2PlasmaElecZ3(iz,ir) =   (T2PlasmaPot3(iz+1,ir) - T2PlasmaPot3(iz,ir)) / DZ
      enddo
    enddo
! Ez is symetric in R
    T2PlasmaElecZ3(:,-1) = T2PlasmaElecZ3(:,0)
! radial field is on the axial grid and halfway of the radial one
    do ir = 0 , IDIMR
      do iz = 0, IDIMZ
        T2PlasmaElecRad3(iz,ir) = -(T2PlasmaPot3(iz,ir+1) - T2PlasmaPot3(iz,ir)) / DR
      enddo
    enddo
! Er is anti-symetric in R 
    T2PlasmaElecRad3(:,-1) = -T2PlasmaElecRad3(:,0)
  endif
! for iTimeStep+1 > MaxNumTimestepWakeAC, we use the interpolation made in Copy3to1 subroutine 
! 
end subroutine   FieldFromWakeAC

  
  
