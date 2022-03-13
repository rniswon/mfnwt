module openspec
  implicit none
  integer :: i
!  Code in this file defines values for OPEN-statement specifiers.  Some
!  of the values are extensions to ANSI Fortran 90 and 95.  One of the
!  specifiers is not included in ANSI FORTRAN 77. The included
!  specifiers are ACCESS, FORM and ACTION.
!
!
!
! Specifiers for OPEN statements for unformatted files, which are
! sometimes compiler specific.
! The included specifiers are ACCESS and FORM.
#ifdef __INTEL_COMPILER
# ifdef _WIN32
! Non-standard Fortran that causes code compiled by Intel 
! compilers (ifort) on Windows.  
  CHARACTER(len=20) :: ACCESS = 'SEQUENTIAL'
  CHARACTER(len=20) :: FORM = 'BINARY'
# else
! Standard Fortran on intel compilers on non-Windows OSs
  CHARACTER(len=20) :: ACCESS = 'STREAM'
  CHARACTER(len=20) :: FORM = 'UNFORMATTED'
# endif
#else
! Standard Fortran on non-intel compilers
  CHARACTER(len=20) :: ACCESS = 'STREAM'
  CHARACTER(len=20) :: FORM = 'UNFORMATTED'
#endif
  CHARACTER(len=20), DIMENSION(2) :: ACTION
  DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
  
  private
  public :: ACCESS, FORM, ACTION
end module openspec
  
