      MODULE GWFAGOMODULE
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::SFRSEG
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::UZFROW
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::UZFCOL
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::WELLIRR
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::IRRFACT
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::IRRPCT
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::SUPWEL
        REAL,             SAVE, DIMENSION(:),   POINTER     ::SUPFLOW
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::IRRWEL
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::PCTSUP
        INTEGER,          SAVE,                 POINTER     ::MAXVAL
        INTEGER,          SAVE,                 POINTER     ::NUMSUP
        INTEGER,          SAVE,                 POINTER     ::UNITSUP
        INTEGER,          SAVE,                 POINTER     ::NUMIRRWEL
        INTEGER,          SAVE,                 POINTER     ::UNITIRRWEL
        INTEGER,          SAVE,                 POINTER     ::MAXSEGS
        INTEGER,          SAVE,                 POINTER     ::MAXCELLS
        INTEGER,          SAVE,               POINTER     ::NUMIRRWELSP
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::NUMCELLS
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::NUMSEGS
        INTEGER,          SAVE,              POINTER   ::ETDEMANDFLAG
! from SFR
        INTEGER, SAVE, POINTER :: NUMIRRSFR,UNITIRRSFR
        INTEGER, SAVE, POINTER :: MAXCELLSSFR,NUMIRRSFRSP
        INTEGER,SAVE,                 POINTER:: IDVFLG   !diverison recharge is active flag
        INTEGER,SAVE,  DIMENSION(:),  POINTER:: DVRCH   !(number of irrigation cells per segment)
        INTEGER,SAVE,  DIMENSION(:),  POINTER:: IRRSEG ! SEGMENT NUMBER BY NUMBER OF IRRIGATION SEGMENTS
        INTEGER,SAVE,  DIMENSION(:,:),POINTER:: IRRROW !(store cells to apply diverted recharge)
        INTEGER,SAVE,  DIMENSION(:,:),POINTER:: IRRCOL
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: SFRIRR  !(store original recharge values)
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: DVRPERC  !(Percentage of diversion applied to each cell)
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: DVEFF  !(store efficiency factor)
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: KCROP  !(crop coefficient)
        REAL,   SAVE,  DIMENSION(:),  POINTER:: DEMAND,SUPACT
        REAL,   SAVE,  DIMENSION(:),  POINTER:: ACTUAL
      END MODULE GWFAGOMODULE


      SUBROUTINE GWF2AGO7AR(IN)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR AGO PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW
      USE GWFAGOMODULE
      USE GWFSFRMODULE, ONLY:NSEGDIM
      USE GWFWELMODULE, ONLY:MXWELL
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS
      integer,intent(inout) :: IN
C     ------------------------------------------------------------------
C        VARIABLES
!      CHARACTER(len=16) :: text        = ' AGO PACKAGE '
      INTEGER :: MXACTWSUP,MXACTWIRR,NUMSUPHOLD,NUMIRRHOLD
      INTEGER :: MAXSEGSHOLD,NUMCOLS,NUMROWS,MAXCELLSHOLD,NUMCELLSHOLD
C     ------------------------------------------------------------------
      ALLOCATE(MAXVAL,NUMSUP,NUMIRRWEL,UNITSUP,UNITIRRWEL)
      ALLOCATE(MAXSEGS,MAXCELLS,NUMIRRWELSP)
      MAXVAL = 1
      NUMSUP = 0
      NUMIRRWEL = 0
      UNITSUP = 0
      UNITIRRWEL = 0
      MAXSEGS = 0
      MAXCELLS = 0
      NUMIRRWELSP = 0
C
C1------IDENTIFY PACKAGE AND INITIALIZE AG OPTIONS.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'AGO -- AGO PACKAGE FOR NWT VERSION 1.1.3, ',
     1' 8/01/2017 INPUT READ FROM UNIT ',I4)
C
C
C2-------CHECK FOR KEYWORDS.  
C
      CALL PARSEAGO7OPTIONS(In, Iout)
C
C        
C3-------ALLOCATE SUPPLEMENTAL AND IRRIGATION WELL ARRAYS.
C
      NUMIRRSFR = 0
      NUMIRRSFRSP = 0
      NUMSUPHOLD = NUMSUP
      MXACTWSUP = MXWELL
      MXACTWIRR = MXWELL
      NUMSUPHOLD = NUMSUP
      NUMIRRHOLD = NUMIRRWEL
      MAXSEGSHOLD = MAXSEGS
      NUMCOLS = NCOL
      NUMROWS = NROW
      MAXCELLSHOLD = MAXCELLS
      IF ( NUMSUPHOLD.EQ.0 ) THEN
          NUMSUPHOLD = 1
          MXACTWSUP = 1
          MAXSEGSHOLD = 1
          ALLOCATE (DEMAND(NSEGDIM),ACTUAL(NSEGDIM),SUPACT(NSEGDIM))
      ELSE
          ALLOCATE (DEMAND(1),ACTUAL(1),SUPACT(1))
      END IF     
      IF ( NUMIRRHOLD.EQ.0 ) THEN
        MAXCELLSHOLD = 1
        NUMIRRHOLD = 1
        MXACTWIRR = 1
        NUMCELLSHOLD = 1
        NUMCOLS = 1
        NUMROWS = 1
        MAXCELLS = 1
      END IF 
      ALLOCATE(SFRSEG(MAXSEGSHOLD,MXACTWSUP),SUPWEL(NUMSUPHOLD),
     +         NUMSEGS(MXACTWSUP),PCTSUP(MAXSEGSHOLD,MXACTWSUP))
      ALLOCATE(UZFROW(MAXCELLSHOLD,MXACTWIRR),
     +         UZFCOL(MAXCELLSHOLD,MXACTWIRR),IRRWEL(NUMIRRHOLD),
     +         WELLIRR(NUMCOLS,NUMROWS),NUMCELLS(MXACTWIRR))
      ALLOCATE (IRRFACT(MAXCELLSHOLD,MXACTWIRR),
     +           IRRPCT(MAXCELLSHOLD,MXACTWIRR),SUPFLOW(MXACTWSUP))
      SFRSEG = 0
      UZFROW = 0
      UZFCOL = 0
      SUPWEL = 0
      IRRWEL = 0
      WELLIRR = 0.0
      NUMCELLS = 0
      IRRFACT = 0.0
      IRRPCT = 0.0
      NUMSEGS = 0
      PCTSUP = 0.0
      SUPFLOW = 0.0
C
C-------allocate for SFR agoptions
      ALLOCATE (NUMIRRSFR,UNITIRRSFR,MAXCELLS,NUMIRRSFRSP)
      IF ( NUMIRRSFR > 0 ) THEN
        ALLOCATE (DVRCH(NSEGDIM),DVEFF(MAXCELLS,NSEGDIM))
        ALLOCATE (KCROP(MAXCELLS,NSEGDIM))
        ALLOCATE (IRRROW(MAXCELLS,NSEGDIM),IRRCOL(MAXCELLS,NSEGDIM)) 
        ALLOCATE (DVRPERC(MAXCELLS,NSEGDIM))  
        ALLOCATE (SFRIRR(NCOL,NROW))  
        ALLOCATE (IRRSEG(NSEGDIM))
      ELSE
        ALLOCATE (DVRCH(1),DVEFF(1,1),KCROP(1,1))      
        ALLOCATE (IRRROW(1,1),IRRCOL(1,1))  
        ALLOCATE (DVRPERC(1,1))  
        ALLOCATE (SFRIRR(1,1))  
        ALLOCATE (IRRSEG(1))
      END IF
      DVRCH = 0    
      DVEFF = 0.0
      KCROP = 0.0
      IRRROW = 0  
      IRRCOL = 0
      SFRIRR = 0.0      
      DVRPERC = 0.0   
      ALLOCATE (IDVFLG)  
      IDVFLG = 0     
      DEMAND = 0.0
      SUPACT = 0.0
      ACTUAL = 0.0
      ALLOCATE(ETDEMANDFLAG)
      ETDEMANDFLAG = 0
C
C6------RETURN
      RETURN
      END SUBROUTINE
C
      SUBROUTINE PARSEAGO7OPTIONS(IN,IOUT)
C     ******************************************************************
C     READ AGO OPTIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IUNIT
      USE GWFAGOMODULE
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     ARGUMENTS
      INTEGER, INTENT(IN) :: IN,IOUT
C     ------------------------------------------------------------------
C     VARIABLES
C     ------------------------------------------------------------------
      INTEGER intchk, Iostat, LLOC,ISTART,ISTOP,I
      logical :: found,option
      real :: R
      character(len=16)  :: text        = 'AGO'
      character(len=200) :: line
C     ------------------------------------------------------------------
C
      LLOC=1
      found = .false.
      option = .false.
      CALL URDCOM(In, IOUT, line)
        DO
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        select case (LINE(ISTART:ISTOP))
        case('OPTIONS')
            write(iout,'(/1x,a)') 'PROCESSING '//
     +            trim(adjustl(text)) //' OPTIONS'
            found = .true.
            option = .true.
! Create wells for supplemental pumping. Pumped amount will be equal to specified diversion minus actual in SFR2.
        case('SUPPLEMENTAL')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMSUP,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXSEGS,R,IOUT,IN)
            IF(NUMSUP.LT.0) NUMSUP=0
            IF ( IUNIT(44) < 1 ) NUMSUP=0
            WRITE(IOUT,33) NUMSUP
            found = .true.
! Pumped water will be added as irrigation
        case('IRRIGATEWEL')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMIRRWEL,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXCELLS,R,IOUT,IN)
            IF( NUMIRRWEL.LT.0 ) NUMIRRWEL = 0
            IF ( MAXCELLS < 1 ) MAXCELLS = 1 
            IF ( IUNIT(55) < 1 ) NUMIRRWEL = 0
            WRITE(IOUT,34) NUMIRRWEL
            found = .true.
! Pumped water will be added as irrigation
          case('IRRIGATESFR')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMIRRSFR,R,IOUT,IN)  !#SEGMENTS
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXCELLS,R,IOUT,IN)   !MAX NUMBER OF CELLS
            IF( NUMIRRSFR.LT.0 ) NUMIRRSFR = 0
            IF ( MAXCELLS < 1 ) MAXCELLS = 1 
            IF ( IUNIT(55) < 1 ) NUMIRRSFR = 0
            WRITE(IOUT,34) NUMIRRSFR
            found = .true.
        case('ETDEMAND')
              ETDEMANDFLAG = 1
              WRITE(iout,*)
              WRITE(IOUT,'(A)')'AGRICULTURAL DEMANDS WILL BE '
     +                        ,'CALCULATED USING ET DEFICIT'
              WRITE(iout,*)
        case ('END')
         write(iout,'(/1x,a)') 'END PROCESSING '//
     +            trim(adjustl(text)) //' OPTIONS'
            CALL URDCOM(In, IOUT, line)
            found = .true.
            exit
          case default
            read(line(istart:istop),*,IOSTAT=Iostat) intchk
            if ( option ) then
              WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
              CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP))

            elseif( Iostat .ne. 0 ) then
              ! Not an integer.  Likely misspelled or unsupported 
              ! so terminate here.
              WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
              CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP))
            else
              exit
            endif
        end select
        if ( found ) CALL URDCOM(In, IOUT, line)
      ENDDO
   33 FORMAT(1X,' Option to pump supplmental water ',
     +          'for surface diversion shortfall is activted. '
     +          ' A total of ',I10, 'supplemental wells are active')
   34 FORMAT(1X,' Option to apply pumped water as irrigtion is active. '
     +'Pumped irrigation water will be applied to ',I10,' UZF Cells.')
      END SUBROUTINE     
C
C
      SUBROUTINE GWF2AGO7RP(IN,KPER)
C     ******************************************************************
C     READ AGO DATA FOR A STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT
      USE GWFAGOMODULE
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN):: IN, KPER
C     ------------------------------------------------------------------
C        VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER(LEN=200)::LINE
      INTEGER I, ITMP
      character(len=16)  :: text1        = 'IRRIGATE STREAM'
      character(len=16)  :: text2        = 'IRRIGATE WELL'
      character(len=16)  :: text3        = 'SUPPLEMENTAL WELL'
      INTEGER LLOC,ISTART,ISTOP
      logical :: FOUND1, FOUND2, FOUND3
      REAL :: R
C     ------------------------------------------------------------------
C
C1----READ AG OPTIONS DATA FOR STRESS PERIOD (OR FLAG SAYING REUSE AGO DATA).
      FOUND1 = .FALSE.
      FOUND2 = .FALSE.
      FOUND3 = .FALSE.
      LLOC=1
      CALL URDCOM(In, IOUT, line)
        DO
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        select case (LINE(ISTART:ISTOP))
        case('IRRIGATESTREAM')
            found1 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +            trim(adjustl(text1)) //' IRRIGATION STREAM INPUT'
            CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,ITMP,R,IOUT,IN)
            CALL SFRIRS(IN,IOUT,ITMP)
        case('IRRIGATEWELL')
            found2 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +            trim(adjustl(text2)) //' IRRIGATION WELL INPUT'
            CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,ITMP,R,IOUT,IN)
            CALL WELIRS(IN,ITMP)
       case('SUPPLEMENTALWELL')
            found3 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +            trim(adjustl(text3)) //' SUPPLEMENTAL WELL INPUT'
            CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,ITMP,R,IOUT,IN)
            CALL WELSUP(IN,ITMP)
        case default
          if ( found1 .or. found2 .or. found3 ) then
              exit
          else
C
C-------- NO KEYWORDS FOUND SO TERMINATE
                WRITE(IOUT,*) 'Invalid '//trim(adjustl(text1))
     +                   //' Option: '//LINE(ISTART:ISTOP)
                CALL USTOP('Invalid '//trim(adjustl(text1))
     +                   //' Option: '//LINE(ISTART:ISTOP))
          end if
        end select
      end do  
C
C6------RETURN
      RETURN
      END
C
C
      SUBROUTINE GWF2AGO7AD(IN,KPER)
C     ******************************************************************
C     UPDATE DEMANDS FOR NEW TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFAGOMODULE
      USE GWFSFRMODULE, ONLY: NSS, SEG
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
      INTEGER, INTENT(IN)::IN, KPER
C
      INTEGER ISEG
C     ------------------------------------------------------------------
C
C1-------RESET DEMAND IF IT CHANGES
      DO ISEG=1, NSS
        DEMAND(ISEG) = SEG(2, ISEG)
        SUPACT(ISEG) = DEMAND(ISEG)
      END DO
C
C6------RETURN
      RETURN
      END
!
      SUBROUTINE WELSUP(IN,ITMP)
C     ******************************************************************
C     READ SUP WELL DATA FOR EACH STRESS PERIOD
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,IUNIT
      USE GWFAGOMODULE
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     ARGUMENTS:
      INTEGER, INTENT(IN)::IN,ITMP
C     ------------------------------------------------------------------
C     VARIABLES:
      CHARACTER(LEN=200)::LINE
      INTEGER :: NUMSUPSP, IERR, LLOC, ISTART, ISTOP, J, ISPWL
      INTEGER :: NMSG, K, IRWL, NMCL
      REAL :: R
C     ------------------------------------------------------------------
C
C
C1------IF ITMP LESS THAN ZERO REUSE DATA FROM PREVIOUS STRESS PERIOD. PRINT MESSAGE.
C
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,6)
    6    FORMAT(1X,/
     1    1X,'REUSING SUPPLEMENTAL WELL DATA ',
     2       'FROM LAST STRESS PERIOD')
        RETURN
      END IF
      IF ( IUNIT(2) < 1 ) THEN
          WRITE(IOUT,99) 
          CALL USTOP(' ')
      END IF
C
C2------READ LIST OF DIVERSION SEGEMENTS FOR CALCALATING SUPPLEMENTAL PUMPING
C
      IERR = 0
      IF ( NUMSUP > 0 ) THEN
      NUMSUPSP = ITMP
      IF ( NUMSUPSP > NUMSUP )THEN
        WRITE(IOUT,*)
        WRITE(IOUT,102)NUMSUP,NUMSUPSP
        CALL USTOP('')
      END IF
        IERR = 0
        DO J = 1, NUMSUPSP
          LLOC = 1
          CALL URDCOM(IN,IOUT,LINE)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISPWL,R,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMSG,R,IOUT,IN)
          IF ( NMSG > MAXSEGS )THEN
            WRITE(IOUT,*)
            WRITE(IOUT,103)MAXSEGS,NMSG
            CALL USTOP('')
          END IF
          BACKSPACE(IN)
          READ(IN,*)SUPWEL(J),NUMSEGS(ISPWL),
     +                    (PCTSUP(K,ISPWL),SFRSEG(K,ISPWL),K=1,NMSG)
          DO K = 1, NUMSEGS(SUPWEL(J))
            IF ( SFRSEG(K,SUPWEL(J)) == 0 ) IERR = 1
          END DO
        END DO
      END IF
      IF ( IERR == 1 ) THEN
        WRITE(IOUT,*)'SEGMENT NUMBER FOR SUPPLEMENTAL WELL ',
     +               'SPECIFIED AS ZERO. MODEL STOPPING'
        CALL USTOP('')
      END IF
C
   99 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'WELL PACKAGE MUST BE ACTIVE TO SIMULATE SUPPLEMENTAL ',
     +        'WELLS')
  102 FORMAT('***Error in WELL*** maximum number of supplimentary ',
     +       'wells is less than the number specified in ',
     +       'stress period. ',/
     +       'Maximum wells and the number specified for stress ',
     +       'period are: ',2i6)
  103 FORMAT('***Error in WELL*** maximum number of segments ',
     +       'for a supplementary well is less than the number  ',
     +       'specified in stress period. ',/
     +       'Maximum segments and the number specified for stress '
     +       'period are: ',2i6)
  106 FORMAT('***Error in SUP WEL*** cell row or column number for '
     +        ,'supplemental well specified as zero. Model stopping')
C
C6------RETURN
      RETURN
      END
!
      SUBROUTINE WELIRS(IN,ITMP)
C     ******************************************************************
C     READ WELL IRRIGATION DATA FOR EACH STRESS PERIOD
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,IUNIT
      USE GWFAGOMODULE
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     ARGUMENTS:
      INTEGER, INTENT(IN)::IN,ITMP
C     ------------------------------------------------------------------
C     VARIABLES:
      CHARACTER(LEN=200)::LINE
      INTEGER :: IERR, LLOC, ISTART, ISTOP, J, ISPWL
      INTEGER :: NMSG, K, IRWL, NMCL
      REAL :: R
C     ------------------------------------------------------------------
C
C
C1------IF ITMP LESS THAN ZERO REUSE DATA FROM PREVIOUS STRESS PERIOD. PRINT MESSAGE.
C
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,6)
    6    FORMAT(1X,/
     1    1X,'REUSING IRRIGATION WELL DATA ',
     2       'FROM LAST STRESS PERIOD')
        RETURN
      END IF
      IF ( IUNIT(2) < 1 ) THEN
          WRITE(IOUT,99) 
          CALL USTOP(' ')
      END IF
      IERR = 0
      NUMIRRWELSP = 0
! READ LIST OF IRRIGATION CELLS FOR EACH WELL        
!
      IF ( NUMIRRWEL > 0 .AND. IUNIT(44) > 0 ) THEN
        LLOC = 1
        CALL URDCOM(IN,IOUT,LINE)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMIRRWELSP,R,IOUT,IN)
        IF ( NUMIRRWELSP > NUMIRRWEL )THEN
          WRITE(IOUT,*)
          WRITE(IOUT,104)NUMIRRWEL,NUMIRRWELSP
          CALL USTOP('')
        END IF
        DO J = 1, NUMIRRWELSP
          LLOC = 1
          CALL URDCOM(IN,IOUT,LINE)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRWL,R,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMCL,R,IOUT,IN)
          IF ( NMCL > MAXCELLS )THEN
            WRITE(IOUT,*)
            WRITE(IOUT,105)MAXCELLS,NMCL
            CALL USTOP('')
          END IF
          BACKSPACE(IN)
          READ(IN,*)IRRWEL(J),NUMCELLS(IRWL),(IRRFACT(K,IRWL),
     +        IRRPCT(K,IRWL),UZFROW(K,IRWL),UZFCOL(K,IRWL),K=1,NMCL)
          DO K = 1, NUMCELLS(IRRWEL(J))
          IF ( UZFROW(K,IRRWEL(J))==0 .OR. UZFCOL(K,IRRWEL(J))==0 ) THEN
            WRITE(IOUT,106) 
            CALL USTOP('')   
          END IF
          END DO
        END DO
      END IF
C
   99 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'WELL PACKAGE MUST BE ACTIVE TO SIMULATE IRRIGATION ',
     +        'WELLS')
  104 FORMAT('***Error in IRR WEL*** maximum number of irrigation ',
     +       'wells is less than the number specified in stress ',
     +       'period. ',/
     +       'Maximum segments and the number specified for stress '
     +       'period are: ',2i6)
  105 FORMAT('***Error in IRR WEL*** maximum number of cells ',
     +       'irrigated by a well is less than the number ',
     +       'specified in stress period. ',/
     +       'Maximum cells and the number specified for stress '
     +       'period are: ',2i6)
  106 FORMAT('***ERROR IN AGO PACKAGE*** cell row or column for ',
     +       'irrigation well specified as zero. Model stopping.')
C
C6------RETURN
      RETURN
      END
!
! ----------------------------------------------------------------------
C
C-------SUBROUTINE SFRAGOPTIONS
      SUBROUTINE SFRIRS(IN,IOUT,ITMP)
C  READ DIVERSION SEGMENT DATA FOR EACH STRESS PERIOD
      USE GWFAGOMODULE, ONLY: DVRCH, IRRROW, IRRCOL, DVEFF, DVRPERC, 
     +                          NUMIRRSFR, IRRSEG, MAXCELLS, 
     +                          KCROP, NUMIRRSFRSP
      USE GLOBAL,       ONLY: IUNIT
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     ARGUMENTS
      INTEGER, INTENT(IN)::IN,IOUT,ITMP
C     ------------------------------------------------------------------
C     VARIABLES
C     ------------------------------------------------------------------
      INTEGER LLOC,ISTART,ISTOP,J,SGNM,NMCL,K
      REAL R 
      DOUBLE PRECISION :: totdum
      CHARACTER(LEN=200)::LINE
C     ------------------------------------------------------------------
C2--- INITIALIZE AG VARIABLES TO ZERO
      DVRPERC = 0.0  
      DVRCH = 0.0 
      KCROP = 0.0
C
C2------IF ITMP LESS THAN ZERO REUSE DATA FROM PREVIOUS STRESS PERIOD. PRINT MESSAGE.
C
      IF(ITMP < 1 ) THEN
         WRITE(IOUT,6)
    6    FORMAT(1X,/
     1    1X,'REUSING IRRIGATION DIVERSION SEGEMENTS ',
     2       'FROM LAST STRESS PERIOD')
        RETURN
      END IF
      IF ( IUNIT(44) < 1 ) THEN
          WRITE(IOUT,9005) 
          CALL USTOP(' ')
      END IF
C
C1
C----READ IRRIGATION SEGEMENT INFORMATION.
C
      IF ( NUMIRRSFR > 0 ) THEN
        NUMIRRSFRSP = ITMP
        IF ( NUMIRRSFRSP > NUMIRRSFR ) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,9008)NUMIRRSFR,NUMIRRSFRSP
            CALL USTOP('')
        END IF
        DO J = 1, NUMIRRSFRSP
          LLOC = 1
          CALL URDCOM(IN,IOUT,LINE)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,SGNM,R,IOUT,IN)  !SEGMENT
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMCL,R,IOUT,IN)  !NUMCELL
          IF ( NMCL > MAXCELLS ) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,9009)MAXCELLS,NMCL
            CALL USTOP('')
          END IF
          IF ( SGNM > 0 ) THEN
            BACKSPACE(IN)
            READ(IN,*)IRRSEG(J),DVRCH(SGNM), 
     +                   (DVEFF(K,SGNM),DVRPERC(K,SGNM),KCROP(K,SGNM),
     +                    IRRROW(K,SGNM),IRRCOL(K,SGNM),K=1,NMCL)
            totdum  = 0.0
            DO K = 1, NMCL
              IF ( IRRROW(K,SGNM)==0 .OR. IRRCOL(K,SGNM)==0 ) THEN
                totdum = totdum + DVRPERC(NMCL,SGNM)
                WRITE(IOUT,9007)
                CALL USTOP('')   
                IF ( totdum.GT.1.000001 ) WRITE(Iout,9006)totdum
              END IF
            END DO
          END IF
        END DO
      END IF
!
 9005 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'SFR2 PACKAGE MUST BE ACTIVE TO SIMULATE IRRIGATION ',
     +        'FROM SEGEMENTS')
 9006 FORMAT(' ***Warning in SFR2*** ',/
     +       'Fraction of diversion for each cell in group sums '/,
     +       'to a value greater than one. Sum = ',E10.5)
 9007 FORMAT('***Error in SFR2*** cell row or column for irrigation',
     +       'cell specified as zero. Model stopping.')
 9008 FORMAT('***Error in SFR2*** maximum number of irrigation ',
     +       'segments is less than the number specified in ',
     +       'stress period. ',/ 
     +       'Maximum segments and the number specified are: ',2i6)
 9009 FORMAT('***Error in SFR2*** maximum number of irrigation ',
     +       'cells is less than the number specified in ',
     +       'stress period. ',/ 
     +       'Maximum cells and the number specified are: ',2i6)
C11-----RETURN.
      RETURN
      END
C
      SUBROUTINE GWF2AGO7FM(Kkper, Kkstp, Kkiter)
C     ******************************************************************
C     CALCULATE APPLIED IRRIGATION, DIVERISONS, AND PUMPING
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:DELR,DELC,issflg
      USE GWFBASMODULE, ONLY:TOTIM
      USE GWFWELMODULE, ONLY:NWELLS,WELL,NUMTAB,WELL,NWELVL
      USE GWFAGOMODULE
      USE GWFSFRMODULE, ONLY: SGOTFLW, NSTRM, ISTRM
      IMPLICIT NONE
C
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: KKPER, KKSTP, KKITER
C
C        VARIABLES:
C     ------------------------------------------------------------------
      INTEGER NWELLSTEMP,l,I,J,ISTSG,ICOUNT,IRR,ICC,IC,IR
      DOUBLE PRECISION :: ZERO, TIME, SUP, FMIN,Q,SUBVOL,SUBRATE,DVT
C     ------------------------------------------------------------------
      ZERO=0.0D0
      WELLIRR = 0.0
      TIME = TOTIM
      ACTUAL = 0.0
      SUP = 0.0
      SFRIRR = 0.0   
      SUPFLOW = 0.0
C
C2------IF DEMAND BASED ON ET DEFICIT THEN CALCULATE VALUES
      IF ( ETDEMANDFLAG > 0 .AND. issflg(kkper) == 0 ) THEN
        IF ( KKITER==1 ) CALL UZFIRRDEMANDSET()
        CALL UZFIRRDEMANDCALC(Kkper, Kkstp, Kkiter)
      END IF
            
C
C3------SET MAX NUMBER OF POSSIBLE SUPPLEMENTARY WELLS.
      NWELLSTEMP = NWELLS
      IF ( NUMTAB.GT.0 ) NWELLSTEMP = NUMTAB
C
C3------CALCULATE DIVERSION SHORTFALL TO SET SUPPLEMENTAL PUMPING DEMAND
      DO L=1,NWELLSTEMP 
        Q = WELL(NWELVL,L)
        IF ( NUMSUP > 0 ) THEN
          IF ( NUMSEGS(L) > 0 ) THEN
            SUP = 0.0
            DO I = 1, NUMSEGS(L)
              J = SFRSEG(I,L)
              FMIN = SUPACT(J)
              FMIN = PCTSUP(I,L)*(FMIN - SGOTFLW(J))              
              IF ( FMIN < 0.0D0 ) FMIN = 0.0D0
              SUP = SUP + FMIN
              SUP = SUP - ACTUAL(J)
            END DO
            IF ( SUP < 0.0 ) SUP = 0.0
            SUPFLOW(L) = SUPFLOW(L) - SUP
C
C3------SET ACTUAL SUPPLEMENTAL PUMPING BY DIVERSION FOR IRRIGATION.
            SUP = 0.0
            DO I = 1, NUMSEGS(L)
              J = SFRSEG(I,L) 
              IF ( Q < 0.0 ) SUP = SUP - Q
              ACTUAL(J)  = ACTUAL(J) + SUP
            END DO
          END IF
        END IF
! APPLY IRRIGATION FROM WELLS
        IF ( NUMIRRWELSP > 0 ) THEN
         IF ( NUMCELLS(L) > 0 ) THEN
            DO I = 1, NUMCELLS(L)
              SUBVOL = -(1.0-IRRFACT(I,L))*Q*IRRPCT(I,L)
              SUBRATE = SUBVOL/(DELR(UZFCOL(I,L))*DELC(UZFROW(I,L)))
              WELLIRR(UZFCOL(I,L),UZFROW(I,L)) = SUBRATE
           END DO
          END IF
        END IF
      END DO
C APPLY IRRIGATION FROM DIVERSIONS
      DO l = 1, NSTRM
        istsg = ISTRM(4, l)
        IF ( istsg.GT.1 .AND. NUMIRRSFRSP > 0 ) THEN
          IF ( DVRCH(istsg) .GT. 0) THEN
            DO icount = 1, DVRCH(istsg)
              irr = IRRROW(icount,istsg)
              icc = IRRCOL(icount,istsg)
              dvt = SGOTFLW(istsg)*DVRPERC(ICOUNT,istsg)
              dvt = dvt/(DELR(IC)*DELC(IR))
              SFRIRR(icc, irr) = SFRIRR(icc, irr) + 
     +                         dvt*(1.0-DVEFF(ICOUNT,istsg))
            END DO
          END IF
        END IF
      END DO
C
C3------RETURN
      RETURN
      END
      SUBROUTINE GWF2AGO7BD(KKSTP,KKPER)
C     ******************************************************************
C     CALCULATE VOLUMETRIC FOR AG OPTIONS (DIVERSIONS AND PUMPING)
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,DELR,DELC,issflg
      USE GWFWELMODULE,ONLY:NWELLS,WELL,NUMTAB,NWELVL
      USE GWFBASMODULE,ONLY:TOTIM
      USE GWFAGOMODULE
      USE GWFSFRMODULE, ONLY: SGOTFLW, NSTRM, ISTRM
      IMPLICIT NONE
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN):: KKSTP,KKPER
C        VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      CHARACTER*20 TEXT1
      DOUBLE PRECISION RATIN,RATOUT,FMIN,ZERO,DVT
      double precision TIME,SUP,SUBVOL,SUBRATE
      real Q
      INTEGER NWELLSTEMP,L,I,J,ISTSG,ICOUNT
      INTEGER IRR,ICC,IC,IR
      DATA TEXT /'           WELLS'/
      DATA TEXT1 /'SUPPLEMENTARY WELLS'/
C     ------------------------------------------------------------------
      ZERO=0.0D0
      WELLIRR = 0.0
      TIME = TOTIM
      ACTUAL = 0.0
      SUP = 0.0
      SFRIRR = 0.0   
      SUPFLOW = 0.0
C
C2------IF DEMAND BASED ON ET DEFICIT THEN CALCULATE VALUES
      IF ( ETDEMANDFLAG > 0 .AND. issflg(kkper) == 0 ) THEN
        CALL UZFIRRDEMANDSET()
        CALL UZFIRRDEMANDCALC(Kkper, Kkstp, 1)
      END IF
            
C
C3------SET MAX NUMBER OF POSSIBLE SUPPLEMENTARY WELLS.
      NWELLSTEMP = NWELLS
      IF ( NUMTAB.GT.0 ) NWELLSTEMP = NUMTAB
C
C3------CALCULATE DIVERSION SHORTFALL TO SET SUPPLEMENTAL PUMPING DEMAND
      DO L=1,NWELLSTEMP 
        Q = WELL(NWELVL,L)
        IF ( NUMSUP > 0 ) THEN
          IF ( NUMSEGS(L) > 0 ) THEN
            SUP = 0.0
            DO I = 1, NUMSEGS(L)
              J = SFRSEG(I,L)
              FMIN = SUPACT(J)
              FMIN = PCTSUP(I,L)*(FMIN - SGOTFLW(J))              
              IF ( FMIN < 0.0D0 ) FMIN = 0.0D0
              SUP = SUP + FMIN
              SUP = SUP - ACTUAL(J)
            END DO
            IF ( SUP < 0.0 ) SUP = 0.0
            SUPFLOW(L) = SUPFLOW(L) - SUP
C
C3------SET ACTUAL SUPPLEMENTAL PUMPING BY DIVERSION FOR IRRIGATION.
            SUP = 0.0
            DO I = 1, NUMSEGS(L)
              J = SFRSEG(I,L) 
              IF ( Q < 0.0 ) SUP = SUP - Q
              ACTUAL(J)  = ACTUAL(J) + SUP
            END DO
          END IF
        END IF
! APPLY IRRIGATION FROM WELLS
        IF ( NUMIRRWELSP > 0 ) THEN
         IF ( NUMCELLS(L) > 0 ) THEN
            DO I = 1, NUMCELLS(L)
              SUBVOL = -(1.0-IRRFACT(I,L))*Q*IRRPCT(I,L)
              SUBRATE = SUBVOL/(DELR(UZFCOL(I,L))*DELC(UZFROW(I,L)))
              WELLIRR(UZFCOL(I,L),UZFROW(I,L)) = SUBRATE
           END DO
          END IF
        END IF
      END DO
C APPLY IRRIGATION FROM DIVERSIONS
      DO l = 1, NSTRM
        istsg = ISTRM(4, l)
        IF ( istsg.GT.1 .AND. NUMIRRSFRSP > 0 ) THEN
          IF ( DVRCH(istsg) .GT. 0) THEN
            DO icount = 1, DVRCH(istsg)
              irr = IRRROW(icount,istsg)
              icc = IRRCOL(icount,istsg)
              dvt = SGOTFLW(istsg)*DVRPERC(ICOUNT,istsg)
              dvt = dvt/(DELR(IC)*DELC(IR))
              SFRIRR(icc, irr) = SFRIRR(icc, irr) + 
     +                         dvt*(1.0-DVEFF(ICOUNT,istsg))
            END DO
          END IF
        END IF
      END DO
C
C3------RETURN
      RETURN
      END
! ----------------------------------------------------------------------
C
      subroutine UZFIRRDEMANDCALC(Kkper, Kkstp, Kkiter)
!     ******************************************************************
!     irrdemand---- sums up irrigation demand based on actual ET
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFUZFMODULE, ONLY: GWET,UZFETOUT,PETRATE,VKS,Isurfkreject,
     +                        surfk
      USE GWFSFRMODULE, ONLY: SEG
      USE GWFAGOMODULE, ONLY: DVRCH,KCROP,IRRROW,IRRCOL,
     +                        SUPACT,NUMIRRSFRSP,IRRSEG,DEMAND 
      USE GLOBAL,     ONLY: DELR, DELC
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
! ----------------------------------------------------------------------
      !modules
      !arguments
      ! -- dummy
      DOUBLE PRECISION :: factor, area, uzet, aet, pet, finfsum, fks
      double precision :: zerod2,zerod30,done,dzero,dum,pettotal, 
     +                    aettotal
      integer :: k,iseg,ic,ir,i,Kkper, Kkstp, Kkiter
! ----------------------------------------------------------------------
!
      zerod30 = 1.0d-30
      zerod2 = 1.0d-2
      done = 1.0d0
      dzero = 0.0d0
      do i = 1, NUMIRRSFRSP   !this won't work for GW only
        iseg = IRRSEG(i)
        finfsum = dzero
        do k = 1, DVRCH(iseg)
           ic = IRRCOL(k,iseg)
           ir = IRRROW(k,iseg)
           fks = VKS(ic, ir)
           IF ( Isurfkreject > 0 ) fks = SURFK(ic, ir)
           area = delr(ic)*delc(ir)
           finfsum = finfsum + fks*area
           pet = PETRATE(ic,ir)
           uzet = uzfetout(ic,ir)/DELT
           aet = (gwet(ic,ir)+uzet)/area
           if ( aet < zerod30 ) aet = zerod30
           factor = pet/aet - done
           SEG(2,iseg) = SEG(2,iseg) + factor*pet*area
           if ( SEG(2,iseg) < dzero ) SEG(2,iseg) = dzero
           dum = pet
           if ( KCROP(K,ISEG) > zerod30 ) dum = pet/KCROP(K,ISEG)
           pettotal = pettotal + pet
           aettotal = aettotal + aet
 !     if(k==2)then
 !     write(222,333)Kkper, Kkstp, Kkiter,ic,ir,iseg,dum,
 !    +              aet,SFRIRR(ic,ir),WELLIRR(ic,ir),SGOTFLW(iseg)
 !333  format(6i6,5e20.10)
 !     end if
        end do
        if ( SEG(2,iseg) > finfsum ) SEG(2,iseg) = finfsum
        if ( SEG(2,iseg) > demand(ISEG) ) SEG(2,iseg) = demand(ISEG)
        SUPACT(iseg) = DEMAND(iseg)
        if ( pettotal-aettotal < zerod2*pettotal ) SUPACT(iseg) = 
     +                                             SEG(2,iseg)
      end do
      return
      end subroutine UZFIRRDEMANDCALC
!
      subroutine UZFIRRDEMANDSET()
!     ******************************************************************
!     irrdemand---- sets initial crop demand to PET
!     ******************************************************************
!     SPECIFICATIONS:
!      USE GWFUZFMODULE, ONLY: PETRATE
      USE GWFAGOMODULE, ONLY: DVRCH,IRRROW,IRRCOL,NUMIRRSFRSP,IRRSEG
      USE GWFSFRMODULE, ONLY: SEG
      USE GLOBAL,     ONLY: DELR, DELC
      IMPLICIT NONE
! ----------------------------------------------------------------------
      DOUBLE PRECISION :: area
      integer :: k,iseg,ic,ir,i
! ----------------------------------------------------------------------
!
      do i = 1, NUMIRRSFRSP
        iseg = IRRSEG(i)
        do k = 1, DVRCH(iseg)
           ic = IRRCOL(k,iseg)
           ir = IRRROW(k,iseg)
           area = delr(ic)*delc(ir)
           SEG(2,iseg) = 0.0d0
        end do
      end do
      return
      end subroutine UZFIRRDEMANDSET
! ----------------------------------------------------------------------
C
      subroutine APPLYKCROP()
!     ******************************************************************
!     APPLYKCROP---- Apply crop ceofficient to ETo
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFUZFMODULE, ONLY: PETRATE
      USE GWFAGOMODULE, ONLY: DVRCH,IRRROW,IRRCOL,NUMIRRSFRSP,
     +                        IRRSEG,KCROP
      IMPLICIT NONE
! ----------------------------------------------------------------------
      integer :: k,iseg,ic,ir,i
! ----------------------------------------------------------------------
!
      do i = 1, NUMIRRSFRSP
        iseg = IRRSEG(i)
        do k = 1, DVRCH(iseg)
           ic = IRRCOL(k,iseg)
           ir = IRRROW(k,iseg)
           PETRATE(ic,ir) = KCROP(K,ISEG)*PETRATE(IC,IR)
        end do
      end do
      return
      END subroutine APPLYKCROP
C
C
      SUBROUTINE GWF2AGO7DA()
C  Deallocate AGO MEMORY
      USE GWFAGOMODULE
C
        DEALLOCATE(NUMSUP)
        DEALLOCATE(NUMIRRWEL)
        DEALLOCATE(SFRSEG)
        DEALLOCATE(UZFROW)
        DEALLOCATE(UZFCOL)
        DEALLOCATE(SUPWEL)
        DEALLOCATE(IRRWEL)
        DEALLOCATE(NUMSEGS)
        DEALLOCATE(MAXSEGS)
        DEALLOCATE(MAXCELLS)
        DEALLOCATE(NUMCELLS)
        DEALLOCATE(WELLIRR)
        DEALLOCATE(IRRFACT)
        DEALLOCATE(IRRPCT)
!SFR
      DEALLOCATE (DVRCH)    
      DEALLOCATE (DVEFF)  
      DEALLOCATE (IRRROW) 
      DEALLOCATE (IRRCOL)
      DEALLOCATE (SFRIRR) 
      DEALLOCATE (DVRPERC) 
      DEALLOCATE (IDVFLG) 
      DEALLOCATE (NUMIRRSFR)
      DEALLOCATE (NUMIRRSFRSP)
      DEALLOCATE (MAXCELLS)
      DEALLOCATE (DEMAND)
      DEALLOCATE (ACTUAL)
      DEALLOCATE (SUPACT)
      DEALLOCATE (NUMIRRWELSP)
C
      RETURN
      END
