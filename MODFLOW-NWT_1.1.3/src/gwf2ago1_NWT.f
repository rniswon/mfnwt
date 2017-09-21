      MODULE GWFAGOMODULE
! from well package
        INTEGER,SAVE,POINTER  :: NWELLS,MXWELL,NWELVL,NPWEL,IPRWEL
        INTEGER,SAVE,POINTER  :: IWELCB,IRDPSI,IWELPB,NNPWEL
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::WELAUX
        REAL,             SAVE, DIMENSION(:,:), POINTER     ::WELL
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::TABTIME
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::TABRATE
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABLAY
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABROW
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABCOL
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABVAL
        REAL,             SAVE,                 POINTER     ::PSIRAMP
        INTEGER,          SAVE,                 POINTER     ::IUNITRAMP
        INTEGER,          SAVE,                 POINTER     ::NUMTAB
        INTEGER,          SAVE,                 POINTER     ::MAXVAL
! AGO orig
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::SFRSEG
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::UZFROW
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::UZFCOL
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::WELLIRR
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::IRRFACT
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::IRRPCT
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::SUPWELVAR
        REAL,             SAVE, DIMENSION(:),   POINTER     ::SUPFLOW
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::IRRWELVAR
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::PCTSUP
        INTEGER,          SAVE,                 POINTER     ::NUMSUP
        INTEGER,          SAVE,                 POINTER     ::UNITSUP
        INTEGER,          SAVE,                 POINTER     ::NUMIRRWEL
        INTEGER,          SAVE,                 POINTER     ::UNITIRRWEL
        INTEGER,          SAVE,                 POINTER     ::MAXSEGS
        INTEGER,          SAVE,               POINTER     ::MAXCELLSWEL
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


      SUBROUTINE GWF2AGO7AR(IN,IUNITNWT)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR AGO PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,IFREFM
      USE GWFAGOMODULE
      USE GWFSFRMODULE, ONLY:NSEGDIM
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS
      integer,intent(in) :: IN,IUNITNWT
C     ------------------------------------------------------------------
C        VARIABLES
!      CHARACTER(len=16) :: text        = ' AGO PACKAGE '
      CHARACTER(len=200) :: LINE
      INTEGER :: MXACTWSUP,MXACTWIRR,NUMSUPHOLD,NUMIRRHOLD
      INTEGER :: MAXSEGSHOLD,NUMCOLS,NUMROWS,MAXCELLSHOLD,NUMCELLSHOLD
      INTEGER :: NUMTABHOLD,MXACTW,LLOC,ISTART,ISTOP,I,NAUX,N
      REAL :: R
C     ------------------------------------------------------------------
      ALLOCATE(NWELLS,MXWELL,NWELVL,IWELCB)
      ALLOCATE(IWELPB,PSIRAMP,IUNITRAMP)
      ALLOCATE(NUMTAB,MAXVAL,NPWEL,NNPWEL)
      PSIRAMP = 0.10
      NUMTAB = 0
      MAXVAL = 1
      ALLOCATE(MAXVAL,NUMSUP,NUMIRRWEL,UNITSUP,MAXCELLSWEL)
      ALLOCATE(MAXSEGS,NUMIRRWELSP)
      ALLOCATE(ETDEMANDFLAG,NUMIRRSFR,NUMIRRSFRSP)
      ALLOCATE (MAXCELLSSFR)
      NWELLS=0
      NNPWEL=0
      MXWELL=0
      NWELVL=0
      IWELCB=0
      IUNITRAMP=IOUT
      MAXVAL = 1
      NUMSUP = 0
      NUMIRRWEL = 0
      UNITSUP = 0
      MAXSEGS = 0
      MAXCELLSWEL = 0
      MAXCELLSSFR = 0
      NUMIRRWELSP = 0
      ETDEMANDFLAG = 0
      NUMIRRSFR = 0
      NUMIRRSFRSP = 0
C
C1------IDENTIFY PACKAGE AND INITIALIZE AG OPTIONS.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'AGO -- AGO PACKAGE FOR NWT VERSION 1.1.3, ',
     1' 8/01/2017 INPUT READ FROM UNIT ',I4)
C
C
C2-------CHECK FOR KEYWORDS.  
C
      CALL PARSEAGO7OPTIONS(In, Iout, Iunitnwt)
C
C3-------ALLOCATE VARIABLES FOR TIME SERIES WELL RATES
      NUMTABHOLD = NUMTAB
      IF ( NUMTABHOLD.EQ.0 ) NUMTABHOLD = 1
      ALLOCATE(TABTIME(MAXVAL,NUMTABHOLD),TABRATE(MAXVAL,NUMTABHOLD))
      ALLOCATE(TABLAY(NUMTABHOLD),TABROW(NUMTABHOLD),TABCOL(NUMTABHOLD))
      ALLOCATE(TABVAL(NUMTABHOLD))
      TABTIME = 0.0
      TABRATE = 0.0
      TABLAY = 0
      TABROW = 0
      TABCOL = 0
      TABVAL = 0
C
C4-------READ MAX WELLS AND CBC FILE UNIT NUMBER
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTW,IWELCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTW,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELCB,R,IOUT,IN)
      END IF
      i = 0
      WRITE(IOUT,3) MXACTW
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE WELLS AT ONE TIME')
      IF(IWELCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(IWELCB.GT.0) WRITE(IOUT,8) IWELCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C5------READ AUXILIARY VARIABLES AND PRINT FLAG.
      ALLOCATE(WELAUX(20))
      NAUX=0
      IPRWEL=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            WELAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) WELAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY WELL VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF WELL CELLS WILL NOT BE PRINTED')
         IPRWEL = 0
         GO TO 10
      END IF
C
C6------THERE ARE FOUR INPUT VALUES PLUS ONE LOCATION FOR
C6------CELL-BY-CELL FLOW.
      NWELVL=5+NAUX
C
C7------ALLOCATE SPACE FOR THE WELL DATA.
      IWELPB=MXACTW+1
      MXWELL=MXACTW
      IF(MXACTW.LT.1) THEN
         WRITE(IOUT,17)
   17    FORMAT(1X,
     1'No wells active in the AGO Package')
      END IF
      ALLOCATE (WELL(NWELVL,MXWELL))
C        
C6-------ALLOCATE SUPPLEMENTAL AND IRRIGATION WELL ARRAYS.
C
      NUMSUPHOLD = NUMSUP
      MXACTWSUP = MXWELL
      MXACTWIRR = MXWELL
      NUMSUPHOLD = NUMSUP
      NUMIRRHOLD = NUMIRRWEL
      MAXSEGSHOLD = MAXSEGS
      NUMCOLS = NCOL
      NUMROWS = NROW
      MAXCELLSHOLD = MAXCELLSWEL
      IF ( NUMSUPHOLD.EQ.0 ) THEN
          NUMSUPHOLD = 1
          MXACTWSUP = 1
          MAXSEGSHOLD = 1
          ALLOCATE (DEMAND(1),ACTUAL(1),SUPACT(1))
      ELSE
          ALLOCATE (DEMAND(NSEGDIM),ACTUAL(NSEGDIM),SUPACT(NSEGDIM))
      END IF     
      IF ( NUMIRRHOLD.EQ.0 ) THEN
        MAXCELLSHOLD = 1
        NUMIRRHOLD = 1
        MXACTWIRR = 1
        NUMCELLSHOLD = 1
        NUMCOLS = 1
        NUMROWS = 1
        MAXCELLSWEL = 1
      END IF 
      ALLOCATE(SFRSEG(MAXSEGSHOLD,MXACTWSUP),SUPWELVAR(NUMSUPHOLD),
     +         NUMSEGS(MXACTWSUP),PCTSUP(MAXSEGSHOLD,MXACTWSUP))
      ALLOCATE(UZFROW(MAXCELLSHOLD,MXACTWIRR),
     +         UZFCOL(MAXCELLSHOLD,MXACTWIRR),IRRWELVAR(NUMIRRHOLD),
     +         WELLIRR(NUMCOLS,NUMROWS),NUMCELLS(MXACTWIRR))
      ALLOCATE (IRRFACT(MAXCELLSHOLD,MXACTWIRR),
     +           IRRPCT(MAXCELLSHOLD,MXACTWIRR),SUPFLOW(MXACTWSUP))
      SFRSEG = 0
      UZFROW = 0
      UZFCOL = 0
      SUPWELVAR = 0
      IRRWELVAR = 0
      WELLIRR = 0.0
      NUMCELLS = 0
      IRRFACT = 0.0
      IRRPCT = 0.0
      NUMSEGS = 0
      PCTSUP = 0.0
      SUPFLOW = 0.0
C
C-------allocate for SFR agoptions
      IF ( NUMIRRSFR > 0 ) THEN
        ALLOCATE (DVRCH(NSEGDIM),DVEFF(MAXCELLSSFR,NSEGDIM))
        ALLOCATE (KCROP(MAXCELLSSFR,NSEGDIM))
        ALLOCATE (IRRROW(MAXCELLSSFR,NSEGDIM))
        ALLOCATE (IRRCOL(MAXCELLSSFR,NSEGDIM)) 
        ALLOCATE (DVRPERC(MAXCELLSSFR,NSEGDIM))  
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
C
C6------RETURN
      RETURN
      END SUBROUTINE
C
      SUBROUTINE PARSEAGO7OPTIONS(IN,IOUT,IUNITNWT)
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
      INTEGER, INTENT(IN) :: IN,IOUT,IUNITNWT
C     ------------------------------------------------------------------
C     VARIABLES
C     ------------------------------------------------------------------
      INTEGER intchk, Iostat, LLOC,ISTART,ISTOP,I
      logical :: found,option,found1
      real :: R
      character(len=16)  :: text        = 'AGO'
      character(len=200) :: line
C     ------------------------------------------------------------------
C
      LLOC=1
      found = .false.
      found1 = .false.
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
        case('SUPPLEMENTALWELL')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMSUP,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXSEGS,R,IOUT,IN)
            IF(NUMSUP.LT.0) NUMSUP=0
            IF ( IUNIT(44) < 1 ) NUMSUP=0
            WRITE(IOUT,33) NUMSUP
            found1 = .true.
            found = .true.
! Pumped water will be added as irrigation
        case('IRRIGATIONWELL')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMIRRWEL,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXCELLSWEL,R,IOUT,IN)
            IF( NUMIRRWEL.LT.0 ) NUMIRRWEL = 0
            IF ( MAXCELLSWEL < 1 ) MAXCELLSWEL = 1 
            IF ( IUNIT(2) < 1 ) NUMIRRWEL = 0
            WRITE(IOUT,34) NUMIRRWEL
            found = .true.
            found1 = .true.
! REDUCING PUMPING FOR DRY CELLS
        case('PHIRAMP')
! CHECK THAT THERE ARE SUP OR IRR WELLS FIRST
          if ( found1 ) then
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PSIRAMP,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITRAMP,R,IOUT,IN)
            IF ( IUNITNWT.EQ.0 ) THEN
              write(IOUT,32)
            ELSE
              IF(PSIRAMP.LT.1.0E-5) PSIRAMP=1.0E-5
              IF ( IUNITRAMP.EQ.0 ) IUNITRAMP = IOUT
              WRITE(IOUT,*)
              WRITE(IOUT,29) PSIRAMP,IUNITRAMP
            END IF
          else
            WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' SUP or IRR wells required'
            CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' SUP or IRR wells required')
          end if
          found = .true.
! SPEICYING PUMPING RATES AS TIMES SERIES INPUT FILE FOR EACH WELL
        case('TABFILES')
          if ( found1 ) then
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTAB,R,IOUT,IN)
            IF(NUMTAB.LT.0) NUMTAB=0
            WRITE(IOUT,30) NUMTAB
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
            IF(MAXVAL.LT.0) THEN
                MAXVAL=1
                NUMTAB=0
            END IF
            WRITE(IOUT,31) MAXVAL
                 else
            WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' SUP or IRR wells required'
            CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' SUP or IRR wells required')
          end if
          found = .true.
! Pumped water will be added as irrigation
        case('IRRIGATIONSTREAM')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMIRRSFR,R,IOUT,IN)  !#SEGMENTS
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXCELLSSFR,R,IOUT,IN)   !MAX NUMBER OF CELLS
            IF( NUMIRRSFR.LT.0 ) NUMIRRSFR = 0
            IF ( MAXCELLSSFR < 1 ) MAXCELLSSFR = 1 
            IF ( IUNIT(44) < 1 ) NUMIRRSFR = 0
            WRITE(IOUT,34) NUMIRRSFR
            found = .true.
        case('ETDEMAND')
              ETDEMANDFLAG = 1
              WRITE(iout,*)
              WRITE(IOUT,'(A)')'AGRICULTURAL DEMANDS WILL BE '
     +                        ,'CALCULATED USING ET DEFICIT'
              WRITE(iout,*)
              IF ( IUNIT(55) < 1 ) ETDEMANDFLAG = 0   ! ALSO NEED TO CHECK THAT UNSAT FLOW AND UZET IS ACTIVE!!!!!
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
        if ( found ) backspace(in)
   29 FORMAT(1X,'NEGATIVE PUMPING RATES WILL BE REDUCED IF HEAD '/
     +       ' FALLS WITHIN THE INTERVAL PHIRAMP TIMES THE CELL '/
     +       ' THICKNESS. THE VALUE SPECIFIED FOR PHIRAMP IS ',E12.5,/
     +       ' WELLS WITH REDUCED PUMPING WILL BE '
     +       'REPORTED TO FILE UNIT NUMBER',I5)
   30 FORMAT(1X,' Pumping rates will be read from time ',
     +                 'series input files. ',I10,' files will be read')
   31 FORMAT(1X,' Pumping rates will be read from time ',
     +                 'series input files. A maximum of ',I10,
     +                 ' row entries will be read per file')
   32 FORMAT(1X,' Option to reduce pumping during cell ',
     +                 'dewatering is activated and NWT solver ',I10,
     +                 ' is not being used. Option deactivated')
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
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
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
      character(len=22)  :: text      = 'AGO STRESS PERIOD DATA'
      character(len=17)  :: text1     = 'IRRIGATION STREAM'
      character(len=16)  :: text2     = 'IRRIGATION WELL'
      character(len=16)  :: text3     = 'SUPPLEMENTAL WELL'
      character(len=16)  :: text4     = 'IRRSFR'
      character(len=16)  :: text5     = 'IRRWEL'
      character(len=16)  :: text6     = 'SUPWEL'
      character(len=16)  :: text7     = 'STRESSPERIOD'
      character(len=16)  :: text8     = 'END'

      INTEGER LLOC,ISTART,ISTOP
      INTEGER NAUX,J,TABUNIT,II
      logical :: FOUND
      logical :: found1,found2,found3
      REAL :: R,TTIME,TRATE
      CHARACTER*6 CWELL
C     ------------------------------------------------------------------
C
C1----READ WELL INFORMATION DATA FOR STRESS PERIOD (OR FLAG SAYING REUSE AGO DATA).
      IF ( KPER.EQ.1 ) THEN
        IF ( NUMTAB.EQ.0 ) THEN
          CALL ULSTRD(NNPWEL,WELL,1,NWELVL,MXWELL,1,IN,IOUT,
     1             'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     2              WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
        ELSE
          DO J = 1, NUMTAB
            READ(IN,*)TABUNIT,TABVAL(J),TABLAY(J),TABROW(J),TABCOL(J)
            IF ( TABUNIT.LE.0 ) THEN
              WRITE(IOUT,100)
              CALL USTOP('')
            END IF
            REWIND(TABUNIT)   !IN CASE FILES ARE REUSED FOR MULTIPLE WELLS
            DO II = 1, TABVAL(J)
              LLOC = 1
              CALL URDCOM(TABUNIT,IOUT,LINE)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TTIME,IOUT,TABUNIT)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TRATE,IOUT,TABUNIT)
              TABTIME(II,J) = TTIME
              TABRATE(II,J) = TRATE
            END DO
          END DO
        END IF
C
C3------PRINT NUMBER OF WELLS USED FOR SUP OR IRR.
        CWELL=' WELLS'
        IF(NWELLS.EQ.1) CWELL=' WELL '
        WRITE(IOUT,101) NWELLS,CWELL
  101 FORMAT(1X,/1X,I6,A)
  100 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'UNIT NUMBER FOR TABULAR INPUT FILE SPECIFIED AS ZERO.')
      END IF
C
C4-------READ AG OPTIONS DATA FOR STRESS PERIOD (OR FLAG SAYING REUSE AGO DATA).
      FOUND = .FALSE.
      found1 = .FALSE.
      found2 = .FALSE.
      found3 = .FALSE.
      if ( NUMIRRSFR == 0 ) found1 = .true.
      if ( NUMIRRWEL == 0 ) found2 = .true.
      if ( NUMSUP == 0 ) found3 = .true.
      write(iout,'(/1x,a)') 'PROCESSING '//
     +            trim(adjustl(text))
        DO
          CALL URDCOM(In, IOUT, line)
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
          select case (LINE(ISTART:ISTOP))
          case('STRESSPERIOD')
            write(iout,'(/1x,a)') 'READING '// trim(adjustl(text)) //''
            found = .true.
          case('IRRSFR')
            found1 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +      trim(adjustl(text1)) //''
            CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,IN)
            IF ( ITMP > 0 ) THEN
              CALL IRRSFR(IN,IOUT,ITMP)
            ELSE IF ( KPER == 1 ) THEN
               WRITE(IOUT,*) 'Key word '//trim(adjustl(text4))
     +                 //' specified with no additional input.'
               CALL USTOP('Keyvword '//trim(adjustl(text4))
     +                //'  specified with no additional input.') 
            END IF
            IF ( .NOT. FOUND ) THEN
               WRITE(IOUT,*) 'Key word '//trim(adjustl(text4))
     +             //' found without key word '// trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text4))
     +             //'  found without key word '// trim(adjustl(text7)))
            END IF
          case('IRRWEL')
            found2 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +            trim(adjustl(text2)) //''
            CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,IN)
            IF ( ITMP > 0 ) THEN
              CALL IRRWEL(IN,ITMP)
            ELSE IF ( KPER == 1 ) THEN
               WRITE(IOUT,*) 'Key word '//trim(adjustl(text5))
     +                 //' specified with no additional input.'
               CALL USTOP('Keyvword '//trim(adjustl(text5))
     +                //'  specified with no additional input.') 
            END IF
            IF ( .NOT. FOUND ) THEN
               WRITE(IOUT,*) 'Key word '//trim(adjustl(text5))
     +             //' found without key word '// trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text5))
     +             //'  found without key word '// trim(adjustl(text7)))
            END IF
         case('SUPWEL')
            found3 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +            trim(adjustl(text3)) //''
                        CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,IN)
            IF ( ITMP > 0 ) THEN
              CALL SUPWEL(IN,ITMP)
            ELSE IF ( KPER == 1 ) THEN
               WRITE(IOUT,*) 'Key word '//trim(adjustl(text6))
     +                 //' specified with no additional input.'
               CALL USTOP('Keyvword '//trim(adjustl(text6))
     +                //'  specified with no additional input.') 
            END IF
             IF ( .NOT. FOUND ) THEN
               WRITE(IOUT,*) 'Key word '//trim(adjustl(text6))
     +             //' found without key word '// trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text6))
     +             //'  found without key word '// trim(adjustl(text7)))
            END IF
         case default
C
C5-------- NO KEYWORDS FOUND DURING FIRST STRESS PERIOD SO TERMINATE
                WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
                CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP))
         case ('END')
            IF ( .NOT. FOUND ) THEN
               WRITE(IOUT,*)
               WRITE(IOUT,*) 'Key word '//trim(adjustl(text8))
     +             //' found without key word '// trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text8))
     +             //'  found without key word '// trim(adjustl(text7)))
            END IF
           if ( kper == 1 ) then
             if(.not. found1 .or. .not. found2 .or. .not. found3) then
C
C6-------- NO KEYWORDS FOUND DURING FIRST STRESS PERIOD SO TERMINATE
                WRITE(IOUT,*)
                WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
                CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP))
             end if
           else
             if ( .not. found1 ) then
                WRITE(IOUT,6)
             end if
             if ( .not. found2 ) then
                WRITE(IOUT,7)
             end if
             if ( .not. found3 ) then
                WRITE(IOUT,8)
             end if
           end if
           write(iout,'(/1x,a)') 'END PROCESSING '//
     +            trim(adjustl(text)) //' OPTIONS'
           exit
        end select
      end do  
    6    FORMAT(1X,/
     1      1X,'REUSING IRRSFR DATA ',
     2       'FROM LAST STRESS PERIOD')
    7 FORMAT(1X,/
     1      1X,'REUSING IRRWEL DATA ',
     2       'FROM LAST STRESS PERIOD')
    8           FORMAT(1X,/
     1      1X,'REUSING SUPWEL DATA ',
     2       'FROM LAST STRESS PERIOD')
C
C7------RETURN
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
      SUBROUTINE SUPWEL(IN,ITMP)
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
C2------READ LIST OF DIVERSION SEGEMENTS FOR CALCALATING SUPPLEMENTAL PUMPING
C
      IERR = 0
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
          READ(IN,*)SUPWELVAR(J),NUMSEGS(ISPWL),
     +                    (PCTSUP(K,ISPWL),SFRSEG(K,ISPWL),K=1,NMSG)
          DO K = 1, NUMSEGS(SUPWELVAR(J))
            IF ( SFRSEG(K,SUPWELVAR(J)) == 0 ) IERR = 1
          END DO
        END DO
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
      SUBROUTINE IRRWEL(IN,ITMP)
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
      IERR = 0
      NUMIRRWELSP = ITMP
! READ LIST OF IRRIGATION CELLS FOR EACH WELL        
!
        IF ( NUMIRRWELSP > NUMIRRWEL )THEN
          WRITE(IOUT,*)
          WRITE(IOUT,104)NUMIRRWEL,NUMIRRWELSP
          CALL USTOP('')
        END IF
        DO J = 1, NUMIRRWELSP
          CALL URDCOM(IN,IOUT,LINE)
          LLOC = 1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRWL,R,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NMCL,R,IOUT,IN)
          IF ( NMCL > MAXCELLSWEL )THEN
            WRITE(IOUT,*)
            WRITE(IOUT,105)MAXCELLSWEL,NMCL
            CALL USTOP('')
          END IF
          BACKSPACE(IN)
          READ(IN,*)IRRWELVAR(J),NUMCELLS(IRWL),(IRRFACT(K,IRWL),
     +        IRRPCT(K,IRWL),UZFROW(K,IRWL),UZFCOL(K,IRWL),K=1,NMCL)
          DO K = 1, NUMCELLS(IRRWELVAR(J))
            IF ( UZFROW(K,IRRWELVAR(J))==0 .OR. 
     +                                UZFCOL(K,IRRWELVAR(J))==0 ) THEN
              WRITE(IOUT,106) 
              CALL USTOP('')   
            END IF
          END DO
        END DO
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
      SUBROUTINE IRRSFR(IN,IOUT,ITMP)
C  READ DIVERSION SEGMENT DATA FOR EACH STRESS PERIOD
      USE GWFAGOMODULE
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
C1
C----READ IRRIGATION SEGEMENT INFORMATION.
C
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
          IF ( NMCL > MAXCELLSSFR ) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,9009)MAXCELLSSFR,NMCL
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
      SUBROUTINE GWF2AGO7FM(Kkper, Kkstp, Kkiter, Iunitnwt)
C     ******************************************************************
C     CALCULATE APPLIED IRRIGATION, DIVERISONS, AND PUMPING
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:DELR,DELC,issflg,IBOUND,HNEW,LBOTM,BOTM,
     +                       RHS
      USE GWFBASMODULE, ONLY:TOTIM
      USE GWFAGOMODULE
      USE GWFSFRMODULE, ONLY: SGOTFLW, NSTRM, ISTRM
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      USE GWFNWTMODULE, ONLY: A, IA, Heps, Icell
      IMPLICIT NONE
C
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: KKPER, KKSTP, KKITER, Iunitnwt
C
C        VARIABLES:
C     ------------------------------------------------------------------
      INTEGER NWELLSTEMP,L,I,J,ISTSG,ICOUNT,IRR,ICC,IC,IR,IL,IJ
      DOUBLE PRECISION :: ZERO, SUP, FMIN,Q,SUBVOL,SUBRATE,DVT
      DOUBLE PRECISION :: H,T,B,dQ
      EXTERNAL :: SMOOTH3, RATETERP
      REAL :: RATETERP,TIME
      DOUBLE PRECISION Qp,Hh,Ttop,Bbot,dQp,SMOOTH3
C      
C     ------------------------------------------------------------------
      ZERO=0.0D0
      WELLIRR = 0.0
      TIME = TOTIM
      ACTUAL = 0.0
      SUP = 0.0
      SFRIRR = 0.0   
      SUPFLOW = 0.0
      Qp = 0.0
      TIME = TOTIM
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
C4------CALCULATE DIVERSION SHORTFALL TO SET SUPPLEMENTAL PUMPING DEMAND
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
            IF ( NUMTAB.LE.0 ) THEN
              IR=WELL(2,L)
              IC=WELL(3,L)
              IL=WELL(1,L)
              Q=WELL(4,L)
            ELSE
              IR = TABROW(L)
              IC = TABCOL(L)
              IL = TABLAY(L)
              Q = RATETERP(TIME,L)
            END IF
C
C5------CHECK IF SUPPLEMENTARY PUMPING RATE EXCEEDS MAX ALLOWABLE RATE
            IF ( SUPFLOW(L) < Q ) SUPFLOW(L) = Q
            Q = SUPFLOW(L)
C
C6------IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.
            IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C7------IF THE CELL IS VARIABLE HEAD THEN SUBTRACT Q FROM
C       THE RHS ACCUMULATOR.
            IF ( Q .LT. ZERO .AND. IUNITNWT.NE.0 ) THEN
              IF ( LAYTYPUPW(il).GT.0 ) THEN
                Hh = HNEW(ic,ir,il)
                bbot = Botm(IC, IR, Lbotm(IL))
                ttop = Botm(IC, IR, Lbotm(IL)-1)
                Qp = Q*smooth3(Hh,Ttop,Bbot,dQp)
                RHS(IC,IR,IL)=RHS(IC,IR,IL)-Qp
C
C8------Derivative for RHS
                ij = Icell(IC,IR,IL)
                A(IA(ij)) = A(IA(ij)) + dQp*Q
              ELSE
                RHS(IC,IR,IL)=RHS(IC,IR,IL)-Q
                Qp = Q
              END IF
            ELSE
              RHS(IC,IR,IL)=RHS(IC,IR,IL)-Q
              Qp = Q
            END IF
  100 CONTINUE
C
C3------SET ACTUAL SUPPLEMENTAL PUMPING BY DIVERSION FOR IRRIGATION.
      SUP = 0.0
            DO I = 1, NUMSEGS(L)
              J = SFRSEG(I,L) 
              SUP = SUP - Qp
              ACTUAL(J)  = ACTUAL(J) + SUP
            END DO
          END IF
        END IF
! APPLY IRRIGATION FROM WELLS
        IF ( NUMIRRWELSP > 0 ) THEN
          IF ( NUMCELLS(L) > 0 ) THEN
            DO I = 1, NUMCELLS(L)
              SUBVOL = -(1.0-IRRFACT(I,L))*Qp*IRRPCT(I,L)
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
              dvt = dvt/(DELR(icc)*DELC(irr))
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
      SUBROUTINE GWF2AGO7BD(KKSTP,KKPER,Iunitnwt)
C     ******************************************************************
C     CALCULATE VOLUMETRIC FOR AG OPTIONS (DIVERSIONS AND PUMPING)
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,DELR,DELC,issflg,NCOL,NROW,NLAY,
     1                      IBOUND,HNEW,BUFF,BOTM,LBOTM
      USE GWFBASMODULE,ONLY:ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBNM,VBVL
      USE GWFAGOMODULE
      USE GWFSFRMODULE, ONLY: SGOTFLW, NSTRM, ISTRM
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      IMPLICIT NONE
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN):: KKSTP,KKPER,Iunitnwt
C        VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      DOUBLE PRECISION :: RATIN,RATOUT,FMIN,ZERO,DVT,RIN,ROUT
      double precision :: SUP,SUBVOL,SUBRATE
      real :: Q,TIME,RATETERP
      INTEGER :: NWELLSTEMP,L,I,J,ISTSG,ICOUNT,IL
      INTEGER :: IRR,ICC,IC,IR,IBD,IBDLBL,IW1,NAUX,MSUM
      EXTERNAL :: SMOOTH3, RATETERP
      DOUBLE PRECISION :: SMOOTH3,bbot,ttop,hh
      DOUBLE PRECISION :: Qp,QQ,Qsave,dQp
      DATA TEXT /'SUPPLEMENTARY WELLS'/
C     ------------------------------------------------------------------
      ZERO=0.0D0
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      WELLIRR = 0.0
      TIME = TOTIM
      ACTUAL = 0.0
      SUP = 0.0
      SFRIRR = 0.0   
      SUPFLOW = 0.0
      Qp = 1.0
      NWELLSTEMP = NWELLS
      IF ( NUMTAB.GT.0 ) NWELLSTEMP = NUMTAB
      
      IF(IWELCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IWELCB.GT.0) IBD=ICBCFL
      IBDLBL=0
      iw1 = 1
C
C3------CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
50    CONTINUE
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NWELVL-5
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KKSTP,KKPER,TEXT,NAUX,WELAUX,IWELCB,NCOL,NROW,NLAY,
     1          NWELLS,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C2------IF DEMAND BASED ON ET DEFICIT THEN CALCULATE VALUES
      IF ( ETDEMANDFLAG > 0 .AND. issflg(kkper) == 0 ) THEN
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
            Q=ZERO
            QQ=ZERO
          END IF
        END IF
        IF ( NUMTAB.LE.0 ) THEN
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING WELL.
          IR=WELL(2,L)
          IC=WELL(3,L)
          IL=WELL(1,L)
C
C5C-----GET FLOW RATE FROM WELL LIST.
          QSAVE=WELL(4,L)
        ELSE
          IR = TABROW(L)
          IC = TABCOL(L)
          IL = TABLAY(L)
          QSAVE = RATETERP(TIME,L)
        END IF
C
        bbot = Botm(IC, IR, Lbotm(IL))
        ttop = Botm(IC, IR, Lbotm(IL)-1)
        Hh = HNEW(ic,ir,il)
C
C5B-----IF THE CELL IS NO-FLOW OR CONSTANT HEAD, IGNORE IT.
C-------CHECK IF PUMPING IS NEGATIVE AND REDUCE FOR DRYING CONDITIONS.
C
        IF(IBOUND(IC,IR,IL) > 0 ) THEN
          IF ( Qsave.LT.zero  .AND. Iunitnwt.NE.0) THEN
            IF ( LAYTYPUPW(il).GT.0 ) THEN
              Qp = smooth3(Hh,Ttop,Bbot,dQp)
              Q = Qsave*Qp
            ELSE
              Q = Qsave
            END IF
          ELSE
            Q = Qsave
          END IF
          QQ=Q
C
C3------SET ACTUAL SUPPLEMENTAL PUMPING BY DIVERSION FOR IRRIGATION.
          SUP = 0.0
          DO I = 1, NUMSEGS(L)
            J = SFRSEG(I,L) 
            IF ( Q < 0.0 ) SUP = SUP - Q
            ACTUAL(J)  = ACTUAL(J) + SUP
          END DO
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
        END IF
        ! write wells with reduced pumping
        IF ( Qp.LT.0.9999D0 .AND. Iunitnwt.NE.0 .AND. 
     +     IPRWEL.NE.0 .and. Qsave < ZERO ) THEN
          IF ( iw1.EQ.1 ) THEN
            WRITE(IUNITRAMP,*)
            WRITE(IUNITRAMP,300)KKPER,KKSTP
            WRITE(IUNITRAMP,400)
          END IF
          WRITE(IUNITRAMP,500)IL,IR,IC,QSAVE,Q,hh,bbot
          iw1 = iw1 + 1 
        END IF
C
C5E-----ADD FLOW RATE TO BUFFER.
        BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+QQ
C
C5F-----SEE IF FLOW IS POSITIVE OR NEGATIVE.
        IF(QQ.GE.ZERO) THEN
C
C5G-----FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
          RATIN=RATIN+QQ
        ELSE
C
C5H-----FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
          RATOUT=RATOUT-QQ
        END IF
C
C5I-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  ALSO
C5I-----COPY FLOW TO WELL LIST.
   99 IF(IBD.EQ.2) CALL UBDSVB(IWELCB,NCOL,NROW,IC,IR,IL,Q,
     1                  WELL(:,L),NWELVL,NAUX,5,IBOUND,NLAY)
      WELL(NWELVL,L)=QQ
      END DO
      
   61 FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
   62 FORMAT(1X,'WELL ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1      '   RATE ',1PG15.6)
  300 FORMAT(' WELLS WITH REDUCED PUMPING FOR STRESS PERIOD ',I5,
     1      ' TIME STEP ',I5)
  400 FORMAT('   LAY   ROW   COL         APPL.Q          ACT.Q',
     1       '        GW-HEAD       CELL-BOT')
  500 FORMAT(3I6,4E15.6)
!
C5D-----PRINT FLOW RATE IF REQUESTED.
       IF(IBDLBL.EQ.0.AND.IBD.LT.0) WRITE(IOUT,61) TEXT,KKPER,KKSTP
       DO L=1,NWELLSTEMP
         IF ( NUMTAB.LE.0 ) THEN
           IR=WELL(2,L)
           IC=WELL(3,L)
           IL=WELL(1,L)
         ELSE
           IR = TABROW(L)
           IC = TABCOL(L)
           IL = TABLAY(L)  
         END IF
         IF(IBD.LT.0) THEN
              WRITE(IOUT,62) L,IL,IR,IC,WELL(NWELVL,L)
         END IF
       END DO
C
      IF (iw1.GT.1 )WRITE(IUNITRAMP,*)
C
C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KKSTP,KKPER,TEXT,IWELCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUM=MSUM+1
C APPLY IRRIGATION FROM DIVERSIONS
      DO l = 1, NSTRM
        istsg = ISTRM(4, l)
        IF ( istsg.GT.1 .AND. NUMIRRSFRSP > 0 ) THEN
          IF ( DVRCH(istsg) .GT. 0) THEN
            DO icount = 1, DVRCH(istsg)
              irr = IRRROW(icount,istsg)
              icc = IRRCOL(icount,istsg)
              dvt = SGOTFLW(istsg)*DVRPERC(ICOUNT,istsg)
              dvt = dvt/(DELR(icc)*DELC(irr))
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
      USE GWFSFRMODULE, ONLY: SEG,SGOTFLW
      USE GWFAGOMODULE
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
      if(k==2)then
      write(222,333)Kkper, Kkstp, Kkiter,ic,ir,iseg,dum,
     +              aet,SFRIRR(ic,ir),WELLIRR(ic,ir),SGOTFLW(iseg)
 333  format(6i6,5e20.10)
      end if
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
        DEALLOCATE(SUPWELVAR)
        DEALLOCATE(IRRWELVAR)
        DEALLOCATE(NUMSEGS)
        DEALLOCATE(MAXSEGS)
        DEALLOCATE(MAXCELLSWEL)
        DEALLOCATE(NUMCELLS)
        DEALLOCATE(WELLIRR)
        DEALLOCATE(IRRFACT)
        DEALLOCATE(IRRPCT)
       DEALLOCATE(PSIRAMP) 
        DEALLOCATE(IUNITRAMP) 
        DEALLOCATE(NWELLS)
        DEALLOCATE(MXWELL)
        DEALLOCATE(NWELVL)
        DEALLOCATE(IWELCB)
        DEALLOCATE(IPRWEL)
        DEALLOCATE(NPWEL)
        DEALLOCATE(IWELPB)
        DEALLOCATE(NNPWEL)
        DEALLOCATE(WELAUX)
        DEALLOCATE(WELL)
        DEALLOCATE(NUMTAB)
        DEALLOCATE(MAXVAL)
        DEALLOCATE(TABTIME)
        DEALLOCATE(TABRATE)
        DEALLOCATE(TABVAL)
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
      DEALLOCATE (MAXCELLSSFR)
      DEALLOCATE (DEMAND)
      DEALLOCATE (ACTUAL)
      DEALLOCATE (SUPACT)
      DEALLOCATE (NUMIRRWELSP)
C
      RETURN
      END
