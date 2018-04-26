      MODULE GWFAWUMODULE
! from well package
        INTEGER,SAVE,POINTER  :: NWELLS,MXWELL,NWELVL,NPWEL,IPRWEL
        INTEGER,SAVE,POINTER  :: IWELLCB,IRDPSI,NNPWEL,NAUX,ISFRCB
        INTEGER,SAVE,POINTER  :: IWELLCBU
        INTEGER,SAVE,POINTER  :: IRRWELLCB,IRRSFRCB
        LOGICAL, SAVE,POINTER :: TSACTIVEGW, TSACTIVESW
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::WELAUX
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::SFRAUX
        REAL,             SAVE, DIMENSION(:,:), POINTER     ::WELL
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::TABTIME
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::TABRATE
        REAL,             SAVE, DIMENSION(:),   POINTER     ::QONLY
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABLAY
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABROW
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABCOL
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABVAL
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABID
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABUNIT
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TSSWUNIT
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TSGWUNIT
        REAL,             SAVE,                 POINTER     ::PSIRAMP
        INTEGER,          SAVE,                 POINTER     ::IUNITRAMP
        INTEGER,          SAVE,                 POINTER     ::NUMTAB
        INTEGER,          SAVE,                 POINTER     ::MAXVAL
        REAL,              SAVE, DIMENSION(:,:), POINTER ::VBVLAG
        CHARACTER(LEN=20), SAVE, DIMENSION(:),   POINTER ::VBNMAG
        INTEGER, SAVE, POINTER  ::MSUMAG
! AWU orig
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::SFRSEG
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::UZFROW
        INTEGER,          SAVE, DIMENSION(:,:),   POINTER     ::UZFCOL
        REAL,             SAVE, DIMENSION(:,:),   POINTER :: AETITERSW
        REAL,             SAVE, DIMENSION(:,:),   POINTER :: AETITERGW
        REAL,             SAVE, DIMENSION(:,:),   POINTER ::WELLIRRUZF
        REAL,             SAVE, DIMENSION(:,:),   POINTER :: WELLIRRPRMS
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::IRRFACT
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::IRRPCT
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::SUPWELVAR
        REAL,             SAVE, DIMENSION(:),   POINTER     ::SUPFLOW
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::IRRWELVAR
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::PCTSUP
        INTEGER,          SAVE,                 POINTER     ::NUMSUP
        INTEGER,          SAVE,                 POINTER     ::NUMSUPSP
        INTEGER,          SAVE,                 POINTER     ::UNITSUP
        INTEGER,          SAVE,                 POINTER     ::NUMIRRWEL
        INTEGER,          SAVE,                 POINTER     ::UNITIRRWEL
        INTEGER,          SAVE, DIMENSION(:), POINTER  :: NUMSUPWELLSEG
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
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: SFRIRRUZF
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: SFRIRRPRMS  !(store IRRIGATION AMOUNTS)
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: DVRPERC  !(Percentage of diversion applied to each cell)
        REAL,   SAVE,  DIMENSION(:,:),POINTER:: DVEFF  !(store efficiency factor)
!        REAL,   SAVE,  DIMENSION(:,:),POINTER:: KCROP  !(crop coefficient)
        REAL,   SAVE,  DIMENSION(:),  POINTER:: DEMAND,SUPACT
        REAL,   SAVE,  DIMENSION(:),  POINTER:: ACTUAL
      END MODULE GWFAWUMODULE


      SUBROUTINE GWF2AWU7AR(IN,IUNITNWT)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR AWU PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,IFREFM
      USE GWFAWUMODULE
      USE GWFSFRMODULE, ONLY:NSEGDIM
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS
      integer,intent(in) :: IN,IUNITNWT
C     ------------------------------------------------------------------
C        VARIABLES
!      CHARACTER(len=16) :: text        = ' AWU PACKAGE '
      CHARACTER(len=200) :: LINE
      INTEGER :: MXACTWSUP,MXACTWIRR,NUMSUPHOLD,NUMIRRHOLD
      INTEGER :: MAXSEGSHOLD,NUMCOLS,NUMROWS,MAXCELLSHOLD,NUMCELLSHOLD
      INTEGER :: NUMTABHOLD,LLOC,ISTART,ISTOP,I,N
      REAL :: R
C     ------------------------------------------------------------------
      ALLOCATE(VBVLAG(4,10),VBNMAG(10),MSUMAG)
      ALLOCATE(NWELLS,MXWELL,NWELVL,IWELLCB,ISFRCB,NAUX,WELAUX(20))
      ALLOCATE(IRRWELLCB,IRRSFRCB,IWELLCBU)
      ALLOCATE(PSIRAMP,IUNITRAMP)
      ALLOCATE(NUMTAB,MAXVAL,NPWEL,NNPWEL,IPRWEL)
      ALLOCATE(TSACTIVEGW,TSACTIVESW)
      VBVLAG = 0.0
      MSUMAG = 0
      PSIRAMP = 0.10
      NUMTAB = 0
      MAXVAL = 1
      IPRWEL = 1
      NAUX = 0
      TSACTIVEGW=.FALSE.
      TSACTIVESW=.FALSE.
!
      ALLOCATE(MAXVAL,NUMSUP,NUMIRRWEL,UNITSUP,MAXCELLSWEL)
      ALLOCATE(NUMSUPSP,MAXSEGS,NUMIRRWELSP)
      ALLOCATE(ETDEMANDFLAG,NUMIRRSFR,NUMIRRSFRSP)
      ALLOCATE (MAXCELLSSFR)
      NWELLS=0
      NNPWEL=0
      MXWELL=0
      NWELVL=0
      IWELLCB=0
      IWELLCBU=0
      ISFRCB=0
      IRRWELLCB=0
      IRRSFRCB=0
      IUNITRAMP=IOUT
      MAXVAL = 1
      NUMSUP = 0
      NUMSUPSP = 0
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
    1 FORMAT(1X,/1X,'AWU -- AWU PACKAGE FOR NWT VERSION 1.1.3, ',
     1' 8/01/2017 INPUT READ FROM UNIT ',I4)
C
C
C2-------CHECK FOR KEYWORDS.  
C
      CALL PARSEAWU7OPTIONS(In, Iout, Iunitnwt)
      NNPWEL = MXWELL
C
C3-------ALLOCATE ARRAYS FOR TIME SERIES OUTPUT
C
      ALLOCATE(TSSWUNIT(NSEGDIM),TSGWUNIT(MXWELL),QONLY(MXWELL))
      TSSWUNIT = 0
      TSGWUNIT = 0
      QONLY = 0.0
C
C4------READ TS
      IF ( TSACTIVEGW .OR. TSACTIVESW ) CALL TSREAD(IN,IOUT)
C
C3-------ALLOCATE VARIABLES FOR TIME SERIES WELL INPUT RATES
      NUMTABHOLD = NUMTAB
      IF ( NUMTABHOLD.EQ.0 ) NUMTABHOLD = 1
      ALLOCATE(TABTIME(MAXVAL,NUMTABHOLD),TABRATE(MAXVAL,NUMTABHOLD))
      ALLOCATE(TABLAY(MXWELL),TABROW(MXWELL),TABCOL(MXWELL))
      ALLOCATE(TABVAL(MXWELL),TABID(MXWELL),TABUNIT(MXWELL))
      TABTIME = 0.0
      TABRATE = 0.0
      TABLAY = 0
      TABROW = 0
      TABCOL = 0
      TABVAL = 0
      TABID = 0
      TABUNIT = 0
C
C6------THERE ARE FOUR INPUT VALUES PLUS ONE LOCATION FOR
C6------CELL-BY-CELL FLOW.
      NWELVL=5+NAUX
C
C7------ALLOCATE SPACE FOR THE WELL DATA.
      IF(MXWELL.LT.1) THEN
         WRITE(IOUT,17)
   17    FORMAT(1X,
     1'No wells active in the AWU Package')
         MXWELL = 1
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
     +         NUMSEGS(MXWELL),PCTSUP(MAXSEGSHOLD,MXACTWSUP))
      ALLOCATE(UZFROW(MAXCELLSHOLD,MXACTWIRR),
     +         UZFCOL(MAXCELLSHOLD,MXACTWIRR),IRRWELVAR(NUMIRRHOLD),
     +         WELLIRRUZF(NUMCOLS,NUMROWS),NUMCELLS(MXACTWIRR))
      ALLOCATE(WELLIRRPRMS(MAXCELLSHOLD,MXACTWIRR))
      ALLOCATE(AETITERGW(MAXCELLSHOLD,MXACTWIRR))
      ALLOCATE (IRRFACT(MAXCELLSHOLD,MXACTWIRR),
     +           IRRPCT(MAXCELLSHOLD,MXACTWIRR),SUPFLOW(MXACTWSUP))
      ALLOCATE (NUMSUPWELLSEG(NUMSUPHOLD))
      SFRSEG = 0
      UZFROW = 0
      UZFCOL = 0
      SUPWELVAR = 0
      IRRWELVAR = 0
      WELLIRRUZF = 0.0
      WELLIRRPRMS = 0.0
      NUMCELLS = 0
      IRRFACT = 0.0
      IRRPCT = 0.0
      NUMSEGS = 0
      PCTSUP = 0.0
      SUPFLOW = 0.0
      AETITERGW = 0.0
      NUMSUPWELLSEG = 0
C
C-------allocate for SFR AWUptions
      IF ( NUMIRRSFR > 0 ) THEN
        ALLOCATE (DVRCH(NSEGDIM),DVEFF(MAXCELLSSFR,NSEGDIM))
!        ALLOCATE (KCROP(MAXCELLSSFR,NSEGDIM))
        ALLOCATE (IRRROW(MAXCELLSSFR,NSEGDIM))
        ALLOCATE (IRRCOL(MAXCELLSSFR,NSEGDIM)) 
        ALLOCATE(AETITERSW(MAXCELLSSFR,NSEGDIM))
        ALLOCATE (DVRPERC(MAXCELLSSFR,NSEGDIM))  
        ALLOCATE (SFRIRRUZF(NCOL,NROW),SFRIRRPRMS(MAXCELLSSFR,NSEGDIM))
        ALLOCATE (IRRSEG(NSEGDIM))
      ELSE
        ALLOCATE (DVRCH(1),DVEFF(1,1)) !  ,KCROP(1,1))      
        ALLOCATE (IRRROW(1,1),IRRCOL(1,1))  
        ALLOCATE (DVRPERC(1,1))  
        ALLOCATE (SFRIRRUZF(1,1),SFRIRRPRMS(1,1))  
        ALLOCATE (IRRSEG(1),AETITERSW(1,1))
      END IF
      DVRCH = 0    
      DVEFF = 0.0
!      KCROP = 0.0
      IRRROW = 0  
      IRRCOL = 0
      AETITERSW = 0.0
      SFRIRRUZF = 0.0
      SFRIRRPRMS = 0.0
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
      SUBROUTINE PARSEAWU7OPTIONS(IN,IOUT,IUNITNWT)
C     ******************************************************************
C     READ AWU OPTIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IUNIT
      USE GWFAWUMODULE
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
      character(len=16)  :: text        = 'AWU'
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
        case('SUPPLEMENTAL_WELL')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMSUP,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXSEGS,R,IOUT,IN)
            IF(NUMSUP.LT.0) NUMSUP=0
            IF ( IUNIT(44) < 1 ) THEN
              WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' SFR2 must be active for this option'
              CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' SFR2 must be active for this option')
            END IF
            WRITE(IOUT,*)
            WRITE(IOUT,33) NUMSUP
            WRITE(IOUT,*)
            found1 = .true.
            found = .true.
! Pumped water will be added as irrigation
        case('IRRIGATION_WELL')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMIRRWEL,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXCELLSWEL,R,IOUT,IN)
            IF( NUMIRRWEL.LT.0 ) NUMIRRWEL = 0
            IF ( MAXCELLSWEL < 1 ) MAXCELLSWEL = 1 
            WRITE(IOUT,*)
            WRITE(IOUT,34) NUMIRRWEL
            WRITE(IOUT,*)
            found = .true.
            found1 = .true.
! Max number of SUP and IRR wells
        case('MAXWELLS')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXWELL,R,IOUT,IN)
            IF( MXWELL.LT.0 ) MXWELL = 0
            WRITE(IOUT,*)
            WRITE(IOUT,36) MXWELL
            WRITE(IOUT,*)
! Option to output list for wells  
        case('WELLLIST')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELLCB,R,IOUT,IN)
            WRITE(IOUT,*)
            WRITE(IOUT,37) IWELLCB
            WRITE(IOUT,*)
! Option to output list for wells  
        case('WELLCBC')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELLCBU,R,IOUT,IN)
            WRITE(IOUT,*)
            WRITE(IOUT,37) IWELLCBU
            WRITE(IOUT,*)
! Option to output list for segments
        case('SFRLIST')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISFRCB,R,IOUT,IN)
            WRITE(IOUT,*)
            WRITE(IOUT,37) ISFRCB
            WRITE(IOUT,*)
! Option to output list for irrigation segments   
        case('SFRIRRLIST')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRRSFRCB,R,IOUT,IN)
            WRITE(IOUT,*)
            WRITE(IOUT,37) IRRSFRCB
            WRITE(IOUT,*)
! Option to output list for irrigation wells
        case('WELLIRRLIST')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRRWELLCB,R,IOUT,IN)
            WRITE(IOUT,*)
            WRITE(IOUT,37) IRRWELLCB
            WRITE(IOUT,*)
! Option to output time series by SW right 
        case('TIMESERIES_SFR')
            TSACTIVESW=.TRUE. 
            WRITE(IOUT,*)
            WRITE(IOUT,39)
            WRITE(IOUT,*)
! Option to output time series by GW right
        case('TIMESERIES_WELL')
            TSACTIVEGW=.TRUE.
            WRITE(IOUT,*)
            WRITE(IOUT,40)
            WRITE(IOUT,*)
! Option to turn off writing to LST file
        case('NOPRINT')
            IPRWEL = 0
            WRITE(IOUT,*)
            WRITE(IOUT,38)
            WRITE(IOUT,*)
! REDUCING PUMPING FOR DRY CELLS
        case('PHIRAMP')
! CHECK THAT THERE ARE SUP OR IRR WELLS FIRST
          if ( found1 ) then
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PSIRAMP,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITRAMP,R,IOUT,IN)
            IF ( IUNITNWT.EQ.0 ) THEN
              WRITE(IOUT,*)
              write(IOUT,32)
              WRITE(IOUT,*)
            ELSE
              IF(PSIRAMP.LT.1.0E-5) PSIRAMP=1.0E-5
              IF ( IUNITRAMP.EQ.0 ) IUNITRAMP = IOUT
              WRITE(IOUT,*)
              WRITE(IOUT,29) PSIRAMP,IUNITRAMP
              WRITE(IOUT,*)
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
            WRITE(IOUT,*)
            WRITE(IOUT,30) NUMTAB
            WRITE(IOUT,*)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
            IF(MAXVAL.LT.0) THEN
                MAXVAL=1
                NUMTAB=0
            END IF
            WRITE(IOUT,*)
            WRITE(IOUT,31) MAXVAL
            WRITE(IOUT,*)
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
        case('IRRIGATION_SFR')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMIRRSFR,R,IOUT,IN)  !#SEGMENTS
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXCELLSSFR,R,IOUT,IN)   !MAX NUMBER OF CELLS
            IF( NUMIRRSFR.LT.0 ) NUMIRRSFR = 0
            IF ( MAXCELLSSFR < 1 ) MAXCELLSSFR = 1 
            IF ( IUNIT(44) < 1 ) THEN
              WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' SFR2 must be active for this option'
              CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' SFR2 must be active for this option')
            END IF
            WRITE(IOUT,*)
            WRITE(IOUT,35) NUMIRRSFR
            WRITE(IOUT,*)
            found = .true.
        case('ETDEMAND')
              ETDEMANDFLAG = 1
              WRITE(iout,*)
              WRITE(IOUT,'(A)')' AGRICULTURAL DEMANDS WILL BE '//
     +        'CALCULATED USING ET DEFICIT'
              WRITE(iout,*)
              IF ( IUNIT(55) < 1 ) THEN
                WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' UZF1 must be active for this option'
                CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
     +                   //' UZF1 must be active for this option')   ! ALSO NEED TO CHECK THAT UNSAT FLOW AND UZET IS ACTIVE!!!!!
              END IF
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
      IF ( NUMIRRWEL > MXWELL) THEN
        WRITE(IOUT,*) 'THE VALUE SPECIFIED FOR NUMIRRWEL: ',NUMIRRWEL,
     +  'IS GREATER THAN THE MAXIMUM NUMBER OF AWU WELLS: ',MXWELL
        CALL USTOP('NUMIRRWEL IS GREATER THAN MAX AWU WELLS')
      END IF
      IF ( NUMSUP > MXWELL) THEN
        WRITE(IOUT,*) 'THE VALUE SPECIFIED FOR NUMSUP: ',NUMSUP,
     +  'IS GREATER THAN THE MAXIMUM NUMBER OF AWU WELLS: ',MXWELL
        CALL USTOP(' NUMSUP IS GREATER THAN MAX AWU WELLS')
      END IF
C
   29 FORMAT(1X,'NEGATIVE PUMPING RATES WILL BE REDUCED IF HEAD '/
     +       ' FALLS WITHIN THE INTERVAL PHIRAMP TIMES THE CELL '/
     +       ' THICKNESS. THE VALUE SPECIFIED FOR PHIRAMP IS ',E12.5,/
     +       ' WELLS WITH REDUCED PUMPING WILL BE '
     +       'REPORTED TO FILE UNIT NUMBER',I5)
   30 FORMAT(1X,' PUMPING RATES WILL BE READ FROM TIME ',
     +                 'SERIES INPUT FILE. ',I10,' FILES WILL BE READ')
   31 FORMAT(1X,' PUMPING RATES WILL BE READ FROM TIME ',
     +                 'SERIES INPUT FILE. A MAXIMUM OF ',I10,
     +                 ' ROW ENTRIES WILL BE READ FROM EACH FILE')
   32 FORMAT(1X,' OPTION TO REDUCE PUMPING DURING CELL ',
     +                 'DEWATERING IS ACTIVATED AND NWT SOLVER ',I10,
     +                 ' IS NOT BEING USED. OPTION DEACTIVATED')
   33 FORMAT(1X,'OPTION TO PUMP SUPPLEMENTARY WATER ',
     +          'FOR SURFACE DIVERSION SHORTFALL IS ACTIVATED. '
     +          ' A TOTAL OF    ',I10, ' SUPPLEMENTAL WELLS ARE ACTIVE')
   34 FORMAT(1X,'OPTION TO APPLY PUMPED WATER AS IRRIGATION IS ACTIVE. '
     +,'PUMPED IRRIGATION WATER WILL BE APPLIED TO ',I10,' CELLS/RHUS.')
   35 FORMAT(1X,'OPTION TO APPLY SURFACE WATER AS IRRIGATION IS ACTIVE.'
     +,' DIVERTED SURFACE WATER WILL BE APPLIED TO ',I10,' CELLS/RHUS.')
   36 FORMAT(1X,'THE MAXIMUM NUMBER OF WELLS FOR SUPPLEMENTING'
     +,' DIVERSIONS OR APPLYING IRRIGATION IS ',I10,' WELLS.')
   37 FORMAT(1X,' UNFORMATTED CELL BY CELL RATES FOR SUP AND IRR WELLS'
     +,' WILL BE SAVED TO FILE UNIT NUMBER ',I10)
   38 FORMAT(1X,' PRINTING OF WELL LISTS IS SUPPRESSED')
   39 FORMAT(1X,' SURFACE WATER IRRIGATION, POTENTIAL AND ACTUAL ET'
     +,' WILL BE SAVED TO TIMES SERIES OUTPUT FILES.')
   40 FORMAT(1X,' GROUND WATER IRRIGATION, POTENTIAL AND ACTUAL ET'
     +,' WILL BE SAVED TO TIMES SERIES OUTPUT FILES.')
      END SUBROUTINE     
C
C
      SUBROUTINE GWF2AWU7RP(IN,KPER)
C     ******************************************************************
C     READ AWU DATA FOR A STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFAWUMODULE
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
      character(len=22)  :: text      = 'AWU STRESS PERIOD DATA'
      character(len=17)  :: text1     = 'IRRIGATION STREAM'
      character(len=16)  :: text2     = 'IRRIGATION WELL'
      character(len=16)  :: text3     = 'SUPPLEMENTAL WELL'
      character(len=16)  :: text4     = 'IRRSFR'
      character(len=16)  :: text5     = 'IRRWEL'
      character(len=16)  :: text6     = 'SUPWEL'
      character(len=16)  :: text7     = 'STRESS PERIOD'
      character(len=16)  :: text8     = 'END'
      character(len=16)  :: char1     = 'WELL LIST'

      INTEGER LLOC,ISTART,ISTOP,ISTARTSAVE
      INTEGER J,II,KPER2,L,MATCH,NUMTABS
      logical :: FOUND
      logical :: found1,found2,found3,found4
      REAL :: R,TTIME,TRATE
      CHARACTER*6 CWELL
C     ------------------------------------------------------------------
      found4 = .false.
C
C1----READ WELL INFORMATION DATA FOR STRESS PERIOD (OR FLAG SAYING REUSE AWU DATA).
      IF ( KPER.EQ.1 ) THEN
        CALL URDCOM(In, IOUT, line)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        ISTARTSAVE = ISTART
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        do 
          select case (LINE(ISTARTSAVE:ISTOP))
          case('WELL LIST')
            found4 = .true.
            write(iout,'(/1x,a)') 'PROCESSING '//
     +                    trim(adjustl(CHAR1)) //''
            IF ( NUMTAB.EQ.0 ) THEN
              CALL ULSTRD(NNPWEL,WELL,1,NWELVL,MXWELL,1,IN,IOUT,
     1             'LAYER   ROW   COL   MAX STRESS RATE',
     2              WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
              DO L=1,NNPWEL
                IF ( WELL(4,L) > 0.0 ) THEN
                  WRITE(IOUT,*)
                  WRITE(IOUT,*) 'ERROR: MAX AWU PUMPING RATE IN LIST',
     +                        ' IS POSITIVE AND SHOULD BE NEGATIVE.',
     +                        ' MODEL STOPPING'
                  WRITE(IOUT,*)
                  CALL USTOP('ERROR: MAX AWU PUMPING RATE IN LIST IS POS
     +ITIVE AND SHOULD BE NEGATIVE. MODEL STOPPING')
                END IF
              END DO
            ELSE
              NUMTABS = 0
              MATCH = 0
              DO J = 1, MXWELL    !RGN 1/22/18 SPECIFY A FILE FOR EACH WELL
                READ(IN,*)TABUNIT(J),TABVAL(J),TABLAY(J),
     +                    TABROW(J),TABCOL(J)
                DO I = 1, J - 1
                  IF ( TABUNIT(I) == TABUNIT(J) ) THEN
                    MATCH = 1
                    TABID(J) = TABID(I)
                  END IF
                END DO
                IF ( MATCH == 0 ) THEN
                  NUMTABS = NUMTABS + 1
                  TABID(J) = NUMTABS
                END IF
                IF ( TABUNIT(J).LE.0 ) THEN
                  WRITE(IOUT,100)
                  CALL USTOP('')
                END IF
                REWIND(TABUNIT(J))   !IN CASE FILES ARE REUSED FOR MULTIPLE WELLS
                DO II = 1, TABVAL(J)
                  LLOC = 1
                  CALL URDCOM(TABUNIT(J),IOUT,LINE)
                  CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TTIME,IOUT,
     +                      TABUNIT(J))
                  CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TRATE,IOUT,
     +                      TABUNIT(J))
                  IF ( TRATE > 0.0 ) THEN
                    WRITE(IOUT,*)
                    WRITE(IOUT,*) 'ERROR: MAX AWU PUMPING RATE IN LIST',
     +                        ' IS POSITIVE AND SHOULD BE NEGATIVE.',
     +                        ' MODEL STOPPING'
                    WRITE(IOUT,*)
                  CALL USTOP('ERROR: MAX AWU PUMPING RATE IN LIST IS POS
     +ITIVE AND SHOULD BE NEGATIVE. MODEL STOPPING')
                  END IF
                  TABTIME(II,TABID(J)) = TTIME
                  TABRATE(II,TABID(J)) = TRATE
                END DO
              END DO
            END IF
          case ('END')
            found4 = .false.
            write(iout,'(/1x,a)') 'FINISHED READING '//
     +                            trim(adjustl(char1))
            exit
          case default
            WRITE(IOUT,*) 'Invalid AWU Input: '//LINE(ISTART:ISTOP)
     +                 //' Should be: '// trim(adjustl(CHAR1))
            CALL USTOP('Invalid AWU Input: '//LINE(ISTART:ISTOP)
     +                 //' Should be: '// trim(adjustl(CHAR1)))
          end select
          if ( found4 ) then
            CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
          end if
        end do
C
C3------PRINT NUMBER OF WELLS USED FOR SUP OR IRR.
        NWELLS = MXWELL
        CWELL=' WELLS'
        IF(NWELLS.EQ.1) CWELL=' WELL '
        WRITE(IOUT,101) NWELLS,CWELL
  101 FORMAT(1X,/1X,I6,A)
  100 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'UNIT NUMBER FOR TABULAR INPUT FILE SPECIFIED AS ZERO.')
      END IF
C
C4-------READ AG OPTIONS DATA FOR STRESS PERIOD (OR FLAG SAYING REUSE AWU DATA).
      FOUND = .FALSE.
      found1 = .FALSE.
      found2 = .FALSE.
      found3 = .FALSE.
      found4 = .false.
      if ( NUMIRRSFR == 0 ) found1 = .true.
      if ( NUMIRRWEL == 0 ) found2 = .true.
      if ( NUMSUP == 0 ) found3 = .true.
      write(iout,'(/1x,a)') 'PROCESSING '//
     +            trim(adjustl(text))
        CALL URDCOM(In, IOUT, line)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        ISTARTSAVE = ISTART
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        DO
          select case (LINE(ISTARTSAVE:ISTOP))
          case('STRESS PERIOD')
            found4 = .true.
            write(iout,'(/1x,a)') 'READING '// trim(adjustl(text)) //''
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KPER2,R,IOUT,IN)
            IF ( KPER /= KPER2 ) THEN
              WRITE(IOUT,*) 'INCORRECT PERIOD FOR '//trim(adjustl(text))
     +                 //' SPECIFIED. CHECK INPUT.'
              CALL USTOP('INCORRECT PERIOD FOR  '//trim(adjustl(text))
     +                //'  SPECIFIED. CHECK INPUT.') 
            END IF
            found = .true.
          case('IRRSFR')
            found1 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +      trim(adjustl(text1)) //''
            CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,IN)
            CALL IRRSFR(IN,IOUT,ITMP)
            IF ( KPER == 1 .AND. ITMP < 0 ) THEN
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
          case('IRRWELL')
            found2 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +            trim(adjustl(text2)) //''
            CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,IN)
            CALL IRRWEL(IN,ITMP)
            IF ( KPER == 1 .AND. ITMP < 0  ) THEN
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
         case('SUPWELL')
            found3 = .true.
            write(iout,'(/1x,a)') 'READING '//
     +            trim(adjustl(text3)) //''
                        CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,IN)
            CALL SUPWEL(IN,ITMP)
            IF ( KPER == 1 .AND. ITMP < 0  ) THEN
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
            found4 = .false.
            IF ( .NOT. FOUND ) THEN
               WRITE(IOUT,*)
               WRITE(IOUT,*) 'Key word '//trim(adjustl(text8))
     +             //' found without key word '// trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text8))
     +             //'  found without key word '// trim(adjustl(text7)))
            END IF
! rgn don't need the following code because farms may not be active during first period.
!           if ( kper == 1 ) then
!             if(.not. found1 .or. .not. found2 .or. .not. found3) then
!C
!C6-------- NO KEYWORDS FOUND DURING FIRST STRESS PERIOD SO TERMINATE
!                WRITE(IOUT,*)
!                WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
!     +                   //' Option: '//LINE(ISTART:ISTOP)
!                CALL USTOP('Invalid '//trim(adjustl(text))
!     +                   //' Option: '//LINE(ISTART:ISTOP))
!             end if
!           else
             if ( .not. found1 ) then
                WRITE(IOUT,6)
             end if
             if ( .not. found2 ) then
                WRITE(IOUT,7)
             end if
             if ( .not. found3 ) then
                WRITE(IOUT,8)
             end if
!           end if
           write(iout,'(/1x,a)') 'END PROCESSING '//
     +            trim(adjustl(text)) //' OPTIONS'
           exit
        end select
        if ( found4 ) then
          CALL URDCOM(In, IOUT, line)
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
          end if
      end do  
    6    FORMAT(1X,/
     1      1X,'NO IRRSFR DATA OR REUSING IRRSFR DATA ',
     2       'FROM LAST STRESS PERIOD ')
    7 FORMAT(1X,/
     1      1X,'NO IRRWEL DATA OR REUSING IRRWEL DATA ',
     2       'FROM LAST STRESS PERIOD')
    8           FORMAT(1X,/
     1      1X,'NO SUPWEL OR REUSING SUPWEL DATA ',
     2       'FROM LAST STRESS PERIOD')
C
C7------RETURN
      RETURN
      END
C
C
      SUBROUTINE GWF2AWU7AD(IN,KPER)
C     ******************************************************************
C     UPDATE DEMANDS FOR NEW TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFAWUMODULE
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
        IF ( NUMSUP>0 ) THEN
          DEMAND(ISEG) = SEG(2, ISEG)
          SUPACT(ISEG) = 0.0
        END IF
      END DO
C2------RESET SAVED AET FROM LAST ITERATION
      AETITERSW = 0.0
      AETITERGW = 0.0
      QONLY = 0.0
!      CALL UZFIRRDEMANDSET()
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
      USE GWFAWUMODULE
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     ARGUMENTS:
      INTEGER, INTENT(IN)::IN,ITMP
C     ------------------------------------------------------------------
C     VARIABLES:
      CHARACTER(LEN=200)::LINE
      INTEGER :: IERR, LLOC, ISTART, ISTOP, J, ISPWL
      INTEGER :: NMSG, K, IRWL, NMCL, L, LL
      REAL :: R
C     ------------------------------------------------------------------
C
C
C1----REUSE VALUES FROM PREVIOUS STRESS PERIOD.
      IF (ITMP < 0 ) RETURN
C
C1----INACTIVATE ALL IRRIGATION WELLS.
      IF (ITMP == 0 ) THEN
        NUMSUPSP = 0
        NUMSEGS = 0
        SUPWELVAR = 0
        NUMSEGS = 0
        SFRSEG = 0
        PCTSUP =0.0
        RETURN
      END IF
C
C2--- INITIALIZE AG VARIABLES TO ZERO.
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
          SUPWELVAR(J) = ISPWL
          NUMSEGS(ISPWL) = NMSG
          DO K=1,NMSG
            READ(IN,*) SFRSEG(K,ISPWL),PCTSUP(K,ISPWL)
          END DO
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
C3---------CALCULATE THE NUMBER OF SUPWELLS ASSOCIATED WITH A DIVERSION SEGEMENT
C
      NUMSUPWELLSEG = 1
      DO L=1, NUMSUPSP
        DO LL=L+1,NUMSUPSP
          IF ( SFRSEG(1,LL) == SFRSEG(1,L) ) 
     +                        NUMSUPWELLSEG(L) = NUMSUPWELLSEG(L) + 1
        END DO
        DO LL=1,L-1
          IF ( SFRSEG(1,LL) == SFRSEG(1,L) ) 
     +                        NUMSUPWELLSEG(L) = NUMSUPWELLSEG(L) + 1
        END DO
      END DO
      
C
C4------RETURN
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
      USE GWFAWUMODULE
!      USE PRMS_MODULE, ONLY: GSFLOW_flag
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     ARGUMENTS:
      INTEGER, INTENT(IN)::IN,ITMP
C     ------------------------------------------------------------------
C     VARIABLES:
      CHARACTER(LEN=200)::LINE
      INTEGER :: IERR, LLOC, ISTART, ISTOP, J, ISPWL
      INTEGER :: NMSG, K, IRWL, NMCL, GSFLOW_flag
      REAL :: R
C     ------------------------------------------------------------------
C
C
C1----REUSE VALUES FROM PREVIOUS STRESS PERIOD.
      IF (ITMP < 0 ) RETURN
C
C2--- INITIALIZE AG VARIABLES TO ZERO.
      IRRWELVAR = 0
      NUMCELLS = 0
      IRRFACT = 0.0
      IRRPCT = 0.0
      UZFROW = 0
      UZFCOL = 0
      GSFLOW_flag = 0
C
C1----INACTIVATE ALL IRRIGATION WELLS.
      IF (ITMP == 0 ) THEN
        NUMIRRWELSP = 0
        RETURN
      END IF
C
C---READ NEW IRRIGATION WELL DATA
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
          IRRWELVAR(J) = IRWL
          NUMCELLS(IRWL) = NMCL
          IF ( GSFLOW_flag == 1 ) then   !uzfrow stores hru number for gsflow
            DO K = 1, NMCL
              READ(IN,*)UZFROW(K,IRWL),IRRFACT(K,IRWL),
     +                IRRPCT(K,IRWL)
            END DO
            DO K = 1, NUMCELLS(IRRWELVAR(J))
              IF ( UZFROW(K,IRRWELVAR(J))==0 ) THEN
                WRITE(IOUT,107) 
                CALL USTOP('')   
              END IF
            END DO
          ELSE
            DO K = 1, NMCL
              READ(IN,*)UZFROW(K,IRWL),UZFCOL(K,IRWL),IRRFACT(K,IRWL),
     +                IRRPCT(K,IRWL)
            END DO
            DO K = 1, NUMCELLS(IRRWELVAR(J))
              IF ( UZFROW(K,IRRWELVAR(J))==0 .OR. 
     +                                UZFCOL(K,IRRWELVAR(J))==0 ) THEN
                WRITE(IOUT,106) 
                CALL USTOP('')   
              END IF
            END DO
          END IF
        END DO
C
   99 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'WELL PACKAGE MUST BE ACTIVE TO SIMULATE IRRIGATION ',
     +        'WELLS')
  104 FORMAT('***Error in IRR WEL*** maximum number of irrigation ',
     +       'wells is less than the number specified in stress ',
     +       'period. ',/
     +       'Maximum wells and the number specified for stress '
     +       'period are: ',2i6)
  105 FORMAT('***Error in IRR WEL*** maximum number of cells ',
     +       'irrigated by a well is less than the number ',
     +       'specified in stress period. ',/
     +       'Maximum cells and the number specified for stress '
     +       'period are: ',2i6)
  106 FORMAT('***ERROR IN AWU PACKAGE*** cell row or column for ',
     +       'irrigation well specified as zero. Model stopping.')
  107 FORMAT('***ERROR IN AWU PACKAGE*** HRU ID for ',
     +       'irrigation well specified as zero. Model stopping.')
C
C6------RETURN
      RETURN
      END
!
! ----------------------------------------------------------------------
C
C-------SUBROUTINE SFRAWUPTIONS
      SUBROUTINE IRRSFR(IN,IOUT,ITMP)
C  READ DIVERSION SEGMENT DATA FOR EACH STRESS PERIOD
      USE GWFAWUMODULE
!      USE PRMS_MODULE, ONLY: GSFLOW_flag
      USE GLOBAL,       ONLY: IUNIT
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     ARGUMENTS
      INTEGER, INTENT(IN)::IN,IOUT,ITMP
C     ------------------------------------------------------------------
C     VARIABLES
C     ------------------------------------------------------------------
      INTEGER LLOC,ISTART,ISTOP,J,SGNM,NMCL,K, GSFLOW_flag
      REAL R 
      DOUBLE PRECISION :: totdum
      CHARACTER(LEN=200)::LINE
C     ------------------------------------------------------------------  
C
C1----REUSE VALUES FROM PREVIOUS STRESS PERIOD.
      IF (ITMP < 0 ) RETURN
C
C2--- INITIALIZE AG VARIABLES TO ZERO.
      DVRPERC = 0.0
      DVEFF = 0.0
      IRRSEG = 0
      IRRROW = 0
      IRRCOL = 0
      DVRCH = 0
      GSFLOW_flag = 0
C1----INACTIVATE ALL IRRIGATION SEGMENTS.
      IF (ITMP == 0 ) THEN
        NUMIRRSFRSP = 0
        RETURN
      END IF
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
            IRRSEG(J) = SGNM
            DVRCH(SGNM) = NMCL
            IF ( GSFLOW_flag == 1 ) then   !irrrow stores hru number for gsflow
              DO K=1,NMCL                
                READ(IN,*)IRRROW(K,SGNM),DVEFF(K,SGNM),
     +                    DVRPERC(K,SGNM)
              END DO
              totdum  = 0.0
              DO K = 1, NMCL
                IF ( IRRROW(K,SGNM)==0 ) THEN
                  totdum = totdum + DVRPERC(NMCL,SGNM)
                  WRITE(IOUT,9010)
                  CALL USTOP('')   
                  IF ( totdum.GT.1.000001 ) WRITE(Iout,9006)totdum
                END IF
              END DO
            ELSE
              DO K=1,NMCL                
                READ(IN,*)IRRROW(K,SGNM),IRRCOL(K,SGNM),
     +                    DVEFF(K,SGNM),DVRPERC(K,SGNM)
              END DO
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
          END IF
        END DO
!
 9005 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'SFR2 PACKAGE MUST BE ACTIVE TO SIMULATE IRRIGATION ',
     +        'FROM SEGEMENTS')
 9006 FORMAT(' ***Warning in AWU*** ',/
     +       'Fraction of diversion for each cell in group sums '/,
     +       'to a value greater than one. Sum = ',E10.5)
 9007 FORMAT('***Error in AWU*** cell row or column for irrigation',
     +       'cell specified as zero. Model stopping.')
 9008 FORMAT('***Error in AWU*** maximum number of irrigation ',
     +       'segments is less than the number specified in ',
     +       'stress period. ',/ 
     +       'Maximum segments and the number specified are: ',2i6)
 9009 FORMAT('***Error in AWU*** maximum number of irrigation ',
     +       'cells is less than the number specified in ',
     +       'stress period. ',/ 
     +       'Maximum cells and the number specified are: ',2i6)
 9010 FORMAT('***Error in AWU*** HRU_ID for irrigation',
     +       'cell specified as zero. Model stopping.')
C11-----RETURN.
      RETURN
      END
C
!
! ----------------------------------------------------------------------
C
C-------SUBROUTINE TSREAD
      SUBROUTINE TSREAD(IN,IOUT)
C  READ SEGMENTS AND WELLS WITH TIME SERIES OUTPUT
      USE GWFAWUMODULE
      USE GWFSFRMODULE, ONLY: NSEGDIM
      USE GLOBAL,       ONLY: IUNIT
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     ARGUMENTS
      INTEGER, INTENT(IN)::IN,IOUT
C     ------------------------------------------------------------------
C     VARIABLES
C     ------------------------------------------------------------------
      INTEGER intchk, Iostat, LLOC,ISTART,ISTOP,I,SGNM,UNIT,WLNM
      INTEGER ISTARTSAVE,NUMFOUNDSW,NUMFOUNDGW
      real :: R
      character(len=16)  :: text        = 'AWU'
      character(len=17)  :: char1     = 'TIME SERIES'
      character(len=200) :: line
C     ------------------------------------------------------------------  
C
C1--- INITIALIZE VARIABLES 
       NUMFOUNDSW=0
       NUMFOUNDGW=0
       !TSSWUNIT = 0
       !TSGWUNIT = 0
       CALL URDCOM(In, IOUT, line)
       LLOC=1
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
       ISTARTSAVE = ISTART
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
       do 
         select case (LINE(ISTARTSAVE:ISTOP))
          case('TIME SERIES')
            write(iout,'(/1x,a)') 'PROCESSING '//
     +                    trim(adjustl(CHAR1)) //''
          case('SFR')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,SGNM,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,UNIT,R,IOUT,IN)
            TSSWUNIT(SGNM) = UNIT
            NUMFOUNDSW = NUMFOUNDSW + 1
            IF ( SGNM > NSEGDIM ) THEN
              WRITE(IOUT,*) 'Bad segment number for AWU time series. '
              CALL USTOP('Bad segment number for AWU time series.')
            END IF
          case('WELL')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,WLNM,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,UNIT,R,IOUT,IN)
            TSGWUNIT(WLNM) = UNIT
            NUMFOUNDGW = NUMFOUNDGW + 1
            IF ( WLNM > MXWELL ) THEN
              WRITE(IOUT,*) 'Bad well number for AWU time series. '
              CALL USTOP('Bad well number for AWU time series.')
            END IF
          case ('END')
            write(iout,'(/1x,a)') 'FINISHED READING '//
     +                            trim(adjustl(char1))
            exit
          case default
            WRITE(IOUT,*) 'Invalid AWU Input: '//LINE(ISTART:ISTOP)
     +                 //' Should be: '// trim(adjustl(CHAR1))
            CALL USTOP('Invalid AWU Input: '//LINE(ISTART:ISTOP)
     +                 //' Should be: '// trim(adjustl(CHAR1)))
        end select
        CALL URDCOM(In, IOUT, line)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTARTSAVE,ISTOP,1,I,R,IOUT,IN)
      end do
C
C11-----output number of files activated for time series output.
      write(iout,6)NUMFOUNDSW
      write(iout,7)NUMFOUNDGW
C
    6 FORMAT(' A total number of ',i10,' AWU output time series files '
     +       'were activated for SURFACE WATER ')
    7 FORMAT(' A total number of ',i10,' AWU output time series files '
     +       'were activated for GROUNDWATER ')

C11-----RETURN.
      RETURN
      END
C
      SUBROUTINE GWF2AWU7FM(Kkper, Kkstp, Kkiter, Iunitnwt)
C     ******************************************************************
C     CALCULATE APPLIED IRRIGATION, DIVERISONS, AND PUMPING
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:DELR,DELC,IBOUND,HNEW,LBOTM,BOTM,
     +                       RHS
      USE GWFBASMODULE, ONLY:TOTIM,DELT
      USE GWFAWUMODULE
      USE GWFSFRMODULE, ONLY: SGOTFLW, NSTRM, ISTRM, IDIVAR
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      USE GWFNWTMODULE, ONLY: A, IA, Heps, Icell
!      USE PRMS_MODULE, ONLY: GSFLOW_flag
      IMPLICIT NONE
C
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: KKPER, KKSTP, KKITER, Iunitnwt
C
C        VARIABLES:
C     ------------------------------------------------------------------
      INTEGER :: NWELLSTEMP,L,I,J,ISTSG,ICOUNT,IRR,ICC,IC,IR,IL,IJ,LL
      DOUBLE PRECISION :: ZERO, DONE, SUP, FMIN,Q,SUBVOL,SUBRATE,DVT
      EXTERNAL :: SMOOTHQ, RATETERPQ,demandgw_uzf !, demandgw_prms
      REAL :: RATETERPQ,TIME
      DOUBLE PRECISION :: Qp,Hh,Ttop,Bbot,dQp,SMOOTHQ,QSW,NEARZERO
      DOUBLE PRECISION :: demandgw_uzf !, demandgw_prms, mf_q2prms_inch
      INTEGER :: ihru, GSFLOW_flag
      
C      
C     ------------------------------------------------------------------
      ZERO=0.0D0
      DONE=1.0D0
      NEARZERO = 1.0D-17
      WELLIRRUZF = ZERO
      WELLIRRPRMS = ZERO
      TIME = TOTIM
      SUP = ZERO
      SFRIRRUZF = ZERO 
      SFRIRRPRMS = ZERO
      SUPFLOW = ZERO
      ACTUAL = ZERO
      Qp = ZERO
      Q = ZERO
      QSW = ZERO
      TIME = TOTIM
      GSFLOW_flag = 0
C
C2------IF DEMAND BASED ON ET DEFICIT THEN CALCULATE VALUES
      IF ( ETDEMANDFLAG > 0 ) THEN
        IF ( GSFLOW_flag == 0 ) THEN
          CALL demandconjunctive_uzf()
        ELSE
 !         CALL demandconjunctive_prms()
        END IF
      END IF
            
C
C3------SET MAX NUMBER OF POSSIBLE WELLS.
      NWELLSTEMP = NWELLS
!      IF ( NUMTAB.GT.0 ) NWELLSTEMP = NUMTAB  Now mxwells/=numtab
C
C4------SET MAX PUMPING RATE OR IRR DEMAND FOR GW
      DO L=1,NWELLSTEMP 
        IF ( NUMTAB.LE.0 ) THEN
          IR=WELL(2,L)
          IC=WELL(3,L)
          IL=WELL(1,L)
          Q=WELL(4,L)   !This is just a limit for sup
        ELSE
          IR = TABROW(L)
          IC = TABCOL(L)
          IL = TABLAY(L)
          Q = RATETERPQ(TIME,TABID(L))  !For IRRWELLS that are not SUPWELLS
        END IF
        IF ( NUMIRRSFRSP + NUMIRRWELSP == 0 ) Q = 0.0
C
C6------IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.
        IF(IBOUND(IC,IR,IL) > 0) THEN
C
C7------CALCULATE SUPPLEMENTAL PUMPING FOR THIS WELL
          SUP = 0.0
          IF ( NUMSEGS(L) > 0 ) THEN
            DO I = 1, NUMSEGS(L)
              J = SFRSEG(I,L)
              IF ( ETDEMANDFLAG > 0 ) THEN
                FMIN = SUPACT(J)
                QSW = DEMAND(J)   !change the name of demand to duty 
              ELSE
                QSW = SGOTFLW(J)
                FMIN = DEMAND(J)   !change the name of demand to duty 
              END IF
              FMIN = PCTSUP(I,L)*(FMIN - QSW)
              IF ( FMIN < ZERO ) FMIN = ZERO
              SUP = SUP + FMIN
!            SUP = SUP - ACTUAL(J)
            END DO
!            IF ( SUP < ZERO ) SUP = ZERO
            SUPFLOW(L) = SUPFLOW(L) - SUP / dble(NUMSUPWELLSEG(L))
C
C5A------CHECK IF SUPPLEMENTARY PUMPING RATE EXCEEDS MAX ALLOWABLE RATE IN TABFILE
            IF ( SUPFLOW(L) < Q ) SUPFLOW(L) = Q
            Q = SUPFLOW(L)
          ELSE
C
C6------CALCULATE ETDEMAND IF NOT SUPPLEMENTAL WELL.
            IF ( ETDEMANDFLAG > 0 ) THEN
              IF ( GSFLOW_flag == 0 ) THEN
                Q = demandgw_uzf(l)
              ELSE
!                Q = demandgw_prms(l)  
              END IF
            END IF
          END IF
C
C7------IF THE CELL IS VARIABLE HEAD THEN SUBTRACT Q FROM
C       THE RHS ACCUMULATOR.
          IF ( IUNITNWT.NE.0 ) THEN
            IF ( LAYTYPUPW(il).GT.0 ) THEN
              Hh = HNEW(ic,ir,il)
              bbot = Botm(IC, IR, Lbotm(IL))
              ttop = Botm(IC, IR, Lbotm(IL)-1)
              Qp = Q*smoothQ(Hh,Ttop,Bbot,dQp)
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
C
C3------SET ACTUAL SUPPLEMENTAL PUMPING BY DIVERSION FOR IRRIGATION.
          SUP = 0.0
          DO I = 1, NUMSEGS(L)  ! need to test when multiple segs supportes by single well
            J = SFRSEG(I,L) 
            SUP = SUP - Qp
            ACTUAL(J)  = ACTUAL(J) + SUP
          END DO
! APPLY IRRIGATION FROM WELLS
          IF ( GSFLOW_flag == 0 ) THEN
            DO I = 1, NUMCELLS(L)
              SUBVOL = -(DONE-IRRFACT(I,L))*Qp*IRRPCT(I,L)
Convert irrigation for UZF to a rate per unit area
              SUBRATE = SUBVOL/(DELR(UZFCOL(I,L))*DELC(UZFROW(I,L)))
              WELLIRRUZF(UZFCOL(I,L),UZFROW(I,L)) = 
     +                  WELLIRRUZF(UZFCOL(I,L),UZFROW(I,L)) + SUBRATE
            END DO
          ELSE
            DO I = 1, NUMCELLS(L)
              SUBVOL = -(DONE-IRRFACT(I,L))*Qp*IRRPCT(I,L)
! Keep irrigation for PRMS as volumetric rate
              WELLIRRPRMS(I,L) = WELLIRRPRMS(I,L) + SUBVOL
            END DO
          END IF
        END IF
      END DO
C APPLY IRRIGATION FROM SW DIVERSIONS
      DO L = 1, NUMIRRSFRSP
        istsg = IRRSEG(L)
        IF ( GSFLOW_flag == 0 ) THEN
          DO icount = 1, DVRCH(istsg)   !THESE VARS COULD BE DIMENSIONED NUMIRRSFR TO SAVE MEMORY
            irr = IRRROW(icount,istsg)
            icc = IRRCOL(icount,istsg)
            dvt = SGOTFLW(istsg)*DVRPERC(ICOUNT,istsg)
            dvt = dvt/(DELR(icc)*DELC(irr))
Convert irrigation for UZF to a rate per unit area
            SFRIRRUZF(icc, irr) = SFRIRRUZF(icc, irr) + 
     +                         dvt*(1.0-DVEFF(ICOUNT,istsg))
          END DO
        ELSE
          DO icount = 1, DVRCH(istsg)   
!            IHRU = IRRROW(icount,istsg)
            dvt = SGOTFLW(istsg)*DVRPERC(ICOUNT,istsg)
            dvt = (1.0-DVEFF(ICOUNT,istsg))*dvt
! Keep irrigation for PRMS as volume
            SFRIRRPRMS(icount,l) = SFRIRRPRMS(icount,l) + dvt
          END DO
        END IF
      END DO
C
C3------RETURN
      RETURN
      END
C
      SUBROUTINE GWF2AWU7BD(kkstp,kkper,Iunitnwt)
C     ******************************************************************
C     CALCULATE FLOWS FOR AG OPTIONS (DIVERSIONS AND PUMPING)
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,DELR,DELC,NCOL,NROW,NLAY,
     1                      IBOUND,HNEW,BUFF,BOTM,LBOTM
      USE GWFBASMODULE,ONLY:ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBNM,VBVL,MSUM,IBUDFL
      USE GWFAWUMODULE
      USE GWFSFRMODULE, ONLY: SGOTFLW, NSTRM, ISTRM, SEG, IDIVAR
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
 !     USE PRMS_MODULE, ONLY: GSFLOW_flag
      IMPLICIT NONE
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN):: KKSTP,KKPER,Iunitnwt
C        VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*22 TEXT2,TEXT6,TEXT7,TEXT8,TEXT1,TEXT3,TEXT4,TEXT5
      DOUBLE PRECISION :: RATIN,RATOUT,FMIN,ZERO,DVT,RIN,ROUT
      DOUBLE PRECISION :: SUP,SUBVOL,SUBRATE,RATINAG,RATOUTAG,AREA
      DOUBLE PRECISION :: QSW,QSWIRR,QWELL,QWELLIRR,QWELLET,QSWET,DONE
      REAL :: Q,TIME,RATETERPQ,QIRR,BUDPERC
      INTEGER :: NWELLSTEMP,L,I,J,ISTSG,ICOUNT,IL
      INTEGER :: IC,IR,IBDLBL,IW1,ISEG
      INTEGER :: IBD1,IBD2,IBD3,IBD4,IBD5
      INTEGER :: TOTWELLCELLS,TOTSFRCELLS,IHRU,GSFLOW_flag
      EXTERNAL :: SMOOTHQ, RATETERPQ
      DOUBLE PRECISION :: SMOOTHQ,bbot,ttop,hh
      DOUBLE PRECISION :: Qp,QQ,Qsave,dQp
      DOUBLE PRECISION :: DONENEG, prms_inch2mf_q
      DATA TEXT1 /'AWU WELLS'/
      DATA TEXT2 /'DIVERSION SEGMENTS'/
      DATA TEXT3 /'SW IRRIGATION'/
      DATA TEXT4 /'GW IRRIGATION'/
      DATA TEXT5 /'SW RETURN FLOW'/
      DATA TEXT6 /'GW RETURN FLOW'/
      DATA TEXT7 /'EFFICIENCY FACTOR SWET'/
      DATA TEXT8 /'EFFICIENCY FACTOR GWET'/
C     ------------------------------------------------------------------
      ZERO=0.0D0
      DONE=1.0D0
      DONENEG = -1.0D0
      RATIN=ZERO
      RATOUT=ZERO
      RATINAG=ZERO
      RATOUTAG=ZERO
      QSW=ZERO
      QSWIRR=ZERO
      QSWET=ZERO
      QWELLIRR=ZERO
      QWELLET=ZERO
      QWELL=ZERO
      QIRR=ZERO
      TOTWELLCELLS=0
      TOTSFRCELLS=0
      TIME = TOTIM
      ACTUAL = ZERO
      SUP = ZERO
      MSUMAG = 1
      IBD1=0
      IBD2=0
      IBD3=0
      IBD4=0
      IBD5=0
      Qp = 1.0
      NWELLSTEMP = NWELLS
      IBDLBL=0
      iw1 = 1
      GSFLOW_flag = 0
! Budget output for wells
      IF(IWELLCB.LT.0 .AND. ICBCFL.NE.0) IBD1=IOUT 
      IF(IWELLCB.GT.0 .AND. ICBCFL.NE.0) IBD1=IWELLCB
! Unformatted cbc budget output for wells
      IF(IWELLCBU.GT.0 ) IBD5=ICBCFL
! Budeget output for segments    
      IF(ISFRCB.LT.0 .AND. ICBCFL.NE.0) IBD2=IOUT
      IF(ISFRCB.GT.0 .AND. ICBCFL.NE.0) IBD2=ISFRCB
! Budeget output for irrigation segments    
      IF(IRRSFRCB.LT.0 .AND. ICBCFL.NE.0) IBD3=IOUT
      IF(IRRSFRCB.GT.0 .AND. ICBCFL.NE.0) IBD3=IRRSFRCB
! Budeget output for irrigation wells    
      IF(IRRWELLCB.LT.0 .AND. ICBCFL.NE.0) IBD4=IOUT
      IF(IRRWELLCB.GT.0 .AND. ICBCFL.NE.0) IBD4=IRRWELLCB
C
C1------ADD UP TOTAL NUMBER OF WELL IRRIGATION CELLS DURING STRESS PERIOD.
      !DO L=1,NWELLSTEMP 
      !  TOTWELLCELLS = TOTWELLCELLS + NUMCELLS(L)
      !END DO
C
C1------ADD UP TOTAL NUMBER OF SFR IRRIGATION CELLS DURING STRESS PERIOD.   
      !DO L = 1, NUMIRRSFRSP
      !  J = IRRSEG(L)
      !  TOTSFRCELLS = TOTSFRCELLS + DVRCH(J)
      !END DO
C
C1------CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
50    CONTINUE
C
C
C4------SET MAX NUMBER OF POSSIBLE SUPPLEMENTARY WELLS.
      NWELLSTEMP = NWELLS
!      IF ( NUMTAB.GT.0 ) NWELLSTEMP = NUMTAB
C
C2-----IF CELL-BY-CELL PUMPING WILL BE SAVED AS A LIST(COMPACT BUDGET), 
C       WRITE HEADER.
         NAUX=NWELVL-5
         IF(IAUXSV.EQ.0) NAUX=0
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD5.EQ.2) THEN
      CALL UBDSV4(KKSTP,KKPER,TEXT1,NAUX,WELAUX,IWELLCBU,NCOL,NROW,NLAY,
     1          NWELLS,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C5------CALCULATE DIVERSION SHORTFALL TO SET SUPPLEMENTAL PUMPING DEMAND
      DO L=1,NWELLSTEMP 
        IF ( NUMTAB.LE.0 ) THEN
          IR=WELL(2,L)
          IC=WELL(3,L)
          IL=WELL(1,L)
          Q=WELL(4,L) 
        ELSE
          IR = TABROW(L)
          IC = TABCOL(L)
          IL = TABLAY(L)
          Q = RATETERPQ(TIME,TABID(L))
        END IF
C
C7------IF THE CELL IS NO-FLOW OR CONSTANT HEAD, IGNORE IT.
C-------CHECK IF PUMPING IS NEGATIVE AND REDUCE FOR DRYING CONDITIONS.
C
        IF(IBOUND(IC,IR,IL) > 0 ) THEN
C
C6------SUPPLEMENTAL WELL SET DEMAND.
          IF ( NUMSEGS(L) > 0 ) THEN
            Q = SUPFLOW(L)
          ELSE
C
C6------NOT SUPPLEMENTAL WELL SET DEMAND
            Q = DONENEG*QONLY(L)
          END IF
          QSAVE = Q
C
          bbot = Botm(IC, IR, Lbotm(IL))
          ttop = Botm(IC, IR, Lbotm(IL)-1)
          Hh = HNEW(ic,ir,il)
          IF ( Iunitnwt.NE.0 ) THEN
            IF ( LAYTYPUPW(il).GT.0 ) THEN
              Qp = smoothQ(Hh,Ttop,Bbot,dQp)
              Q = Q*Qp
            ELSE
              Q = Qsave
            END IF
          ELSE
            Q = Qsave
          END IF
          QQ=Q
C8------SET ACTUAL SUPPLEMENTAL PUMPING BY DIVERSION FOR IRRIGATION.
          SUP = 0.0
          DO I = 1, NUMSEGS(L)
            J = SFRSEG(I,L) 
            SUP = SUP - Q
            ACTUAL(J)  = ACTUAL(J) + SUP
          END DO
C
C9------CALCULATE IRRIGATION FROM WELLS
          IF ( NUMCELLS(L)>0 ) QWELL = QWELL + QQ
          DO I = 1, NUMCELLS(L)
            SUBVOL = -(1.0-IRRFACT(I,L))*QQ*IRRPCT(I,L)
            QWELLIRR=QWELLIRR+SUBVOL
            QWELLET=QWELLET-IRRFACT(I,L)*QQ*IRRPCT(I,L)
          END DO
C
C10------WRITE WELLS WITH REDUCED PUMPING
          IF ( Qp.LT.0.9999D0 .AND. Iunitnwt.NE.0 .AND. 
     +       IPRWEL.NE.0 .and. Qsave < ZERO ) THEN
            IF ( iw1.EQ.1 ) THEN
              WRITE(IUNITRAMP,*)
              WRITE(IUNITRAMP,300)KKPER,KKSTP
              WRITE(IUNITRAMP,400)
            END IF
            WRITE(IUNITRAMP,500)IL,IR,IC,QSAVE,Q,hh,bbot
            iw1 = iw1 + 1 
          END IF
C
C11A-----ADD FLOW RATE TO BUFFER.
          BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+QQ
C
C11D-----FLOW RATE IS ALWAYS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
          RATOUT=RATOUT-QQ
C
C11E-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  
C
          IF(IBD5.EQ.2) CALL UBDSVB(IWELLCBU,NCOL,NROW,IC,IR,IL,Q,
     1                  WELL(:,L),NWELVL,NAUX,5,IBOUND,NLAY)
C
C--------COPY FLOW TO WELL LIST.
          WELL(NWELVL,L)=QQ
        END IF
      END DO
C
C12-------APPLY IRRIGATION FROM DIVERSIONS
C         IF SAVING CELL-BY-CELL FLOWS IN A LIST (COMPACT BUDGET), WRITE SW IRRIGATION
C         AND DIVERTED SW.  
      DO L = 1, NUMIRRSFRSP
        istsg = IRRSEG(L)
        DO icount = 1, DVRCH(istsg)
          DVT = SGOTFLW(istsg)*DVRPERC(ICOUNT,istsg)
          QSW=QSW+DVT
          QSWET=QSWET+DVT*(DVEFF(ICOUNT,istsg))
          QSWIRR = QSWIRR + DVT*(DONE-DVEFF(ICOUNT,istsg))
        END DO
      END DO
!
C13-----PRINT PUMPING RATE IF REQUESTED.
      IF(IBD1.GT.0) THEN
        WRITE(IBD1,*)
        WRITE(IBD1,61) TEXT1,KKPER,KKSTP
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
          WRITE(IBD1,62) L,IL,IR,IC,WELL(NWELVL,L)
        END DO
        WRITE(IBD1,*)
      END IF
!
C13-----PRINT IRRIGATION DIVERSION RATE IF REQUESTED.
      IF(IBD2.GT.0) THEN
        WRITE(IBD2,*)
        WRITE(IBD2,61) TEXT2,KKPER,KKSTP
        DO L = 1, NUMIRRSFRSP
          istsg = IRRSEG(L)
          WRITE(IBD2,63) istsg,SGOTFLW(istsg)
        END DO
        WRITE(IBD2,*)
      END IF
!
C13-----PRINT APPLIED SW IRRIGATION FOR EACH CELL      
      IF(IBD3.GT.0) THEN
        WRITE(IBD3,*)
        WRITE(IBD3,61) TEXT3,KKPER,KKSTP
        DO L = 1, NUMIRRSFRSP
          ISTSG = IRRSEG(L)
          IF ( GSFLOW_flag == 0 ) THEN
            DO icount = 1, DVRCH(istsg)
              ir = IRRROW(icount,istsg)
              ic = IRRCOL(icount,istsg)
              AREA = DELR(ic)*DELC(ir)
              WRITE(IBD3,65) ISTSG,IR,IC,SFRIRRUZF(IC,IR)*AREA
            END DO
          ELSE
            DO icount = 1, DVRCH(istsg)
              ihru = IRRROW(icount,istsg)
              WRITE(IBD3,66) ISTSG,IHRU,SFRIRRPRMS(icount,l)
            END DO 
          END IF
        END DO
        WRITE(IBD3,*)
      END IF
C   
!
C13-----PRINT APPLIED GW IRRIGATION FOR EACH CELL
      IF(IBD4.GT.0) THEN
        WRITE(IBD4,*)
        WRITE(IBD4,61) TEXT4,KKPER,KKSTP
        DO L=1,NWELLSTEMP 
          IF ( GSFLOW_flag == 0 ) THEN
            DO I = 1, NUMCELLS(L)
              IC = UZFCOL(I,L)
              IR = UZFROW(I,L)
              AREA = DELR(ic)*DELC(ir)
              WRITE(IBD4,64) L,IR,IC,WELLIRRUZF(IC,IR)*AREA
            END DO
          ELSE
            DO I = 1, NUMCELLS(L)
              IHRU = UZFROW(I,L)
              WRITE(IBD4,67) L,IHRU,WELLIRRPRMS(I,L)
            END DO  
          END IF
        END DO
        WRITE(IBD4,*)
      END IF
C
      IF (iw1.GT.1 )WRITE(IUNITRAMP,*)
C
C14------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C-------CALL UBUDSV TO SAVE THEM.
      IF(IBD5.EQ.1) CALL UBUDSV(KKSTP,KKPER,TEXT1,IWELLCBU,BUFF,NCOL,
     1                          NROW,NLAY,IOUT)
C
C15------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
      RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT1
C
C16------INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUM=MSUM+1
C
C18------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING 
C        GW PUMPING   
      RIN = -QWELL   !pumping is always negative
      ROUT = 0.0
      VBVLAG(3,MSUMAG)=RIN
      VBVLAG(4,MSUMAG)=ROUT
      VBVLAG(1,MSUMAG)=VBVLAG(1,MSUMAG)+RIN*DELT
      VBVLAG(2,MSUMAG)=VBVLAG(2,MSUMAG)+ROUT*DELT
      VBNMAG(MSUMAG)=TEXT1
      MSUMAG = MSUMAG + 1
C
C18-------SW DIVERSIONS
      RIN = QSW
      ROUT = 0.0
      VBVLAG(3,MSUMAG)=RIN
      VBVLAG(4,MSUMAG)=ROUT
      VBVLAG(1,MSUMAG)=VBVLAG(1,MSUMAG)+RIN*DELT
      VBVLAG(2,MSUMAG)=VBVLAG(2,MSUMAG)+ROUT*DELT
      VBNMAG(MSUMAG)=TEXT2
      MSUMAG = MSUMAG + 1
C
C18-------GW IRRIGATION
      RIN = 0.0
      ROUT = QWELLIRR
      VBVLAG(3,MSUMAG)=RIN
      VBVLAG(4,MSUMAG)=ROUT
      VBVLAG(1,MSUMAG)=VBVLAG(1,MSUMAG)+RIN*DELT
      VBVLAG(2,MSUMAG)=VBVLAG(2,MSUMAG)+ROUT*DELT
      VBNMAG(MSUMAG)=TEXT4
      MSUMAG = MSUMAG + 1
C
C18-------SW IRRIGATION
      RIN = 0.0
      ROUT = QSWIRR
      VBVLAG(3,MSUMAG)=RIN
      VBVLAG(4,MSUMAG)=ROUT
      VBVLAG(1,MSUMAG)=VBVLAG(1,MSUMAG)+RIN*DELT
      VBVLAG(2,MSUMAG)=VBVLAG(2,MSUMAG)+ROUT*DELT
      VBNMAG(MSUMAG)=TEXT3
      MSUMAG = MSUMAG + 1
C
C18-------GW EFFICIENCY ET
      RIN = 0.0
      ROUT = QWELLET
      VBVLAG(3,MSUMAG)=RIN
      VBVLAG(4,MSUMAG)=ROUT
      VBVLAG(1,MSUMAG)=VBVLAG(1,MSUMAG)+RIN*DELT
      VBVLAG(2,MSUMAG)=VBVLAG(2,MSUMAG)+ROUT*DELT
      VBNMAG(MSUMAG)=TEXT8
      MSUMAG = MSUMAG + 1
C
C18-------SW EFFICIENCY ET
      RIN = 0.0
      ROUT = QSWET
      VBVLAG(3,MSUMAG)=RIN
      VBVLAG(4,MSUMAG)=ROUT
      VBVLAG(1,MSUMAG)=VBVLAG(1,MSUMAG)+RIN*DELT
      VBVLAG(2,MSUMAG)=VBVLAG(2,MSUMAG)+ROUT*DELT
      VBNMAG(MSUMAG)=TEXT7
      MSUMAG = MSUMAG + 1
      IF(IBUDFL.NE.0)
     1CALL SGWF2AWU7V(MSUMAG,VBNMAG,VBVLAG,KKSTP,KKPER,IOUT,BUDPERC)
C
   61 FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
   62 FORMAT(1X,'AWU WELL ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1      '   RATE ',1PG15.6)
   63 FORMAT(1X,'SEGMENT ',I6,'   RATE ',1PG15.6)
   64 FORMAT(1X,'AWU WELL ',I6,'   ROW ',I5,'   COL ',I5,
     1      '   RATE ',1PG15.6)
   65 FORMAT(1X,'SEGMENT ',I6,'   ROW ',I5,'   COL ',I5,
     1      '   RATE ',1PG15.6)
   66 FORMAT(1X,'SEGMENT ',I6,'   HRU ',I5,'   RATE ',1PG15.6)
   67 FORMAT(1X,'AWU WELL ',I6,'   HRU ',I5,'   RATE ',1PG15.6)
  300 FORMAT(' AWU WELLS WITH REDUCED PUMPING FOR STRESS PERIOD ',I5,
     1      ' TIME STEP ',I5)
  400 FORMAT('   LAY   ROW   COL         APPL.Q          ACT.Q',
     1       '        GW-HEAD       CELL-BOT')
  500 FORMAT(3I6,4E15.6)
C19------RETURN
      RETURN
      END
! ----------------------------------------------------------------------
C
      subroutine demandconjunctive_uzf()
!     ******************************************************************
!     demandconjunctive---- sums up irrigation demand using ET deficit
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFUZFMODULE, ONLY: GWET,UZFETOUT,PETRATE,VKS,Isurfkreject,
     +                        surfk
      USE GWFSFRMODULE, ONLY: SEG,SGOTFLW
      USE GWFAWUMODULE
      USE GLOBAL,     ONLY: DELR, DELC
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
! ----------------------------------------------------------------------
      !modules
      !arguments
      !dummy
      DOUBLE PRECISION :: factor, area, uzet, aet, pet, finfsum, fks
      double precision :: zerod2,zerod30,done,dzero,dum,pettotal, 
     +                    aettotal,dhundred
      integer :: k,iseg,ic,ir,i
! ----------------------------------------------------------------------
!
      zerod30 = 1.0d-30
      zerod2 = 1.0d-2
      done = 1.0d0
      dhundred = 100.0d0
      dzero = 0.0d0
C
C1------loop over diversion segments that supply irrigation
C
      do 300 i = 1, NUMIRRSFRSP
        iseg = IRRSEG(i)
        IF ( DEMAND(iseg) < zerod30 ) goto 300
        finfsum = dzero
C
C1------loop over cells irrigated diversion
C
        do k = 1, DVRCH(iseg)
           ic = IRRCOL(k,iseg)
           ir = IRRROW(k,iseg)
           area = delr(ic)*delc(ir)
           pet = PETRATE(ic,ir)
           uzet = uzfetout(ic,ir)/DELT
           aet = (gwet(ic,ir)+uzet)/area
           if ( aet < zerod30 ) aet = zerod30
           factor = pet/aet - done
           if( abs(AETITERSW(K,ISEG)-AET) < zerod2*pet ) factor = 0.0
           IF ( FACTOR > dhundred ) FACTOR = dhundred
           SUPACT(iseg) = SUPACT(iseg) + factor*pet*area
           if ( SUPACT(iseg) < dzero ) SUPACT(iseg) = dzero
           dum = pet
!if ( KCROP(K,ISEG) > zerod30 ) dum = pet/KCROP(K,ISEG)   !need this for PRMS
           pettotal = pettotal + pet
           aettotal = aettotal + aet
           AETITERSW(K,ISEG) = AET
        end do
C
C1------set diversion to demand
C
      SEG(2,iseg) = SUPACT(iseg)
C
C1------limit diversion to water right
C
      if ( SEG(2,iseg) > demand(ISEG) ) SEG(2,iseg) = demand(ISEG)
300   continue
      return
      end subroutine demandconjunctive_uzf
C
!      subroutine demandconjunctive_prms()
!!     ******************************************************************
!!     demandconjunctive---- sums up irrigation demand using ET deficit
!!     ******************************************************************
!!     SPECIFICATIONS:
!      USE GWFSFRMODULE, ONLY: SEG,SGOTFLW
!      USE GWFAWUMODULE
!      USE GWFBASMODULE, ONLY: DELT
!      USE PRMS_BASIN, ONLY: HRU_PERV
!      USE PRMS_FLOWVARS, ONLY: SOIL_MOIST,HRU_ACTET
!      USE PRMS_CLIMATEVARS, ONLY: POTET
!      USE GSFMODFLOW, ONLY: Mfl2_to_acre, Mfl_to_inch
!      IMPLICIT NONE
!! ----------------------------------------------------------------------
!      !modules
!      !arguments
!      !dummy
!      DOUBLE PRECISION :: factor, area, aet, pet, finfsum, fks
!      double precision :: zerod2,zerod30,done,dzero,dum,pettotal, 
!     +                    aettotal,dhundred,prms_inch2mf_q
!      integer :: k,iseg,hru_id,i
!! ----------------------------------------------------------------------
!!
!      zerod30 = 1.0d-30
!      zerod2 = 1.0d-2
!      done = 1.0d0
!      dhundred = 100.0d0
!      dzero = 0.0d0
!      prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
!C
!C1------loop over diversion segments that supply irrigation
!C
!      do 300 i = 1, NUMIRRSFRSP
!        iseg = IRRSEG(i)
!        IF ( DEMAND(iseg) < zerod30 ) goto 300
!        finfsum = dzero
!C
!C1------loop over hrus irrigated by diversion
!C
!        do k = 1, DVRCH(iseg)
!           hru_id = IRRROW(k,iseg)
!           area = hru_perv(hru_id)
!           pet = potet(hru_id)
!           aet = hru_actet(hru_id)
!           if ( aet < zerod30 ) aet = zerod30
!           factor = pet/aet - done
!           if( abs(AETITERSW(K,ISEG)-AET) < zerod2*pet ) factor = 0.0
!           IF ( FACTOR > dhundred ) FACTOR = dhundred
!! convert PRMS ET deficit to MODFLOW flow
!           SUPACT(iseg) = SUPACT(iseg) + factor*pet*area*prms_inch2mf_q
!           if ( SUPACT(iseg) < dzero ) SUPACT(iseg) = dzero
!           dum = pet
!!if ( KCROP(K,ISEG) > zerod30 ) dum = pet/KCROP(K,ISEG)   !need this for PRMS
!           pettotal = pettotal + pet
!           aettotal = aettotal + aet
!           AETITERSW(K,ISEG) = AET
!        end do
!C
!C1------set diversion to demand
!C
!      SEG(2,iseg) = SUPACT(iseg)
!C
!C1------limit diversion to water right
!C
!      if ( SEG(2,iseg) > demand(ISEG) ) SEG(2,iseg) = demand(ISEG)
!300   continue
!      return
!      end subroutine demandconjunctive_prms
C
      double precision function demandgw_uzf(l)
!     ******************************************************************
!     demandgw---- sums up irrigation demand using ET deficit for gw
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFUZFMODULE, ONLY: GWET,UZFETOUT,PETRATE
      USE GWFAWUMODULE
      USE GLOBAL,     ONLY: DELR, DELC
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
! ----------------------------------------------------------------------
      !modules
      !arguments
      integer, intent(in) :: l
      !dummy
      DOUBLE PRECISION :: factor, area, uzet, aet, pet, finfsum, fks
      double precision :: zerod2,zerod30,done,dzero,dum,dhundred,doneneg
      integer :: iseg,ic,ir,i
! ----------------------------------------------------------------------
!
      zerod30 = 1.0d-30
      zerod2 = 1.0d-2
      done = 1.0d0
      doneneg = -1.0d0
      dhundred = 100.0d0
      dzero = 0.0d0
      demandgw_uzf = DZERO
      DO I = 1, NUMCELLS(L)
        IC = UZFCOL(I,L)
        IR = UZFROW(I,L)
        area = delr(ic)*delc(ir)
        pet = PETRATE(ic,ir)
        uzet = uzfetout(ic,ir)/DELT
        aet = (gwet(ic,ir)+uzet)/area
        if ( aet < zerod30 ) aet = zerod2*pet
        factor = pet/aet - done
        if( abs(AETITERGW(I,L)-AET) < zerod2*pet ) factor = 0.0
        IF ( FACTOR > dhundred ) FACTOR = dhundred
        QONLY(L) = QONLY(L) + factor*pet*area
        dum = pet
!if ( KCROP(K,ISEG) > zerod30 ) dum = pet/KCROP(K,ISEG)   !need this for PRMS
        AETITERGW(I,L) = AET
      end do
      demandgw_uzf = doneneg*QONLY(L)
      end function demandgw_uzf
C
!      double precision function demandgw_prms(l)
!!     ******************************************************************
!!     demandgw---- sums up irrigation demand using ET deficit for gw
!!     ******************************************************************
!!     SPECIFICATIONS:
!      USE GWFAWUMODULE
!      USE GWFBASMODULE, ONLY: DELT
!      USE PRMS_BASIN, ONLY: HRU_PERV
!      USE PRMS_FLOWVARS, ONLY: SOIL_MOIST,HRU_ACTET
!      USE PRMS_CLIMATEVARS, ONLY: POTET
!      USE GSFMODFLOW, ONLY: Mfl2_to_acre, Mfl_to_inch
!      IMPLICIT NONE
!! ----------------------------------------------------------------------
!      !modules
!      !arguments
!      integer, intent(in) :: l
!      !dummy
!      DOUBLE PRECISION :: factor, area, aet, pet, prms_inch2mf_q
!      double precision :: zerod2,zerod30,done,dzero,dum,dhundred,doneneg
!      integer :: i,ihru
!! ----------------------------------------------------------------------
!!
!      zerod30 = 1.0d-30
!      zerod2 = 1.0d-2
!      done = 1.0d0
!      doneneg = -1.0d0
!      dhundred = 100.0d0
!      dzero = 0.0d0
!      demandgw_prms = DZERO
!      prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
!      DO I = 1, NUMCELLS(L)
!        ihru = UZFROW(I,L)
!        pet = potet(ihru)
!        aet = hru_actet(ihru)
!        area = HRU_PERV(ihru)
!        if ( aet < zerod30 ) aet = zerod2*pet
!        factor = pet/aet - done
!        if( abs(AETITERGW(I,L)-aet) < zerod2*pet ) factor = 0.0
!        IF ( factor > dhundred ) factor = dhundred
!!
!! convert PRMS ET deficit to MODFLOW flow
!!
!        QONLY(L) = QONLY(L) + factor*pet*area*prms_inch2mf_q
!        dum = pet
!!if ( KCROP(K,ISEG) > zerod30 ) dum = pet/KCROP(K,ISEG)   !need this for PRMS
!        AETITERGW(I,L) = AET
!      end do
!      demandgw_prms = doneneg*QONLY(L)
!      end function demandgw_prms
C
      subroutine timeseries(Kkper, Kkstp, Kkiter,numwells)
!     ******************************************************************
!     timeseries---- write AWU water use time series for SW and GW
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFUZFMODULE, ONLY: GWET,UZFETOUT,PETRATE
      USE GWFAWUMODULE
      USE GLOBAL,     ONLY: DELR, DELC
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
! ----------------------------------------------------------------------
      !modules
      !arguments
      integer, intent(in) :: kkper,kkstp,kkiter,numwells
      ! -- dummy
      double precision :: zerod2,done
      integer :: k,iseg,ic,ir,i,l,il
! ----------------------------------------------------------------------
!
      zerod2 = 1.0d-2
      done = 1.0d0
      DO L=1,numwells
        IF ( NUMTAB.LE.0 ) THEN
          IR=WELL(2,L)
          IC=WELL(3,L)
          IL=WELL(1,L)
        ELSE
          IR = TABROW(L)
          IC = TABCOL(L)
          IL = TABLAY(L)
        END IF
      END DO
      return
      end subroutine timeseries
!
!      subroutine UZFIRRDEMANDSET()
!!     ******************************************************************
!!     irrdemand---- sets initial crop demand to PET
!!     ******************************************************************
!!     SPECIFICATIONS:
!!      USE GWFUZFMODULE, ONLY: PETRATE
!      USE GWFAWUMODULE, ONLY: DVRCH,IRRROW,IRRCOL,NUMIRRSFRSP,IRRSEG
!      USE GWFSFRMODULE, ONLY: SEG
!      USE GLOBAL,     ONLY: DELR, DELC
!      IMPLICIT NONE
!! ----------------------------------------------------------------------
!      DOUBLE PRECISION :: area
!      integer :: k,iseg,ic,ir,i
!! ----------------------------------------------------------------------
!!
!      do i = 1, NUMIRRSFRSP
!        iseg = IRRSEG(i)
!        do k = 1, DVRCH(iseg)
!           ic = IRRCOL(k,iseg)
!           ir = IRRROW(k,iseg)
!           area = delr(ic)*delc(ir)
!           SEG(2,iseg) = 0.0d0
!        end do
!      end do
!      return
!      end subroutine UZFIRRDEMANDSET
! ----------------------------------------------------------------------
C
!      subroutine APPLYKCROP()
!!     ******************************************************************
!!     APPLYKCROP---- Apply crop ceofficient to ETo
!!     ******************************************************************
!!     SPECIFICATIONS:
!      USE GWFUZFMODULE, ONLY: PETRATE
!      USE GWFAWUMODULE, ONLY: DVRCH,IRRROW,IRRCOL,NUMIRRSFRSP,
!     +                        IRRSEG
!      IMPLICIT NONE
!! ----------------------------------------------------------------------
!      integer :: k,iseg,ic,ir,i
!! ----------------------------------------------------------------------
!!
!      do i = 1, NUMIRRSFRSP
!        iseg = IRRSEG(i)
!        do k = 1, DVRCH(iseg)
!           ic = IRRCOL(k,iseg)
!           ir = IRRROW(k,iseg)
!           PETRATE(ic,ir) = PETRATE(IC,IR)
!        end do
!      end do
!      return
!      END subroutine APPLYKCROP
C
      DOUBLE PRECISION FUNCTION smoothQ(H,T,B,dQ)
C     ******************************************************************
C     SMOOTHLY REDUCES PUMPING TO ZERO FOR DEWATERED CONDITIONS
C     ******************************************************************
! h is the depth 
      USE GWFAWUMODULE,ONLY:PSIRAMP
      IMPLICIT NONE
      DOUBLE PRECISION s, aa, bb, x
      DOUBLE PRECISION cof1, cof2, cof3, Qp
      DOUBLE PRECISION, INTENT(IN) :: H
      DOUBLE PRECISION, INTENT(IN) :: T
      DOUBLE PRECISION, INTENT(IN) :: B
      DOUBLE PRECISION, INTENT(OUT) :: dQ
      smoothQ = 0.0D0
      s = PSIRAMP
      s = s*(T-B)   ! puming rate begins to be ramped down.
      x = (H-B)
      aa = -6.0d0/(s**3.0d0)
      bb = -6.0d0/(s**2.0d0)
      cof1 = x**2.0D0
      cof2 = -(2.0D0*x)/(s**3.0D0)
      cof3 = 3.0D0/(s**2.0D0)
      Qp = cof1*(cof2+cof3)
      dQ = (aa*x**2.0D0-bb*x)
      IF ( x.LT.0.0D0 ) THEN
        Qp = 0.0D0
        dQ = 0.0D0
      ELSEIF ( x-s.GT.-1.0e-14 ) THEN
        Qp = 1.0D0
        dQ = 0.0D0
      END IF
      smoothQ = Qp
      END FUNCTION smoothQ
C
      REAL FUNCTION RATETERPQ (TIME,INUM)
C     ******************************************************************
C     LINEARLY INTERPOLATE PUMPING RATE FROM TABFILE
C     ******************************************************************
C     FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C     OF TIME TO CACULATE SPECIFIED PUMPING RATES.
      USE GWFAWUMODULE
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
!ARGUMENTS
      INTEGER, INTENT(IN):: INUM
      REAL, INTENT(IN):: TIME
!
      REAL CLOSEZERO
      REAL FLOW, TIMEBEG, TIMEND, TIMESTART, SUMFLOW, TOLF2
      INTEGER IEND, ISTM1, ISTART, iflg, NVAL, I
      TOLF2=1.0E-4
      CLOSEZERO=1.0E-15
      FLOW = 0.0
      NVAL = TABVAL(INUM)
      IFLG = 0
      SUMFLOW = 0.0
      I = 1
      TIMEBEG = TIME - DELT
      IF ( TIMEBEG-TABTIME(1,INUM).LT.0.0 ) THEN
        RATETERPQ = TABRATE(1,INUM)
      ELSEIF ( TIMEBEG-TABTIME(NVAL,INUM).GE.0.0 ) THEN
        RATETERPQ = TABRATE(NVAL,INUM)
      ELSE
! Find table value before beginning of time step.
        DO WHILE ( I.LE.NVAL-1 )
          IF ( TIMEBEG-TABTIME(I,INUM).LE.CLOSEZERO ) THEN
            EXIT
          ELSEIF ( TIMEBEG-TABTIME(I+1,INUM).LE.CLOSEZERO ) THEN
            EXIT
          ELSE
            I = I + 1
          END IF
        END DO
        ISTART = I
        ISTM1 = I
        IF ( I.GT.1 ) ISTM1 = ISTM1 - 1
! Find table value after end of time step
        DO WHILE ( I.LE.NVAL ) 
          IF ( TIME-TABTIME(I,INUM).LE.0.0 ) THEN
            EXIT
          ELSE
            I = I + 1
          END IF
        END DO
        IEND = I
        IF ( IEND.GT.NVAL ) IEND = NVAL
        DO I = ISTART, IEND - 1
          TIMESTART = TABTIME(I,INUM)
          TIMEND = TABTIME(I+1,INUM)
          IF ( TIMEBEG-TIMESTART.GT.0.0 ) TIMESTART = TIMEBEG
          IF ( TIME-TIMEND.LT.0.0 ) TIMEND = TIME
          SUMFLOW = SUMFLOW + (TIMEND-TIMESTART)*TABRATE(I,INUM)
        END DO
        RATETERPQ = SUMFLOW/DELT
      END IF
      RETURN
      END FUNCTION RATETERPQ
C
      SUBROUTINE SGWF2AWU7V(MSUM,VBNMAG,VBVLAG,KSTP,KPER,IOUT,BUDPERC)
C     ******************************************************************
C     PRINT VOLUMETRIC BUDGET
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*20 VBNMAG(MSUM)
      DIMENSION VBVLAG(4,MSUM)
      CHARACTER*17 VAL1,VAL2
C     ------------------------------------------------------------------
C
C1------DETERMINE NUMBER OF INDIVIDUAL BUDGET ENTRIES.
      BUDPERC=0.
      MSUM1=MSUM-1
      IF(MSUM1.LE.0) RETURN
C
C2------CLEAR RATE AND VOLUME ACCUMULATORS.
      ZERO=0.
      TWO=2.
      HUND=100.
      BIGVL1=9.99999E11
      BIGVL2=9.99999E10
      SMALL=0.1
      TOTRIN=ZERO
      TOTROT=ZERO
      TOTVIN=ZERO
      TOTVOT=ZERO
C
C3------ADD RATES AND VOLUMES (IN AND OUT) TO ACCUMULATORS.
      DO 100 L=1,MSUM1
      TOTRIN=TOTRIN+VBVLAG(3,L)
      TOTROT=TOTROT+VBVLAG(4,L)
      TOTVIN=TOTVIN+VBVLAG(1,L)
      TOTVOT=TOTVOT+VBVLAG(2,L)
  100 CONTINUE
C
C4------PRINT TIME STEP NUMBER AND STRESS PERIOD NUMBER.
      WRITE(IOUT,260) KSTP,KPER
      WRITE(IOUT,265)
C
C5------PRINT INDIVIDUAL INFLOW RATES AND VOLUMES AND THEIR TOTALS.
      DO 200 L=1,MSUM1
      IF(VBVLAG(1,L).NE.ZERO .AND.
     1       (VBVLAG(1,L).GE.BIGVL1 .OR. VBVLAG(1,L).LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') VBVLAG(1,L)
      ELSE
         WRITE(VAL1,'(F17.4)') VBVLAG(1,L)
      END IF
      IF(VBVLAG(3,L).NE.ZERO .AND.
     1       (VBVLAG(3,L).GE.BIGVL1 .OR. VBVLAG(3,L).LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') VBVLAG(3,L)
      ELSE
         WRITE(VAL2,'(F17.4)') VBVLAG(3,L)
      END IF
      WRITE(IOUT,275) VBNMAG(L),VAL1,VBNMAG(L),VAL2
  200 CONTINUE
      IF(TOTVIN.NE.ZERO .AND.
     1      (TOTVIN.GE.BIGVL1 .OR. TOTVIN.LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') TOTVIN
      ELSE
         WRITE(VAL1,'(F17.4)') TOTVIN
      END IF
      IF(TOTRIN.NE.ZERO .AND.
     1      (TOTRIN.GE.BIGVL1 .OR. TOTRIN.LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') TOTRIN
      ELSE
         WRITE(VAL2,'(F17.4)') TOTRIN
      END IF
      WRITE(IOUT,286) VAL1,VAL2
C
C6------PRINT INDIVIDUAL OUTFLOW RATES AND VOLUMES AND THEIR TOTALS.
      WRITE(IOUT,287)
      DO 250 L=1,MSUM1
      IF(VBVLAG(2,L).NE.ZERO .AND.
     1       (VBVLAG(2,L).GE.BIGVL1 .OR. VBVLAG(2,L).LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') VBVLAG(2,L)
      ELSE
         WRITE(VAL1,'(F17.4)') VBVLAG(2,L)
      END IF
      IF(VBVLAG(4,L).NE.ZERO .AND.
     1       (VBVLAG(4,L).GE.BIGVL1 .OR. VBVLAG(4,L).LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') VBVLAG(4,L)
      ELSE
         WRITE(VAL2,'(F17.4)') VBVLAG(4,L)
      END IF
      WRITE(IOUT,275) VBNMAG(L),VAL1,VBNMAG(L),VAL2
  250 CONTINUE
      IF(TOTVOT.NE.ZERO .AND.
     1      (TOTVOT.GE.BIGVL1 .OR. TOTVOT.LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') TOTVOT
      ELSE
         WRITE(VAL1,'(F17.4)') TOTVOT
      END IF
      IF(TOTROT.NE.ZERO .AND.
     1      (TOTROT.GE.BIGVL1 .OR. TOTROT.LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') TOTROT
      ELSE
         WRITE(VAL2,'(F17.4)') TOTROT
      END IF
      WRITE(IOUT,298) VAL1,VAL2
C
C7------CALCULATE THE DIFFERENCE BETWEEN INFLOW AND OUTFLOW.
C
C7A-----CALCULATE DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      DIFFR=TOTRIN-TOTROT
      ADIFFR=ABS(DIFFR)
C
C7B-----CALCULATE PERCENT DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      PDIFFR=ZERO
      AVGRAT=(TOTRIN+TOTROT)/TWO
      IF(AVGRAT.NE.ZERO) PDIFFR=HUND*DIFFR/AVGRAT
      BUDPERC=PDIFFR
C
C7C-----CALCULATE DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      DIFFV=TOTVIN-TOTVOT
      ADIFFV=ABS(DIFFV)
C
C7D-----GET PERCENT DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      PDIFFV=ZERO
      AVGVOL=(TOTVIN+TOTVOT)/TWO
      IF(AVGVOL.NE.ZERO) PDIFFV=HUND*DIFFV/AVGVOL
C
C8------PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN INPUT
C8------AND OUTPUT RATES AND VOLUMES.
      IF(ADIFFV.NE.ZERO .AND.
     1      (ADIFFV.GE.BIGVL2 .OR. ADIFFV.LT.SMALL)) THEN
         WRITE(VAL1,'(1PE17.4)') DIFFV
      ELSE
         WRITE(VAL1,'(F17.4)') DIFFV
      END IF
      IF(ADIFFR.NE.ZERO .AND.
     1      (ADIFFR.GE.BIGVL2 .OR. ADIFFR.LT.SMALL)) THEN
         WRITE(VAL2,'(1PE17.4)') DIFFR
      ELSE
         WRITE(VAL2,'(F17.4)') DIFFR
      END IF
      WRITE(IOUT,299) VAL1,VAL2
      WRITE(IOUT,300) PDIFFV,PDIFFR
C
C9------RETURN.
      RETURN
C
C    ---FORMATS
C
  260 FORMAT('1',/2X,'VOLUMETRIC BUDGET FOR ENTIRE MODEL AT END OF'
     1,' TIME STEP',I5,', STRESS PERIOD',I4/2X,78('-'))
  265 FORMAT(1X,/5X,'CUMULATIVE VOLUMES',6X,'L**3',7X
     1,'RATES FOR THIS TIME STEP',6X,'L**3/T'/5X,18('-'),17X,24('-')
     2//11X,'IN:',38X,'IN:'/11X,'---',38X,'---')
  275 FORMAT(1X,3X,A20,' =',A17,6X,A20,' =',A17)
  286 FORMAT(1X,/16X,'TOTAL IN =',A,18X,'TOTAL IN =',A)
  287 FORMAT(1X,/10X,'OUT:',37X,'OUT:'/10X,4('-'),37X,4('-'))
  298 FORMAT(1X,/15X,'TOTAL OUT =',A,17X,'TOTAL OUT =',A)
  299 FORMAT(1X,/16X,'IN - OUT =',A,18X,'IN - OUT =',A)
  300 FORMAT(1X,/5X,'PERCENT DISCREPANCY =',F15.2
     1,9X,'PERCENT DISCREPANCY =',F15.2,///)
C
      END
C
      SUBROUTINE GWF2AWU7DA()
C  Deallocate AWU MEMORY
      USE GWFAWUMODULE
C
        DEALLOCATE(NUMSUP)
        DEALLOCATE(NUMSUPSP)
        DEALLOCATE(NUMSUPWELLSEG)
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
        DEALLOCATE(WELLIRRUZF)
        DEALLOCATE(WELLIRRPRMS)
        DEALLOCATE(IRRFACT)
        DEALLOCATE(IRRPCT)
        DEALLOCATE(PSIRAMP) 
        DEALLOCATE(IUNITRAMP) 
        DEALLOCATE(NWELLS)
        DEALLOCATE(MXWELL)
        DEALLOCATE(NWELVL)
        DEALLOCATE(IWELLCB)
        DEALLOCATE(IWELLCBU)
        DEALLOCATE(ISFRCB)
        DEALLOCATE(IRRWELLCB)
        DEALLOCATE(IRRSFRCB)
        DEALLOCATE(IPRWEL)
        DEALLOCATE(NPWEL)
        DEALLOCATE(NNPWEL)
        DEALLOCATE(WELL)
        DEALLOCATE(NUMTAB)
        DEALLOCATE(MAXVAL)
        DEALLOCATE(TABTIME)
        DEALLOCATE(TABRATE)
        DEALLOCATE(TABVAL)
        DEALLOCATE(TABID)
        DEALLOCATE(TABUNIT)
!SFR
      DEALLOCATE (DVRCH)    
      DEALLOCATE (DVEFF)  
      DEALLOCATE (IRRROW) 
      DEALLOCATE (IRRCOL)
      DEALLOCATE (SFRIRRUZF) 
      DEALLOCATE (SFRIRRPRMS) 
      DEALLOCATE (DVRPERC) 
      DEALLOCATE (IDVFLG) 
      DEALLOCATE (NUMIRRSFR)
      DEALLOCATE (NUMIRRSFRSP)
      DEALLOCATE (MAXCELLSSFR)
      DEALLOCATE (DEMAND)
      DEALLOCATE (ACTUAL)
      DEALLOCATE (SUPACT)
      DEALLOCATE (NUMIRRWELSP)
      DEALLOCATE (TSSWUNIT)
      DEALLOCATE (TSGWUNIT)  
      DEALLOCATE (TSACTIVESW)
      DEALLOCATE (TSACTIVEGW)
      DEALLOCATE (QONLY)
C
      RETURN
      END