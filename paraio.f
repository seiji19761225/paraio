!=======================================================================
! PARAIO: Parallel I/O BandWidth Benchmark
! (c)2013-2014 Seiji Nishimura
! $Id: paraio.f,v 1.1.1.2 2015/08/22 00:00:00 seiji Exp seiji $
!=======================================================================
      PROGRAM PARAIO
      IMPLICIT REAL*8(A-H,O-Z)
      INCLUDE 'mpif.h'
!
      INTEGER           ::NPROCS,MYRANK,INRANK,ITER,IERR
      INTEGER*8         ::NSIZE
      REAL*8            ::TS,TM,TOTAL_MB,TOTAL_TM,CHKSUM
      REAL*8,ALLOCATABLE::OUTDATA(:),INDATA(:)
      CHARACTER*16      ::FNAME='./IODATA.XXXXXXX'
!
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NPROCS,IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,MYRANK,IERR)
!
      IF (MYRANK.eq.0) THEN
!        WRITE(*,*) 'SIZE,ITER?'
         READ (*,*) NSIZE,ITER
      END IF
      CALL MPI_BCAST(NSIZE,1,MPI_INTEGER8,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(ITER ,1,MPI_INTEGER ,0,MPI_COMM_WORLD,IERR)
      TOTAL_MB=NSIZE*ITER*NPROCS*8.E-6
!
      ALLOCATE(OUTDATA(NSIZE))
      ALLOCATE( INDATA(NSIZE))
#ifdef USE_LOCAL_DISK
      INRANK=MYRANK
      CALL RANDOM_NUMBER(OUTDATA(1:NSIZE))
#else
! Swap target file to avoid I/O cache effect. >>>DO NOT USE folded-rank placement.<<<
      INRANK=NPROCS-MYRANK-1
      DO I=1,NSIZE
         OUTDATA(I)=INRANK+I
      END DO
#endif
      IF (MYRANK.eq.0) WRITE(*,200) NPROCS,INT(NSIZE*1.E-6),ITER
! write test -----------------------------------------------------------
      WRITE(FNAME(10:16),100) MYRANK
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      TS=MPI_WTIME()
      OPEN(UNIT=10,FILE=FNAME,FORM='unformatted',STATUS='replace')
      DO I=1,ITER
         WRITE(10) OUTDATA(1:NSIZE)
      END DO
      CLOSE(10)
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      TM      =MPI_WTIME()-TS
      TOTAL_TM=TM
      IF (MYRANK.eq.0) WRITE(*,300) 'WRITE ',TM,TOTAL_MB/TM
! read  test -----------------------------------------------------------
      WRITE(FNAME(10:16),100) INRANK
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      TS=MPI_WTIME()
      OPEN(UNIT=10,FILE=FNAME,FORM='unformatted',STATUS='old')
      DO I=1,ITER
         READ (10)  INDATA(1:NSIZE)
      END DO
      CLOSE(10)
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      TM      =MPI_WTIME()-TS
      TOTAL_TM=TOTAL_TM+TM
      IF (MYRANK.eq.0) WRITE(*,300) 'READ  ',TM,TOTAL_MB/TM
! checksum -------------------------------------------------------------
      CHKSUM=0.D0
      DO I=1,NSIZE
#ifdef USE_LOCAL_DISK
         CHKSUM=CHKSUM+(INDATA(I)-OUTDATA(I))**2
#else
         CHKSUM=CHKSUM+(INDATA(I)-(MYRANK+I))**2
#endif
      END DO
      CALL MPI_ALLREDUCE(MPI_IN_PLACE,CHKSUM,1,MPI_REAL8,MPI_SUM,
     &                   MPI_COMM_WORLD,IERR)
      CHKSUM=DSQRT(CHKSUM)
      IF (MYRANK.eq.0) THEN 
          WRITE(*,300) 'TOTAL ',TOTAL_TM,2.D0*TOTAL_MB/TOTAL_TM
          WRITE(*,400) CHKSUM
      END IF
!-----------------------------------------------------------------------
      DEALLOCATE( INDATA)
      DEALLOCATE(OUTDATA)
!
      CALL MPI_FINALIZE(IERR)
      STOP
!
 100  FORMAT(I7.7)
 200  FORMAT('PARAIO: ',I7,'PE, ',I15,'MWORDS, ITER=',I5)
 300  FORMAT(A6,': ',F15.7,'[SEC.], ',F15.7,'[MB/S]')
 400  FORMAT('CHKSUM: ',F15.7)
!
      END
