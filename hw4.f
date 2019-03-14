C     _    ____  _____ _   _   ____    _   _  _      ___
C    / \  / ___|| ____| \ | | | ___|  / | | || |    ( _ )
C   / _ \ \___ \|  _| |  \| | |___ \  | | | || |_   / _ \
C  / ___ \ ___) | |___| |\  |  ___) | | | |__   _| | (_) |
C /_/   \_\____/|_____|_| \_| |____/  |_|    |_|    \___/
C  _   ___        ___  _
C | | | \ \      / / || |
C | |_| |\ \ /\ / /| || |_
C |  _  | \ V  V / |__   _|
C |_| |_|  \_/\_/     |_|
C 
C Jacob Killelea - 105510162
C
      program homework4
C To run: gfortran -O3 -freal-8-real-16 hw4.f -o hw4 && ./hw4 && cat output
C Results are written to 'output'
C Raw timeseries data is written to the file 'timeseries'
          implicit none
          integer N, i, numsteps, outfile, timeseriesfile,
     &            idxstart, idxend, ierr
C         Number of data points
          parameter (N               = 10000)
          parameter (outfile         = 3)
          parameter (timeseriesfile  = 4)
C         Timeseries data, high and low frequencies (Hz), 
C         the total timespan (sec), and the time interval between measurements
          real*16 timeseries(N), t(N), highfreq, lowfreq, 
     &           displacements(5), timespan, timedelta, accuracy, 
     &           displacement, jitter, stability
C         10 seconds of data
          parameter (timespan  = 10.0)
C         10 Hz
          parameter (highfreq  = 10.0)
C         0.1 Hz
          parameter (lowfreq   =  0.1)
C         1000 points per second
          parameter (timedelta = 0.001)
C         discards decimal when going real -> int, get 9999 instead of 10000 so add 1
          numsteps = timespan / timedelta + 1

C         Input/Output
          open(outfile, file="output", form="formatted",
     &         iostat=ierr)
          if (ierr .ne. 0) then
              print *, "Couldn't write to file 'output'"
              stop
          end if

          write (outfile, *) "Initial Conditions"
          write (outfile, 9999) "High frequency", highfreq
          write (outfile, 9999) "Low frequency",  lowfreq
          write (outfile, 9999) "Time span",      timespan
          write (outfile, 9999) "Time delta",     timedelta
          write (outfile, 9998) "Num Steps",      numsteps
          write (outfile, 9998) "N",              N

          write (outfile, *) "Results"
C         Compute pointing error over the timeseries
          do 10 i = 0,numsteps-1
            t(i+1) = i*timedelta
10        continue
          timeseries = sin(lowfreq*t) + sin(highfreq*t)

C         Pointing accuracy - RMS of pointing error over time
          accuracy = sqrt(sum(timeseries**2) / N)
          write (outfile, 9999) "Accuracy", accuracy

C         Pointing displacement - mean of pointing error over time
          displacement = sum(timeseries) / numsteps
          write (outfile, 9999) "Displacement", displacement

C         Jitter
          jitter = sqrt( (accuracy / timespan)**2 
     &                 - (displacement / timespan)**2 )

          write (outfile, 9999) "Jitter", jitter

C         Stability - t_d = 2sec, then rms them all
          do 20 i = 0, 4
              idxstart = int(2*i/timedelta) + 1
              idxend   = idxstart + 2/timedelta
              displacements(i+1) = sum(timeseries(idxstart:idxend)) 
     &                                / (idxend - idxstart)
20        continue
        
          stability = sum((displacements(1:4)-displacements(2:5))**2)
          stability = stability / 4

          write (outfile, 9999) "Stability", stability

C         Close output file
          close(outfile, iostat=ierr)
          if (ierr .ne. 0) then
              print *, "Couldn't close file 'output'"
          end if

C         Write out pointing error over time for separate analysis
          open(timeseriesfile, file="timeseries", form="formatted", 
     &         iostat=ierr)
          if (ierr .ne. 0) then
              print *, "Couldn't write to file 'timeseries'"
              stop
          end if

          write (timeseriesfile, *)
     &          "# Time (s), Pointing error (arcsec)"
          do 30 i = 1,numsteps
            write (timeseriesfile, "(2f8.4)") t(i), timeseries(i)
30        continue

          close(timeseriesfile, iostat=ierr)
          if (ierr .ne. 0) then
              print *, "Couldn't close file 'timeseries'"
          end if

C         Done
          stop

C         Label + integer
9998      format(A15, 1x, i9)
C         Label + float
9999      format(A15, 1x, f15.8)
      end program homework4
