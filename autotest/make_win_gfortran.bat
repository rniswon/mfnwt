set PATH=%PATH%;C:\Program Files (x86)\mingw-w64\i686-8.1.0-posix-dwarf-rt_v6-rev0\mingw32\bin
gfortran --version
C:\Users\rniswon\Anaconda3\envs\py39\python.exe make_gfortran.py -fc gfortran -cc gcc -sd -mc -e ..\MODFLOW-NWT\src mfnwt.exe 
make_gfortran.py -fc gfortran -cc gcc -sd -mc ..\MODFLOW-NWT\src mfnwt.exe
pause