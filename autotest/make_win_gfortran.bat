set PATH=%PATH%;C:\MinGW\bin
gfortran --version
make_gfortran.py -fc gfortran -sd -mc ..\MODFLOW-NWT\src mfnwt.exe 
pause
