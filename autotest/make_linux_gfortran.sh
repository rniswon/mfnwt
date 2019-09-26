#! bin/bash

gfortran --version
make_gfortran.py -fc gfortran -sd -mc -mf ../MODFLOW-NWT/src mfnwt.exe
pause