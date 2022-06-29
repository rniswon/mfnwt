

                     MODFLOW-NWT - Version: 1.3.0
                 Newton Formulation for MODFLOW-2005

      
This software has been approved for release by the U.S. Geological Survey (USGS). 
Although the software has been subjected to rigorous review, the USGS reserves 
the right to update the software as needed pursuant to further analysis and review. 
No warranty, expressed or implied, is made by the USGS or the U.S. Government as 
to the functionality of the software and related material nor shall the fact of 
release constitute any such warranty. Furthermore, the software is released on 
condition that neither the USGS nor the U.S. Government shall be held liable for 
any damages resulting from its authorized or unauthorized use.

Any use of trade, product or firm names is for descriptive purposes only and 
does not imply endorsement by the U.S. Government.

MODFLOW-NWT version 1.2.0 is packaged for personal computers using one of the 
Microsoft Windows operating systems. Executable files for personal 
computers are provided as well as the source code at the following URL:

http://water.usgs.gov/ogw/modflow-nwt/

The executable files were compiled on a personal computer with the Intel(R) 
Core(TM) i7-7820HQ CPU, running the Microsoft Windows 10 Enterprise 
operating system, using the Microsoft Visual Studio 2017 Version 15.9.13
development environment and the Intel® Parallel Studio XE 2019 Update 4 Composer 
Edition for Fortran Windows* Integration for Microsoft Visual Studio* 2017, 
Version 19.0.0052.15 compiler. The source code is provided to aid users in  
compilation on other computers. However, no support is provided for compilation.

IMPORTANT: Users should review the file Summary_MODFLOW-NWT.txt for a description
of, and references for, this software. Users should also review the file 
release.txt, which describes changes that have been introduced into MODFLOW-NWT
with each official release; these changes may substantially affect users.

Instructions for installation, execution, and testing of this version of
MODFLOW-NWT are provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following distribution file is for use on personal computers:

         MODFLOW-NWT_1.3.0.zip

The distribution file contains:

          Executables and source code for MODFLOW-NWT.
          MODFLOW-NWT documentation.
          Eleven MODFLOW-NWT sample problems.

The MODFLOW-NWT program and related files can be extracted from the 
distribution file.  Extracting these files is all that is required 
for installation.  It is recommended that the directory structure
in the zip file be maintained when the files are extracted.  The 
recommended installation directory is C:\WRDAPP.  The distribution 
file contains the following directory structure.


   |
   |--MODFLOW-NWT
   |    |--bin           ; Compiled MODFLOW-NWT executables for personal computers
   |    |--data          ; Sample problems
   |    |--doc           ; Documentation report for MODFLOW-NWT and the Surface-
                           Water Routing Package, updated input instructions for UZF and SFR2
   |    |--output_test   ; Output files from running the sample problems
   |    |--output_test_64   ; Output files from running the sample problems, 64 bit versions
   |    |--src           ; Source code for MODFLOW-NWT

Included in directory MODFLOW-NWT\doc is the MODFLOW-NWT documentation 
report, which is a Portable Document Format (PDF) file. The PDF file is 
readable and printable on various computer platforms using Acrobat Reader 
from Adobe. The Acrobat Reader is freely available from the following World
Wide Web site:
      http://www.adobe.com/


B. INSTALLING

To make the executable versions of MODFLOW-NWT accessible from any
directory, the directory containing the executables (MODFLOW-NWT\bin)
should be included in the PATH environment variable.  

As an alternative, the executable files, MODFLOW-NWT.exe and MODFLOW-NWT_64.exe, 
in the MODFLOW-NWT\bin directory can be copied into a directory already
included in the PATH environment variable.


C. EXECUTING THE SOFTWARE

A 32 bit and a 64 bit executable are provided in the MODFLOW-NWT\bin  
directory. Two executables are provided because computers often use either  
the 32 bit Windows XP or the 64 bit Windows 7 operating systems. Large 
simulations may not run on a 32 bit operating system due to limitations 
in the amount of available random access memory (RAM). A 64 bit operating
system provides much more available RAM than a 32 bit operating system. 
Thus, it is recommended that a 64 bit executable be used on a 64 bit operating  
system for large simulations.   

After the executable files in the MODFLOW-NWT\bin directory are installed in
a directory that is included in your PATH, MODFLOW-NWT is initiated in
a Windows Command-Prompt window using the commands:

      MODFLOW-NWT.exe [Fname]

or
      MODFLOW-NWT_64.exe [Fname]

The optional Fname argument is the name of the MODFLOW-NWT Name File.

The data arrays in MODFLOW-NWT are dynamically allocated, so models are not
limited by hard-coded array limits. However, it is best to have at least 
2 MB of RAM available to hold all of the required data. If there is 
less available RAM than the model requires, which depends on the size of the 
application, the program will use virtual memory; however, this can
slow execution significantly. If there is insufficient RAM to run
the model, then MODFLOW-NWT will not initiate the beginning of the 
simulation; however, the Windows Command-Prompt window may continue to 
indicate that MODFLOW-NWT is executing. For this circumstance, the program 
must be terminated manually using the Windows Task Manager application.

Some of the files written by MODFLOW-NWT are unformatted files. The structure
of these files depends on the compiler and options in the Fortran write
statement.  MODFLOW-NWT is compiled with the unformatted file type specified
as "BINARY". Any program that reads the unformatted files produced by
MODFLOW-NWT must be compiled with a compiler that produces programs that
use the same structure for unformatted files.  For example, Zonebudget
and Modpath use unformatted budget files produced by MODFLOW-NWT.  
Another example are head files that are generated by one MODFLOW-NWT
simulation and used in a following simulation as initial heads.  Both 
simulations must be run using an executable version of MODFLOW-NWT that uses 
the same unformatted file structure.


D. TESTING

Thirteen sample problems with MODFLOW-NWT data sets are provided to verify that 
MODFLOW-NWT is correctly installed and running on the system.  The sample 
problems also may be looked at as examples of how to use the program. Five of 
the test problems (Pr1a, Pr1b, Pr2, Pr3_lower (32 bit and 64 bit), and  
Pr3_higher (32 bit and 64 bit) are documented in Niswonger and others (2011),  
one test problems (l1b2k) is documented in Merritt and Konikow (2000) as Test 
Simulation 1, one test problem (SwrSample05) is documented in Hughes and others 
(2012), and one test problem (swi2ex4sww) is documented in Bakker and others (2013). 
A test problem (Sfr2weltab) was added that was modified from the hypothetical test 
simulation described by Niswonger and Prudic (2005) to provide an example of specifying 
tabular time series input files for pumping rates. Another test problem 
(Uzf_testoptions) was added that was modified from test simulation 2 of Niswonger and 
others (2006) to provide as example of the new input options for UZF. Another test 
problem (SFR_LAK_floodplain) was created that combines the SFR2, LAK7, and UZF1 packages   
to demonstrate how the lake package can be used to simulate floodplains adjacent 
to streams. Two additional test problems were added that was documented as test problem 1
and Test Problem 2 in Niswonger and others (2006). These test problems can be run using  
either the 32-bit or 64-bit versions of the MODFLOW-NWT executable. Saved results for these  
simulations are included in the Output_test and Output_test_64 directories for comparison.
All test problems can be run at once by executing the file run_all.bat in the data directory.

E. COMPILING

The executable files are provided in MODFLOW-NWT\bin. Although executable versions   
of the program are provided, the source code also is provided in the MODFLOW-NWT\src 
directory so that MODFLOW-NWT can be recompiled if necessary. However, the USGS cannot provide  
assistance to those compiling MODFLOW-NWT.

F. REFERENCES

Bakker, Mark, Schaars, Frans, Hughes, J.D., Langevin, C.D., and Dausman, A.M., 2013, 
Documentation of the seawater intrusion (SWI2) package for MODFLOW: 
U.S. Geological Survey Techniques and Methods, book 6, chap. A46, 47 p. Available online at http://pubs.usgs.gov/tm/6a46/

Hughes, J.D., Langevin, C.D., Chartier, K.L., and White, J.T., 2012, Documentation 
of the Surface-Water Routing (SWR1) Process for modeling surface-water flow with the 
U.S. Geological Survey Modular Ground-Water Model (MODFLOW-2005): U.S. Geological 
Survey Techniques and Methods, book 6, chap. A40 (Version 1.0), 113 p. Available online at http://pubs.usgs.gov/tm/6a40/

Merritt, M.L., and Konikow, K.F., 2000, Documentation of a computer program to simulate 
lake-aquifer interaction using the MODFLOW ground-water flow model and the MOC3D 
solute-transport model: Water-Resources Investigations Report 00-4167, 146 p. Available online at http://pubs.er.usgs.gov/publication/wri004167

Niswonger, R.G., Panday, Sorab, and Ibaraki, Motomu, 2011, MODFLOW-NWT, A Newton
formulation for MODFLOW-2005: U.S. Geological Survey Techniques and Methods 6-A37, 44 p. Available online at http://pubs.usgs.gov/tm/tm6a37/

Niswonger, R.G., and Prudic, D.E., 2005, Documentation of the Streamflow-Routing (SFR2) Package to include unsaturated flow beneath streams—
A modification to SFR1: U.S. Geological Survey Techniques and Methods 6-A13, 50 p.

Niswonger, R.G., Prudic, D.E., and Regan, R.S., 2006, Documentation of the Unsaturated-Zone Flow (UZF1) Package
for modeling unsaturated flow between the land surface and the water table with MODFLOW-2005: U.S. Geological
Survey Techniques and Methods 6-A19, 62 p

Niswonger, R. G. (2020). An Agricultural Water Use Package for MODFLOW and GSFLOW. Environmental Modelling & Software, 104617.
https://www.sciencedirect.com/science/article/pii/S1364815219305080
