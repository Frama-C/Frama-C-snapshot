========================================================================
    Robot Controller ICPC2011-Challenge Project Overview
========================================================================

This section contains a summary of what you will find in each of the files
that make up your ICPC2011-Challenge application. All challenge source code
is written in C.

Source code in directory "src":

 main.c
    This is the main application source file. You wrote this when you
    started your job to try out the controller with different inputs.
 main_testcase_1.c
    This is the first testcase of a bad execution. It loads
    roco_config_testcase_1.h and changes the commands at the end of main().
 main_testcase_2.c
	This is the second testcase. It loads roco_config_testcase_2.h and
    changes the commands at the end of main().
 main_testcase_3.c
    This is the third testcase. It loads roco_config_testcase_3.h and
    changes the commands at the end of main().
 roco.h, roco.c
    Code for the robot controller.
 roco_config.h, roco_config_testcase_1.h, roco_config_testcase_2.h,
    roco_config_testcase_3.h
    One default and three customer-supplied configurations that are used in
    robot leg applications. 
 sim.h, sim.c
    Robot Controller simulator code. Makes the controller think there's a
    robot leg attached to it.
 external.h
    Typedefs to insulate us from the changing whims of the C language
    committee, and declarations for many controller variables and functions.
 impls.c
    Standard library functions that realize some common control system
    functions.
 resource.h, stdafx.cpp
    Boilerplate code for Visual Studio 2010 C++ project.
 stdafx.h
    The main include file for the Visual Studio 2010 project. Every .c (and
    .cpp) file includes this.

-----------------------------------------------------------------------------

This section describes how to compile and build the code.

Visual Studio 2010-specific directions:
   1. First unzip the VS2010.zip file to create a VS2010 directory with a
      pre-configured C++ solution (ICPC2011-Challenge.sln) for this
      challenge. Be sure to put the VS2010 directory in the same parent
      directory that the src/ directory is in (or you'll have to edit a lot
      of file paths in ICPC2011-Challenge.vcxproj).
   2. To define a preprocessor directive for the entire project, right-click
      on the ICPC2011-Challenge project in the Solution Explorer tool pane
      and choose Properties. Open up the C/C++ node in the treeview and
      click on Preprocessor. Add your desired directive to the semi-colon
      separated list of preprocessor directives.

Other operating systems:
   1. Use "make" with your favorite C compiler (we use gcc) to compile the
      four different variants of the test code.

-----------------------------------------------------------------------------

This section describes the bugs that have been reported by the
RobotControllers.com customers.

For each reported erroneous test case, see the attached customer
configuration and log file.
All times are given in milliseconds in the log files and in seconds in the
following (t=time).

Test Case 1: (To reproduce, define a preprocessor directive TESTCASE=1)
   Error description: From time t=50, it takes around 85 seconds to move by
                      70 degrees! With the given configuration
                      (roco_config_testcase_1.h), it should only take about
                      10 seconds.
                      Movement in the other direction, e.g. at t=30 or t=160,
                      is OK.
   Issued commands:   t=30: moveto 25, t=50: moveby -70, t=160: moveto -20
   
Test Case 2: (To reproduce, define a preprocessor directive TESTCASE=2)
   Error description: The robot leg does not stop jiggling when it hits the
                      target angle.
   Issued commands:   t=60: moveto -30, t=100: moveto 30, t=140: moveto -10,
                      t=180: shutoff

Test Case 3: (To reproduce, define a preprocessor directive TESTCASE=3)
   Error description: This test case destroyed the robot!
                      After destroying 3 or 4 robots in our debugging
                      process, they believe the cause is due to the voltage
                      being inverted too quickly at time t=90.
					  They also noticed that at time t=50, the voltage drops
                      much too quickly.
   Issued commands:   t=60: moveto 20, t=100: moveto -20, t=110: moveto 20

To run with a default (non-erroneous) configuration and command sequence,
remove any preprocessor definition for TESTCASE.

-----------------------------------------------------------------------------

This section describes how to use the acceptance test script we have provided.

eval.py is a Python (2.6) script that you should use to validate your fixes.
Redirect the test driver's output into this script, and give the test case
number as an argument. The test case commands are coded into the script, so
take care that you don't modify the test commands sequence and timing (or if
you must, be sure to adjust/extend the script accordingly). When the script
outputs "found 0 error(s)", then your solution is very likely correct.

Example usage: python eval.py 2 < log2.csv
