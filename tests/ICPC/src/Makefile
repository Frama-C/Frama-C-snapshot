CC = gcc
CFLAGS = -lm
#-DFIX -DXXX -g

SRC = roco.c impls.c sim.c main.c main_testcase_1.c main_testcase_2.c main_testcase_3.c
HEADERS = roco.h external.h sim.h stdafx.h roco_config.h roco_config_testcase_1.h roco_config_testcase_2.h roco_config_testcase_3.h

all: roco_0.exe roco_1.exe roco_2.exe roco_3.exe

roco_0.exe: $(SRC) $(HEADERS) roco_config.h
	$(CC) $(CFLAGS) $(SRC) -o $@
roco_1.exe: $(SRC) $(HEADERS)
	$(CC) $(CFLAGS) -DTESTCASE=1 $(SRC) -o $@
roco_2.exe: $(SRC) $(HEADERS)
	$(CC) $(CFLAGS) -DTESTCASE=2 $(SRC) -o $@
roco_3.exe: $(SRC) $(HEADERS)
	$(CC) $(CFLAGS) -DTESTCASE=3 $(SRC) -o $@

clean:
	rm -f roco_*.exe
