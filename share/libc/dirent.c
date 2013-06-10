/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2013                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  Contact CEA LIST for licensing.                                       */
/*                                                                        */
/**************************************************************************/

#include "dirent.h"
#include "string.h"
static struct dirent __fc_directories[]=
  { {1,1,1,1,"foo"},
    {2,2,2,2,"bar"},
    0 };
static DIR __fc_opendir_result;
DIR *opendir(const char *path){
  int dir_id=0;
  while(__fc_directories[dir_id].d_ino!=0) {
    if (strcmp(path,__fc_directories[dir_id].d_name)==0)
	       {__fc_opendir_result.__fc_dir_contents = dir_id;
		 return &__fc_opendir_result;}
    dir_id++;
}
  return NULL;
}

