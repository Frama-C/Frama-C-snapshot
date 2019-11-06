##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2019                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

########################################
# Server as a standard Frama-C plug-in #
########################################

m4_define([plugin_file],Makefile.in)

m4_define([FRAMAC_SHARE_ENV],
          [m4_normalize(m4_esyscmd([echo $FRAMAC_SHARE]))])

m4_define([FRAMAC_SHARE],
	  [m4_ifval(FRAMAC_SHARE_ENV,[FRAMAC_SHARE_ENV],
                                     [m4_esyscmd(frama-c -print-path)])])

m4_ifndef([FRAMAC_M4_MACROS], [m4_include(FRAMAC_SHARE/configure.ac)])

check_plugin(server,PLUGIN_RELATIVE_PATH(plugin_file),[Server plug-in],yes)

########################################
# Server ZMQ Configuration             #
########################################

AC_ARG_ENABLE(
  server-zmq,
  [  --enable-server-zmq     Server ZeroMQ support (default: yes)],
  SERVER_ZMQ=$enableval,
  SERVER_ZMQ=yes
)

if test "$SERVER_ZMQ" = "yes" ;
then
        configure_pkg([zmq],
                      [Server support for ZeroMQ disabled (try 'opam install zmq').])
        plugin_use_pkg(server,zmq)
        SERVER_ZMQ=$HAS_OCAML_ZMQ
fi

AC_SUBST(SERVER_ZMQ)

#######################
# Generating Makefile #
#######################

write_plugin_config(Makefile)
