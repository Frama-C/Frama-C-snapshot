(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

let css = "\
body {\
     display:block;\
     position: relative;\
     left: 5%;\
     width: 90%;\
     font-family: Georgia, Times, serif;\
     font-size: 10pt; /* base size */\
     min-height: 30em;\
     background: #ffffff;\
     color: #444444;\
}\
\
h1 {\
    font-family: Optima, Verdana, Arial, sans;\
    font-size: 1.6em;\
    font-weight: normal;\
    color: black;\
    margin: 0.4em 0em 0.4em 0em;\
    padding: 0.4em 0em 0em 1em;\
    border-bottom: thin solid #404040;\
}\
\
h2 {\
    font-family: Optima, Verdana, Arial, sans;\
    font-size: 1.2em;\
    font-weight: normal;\
    color: black;\
    margin: 0.4em 0em 0.4em 0em;\
    padding: 0.4em 0em 0em 1em;\
    border-bottom: thin dotted #404040;\
}\
\
h3 {\
    font-family: Optima, Verdana, Arial, sans;\
    font-size: 1.2em;\
    font-weight: normal;\
    color: black;\
    margin: 0.4em 0em 0.4em 0em;\
    padding: 0.4em 0em 0em 1em;\
}\
\
td {\
  text-align: center;\
  border: thin solid black; \
}\
\
th { \
    text-align: center;\
    font-weight: normal;\
    color: black;\
    border: thin solid black; \
    padding: 3pt;\
    background-color: #bfb4b4;\
}\
 \
td.entry { \
    text-align: left;\
    font-weight: normal;\
    color: black;\
    border: thin solid black; \
    padding: 3pt;\
    background-color: #e8e8e8 ;\
}\
td.stat { \
    text-align: center;\
    color: black;\
    border: thin solid black; \
    padding: 3pt;\
    width: 20%; \
}\
 \
td.result { \
    text-align: center;\
    color: black;\
    border: thin solid black; \
    padding: 3pt;\
    background-color: #AFC7C7 ;\
}\
\
tr {}\
\
caption {\
    caption-side: bottom;\
}\
\
table {\
    border: medium solid black;\
    width: 90%; \
}\
\
div.graph {\
    text-align: center;\
}\
\
ul.horizontal {\
 padding:0;\
 margin:0;\
 list-style-type:none;\
 }\
\
li.horizontal {\
 margin-left:1.5em;\
 float:left; /*pour IE*/\
 }\
\
span {\
    font-weight: bold;\
}\
\
a.varinfo, span.vdecl a.varinfo_fun {\
    text-decoration: none;\
}\
\
a.varinfo, a.varinfo_fun {\
    color: #000;\
}\
\
h3.back {\
    font-family: Optima, Verdana, Arial, sans;\
    padding-top: 2em;\
}\
\
h3.back a {\
    color:black;\
}\
"
