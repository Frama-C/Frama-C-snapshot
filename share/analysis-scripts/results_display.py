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

import re
import sys
import string
import curses


sensitivity = 0.02

class ResultsFormatter(string.Formatter):
    @staticmethod
    def format_memory(kilobytes):
        if kilobytes < 4096:
            return str(kilobytes) + " kiB"
        megabytes = round(kilobytes / 1024)
        if megabytes < 4096:
            return str(megabytes) + " MiB"
        gigabytes = round(megabytes / 1024)
        return str(gygabytes) + " GiB"

    @staticmethod
    def format_time(seconds):
        if seconds < 10:
            return str(round(seconds,2)) + "s"
        if seconds < 100:
            return str(round(seconds,1)) + "s"
        if seconds < 600:
            return str(round(seconds)) + "s"
        minutes = round(seconds / 60)
        if minutes < 600:
            return str(minutes) + "m"
        hours = round(minutes / 60)
        return str(hours) + "h"

    @staticmethod
    def attribute(value, inverted):
        if value > sensitivity:
            return "@-" if inverted else "@+"
        elif value < -sensitivity:
            return "@+" if inverted else "@-"
        else:
            return "@="

    def get_field(self, field_name, args, kwargs):
        try:
            return super().get_field(field_name, args, kwargs)
        except (KeyError, AttributeError):
            return None,field_name

    def format_field(self, value, format_spec):
        if value == None:
            return ""
        elif format_spec.startswith('+cmp:'):
            remainder = format_spec.split("+cmp:",1)[1]
            return (self.attribute(value, False) +
                self.format_field(value, remainder) + "@=")
        elif format_spec.startswith('-cmp:'):
            remainder = format_spec.split("-cmp:",1)[1]
            return (self.attribute(value, True) +
                self.format_field(value, remainder) + "@=")
        elif format_spec == 'time':
            return self.format_time(value)
        elif format_spec == 'memory':
            return self.format_memory(value)
        else:
            return super().format_field(value, format_spec)

class UserExitRequest (Exception):
    pass

class PlainDisplay:
    NEGATIVE = 1
    POSITIVE = 2
    RUNNING = 3
    HEADER = 4

    columns = [
        {"size":64, "caption":"Case", "format":"{target_name:s}"},
        {"size":14, "caption":"Coverage",
         "format":"{coverage:>8.0%} {diff_coverage:+cmp:+.0%}"},
        {"size":14, "caption":"Alarms",
         "format":"{alarms:>8d} {diff_alarms:-cmp:+d}"},
        {"size":14 , "caption":"Warnings",
         "format":"{warnings:>8d} {diff_warnings:-cmp:+d}"},
        {"size":14, "caption":"Time",
         "format":"{user_time:time} {diff_user_time:-cmp:+.0%}"},
        {"size":14, "caption":"Memory",
         "format":"{memory:memory} {diff_memory:-cmp:+.0%}"}]

    def __init__(self):
        self.NEGATIVE = 0
        self.POSITIVE = 0
        self.RUNNING = 0
        self.HEADER = 0
        self.OBSOLETE = 0
        self.needs_update = False

    def write(self, text, attributes=0):
        sys.stdout.write(text)

    def rich_write(self, text, override=None, size=0):
        attributes = 0
        n = 0
        for s in re.split(r'(@.)', text):
            if s == "@=":
                attributes = 0
            elif s == "@+":
                attributes = self.POSITIVE
            elif s == "@-":
                attributes = self.NEGATIVE
            else:
                n += len(s)
                self.write(s, attributes if override is None else override)
        if n < size:
            self.write(' ' * (size - n), attributes if override is None else override)

    fmt = ResultsFormatter()

    def format(self, *args, **kwargs):
        return self.fmt.format(*args, **kwargs)

    def print_table(self, results):
        self.write(" ", self.HEADER)
        for column in self.columns:
            self.write(self.format('{caption:^{size}}', **column), self.HEADER)
            self.write(" ", self.HEADER)

        self.write("\n-")
        for column in self.columns:
            self.write(self.format('{:-^{size}}', "", **column))
            self.write("-")

        self.write("\n")
        for result in results:
            self.write(" ")
            for column in self.columns:
                s = self.fmt.format(column["format"], **result)
                if result["is_running"]:
                    attribute = self.RUNNING
                elif not result["up_to_date"]:
                    attribute = self.OBSOLETE
                else:
                    attribute = None
                self.rich_write(s, attribute, size=column['size'])

                self.write(" ")
            self.write("\n")

        self.write("\n")
        self.needs_update = False

    def process_inputs(self):
        pass


class CursesDisplay(PlainDisplay):
    def __init__(self, stdscr):
        self.stdscr = stdscr
        #curses.mousemask(curses.ALL_MOUSE_EVENTS)
        stdscr.nodelay(True)
        stdscr.refresh() # Needs to be done once or nothing will be output

        self.window = curses.newpad(400, 160)
        curses.init_color(curses.COLOR_YELLOW, 300, 300, 300)
        curses.init_pair(1, curses.COLOR_RED, 0)
        curses.init_pair(2, curses.COLOR_GREEN, 0)
        curses.init_pair(3, curses.COLOR_WHITE, curses.COLOR_YELLOW)
        curses.init_pair(4, curses.COLOR_YELLOW, 0)
        self.NEGATIVE = curses.color_pair(1)
        self.POSITIVE = curses.color_pair(2)
        self.RUNNING = curses.A_BLINK | curses.color_pair(3)
        self.HEADER = curses.A_BOLD
        self.OBSOLETE = curses.color_pair(4)
        self.scroll_y = 0

    def write(self, text, attributes=0):
        self.window.addstr(text, attributes)

    def print_table(self, results):
        self.window.clear()
        PlainDisplay.print_table(self, results)
        height, width = self.stdscr.getmaxyx()
        try:
            self.window.refresh(0, 0, 0, 0, 1, width-1)
            self.window.refresh(self.scroll_y+2, 0, 2, 0, height-1, width-1)
        except Exception:
            # getmaxyx may be out of date, especially when resizing down the
            # window ; just ignore errors
            pass


    def process_inputs(self):
        previous_y = self.scroll_y
        c = self.stdscr.getch()
        while c != -1:
            if c == ord('q'):
                raise UserExitRequest
            elif c == curses.KEY_UP:
                self.scroll_y -= 1
            elif c == curses.KEY_DOWN:
                self.scroll_y += 1
            elif c == curses.KEY_NPAGE:
                self.scroll_y += 10
            elif c == curses.KEY_PPAGE:
                self.scroll_y -= 10
            elif c == curses.KEY_MOUSE:
                id,x,y,z,bstate = curses.getmouse()
                if z > 0:
                    self.scroll_y += 1
                elif z < 0:
                    self.scroll_y -= 1
            self.scroll_y = max(0, self.scroll_y)
            c = self.stdscr.getch()
        if self.scroll_y != previous_y:
            self.needs_update = True


def wrapper(f, *args, **kwargs):
    if 'curses' in kwargs:
        use_curses = kwargs['curses']
        del kwargs['curses']
    else:
        use_curses = False

    if use_curses:
        def g(stdscr):
            nonlocal f, args, kwargs
            display = CursesDisplay(stdscr)
            return f(display, *args, **kwargs)
        return curses.wrapper(g)
    else:
        display = PlainDisplay()
        return f(display, *args, **kwargs)

