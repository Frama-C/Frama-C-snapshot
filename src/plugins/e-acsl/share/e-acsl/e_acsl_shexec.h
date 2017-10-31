/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2017                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

/*! ***********************************************************************
 * \file  e_acsl_shexec.h
 * \brief Interface for running shell commands
***************************************************************************/

#ifndef E_ACSL_SHEXEC_H
#define E_ACSL_SHEXEC_H

#include <stddef.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include "e_acsl_string.h"

/*! \class ipr_t
 *  \brief Result struct for `shexec` function -- execute a command in the
 *  shell via fork/exec and return results */
typedef struct {
  /** \brief resulting STDERR stream as \p const \p char* */
  char *stderrs;
  /** \brief Supplied STDIN stream as \p const \p char* */
  char *stdins;
  /** \brief resulting STDOUT stream as \p const \p char* */
  char *stdouts;
  /** \brief Exit status of a program */
  int   exit_status;
  /** \brief ID of a child process this command has been executed in */
  pid_t pid;
  /** \brief Set to non-zero if child process is interrupted via a signal */
  int   signaled;
  /** \brief If \p signalled is set, \p signo is set to the number of signal
   * that interrupted execution of a child process */
  int   signo;
  /** \brief A command to execute. Needs to be NULL terminated  */
  char  **argv; /** \brief ARGV */
  /** \brief Message if the command has failed to run  */
  char *error;
} ipr_t;

/* \brief Read characters from a buffer associated with a file descriptor into
 * a C string
 *
 * Read a string from a buffer associated with a file descriptor by allocating
 * a string of an initial size and increasing the size of a buffer with
 * realloc.  This is for the cases when we can not quickly seek through the
 * file and identify the size of a buffer associated with a file descriptor
 *
 * \param fd - file descriptor to read from
 * \param bufsize - the number of characters we read a t a time
 *
 * \return NUL-terminated C string on success
 * \return NULL on failure */
static char* fd_read (int fd, short bufsize) {
  /* Read `buffer_size` chars at a time */
  short buffer_size = bufsize*sizeof(char);
  /* Size of the fetched string */
  int size = buffer_size;
  /* Buffer where for read data */
  char *buffer = (char*)private_malloc(size);
  /* The number of read bytes  */
  short fetched = 0;
  int rd = 0; /* Count of fetched characters */

  /* Each time the pointer is moved by `size - buffer_size`.
   * This is because initially the size of `buffer` is `buffer_size`. */
  while ((fetched = read(fd, buffer + size - buffer_size, buffer_size))) {
    rd += fetched;
    if (fetched != -1) {
      size += fetched*sizeof(char);
      buffer = private_realloc(buffer, size + 1);
    } else {
      return NULL;
    }
  }
  buffer[rd] = '\0';
  return buffer;
}

/* Execute a command in the shell and place results to data */
static ipr_t* __shexec (ipr_t *data) {
  int outfd[2], errfd[2], infd[2];
  int oldstdout, oldstderr, oldstdin;

  if (pipe(infd))  /* From where parent is going to read */
    data->error = nstrdup("Can not create a pipe for STDIN");
  if (pipe(outfd))  /* From where parent is going to read */
    data->error = nstrdup("Can not create a pipe for STDOUT");
  if (pipe(errfd))  /* From where parent is going to read */
    data->error = nstrdup("Can not create a pipe for STDERR");

  /* Immediately return if reading from one of the STD pipes failed */
  if (data->error)
    return data;

  /* Save stdin, stdout and stderr */
  oldstdin  = dup(0);
  oldstdout = dup(1);
  oldstderr = dup(2);

  /* Close stdin, stdout and stderr */
  close(0);
  close(1);
  close(2);

  dup2(infd[0], 0);  /* Make the read end of infd as STDIN */
  dup2(outfd[1],1);  /* Make the write end of outfd as STDOUT */
  dup2(errfd[1],2);  /* Make the write end of outfd as STDERR */

  pid_t pid = fork();

  if(!pid) {
    /* Close the streams as they are not required for a child */
    close(infd[0]); close(outfd[0]); close(errfd[0]);
    close(infd[1]); close(outfd[1]); close(errfd[1]);

    execvp(data->argv[0],data->argv);
    if (errno) {
      data->error = nstrdup("Failed to execute:\n  ");
      char **arg = data->argv - 1;
      while(*++arg)
        data->error = sappend(*arg, data->error, " ");
    }
  } else {
    close(0);
    close(1);
    close(2);
    dup2(oldstdin, 0);
    dup2(oldstdout,1);
    dup2(oldstderr,2);
    close(outfd[1]);
    close(errfd[1]);
    close(infd[0]);

    /* If data->stdin string is supplied, write that string to the child's
       stdin first */
    if (data->stdins)  /* Return NULL if write fails */
      if (write(infd[1], data->stdins, strlen(data->stdins)) == -1)
        return NULL;

    /* Read from child's stdout and stderr */
    data->stdouts = fd_read(outfd[0], 256);
    if (!data->stdouts)
      data->error = nstrdup("Error reading from STDOUT pipe");
    data->stderrs = fd_read(errfd[0], 256);
    if (!data->stderrs)
      data->error = nstrdup("Error reading from STDERR pipe");

    /* Close file descriptors that are still open */
    close(outfd[0]); /* read  end of STDOUT */
    close(errfd[0]); /* read  end of STDERR */
    close(infd[1]);  /* write end of STDIN */

    int status;
    waitpid(pid, &status, 0); /* wait for the child to finish */

    data->exit_status = WEXITSTATUS(status); /* exit status */
    data->pid = pid;                         /* process number */
    data->signaled = WIFSIGNALED(status);    /* signal caught */
    data->signo = WTERMSIG(status);          /* signal number caught */
    return data;
  }
  return NULL;
}

/* \brief Deallocate an `ipr_t` structure returned by `shexec` */
static void free_ipr (ipr_t* ipr) {
  if (ipr) {
    if (ipr->stdouts)
      private_free(ipr->stdouts);
    if (ipr->stderrs)
      private_free(ipr->stderrs);
    if (ipr->error)
      private_free(ipr->error);
    if (ipr->stdins)
      private_free(ipr->stdins);
    private_free(ipr);
  }
}

/* \brief Execute a command given via parameter `data` in the current shell
 *  and return the dynamically allocated struct `ipr_t` which captures the
 *  results of the command's execution.
 *
 * \param data - command to execute. `data` is expected to be a NULL-terminated
 *  array of C strings.
 * \param sin - if not NULL, a C string given via `sin` is supplied as standard
 *  input to the executed command.
 * \return - heap-allocated struct `ipr_t` which describes the output of the
 *  executed command. Deallocation of this struct must be performed via the
 *  `free_ipr` function. */
static ipr_t* shexec (char **data, const char *sin) {
  /* Allocate and initialise the `ipr_t` struct to store the results
   * of the command execution */
  ipr_t  *ipr = (ipr_t*)private_malloc(sizeof(ipr_t));
  ipr->stderrs = NULL;
  ipr->stdouts = NULL;
  ipr->stdins = nstrdup(sin);
  ipr->argv = data;
  ipr->exit_status = 0;
  ipr->pid = 0;
  ipr->signaled = 0;
  /* Run the command returning a pointer to `ipr_t` */
  return __shexec(ipr);
}
#endif
