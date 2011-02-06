/* evoting.c - An example program of electronic voting in C */
/* copyright 2004-2009 David MENTRE */

/* this program is under GNU GPL license */

/* This program presents a prototype of electronic machine written in C.
   Its intent is to implement the core algorithms as a reference for
   further analysis */

/* TODO:

*/

#define _GNU_SOURCE
#define __SINGLE_THREAD__

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/time.h>
#include <time.h>
#include <string.h>


#if 0
#include "check_specs.h"
#if 0
#include <C:/Frama-C/share/frama-c/jessie/jessie_prolog.h>
#else
#include </usr/local/share/frama-c/jessie/jessie_prolog.h>
#endif
#endif

#include "jessie_prolog.h"

#define MAX_CANDIDATES 20
#define MAX_STRING_SIZE 255
#define MAX_VOTES_PER_CANDIDATE 100000

char *candidates[MAX_CANDIDATES];
int num_candidates;
int number_of_votes;
int counters[MAX_CANDIDATES];

FILE *log_desc;

/*@ assigns \nothing;
    ensures \result <= a && \result <= b;
    ensures \result == a || \result == b;
 */
int min(int a, int b)
{
  return (a)<(b)?(a):(b);
}

#ifdef NO_FRAMA_C
/*@ requires valid_string(str);
    assigns \nothing;
 */
void print_string(char *str)
{
  printf(str);
}

/*@ assigns \nothing;
 */
void print_int(int n)
{
  printf("%d", n);
}
#else
/* to avoid overhead of Frama-C regarding static C strings */
#define print_string(s)
#define print_int(n)
#endif

/*@ requires choice >= 0 && choice <= num_candidates;
    assigns \nothing;
 */
void print_candidate(int choice)
{
  print_int(choice);
  print_string("-'");
  print_string(candidates[choice]);
  print_string("'");
}

/*@ requires valid_string(filename);
    assigns candidates[..];
    ensures \result > 1 && \result < MAX_CANDIDATES;
    ensures \forall integer i;
                0 <= i < \result ==> valid_string(candidates[i]);
 */
int read_candidates(char *filename)
{
  int i;
  FILE *f;
  char buf[MAX_STRING_SIZE];
  char *s;

  f = fopen(filename, "r");
  if (f == NULL) {
    perror("unable to open candidates file");
    exit(1);
  }

  candidates[0] = strndup("None of those candidates", MAX_STRING_SIZE);

  i = 1;
  s = fgets(buf, MAX_STRING_SIZE, f); 
  /*@ loop invariant 1 <= i && i < MAX_CANDIDATES;
      loop invariant \forall integer j;
                           0 <= j < i ==> valid_string(candidates[j]);
      loop invariant s == NULL || valid_string(buf);
   */
  while (i < MAX_CANDIDATES - 1 && s != NULL) {
    int len, copied_len;

    len = strlen(buf);
    if (len > 0) {
      copied_len = min(MAX_STRING_SIZE, len - 1); /* do not copy newline */
      candidates[i] = strndup(buf, copied_len);
      i++;
    }
    s = fgets(buf, MAX_STRING_SIZE, f); 
  }

  if (i <= 1) {
    print_string("Not enough candidates\n");
    exit(1);
  }

  fclose(f);

  return i;
}

/*@ assigns \nothing; 
 */
void print_candidate_array(void)
{
  int i;

  for (i = 0; i < num_candidates; i++) {
    print_string("   ");
    print_int(i);
    print_string(". ");
    print_string(candidates[i]);
    print_string("\n");
  }
}

/*@ requires \valid(argv +(0..1));
    requires valid_string(argv[1]);
    assigns num_candidates, candidates[..];
    ensures num_candidates > 1 && num_candidates < MAX_CANDIDATES;
    ensures \forall integer i;
                0 <= i < num_candidates ==> valid_string(candidates[i]);
 */
void vote_setup(char **argv)
{
  print_string("**** Vote Setup ****\n");
  print_string("* reading candidates file '");
  print_string(argv[1]);
  print_string("'\n");

  num_candidates = read_candidates(argv[1]);

  print_int(num_candidates);
  print_string(" candidates:\n");
  print_candidate_array();
}

/*@ requires \valid(stdin);
    assigns \nothing;
    ensures \result >= 0 && \result < num_candidates;
 */
int get_vote(void)
{
  char buf[MAX_STRING_SIZE];
  char *s = NULL;

  while (1) {
    print_string("Choose a candidate:\n");
    print_candidate_array();
    print_string("Your choice (0-");
    print_int(num_candidates - 1);
    print_string("):");
    s = fgets(buf, MAX_STRING_SIZE, stdin);

    if (s != NULL) {
      int choice;

      choice = atoi(buf);
      if (choice >= 0 && choice < num_candidates) {
        print_string("Are you sure your vote ");
	print_candidate(choice);
	print_string(" is correct? (y/n)");
        s = fgets(buf, MAX_STRING_SIZE, stdin);
        if (s != NULL && strncmp(buf, "y", 1) == 0) {
          return choice;
        } else {
          print_string("Vote canceled, redo\n");
        }
      } else {
        print_string("Invalid choice (");
	print_int(choice);
	print_string("), redo\n");
      }
    } else {
      print_string("bad input, redo\n");
    }
  }
}

/*@ requires number_of_votes == 0;
    requires \forall integer i; 0 <= i < MAX_CANDIDATES ==> counters[i] == 0;
    requires 1 < num_candidates && num_candidates < MAX_CANDIDATES;
    requires \valid(stdin);
    assigns counters[..];
    assigns number_of_votes;
    ensures number_of_votes < MAX_CANDIDATES * MAX_VOTES_PER_CANDIDATE;
    ensures number_of_votes >= 0;
    ensures \forall integer i;
        0 <= i < num_candidates ==> counters[i] < MAX_VOTES_PER_CANDIDATE;
    ensures \forall integer i;
        0 <= i < num_candidates ==> counters[i] >= 0;
    ensures \forall integer i;
        num_candidates <= i < MAX_CANDIDATES ==> counters[i] == 0;
    ensures num_candidates < MAX_CANDIDATES;
 */
//     ensures number_of_votes == \sum(0, num_candidates,
//                                         \lambda integer i; counters[i]);
void voting(void)
{
  char buf[MAX_STRING_SIZE];
  char *s = "";

  print_string("**** Voting ****\n");
  /*@ loop invariant \forall integer i; 
         0 <= i < MAX_CANDIDATES ==> 0 <= counters[i]
                                     && counters[i] < MAX_VOTES_PER_CANDIDATE;
      loop invariant 0 <= number_of_votes;
      loop invariant number_of_votes < MAX_CANDIDATES * MAX_VOTES_PER_CANDIDATE;
      loop invariant \forall integer i; 
         num_candidates <= i < MAX_CANDIDATES ==> counters[i] == 0;
      loop invariant \valid(stdin);
   */
  while (number_of_votes < num_candidates * MAX_VOTES_PER_CANDIDATE) {
    print_string("* Do you want to Vote or Stop the vote (v/\"end of vote\")?");
    s = fgets(buf, MAX_STRING_SIZE, stdin);

    if (s != NULL && strncmp(buf, "end of vote", 11) == 0) {
      return;
    }
    
    if (s != NULL && strncmp(buf, "v", 1) == 0) {
      int chosen_vote;

      chosen_vote = get_vote();
      // Stop votes when we have reach the max of vote per candidate
      if (counters[chosen_vote] < MAX_VOTES_PER_CANDIDATE - 1) {
	counters[chosen_vote]++;
	number_of_votes++;
	print_string("Vote stored: ");
	print_candidate(chosen_vote);
	print_string("\n");
      } else
	return;
    }
  }
}

/* FIXME: in following function, we don't consider the case when two
          candidates have the same counters.
 */
/*@ requires 1 < num_candidates && num_candidates < MAX_CANDIDATES;
    assigns \nothing;
    ensures \result >= 1 && \result < num_candidates;
    ensures \forall integer i; 
               1 <= i < num_candidates ==> counters[\result] >= counters[i];
 */
int compute_winner(void)
{
  int i, winner;

  winner = 1; /* "No vote" is NOT taken into account */
  /*@ loop invariant 2 <= i && i < MAX_CANDIDATES;
      loop invariant \forall integer j;
                         1 <= j < i ==> counters[winner] >= counters[j];
      loop invariant winner >= 1 && winner < num_candidates;
   */
  for (i = 2; i < num_candidates; i++) {
    if (counters[i] > counters[winner]) { winner = i; }
  }
  return winner;
}

/*@ requires 1 < num_candidates && num_candidates < MAX_CANDIDATES;
    requires \forall integer i; 0 <= i < MAX_CANDIDATES
                ==> counters[i] < MAX_VOTES_PER_CANDIDATE && counters[i] >= 0;
    assigns \nothing;
 */
void compute_results(void)
{
  int total = 0;
  int i, winner, valid_total;

  print_string("**** Result ****\n");

  /*@ loop invariant 0 <= i && i <= num_candidates;
      loop invariant 0 <= counters[i] < MAX_VOTES_PER_CANDIDATE;
      loop invariant total >= 0;
      loop invariant total <= i * MAX_VOTES_PER_CANDIDATE;
      loop invariant \forall integer j; 0<=j<i ==> total >= counters[j];
  */
  for (i = 0; i < num_candidates; i++) {
    total += counters[i];
  }
  //@ assert total < MAX_CANDIDATES * MAX_VOTES_PER_CANDIDATE;
  //@ assert total >= counters[0];
  //@ assert counters[0] >= 0;
  valid_total = total - counters[0];

  //@ loop invariant 0 <= i;
  for (i = 0; i < num_candidates; i++) {
    print_int(counters[i]);
    print_string(" vote(s): ");
    print_candidate(i);
    print_string("\n");
  }

/*   for (i = 0; i < num_candidates; i++) { */
/*     dual_log("  %d. %s % 5d vote(s) (%.03f %%)\n", */
/*            i, candidates[i], counters[i], */
/*            (double)counters[i] / (double)valid_total * 100.0); */
/*   } */

  print_string("Total number of votes: ");
  print_int(total);
  print_string("\n");

  print_string("Total number of valid votes: ");
  print_int(valid_total);
  print_string("\n");

  winner = compute_winner();
  print_string("* Winner: ");
  print_int(winner);
  print_string("-");
  print_string(candidates[winner]);
  print_string("\n");
}

/*@ requires \valid(stdin);
    requires \valid(argv +(0..argc-1));
    requires valid_string(argv[1]);
    assigns log_desc;
    assigns num_candidates, candidates[..];
    assigns number_of_votes;
    assigns counters[..];
 */
int main(int argc, char **argv)
{
  int i;

  log_desc = fopen("c-evote.log", "w");
  if (log_desc == NULL) {
    perror("unable to open log file c-evote.log");
    exit(1);
  }

  print_string("**** Start of vote program (C version) ****\n");

  if (argc != 2) {
    fputs("bad number of arguments\n", stderr);
    fputs("usage: c-evoting candidates.txt\n", stderr);
    exit(1);
  }

  // clean-up all global data structures
  number_of_votes = 0;
  /*@ loop invariant \forall integer j; 0 <= j < i ==> counters[j] == 0;
      loop invariant 0 <= i && i <= MAX_CANDIDATES;
      loop invariant \valid(argv +(0..argc-1));
      loop invariant argc == 2;
      loop invariant valid_string(argv[1]);
   */
  for (i = 0; i < MAX_CANDIDATES; i++)
    counters[i] = 0;

  vote_setup(argv);
  // We have at least the "None of those candidates" option for voters
  //@ assert num_candidates > 1;

  // All candidates have a valid description string
  /*@ assert \forall integer i;
              0 <= i < num_candidates ==> valid_string(candidates[i]);
  */

  // All candidates have their counters set to zero and no vote registered
  //@ assert \forall integer i; 0 <= i < MAX_CANDIDATES ==> counters[i] == 0;
  //@ assert number_of_votes == 0;
  voting();
  // All candidates have their counters potentially incremented
  //@ assert \forall integer i; 0 <= i < num_candidates ==> counters[i] >= 0;

  // FIXME: How to specify that nothing is changed after voting?
  compute_results();

  /* free data structures */
  //@ assert num_candidates < MAX_CANDIDATES;
  //@ loop invariant 0 <= i && i <= MAX_CANDIDATES;
  for (i = 0; i < num_candidates; i++)
    free(candidates[i]);

  print_string("**** Stop of vote program (C version) ****\n");

  fclose(log_desc);
  return 0;
}
