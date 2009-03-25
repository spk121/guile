/* Copyright (C) 2009 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*
 * test-define-race.c
 *
 * Program to exhibit a race in the guile-1.8.x define code.
 * See https://savannah.gnu.org/bugs/index.php?24867 for general 
 * status and description.
 *
 * Summary: variable definition and lookup is not thread-safe in guile;
 * attempting to look up a variable while another thread is defining 
 * a variable can sometimes lead to the first thread loosing, and not
 * seeing an existing, defined variable. Alternately, difining two
 * different variables at the same time can result in one of them
 * failing to be defined; on rarer occasions, a seg-fault results.
 *
 * Compile as:
 * cc test-define-race.c -lpthread -lguile
 *
 * May need to run several times to see the bug(s).
 *
 * Linas Vepstas <linasvepstas@gmail.com> December 2008
 */

#include <libguile.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct
{
  int id;
  int count;
  int do_exit;
  int error_count;
  int last_ok;

} state;

static void * guile_mode_definer(void * ud)
{
  SCM result;
  int val;

  char buff[1000];
  state * s = (state *) ud;

  /* Increment before evaluation, in case evaluation raises an
     error. */
  s->count ++;
  
  if (s->last_ok)
    {
      /* See if the previous definition holds the expected value.  If
	 the expected value is undefined, scm_c_eval_string will raise
	 an error. */
      s->error_count ++;
      s->last_ok = 0;
      sprintf (buff, "x%d-%d\n", s->id, s->count - 1);
      result = scm_c_eval_string (buff);
      val = scm_to_int(result);

      if (val != s->count - 1)
	printf ("Define mismatch on thread %d\n", s->id);
      else
	s->error_count --;
    }

  /* Define a new variable with a new value. */
  sprintf (buff, "(define x%d-%d %d)\n", s->id, s->count, s->count);
  scm_c_eval_string (buff);

  /* If we reach here, the definition was apparently successful, so we
     can check it on the next iteration. */
  s->last_ok = 1;

  return NULL;
}

static void * definer (void *ud)
{
  int i;
  state * s = (state *) ud;

  while(!s->do_exit)
    for (i=0; i<4000; i++)
      {
	scm_with_guile (guile_mode_definer, ud);
	sched_yield();  /* try to get the threads to inter-leave a lot */
      }
  return NULL;
}

static void init_ctr(state *s, int val)
{
  s->id = val;
  s->count = 0;
  s->do_exit = 0;
  s->error_count = 0;
  s->last_ok = 0;
}

static void * setup(void * ud)
{
  int *duration = (int *)ud;

  /* Query an environment variable to find out how long to run this
     test for, defaulting to 10s. */
  *duration = scm_to_int (scm_c_eval_string ("(catch #t "
    "(lambda () "
    "  (round (string->number (string-append \"#e\" "
				    "(or (getenv \"GUILE_TEST_DEFINE_RACE_DURATION\") \"10\"))))) "
    "(lambda _ "
    "  (write _) (newline) 10))"));

  return NULL;
}

int main(int argc, char ** argv)
{
  pthread_t th1, th2, th3, th4;
  state counter1, counter2, counter3, counter4;
  int error_total;
  int duration;

  scm_with_guile (setup, &duration);

  init_ctr (&counter1, 1);
  init_ctr (&counter2, 2);
  init_ctr (&counter3, 3);
  init_ctr (&counter4, 4);

  pthread_create(&th1, NULL, definer, (void *) &counter1);
  pthread_create(&th2, NULL, definer, (void *) &counter2);
  pthread_create(&th3, NULL, definer, (void *) &counter3);
  pthread_create(&th4, NULL, definer, (void *) &counter4);

  sleep(duration);
  counter1.do_exit = 1;
  counter2.do_exit = 1;
  counter3.do_exit = 1;
  counter4.do_exit = 1;

  pthread_join(th1, NULL);
  pthread_join(th2, NULL);
  pthread_join(th3, NULL);
  pthread_join(th4, NULL);

  error_total = (counter1.error_count + counter2.error_count +
		 counter3.error_count + counter4.error_count);
  printf("test-define-race: %d error(s) in %ds\n", error_total, duration);
  exit (error_total);
}
