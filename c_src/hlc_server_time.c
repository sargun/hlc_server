#include "erl_nif.h"

#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>


#if defined(__MACH__) && defined(__APPLE__)
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <mach/clock.h>
#else
#include <syscall.h>
#endif

#if defined(__MACH__) && defined(__APPLE__)
clock_serv_t cclock;
#endif

#ifdef SYS_adjtimex
#include <sys/timex.h>
#endif

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_NOSYNC;
static ERL_NIF_TERM ATOM_CLOCKERR;


#ifdef _SYS_TIMEX_H
static ERL_NIF_TERM pt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct timex timex_buf;
  memset(&timex_buf, 0, sizeof(timex_buf));
  timex_buf.modes = 0;
  int status = adjtimex(&timex_buf);
  // status can also be there to handle leap sconds
  // We don't want to deal with that, we'll just error...at least for now
  if (status == TIME_BAD)
  {
    return ATOM_NOSYNC;
  }
  else if (status < 0)
  {
    return ATOM_ERROR;
  }
  else if (timex_buf.status & STA_CLOCKERR)
  {
    return ATOM_CLOCKERR;
  }
  else if (timex_buf.status & STA_UNSYNC)
  {
    return ATOM_NOSYNC;
  }
  // If we're more than 1 seconds off calendar time, abort
  else if (timex_buf.offset > 1000000)
  {
    return ATOM_NOSYNC;
  }
  else if (timex_buf.esterror > 1000000)
  {
    return ATOM_NOSYNC;
  }
  else if (!(timex_buf.status & STA_PLL))
  {
    return ATOM_NOSYNC;
  }
  else {
    uint64_t now = (uint64_t)timex_buf.time.tv_sec * 1000000000;
    if (timex_buf.status & STA_NANO)
    {
      now = now + timex_buf.time.tv_usec;
    }
    else
    {
      now = now + timex_buf.time.tv_usec * 1000;
    }
    return enif_make_tuple2(env, ATOM_OK, enif_make_uint64(env, now));
  }
}
#elif defined(__MACH__) && defined(__APPLE__)
static ERL_NIF_TERM pt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mach_timespec_t     mts;
    clock_get_time(cclock, &mts);
    uint64_t now = (uint64_t)mts.tv_sec * 1000000000;
    now = now + mts.tv_nsec;

    if(now) {
        return enif_make_tuple2(env, ATOM_OK, enif_make_uint64(env, now));
    }
    else {
        return ATOM_ERROR;
    }
}
#endif

static void init(ErlNifEnv *env)
{
  ATOM_OK = enif_make_atom(env, "ok");
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_NOSYNC = enif_make_atom(env, "nosync");
  ATOM_CLOCKERR = enif_make_atom(env, "clockerr");
#if defined(__MACH__) && defined(__APPLE__)
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
#endif

}


static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  init(env);
  return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
                      ERL_NIF_TERM load_info)
{
  init(env);
  return 0;
}

static void on_unload(ErlNifEnv *env, void *priv_data)
{
#if defined(__MACH__) && defined(__APPLE__)
  mach_port_deallocate(mach_task_self(), cclock);
#endif
}

static ErlNifFunc nif_funcs[] = {
  {"pt", 0, pt}
};

ERL_NIF_INIT(hlc_server_time, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)

