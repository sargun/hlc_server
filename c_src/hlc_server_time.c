#include "erl_nif.h"

#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>


#include <syscall.h>
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
#endif


static void init(ErlNifEnv *env)
{
  ATOM_OK = enif_make_atom(env, "ok");
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_NOSYNC = enif_make_atom(env, "nosync");
  ATOM_CLOCKERR = enif_make_atom(env, "clockerr");
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
}

static ErlNifFunc nif_funcs[] = {
  {"pt", 0, pt}
};

ERL_NIF_INIT(hlc_server_time, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
