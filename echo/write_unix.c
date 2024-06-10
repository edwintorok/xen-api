/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS 1
#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include "caml/unixsupport.h"
#include <caml/signals.h>
#include <sys/signal.h>

#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

CAMLprim value caml1_unix_write(value fd, value buf, value vofs, value vlen)
{
  CAMLparam1(buf);
  long ofs, len, written;
  int numbytes, ret;
  char iobuf[UNIX_BUFFER_SIZE];

  ofs = Long_val(vofs);
  len = Long_val(vlen);
  written = 0;
  while (len > 0) {
    numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
    memmove (iobuf, &Byte(buf, ofs), numbytes);
    caml_enter_blocking_section();
    ret = write(Int_val(fd), iobuf, numbytes);
    caml_record_signal(SIGVTALRM);
    caml_leave_blocking_section();
    if (ret == -1) {
      if ((errno == EAGAIN || errno == EWOULDBLOCK) && written > 0) break;
      uerror("write", Nothing);
    }
    written += ret;
    ofs += ret;
    len -= ret;
  }
  CAMLreturn(Val_long(written));
}

