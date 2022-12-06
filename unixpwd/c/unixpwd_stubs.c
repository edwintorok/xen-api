/*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */

#include <errno.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>

#include "unixpwd.h"


CAMLprim        value
caml_unixpwd_getpwd(value caml_user)
{
    CAMLparam1(caml_user);
    char     *user;
    char     *passwd;
    CAMLlocal1(pw);

    user = caml_stat_strdup(String_val(caml_user));
    caml_enter_blocking_section();
    passwd = unixpwd_getpwd(user);
    caml_leave_blocking_section();
    caml_stat_free(user);

    if (passwd == NULL && errno != 0)
        uerror("getpwnam_r", caml_user);
    if (passwd == NULL)
        caml_failwith("unspecified error in caml_unixpwd_getpwd()");

    pw = caml_copy_string(passwd);
    free(passwd);
    CAMLreturn(pw);
}

CAMLprim        value
caml_unixpwd_getspw(value caml_user)
{
    CAMLparam1(caml_user);
    char     *user;
    char     *passwd;
    CAMLlocal1(pw);

    user = caml_stat_strdup(String_val(caml_user));
    caml_enter_blocking_section();
    passwd = unixpwd_getspw(user);
    caml_leave_blocking_section();
    caml_stat_free(user);

    if (passwd == NULL && errno != 0)
        uerror("getspnam_r", caml_user);
    if (passwd == NULL)
        caml_failwith("unspecified error in caml_unixpwd_getspw()");

    pw = caml_copy_string(passwd);
    free(passwd);
    CAMLreturn(pw);
}



CAMLprim        value
caml_unixpwd_get(value caml_user)
{
    CAMLparam1(caml_user);
    char     *user;
    char     *passwd;
    CAMLlocal1(pw);

    user = caml_stat_strdup(String_val(caml_user));
    caml_enter_blocking_section();
    passwd = unixpwd_get(user);
    caml_leave_blocking_section();
    caml_stat_free(user);

    if (passwd == NULL && errno != 0)
        uerror("unixpwd_get", caml_user);
    if (passwd == NULL)
        caml_failwith("unspecified error in caml_unixpwd_get()");

    pw = caml_copy_string(passwd);
    free(passwd);
    CAMLreturn(pw);
}

CAMLprim        value
caml_unixpwd_setpwd(value caml_user, value caml_password)
{
    CAMLparam2(caml_user, caml_password);
    char     *user;
    char     *password;
    int       rc;

    user = caml_stat_strdup(String_val(caml_user));
    password = caml_stat_strdup(String_val(caml_password));
    caml_enter_blocking_section();
    rc = unixpwd_setpwd(user, password);
    caml_leave_blocking_section();
    caml_stat_free(user);
    caml_stat_free(password);

    if (rc != 0)
        unix_error(rc, "unixpwd_setpwd", caml_user);
    CAMLreturn(Val_unit);
}

CAMLprim        value
caml_unixpwd_setspw(value caml_user, value caml_password)
{
    CAMLparam2(caml_user, caml_password);
    char     *user;
    char     *password;
    int       rc;

    user = caml_stat_strdup(String_val(caml_user));
    password = caml_stat_strdup(String_val(caml_password));
    caml_enter_blocking_section();
    rc = unixpwd_setspw(user, password);
    caml_leave_blocking_section();
    caml_stat_free(user);
    caml_stat_free(password);

    if (rc != 0)
        uerror("unixpwd_setspw", caml_user);
    CAMLreturn(Val_unit);
}

CAMLprim        value
caml_unixpwd_unshadow(void)
{
    CAMLparam0();
    char           *passwords;
    CAMLlocal1(str);

    caml_enter_blocking_section();
    passwords = unixpwd_unshadow();
    caml_leave_blocking_section();
    if (passwords == NULL && errno != 0)
        uerror("unixpwd_unshadow", Nothing);
    if (passwords == NULL)
        caml_failwith("unspecified error in caml_unixpwd_unshadow()");

    str = caml_copy_string(passwords);
    free(passwords);
    CAMLreturn(str);
}
