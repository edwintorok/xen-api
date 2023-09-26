/*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
#ifndef _XA_AUTH_H_
#define _XA_AUTH_H_

#include <security/pam_appl.h>
#define XA_SUCCESS 0
#define XA_ERR_EXTERNAL 1

pam_handle_t *XA_mh_authorize_start (const char **error);
int XA_mh_authorize_stop (pam_handle_t *pamh, int rc, const char **error);

extern int XA_mh_authorize (pam_handle_t *handle, const char *username, const char *password, 
			    const char **error);

extern int XA_mh_chpasswd (pam_handle_t *handle, const char *username, const char *new_passwd, 
			   const char **error);

#endif /* _XA_AUTH_H_ */
