dnl Copyright © 2020 Roel Janssen <roel@gnu.org>
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program. If not, see <http://www.gnu.org/licenses/>.
dnl -*- Autoconf -*-

AC_INIT([sgf-data-access-requirements], [0.0.1], [roel@gnu.org])
AM_INIT_AUTOMAKE([color-tests -Wall -Werror -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

dnl Installation directories for .scm and .go files.
moddir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
godir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([moddir])
AC_SUBST([godir])

if $GUILE_TOOLS | grep -q compile; then
   true
else
   AC_MSG_ERROR([Guile 3.0, 2.2, or 2.0 required.])
fi

dnl Test whether the "sg-web" command is somewhere in PATH to determine
dnl SPARQLing-genomics is available.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([SGWEB], [sg-web])
if test "x$SGWEB" = "x"; then
   AC_MSG_ERROR([Cannot find the installation of SPARQLing-genomics.])
fi

AC_CONFIG_FILES([Makefile
                 web/Makefile
                 web/www/db/data-access-requirements.scm])

AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT
