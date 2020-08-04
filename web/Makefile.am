# Copyright © 2020 Roel Janssen <roel@gnu.org>
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/guile.am

STATIC_FILES    = static/css/udac.css                      \
                  static/js/data-access-requirements.js    \
                  static/images/ubec-logo.png

SOURCES         = www/db/data-access-requirements.scm      \
                  www/forms/data-access-requirements.scm   \
                  www/reports/data-access-requirements.scm

webdir          = $(pkgdatadir)/web
nobase_web_DATA = $(STATIC_FILES)
EXTRA_DIST     += $(STATIC_FILES)
