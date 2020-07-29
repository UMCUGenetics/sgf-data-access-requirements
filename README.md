Data access requirements form
=============================

This project extends SPARQLing-genomics with a form for registering data
access requirements.

Building the code requires:

- GNU Guile
- SPARQLing-genomics

Extending `sg-web` works as following:

1. Determine a place to install the form package and determine the
   version of GNU Guile.

```
export PREFIX="/path/to/installation/folder"
export GUILE_VERSION="3.0"
```

2. Build the form libraries and install them to `PREFIX`:

```
./configure --prefix="${PREFIX}"
make && make install
```

3. Set the following paths to extend `sg-web`:

```
export SG_WEB_ROOT="${PREFIX}/share/sgf-data-access-requirements/web"
export GUILE_LOAD_PATH="${PREFIX}/share/guile/site/${GUILE_VERSION}"
export GUILE_LOAD_COMPILED_PATH="${PREFIX}/lib/guile/${GUILE_VERSION}/site-ccache"
```

4. Run `sg-web` the usual way.

```
sg-web -f /path/to/sg-web.xml
```
