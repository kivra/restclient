PROJECT = restc

# Dependecies ##########################################################
DEPS = hackney jsx erlsom mochiweb_util

dep_hackney       = hex 1.15.1
dep_jsx           = hex 2.9.0
dep_erlsom        = hex 1.5.0
dep_mochiweb_util = hex 0.1.0

# Standard targets #####################################################
include erlang.mk

app:: rebar.config

# eof
