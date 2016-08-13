PROJECT = restc

# Dependecies ##########################################################
DEPS = hackney jsx erlsom mochiweb_util

dep_hackney       = hex 1.6.1
dep_jsx           = hex 2.8.0
dep_erlsom        = hex 1.4.1
dep_mochiweb_util = git https://github.com/kivra/mochiweb_util de4fd402f7c1e1a6e683f73a41ae863b69888402

# Standard targets #####################################################
include erlang.mk

app:: rebar.config

# eof
