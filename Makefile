PROJECT = restc

# Dependecies ##########################################################
DEPS = hackney jsx erlsom mochiweb_util

dep_hackney       = git git@github.com:benoitc/hackney.git     1.3.2
dep_jsx           = git git@github.com:talentdeficit/jsx.git   v2.6.1
dep_erlsom        = git git@github.com:willemdj/erlsom.git     master
dep_mochiweb_util = git git@github.com:kivra/mochiweb_util.git master

# Standard targets #####################################################
include erlang.mk

# eof
