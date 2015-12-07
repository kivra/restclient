PROJECT = restc

# Dependecies ##########################################################
DEPS = hackney jsx erlsom mochiweb_util

dep_hackney       = git https://github.com/benoitc/hackney     1.3.2
dep_jsx           = git https://github.com/talentdeficit/jsx   v2.6.1
dep_erlsom        = git https://github.com/willemdj/erlsom     master
dep_mochiweb_util = git https://github.com/kivra/mochiweb_util master

# Standard targets #####################################################
include erlang.mk

# eof
