PROJECT = restc

# Dependecies ##########################################################
DEPS        = hackney
dep_hackney = git@github.com:benoitc/hackney.git 0.10.1

# Test Dependencies ####################################################
TEST_DEPS  = jsx erlsom
dep_jsx    = git://github.com/talentdeficit/jsx.git master
dep_erlsom = git://github.com/willemdj/erlsom.git   master

# Standard targets #####################################################
include erlang.mk

# eof
