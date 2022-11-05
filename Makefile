PROJECT = trace_helper
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

ERLC_OPTS += +debug_info

SHELL_OPTS += -eval 'application:ensure_all_started($(PROJECT)).'

include erlang.mk
