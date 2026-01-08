PROJECT = daymate

LOCAL_DEPS = inets ssl

DEPS += cowboy jsx esqlite uuid

dep_cowboy_commit = 2.14.2
dep_jsx      = git https://github.com/talentdeficit/jsx.git       v3.0.0
dep_esqlite  = git https://github.com/mmzeeman/esqlite	  		  master
dep_uuid     = git https://github.com/okeuday/uuid.git            v1.7.5

include erlang.mk