#!/bin/sh

exec erl +B -env ERL_LIBS _build/default/lib -config config/sys.config -setcookie ogonek -sname ogonek -eval 'application:ensure_all_started(ogonek).' -noinput
