#!/bin/sh
clear
erl -sname mrsmaster -pa ebin -setcookie SECRET -s bootstrap start