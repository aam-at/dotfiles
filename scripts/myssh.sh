#!/usr/bin/expect -f
spawn ssh $argv
expect "password:"
send "PASSWORD\r"
interact
