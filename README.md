NAME
====

log_funnel

SYNOPSIS
========

    > application:start(log_funnel).
    > log_funnel_client:open("/var/log/log_funnel.log").
    > log_funnel_client:append("blah blha blah").
    > file:rename("/var/log/log_funnel.log", "/var/log/log_funnel.log.1").
    > log_funnel_client:reopen().

DESCRIPTION
===========

This is a logging module.

i planned this module will work with a single process.
The exception is when all processes have opend different file each other.

`funnel` is from a image that;
many logs go to a funnel, and go out from thin tube.

ROTATION STORATEGY
==================

`reopen` can rotate, but avoid suddenly closing io device by
sleeping some micro seconds.

TODO
====

- trap error, and try reopen file
- change behaviour to gen_event to control mail box
