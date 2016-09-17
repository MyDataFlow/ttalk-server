
TTalk
============
TTalk is a open source XMPP Server based on MongooseIM. Specifically designed for enterprise purposes, it is fault-tolerant, can utilize resources of multiple clustered machines and easily scale in need of more capacity (by just adding a box/VM). 
 

Main differences from the parent project
----------------------------------------
This project began its life as a fork of
[MongooseIM v16.1](http://github.com/esl/MongooseIM)
and later underwent some major cleanup, refactorization and optimization.

Major steps performed:


*   advanced offline messages storage
*   advanced message ack
*   advanced group support
*   web managent 

How to build
------------
1.  Requirements.

    To compile MongooseIM you need:
    *   GNU Make,
    *   GCC,
    *   Libexpat 1.95 or higher,
    *   Erlang/OTP R16B03-1 or higher,
    *   Reltool 0.6.4.1 or higher,
    *   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption,
    *   Zlib 1.2.3 or higher for Stream Compression support (XEP-0138). Optional.

2.  Compiling on UNIX-like systems.

    To compile MongooseIM, go to the main repo directory `$REPO` and execute
    the command (`$` stands for the shell prompt):

        $ make

    or

        $ ./rebar get-deps
        $ ./rebar compile

    To generate full MongooseIM release (with mysql, pgsql or other deps):

        $ make rel

    or

        $ ./rebar generate


    If more advanced release is required (with some specifyc db support only, f.e. mysql or pgsql) a `make configure` script with appropirate option(s) can be run before `make rel` or `./rebar generate`. `make configure` without any option will print following help message.

    ```
specifies which 3rd party deps will be included in release
possible options:
with-mysql	include mysql driver
with-pgsql	include pgsql driver
with-odbc	include standard ODBC driver shipped with Erlang/OTP
with-redis	include redis driver
with-riak      include riak driver
with-cassandra	include cassandra driver
full		include all above deps
    ```

    For example if mysql and redis support has to be added to the release, following command has to be run before `make rel`:

        $ make configure with-mysql with-redis

    The `make configure` command has to be run only once (unless one need to change the relase config and include some other dependecies).

    Take a look [here](http://mongooseim.readthedocs.org/en/latest/advanced-configuration/database-backends-configuration/)
    for instructions how to setup the external databases.

    `make rel` or `./rebar generate` commands will generate a self-contained OTP system image in the
    project's `rel/mongooseim` subdirectory. The contents of that directory are as
    follows:
    *   `rel/mongooseim/bin` - startup/administration scripts,
    *   `rel/mongooseim/etc` - configuration files,
    *   `rel/mongooseim/lib` - MongooseIM binary, header and runtime files,
    *   `rel/mongooseim/var` - spool directory,
    *   `rel/mongooseim/log` - log file directory,
    *   `rel/mongooseim/releases` - release files directory.

3.  Running MongooseIM.

    To run MongooseIM from the project tree after compiling it, change
    to `$REPO/rel/mongooseim`.

    There you can use the `mongooseim` command line administration script to
    start and stop MongooseIM. For example:

        $ bin/mongooseim start

    will start the server.

    You can also run the server in interactive mode:

        $ bin/mongooseim live

    There's also a tool called `mongooseimctl` allowing you to perform some
    operations on a running instance, e.g.:

        $ bin/mongooseimctl status
        The node mongooseim@localhost is started with status: started
        MongooseIM version 1.3.1 is running on that node

4.  Building the testing target and running tests.

    For testing purposes there's a different make target available:

        $ make devrel

    which will generate releases in `$REPO/dev/` and prepare
    them for testing and generating coverage reports.

    To run the tests (from project's root directory, i.e. `$REPO`):

        $ cd test
        $ make quicktest

    The test results will show up in the console`.



TTalk
============
TTalk 是一款基于MongooseIM的服务器。重点在企业级应用支持上，如果高可用，可扩展等。

主要不同
----------------------------------------

*   更好的离线存储机制
*   更好的消息确认机制
*   更好的组支持
*   web管理

交流
----------------------------------------
由于目前ttalk还无法运行目前只能用QQ群进行交流   
QQ群：216591294

TODO
----------------------------------------

* 简化链接流程
* 减少路由的流程
* disable negotiate features
* make message routing more simple

