# ECoveralls

[Coveralls](https://coveralls.io) reports for Erlang projects.

[![Build Status](https://travis-ci.org/nifoc/ecoveralls.png)](https://travis-ci.org/nifoc/ecoveralls) [![Coverage Status](https://coveralls.io/repos/nifoc/ecoveralls/badge.png?branch=master)](https://coveralls.io/r/nifoc/ecoveralls?branch=master)

## Status

This is alpha software. Things might still change in ways that break everything.

## Usage

**This is currently only known to work with erlang.mk, Common Test and Travis CI.**

### cover.spec

In order to have one `.coverdata` file that includes all test suites it is recommended to add the following line to your `cover.spec`:

```erlang
{export, "logs/all.coverdata"}.
```

This will write an `all.coverdata` file to your `logs` directory, which is what Common Test uses by default.

### erlang.mk

Just add ECoveralls as a testing dependency.

```makefile
TEST_DEPS = ecoveralls
dep_ecoveralls = git https://github.com/nifoc/ecoveralls master
```

### Travis CI

Add the following target to your `Makefile` (after the erlang.mk include):

```makefile
coverage-report: $(shell ls -1rt `find logs -type f -name \*.coverdata 2>/dev/null` | tail -n1)
	$(gen_verbose) erl -noshell -pa ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()'

.PHONY: coverage-report
```

If you're not using erlang.mk you should replace `$(gen_verbose) ` with `@`.

Now you have to tell Travis to send data to Coveralls after a (successful) test run. You can do this by adding the following lines to your `.travis.yml`:

```yaml
after_success:
  - make coverage-report
```

## License

[ISC](https://en.wikipedia.org/wiki/ISC_license).

```
Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
```
