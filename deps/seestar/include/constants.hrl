%%% Copyright 2014 Aleksey Yeschenko
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-define(DEFAULT_PORT, 9042).

%% Error codes.
-define(SERVER_ERROR,      16#0000).
-define(PROTOCOL_ERROR,    16#000A).

-define(BAD_CREDENTIALS,   16#0100).

%% 1xx: problem during request execution
-define(UNAVAILABLE,       16#1000).
-define(OVERLOADED,        16#1001).
-define(IS_BOOTSTRAPPING0, 16#1002).
-define(TRUNCATE_ERROR,    16#1003).
-define(WRITE_TIMEOUT,     16#1100).
-define(READ_TIMEOUT,      16#1200).

%% 2xx: problem validating the request
-define(SYNTAX_ERROR,      16#2000).
-define(UNAUTHORIZED,      16#2100).
-define(INVALID,           16#2200).
-define(CONFIG_ERROR,      16#2300).
-define(ALREADY_EXISTS,    16#2400).
-define(UNPREPARED,        16#2500).
