-module(ttalk_time).
-export([millisecond/0,microsecond/0]).
millisecond()->
    {Mega,Sec,Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) *1000 + Micro div 1000. 

microsecond()->
	{Mega,Sec,Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) *1000000 + Micro. 
	
