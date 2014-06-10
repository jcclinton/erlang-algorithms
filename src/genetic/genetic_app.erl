-module(genetic_app).
-behavior(application).

-export([start/2, stop/1]).


% processing starts automatically when the application starts, there is no api

% genetic algorithm used is the trying to find the number 42 from 4 random bytes
% with the descripton here http://www.ai-junkie.com/ga/intro/gat3.html

start(normal, _Args) ->
	genetic_sup:start_link().

stop(_State) ->
	ok.
