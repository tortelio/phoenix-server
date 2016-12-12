-module(fixtures).
-include("phoenix_internal.hrl").

-export([user/1]).
-export([item/1]).

user(user1) ->
    #phoenix_user{
       id = <<"9c29ea93-55a0-51aa-a3d1-40fe55e30164">>,
       name = <<"TEST_USER">>,
       password = <<"$2a$12$nm3dZoKKC9wD2EjDBhRB6.bW1zyB07TJk0NNCGg9SZIaxtn/VXFdS">>
      };
user(user2) ->
    #phoenix_user{
       id = <<"c75c5edb-cf3a-5360-867d-a9c2016ca975">>,
       name = <<"TEST_USER2">>,
       password = <<"$2a$12$4WL/0E2Fa4ibqBnSyg1TJuxhmWR9Ct4ofMOtPC2wUGpQlgUhbQoPW">>
      }.

item(user1_new_item) ->
    phoenix_item:defaults(
      #phoenix_item{
         id = <<"e904247f-fb8b-5671-a44e-c66a8269ff2a">>,
         title =  <<"TEST_ITEM_1_TITLE">>,
         done = false,
         owner = <<"9c29ea93-55a0-51aa-a3d1-40fe55e30164">>
        }).

