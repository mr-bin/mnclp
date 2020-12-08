%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.15.2

-ifndef(kv_pb).
-define(kv_pb, true).

-define(kv_pb_gpb_version, "4.15.2").

-ifndef('DATA_PB_H').
-define('DATA_PB_H', true).
-record(data,
        {key                    :: iodata() | undefined, % = 1, required
         value                  :: iodata() | undefined % = 2, required
        }).
-endif.

-ifndef('SET_REQUEST_PB_H').
-define('SET_REQUEST_PB_H', true).
-record(set_request,
        {req                    :: kv_pb:data() | undefined % = 1, required
        }).
-endif.

-ifndef('SET_RESPONSE_PB_H').
-define('SET_RESPONSE_PB_H', true).
-record(set_response,
        {error                  :: ok | internal | integer() | undefined % = 1, required, enum set_response.error_t
        }).
-endif.

-ifndef('GET_REQUEST_PB_H').
-define('GET_REQUEST_PB_H', true).
-record(get_request,
        {key                    :: iodata() | undefined % = 1, required
        }).
-endif.

-ifndef('GET_RESPONSE_PB_H').
-define('GET_RESPONSE_PB_H', true).
-record(get_response,
        {error                  :: ok | not_found | internal | integer() | undefined, % = 1, required, enum get_response.error_t
         req                    :: kv_pb:data() | undefined % = 2, optional
        }).
-endif.

-ifndef('REQ_ENVELOPE_PB_H').
-define('REQ_ENVELOPE_PB_H', true).
-record(req_envelope,
        {type                   :: set_request_t | set_response_t | get_request_t | get_response_t | integer() | undefined, % = 1, required, enum req_envelope.msg_type
         set_req                :: kv_pb:set_request() | undefined, % = 2, optional
         set_resp               :: kv_pb:set_response() | undefined, % = 3, optional
         get_req                :: kv_pb:get_request() | undefined, % = 4, optional
         get_resp               :: kv_pb:get_response() | undefined % = 5, optional
        }).
-endif.

-endif.