%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

%%
%%----------------------------------------------------------------------
%% Purpose: Handle DTLS record protocol. (Parts that are not shared with SSL/TLS)
%%----------------------------------------------------------------------
-module(dtls_record).

-include("dtls_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("dtls_handshake.hrl").
-include("ssl_cipher.hrl").

%% Handling of incoming data
-export([get_dtls_records/2,  init_connection_states/2]).

%% Decoding
-export([decode_cipher_text/2]).

%% Encoding
-export([encode_plain_text/4, encode_tls_cipher_text/5, encode_change_cipher_spec/2]).

%% Protocol version handling
-export([protocol_version/1, lowest_protocol_version/1, lowest_protocol_version/2,
	 highest_protocol_version/1, highest_protocol_version/2,
	 is_higher/2, supported_protocol_versions/0,
	 is_acceptable_version/2]).

%% DTLS Epoch handling
-export([init_connection_state_seq/2, current_connection_state_epoch/2,
	 set_connection_state_by_epoch/3, connection_state_by_epoch/3]).

-export_type([dtls_version/0, dtls_atom_version/0]).

-type dtls_version()       :: ssl_record:ssl_version().
-type dtls_atom_version()  :: dtlsv1 | 'dtlsv1.2'.

-compile(inline).

%%====================================================================
%% Internal application API
%%====================================================================
%%--------------------------------------------------------------------
-spec init_connection_states(client | server, one_n_minus_one | zero_n | disabled) ->
 				    ssl_record:connection_states().
%% %
						%
%% Description: Creates a connection_states record with appropriate
%% values for the initial SSL connection setup.
%%--------------------------------------------------------------------
init_connection_states(Role, BeastMitigation) ->
    ConnectionEnd = ssl_record:record_protocol_role(Role),
    Current = initial_connection_state(ConnectionEnd, BeastMitigation),
    Pending = ssl_record:empty_connection_state(ConnectionEnd, BeastMitigation),
    #{write_msg_seq => 0, 
      prvious_read  => undefined,
      current_read  => Current,
      pending_read  => Pending,
      prvious_write => undefined,
      current_write => Current,
      pending_write => Pending}.
  
%%--------------------------------------------------------------------
-spec get_dtls_records(binary(), binary()) -> {[binary()], binary()} | #alert{}.
%%
%% Description: Given old buffer and new data from UDP/SCTP, packs up a records
%% and returns it as a list of tls_compressed binaries also returns leftover
%% data
%%--------------------------------------------------------------------
get_dtls_records(Data, <<>>) ->
    get_dtls_records_aux(Data, []);
get_dtls_records(Data, Buffer) ->
    get_dtls_records_aux(list_to_binary([Buffer, Data]), []).

get_dtls_records_aux(<<?BYTE(?APPLICATION_DATA),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary, Rest/binary>>,
		     Acc) ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?APPLICATION_DATA,
					 version = {MajVer, MinVer},
					 epoch = Epoch, sequence_number = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?HANDSHAKE),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length),
		       Data:Length/binary, Rest/binary>>, Acc) when MajVer >= 128 ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?HANDSHAKE,
					 version = {MajVer, MinVer},
					 epoch = Epoch, sequence_number = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?ALERT),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary,
		       Rest/binary>>, Acc) ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?ALERT,
					 version = {MajVer, MinVer},
					 epoch = Epoch, sequence_number = SequenceNumber,
					 fragment = Data} | Acc]);
get_dtls_records_aux(<<?BYTE(?CHANGE_CIPHER_SPEC),?BYTE(MajVer),?BYTE(MinVer),
		       ?UINT16(Epoch), ?UINT48(SequenceNumber),
		       ?UINT16(Length), Data:Length/binary, Rest/binary>>,
		     Acc) ->
    get_dtls_records_aux(Rest, [#ssl_tls{type = ?CHANGE_CIPHER_SPEC,
					 version = {MajVer, MinVer},
					 epoch = Epoch, sequence_number = SequenceNumber,
					 fragment = Data} | Acc]);

get_dtls_records_aux(<<0:1, _CT:7, ?BYTE(_MajVer), ?BYTE(_MinVer),
		       ?UINT16(Length), _/binary>>,
		     _Acc) when Length > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_dtls_records_aux(<<1:1, Length0:15, _/binary>>,_Acc)
  when Length0 > ?MAX_CIPHER_TEXT_LENGTH ->
    ?ALERT_REC(?FATAL, ?RECORD_OVERFLOW);

get_dtls_records_aux(Data, Acc) ->
    case size(Data) =< ?MAX_CIPHER_TEXT_LENGTH + ?INITIAL_BYTES of
	true ->
	    {lists:reverse(Acc), Data};
	false ->
	    ?ALERT_REC(?FATAL, ?UNEXPECTED_MESSAGE)
    end.

encode_plain_text(Type, Version, Data,
		  #{current_write :=
			#{epoch := Epoch,
			  sequence_number := Seq,
			  compression_state := CompS0,
			  security_parameters :=
			      #security_parameters{
				 cipher_type = ?AEAD,
				 compression_algorithm = CompAlg}
			 }= WriteState0} = ConnectionStates) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Data, CompS0),
    WriteState1 = WriteState0#{compression_state => CompS1},
    AAD = calc_aad(Type, Version, Epoch, Seq),
    {CipherFragment, WriteState} = ssl_record:cipher_aead(dtls_v1:corresponding_tls_version(Version),
							  Comp, WriteState1, AAD),
    CipherText = encode_tls_cipher_text(Type, Version, Epoch, Seq, CipherFragment),
    {CipherText, ConnectionStates#{current_write => WriteState#{sequence_number => Seq +1}}};

encode_plain_text(Type, Version, Data,
		  #{current_write := 
			#{epoch := Epoch,
			  sequence_number := Seq,
			  compression_state := CompS0,
			  security_parameters :=
			      #security_parameters{compression_algorithm = CompAlg}
			 }= WriteState0} = ConnectionStates) ->
    {Comp, CompS1} = ssl_record:compress(CompAlg, Data, CompS0),
    WriteState1 = WriteState0#{compression_state => CompS1},
    MacHash = calc_mac_hash(WriteState1, Type, Version, Epoch, Seq, Comp),
    {CipherFragment, WriteState} = ssl_record:cipher(dtls_v1:corresponding_tls_version(Version), 
						     Comp, WriteState1, MacHash),
    CipherText = encode_tls_cipher_text(Type, Version, Epoch, Seq, CipherFragment),
    {CipherText, ConnectionStates#{current_write => WriteState#{sequence_number => Seq +1}}}.

decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    epoch = Epoch,
			    sequence_number = Seq,
			    fragment = CipherFragment} = CipherText,
		   #{current_read :=
			 #{compression_state := CompressionS0,
			   security_parameters :=
			       #security_parameters{
				  cipher_type = ?AEAD,
				  compression_algorithm = CompAlg}
			  } = ReadState0} = ConnnectionStates0) ->
    AAD = calc_aad(Type, Version, Epoch, Seq),
    case ssl_record:decipher_aead(dtls_v1:corresponding_tls_version(Version),
				  CipherFragment, ReadState0, AAD) of
	{PlainFragment, ReadState1} ->
	    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
							   PlainFragment, CompressionS0),
	    ConnnectionStates = ConnnectionStates0#{
				  current_read => ReadState1#{
						    compression_state => CompressionS1}},
	    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
	#alert{} = Alert ->
	    Alert
    end;

decode_cipher_text(#ssl_tls{type = Type, version = Version,
			    epoch = Epoch,
			    sequence_number = Seq,
			    fragment = CipherFragment} = CipherText,
		   #{current_read :=
			 #{compression_state := CompressionS0,
			   security_parameters :=
			       #security_parameters{
				  compression_algorithm = CompAlg}
			  } = ReadState0}= ConnnectionStates0) ->
    {PlainFragment, Mac, ReadState1} = ssl_record:decipher(dtls_v1:corresponding_tls_version(Version),
							   CipherFragment, ReadState0, true),
    MacHash = calc_mac_hash(ReadState1, Type, Version, Epoch, Seq, PlainFragment),
    case ssl_record:is_correct_mac(Mac, MacHash) of
	true ->
	    {Plain, CompressionS1} = ssl_record:uncompress(CompAlg,
							   PlainFragment, CompressionS0),
	    ConnnectionStates = ConnnectionStates0#{
				  current_read => ReadState1#{
						    compression_state => CompressionS1}},
	    {CipherText#ssl_tls{fragment = Plain}, ConnnectionStates};
	false ->
	    ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC)
    end.

%%--------------------------------------------------------------------
-spec encode_change_cipher_spec(dtls_version(), ssl_record:connection_states()) ->
				       {iolist(), ssl_record:connection_states()}.
%%
%% Description: Encodes a change_cipher_spec-message to send on the ssl socket.
%%--------------------------------------------------------------------
encode_change_cipher_spec(Version, ConnectionStates) ->
    encode_plain_text(?CHANGE_CIPHER_SPEC, Version, <<1:8>>, ConnectionStates).

%%--------------------------------------------------------------------
-spec protocol_version(dtls_atom_version() | dtls_version()) ->
			      dtls_version() | dtls_atom_version().
%%
%% Description: Creates a protocol version record from a version atom
%% or vice versa.
%%--------------------------------------------------------------------
protocol_version('dtlsv1.2') ->
    {254, 253};
protocol_version(dtlsv1) ->
    {254, 255};
protocol_version({254, 253}) ->
    'dtlsv1.2';
protocol_version({254, 255}) ->
    dtlsv1.
%%--------------------------------------------------------------------
-spec lowest_protocol_version(dtls_version(), dtls_version()) -> dtls_version().
%%
%% Description: Lowes protocol version of two given versions
%%--------------------------------------------------------------------
lowest_protocol_version(Version = {M, N}, {M, O}) when N > O ->
    Version;
lowest_protocol_version({M, _}, Version = {M, _}) ->
    Version;
lowest_protocol_version(Version = {M,_}, {N, _}) when M > N ->
    Version;
lowest_protocol_version(_,Version) ->
    Version.

%%--------------------------------------------------------------------
-spec lowest_protocol_version([dtls_version()]) -> dtls_version().
%%     
%% Description: Lowest protocol version present in a list
%%--------------------------------------------------------------------
lowest_protocol_version([]) ->
    lowest_protocol_version();
lowest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    lowest_list_protocol_version(Ver, Vers).

%%--------------------------------------------------------------------
-spec highest_protocol_version([dtls_version()]) -> dtls_version().
%%
%% Description: Highest protocol version present in a list
%%--------------------------------------------------------------------
highest_protocol_version([]) ->
    highest_protocol_version();
highest_protocol_version(Versions) ->
    [Ver | Vers] = Versions,
    highest_list_protocol_version(Ver, Vers).

%%--------------------------------------------------------------------
-spec highest_protocol_version(dtls_version(), dtls_version()) -> dtls_version().
%%
%% Description: Highest protocol version of two given versions
%%--------------------------------------------------------------------
highest_protocol_version(Version = {M, N}, {M, O})   when N < O ->
    Version;
highest_protocol_version({M, _},
			Version = {M, _}) ->
    Version;
highest_protocol_version(Version = {M,_},
			{N, _}) when M < N ->
    Version;
highest_protocol_version(_,Version) ->
    Version.

%%--------------------------------------------------------------------
-spec is_higher(V1 :: dtls_version(), V2::dtls_version()) -> boolean().
%%
%% Description: Is V1 > V2
%%--------------------------------------------------------------------
is_higher({M, N}, {M, O}) when N < O ->
    true;
is_higher({M, _}, {N, _}) when M < N ->
    true;
is_higher(_, _) ->
    false.

%%--------------------------------------------------------------------
-spec supported_protocol_versions() -> [dtls_version()].
%%
%% Description: Protocol versions supported
%%--------------------------------------------------------------------
supported_protocol_versions() ->
    Fun = fun(Version) ->
		  protocol_version(Version)
	  end,
    case application:get_env(ssl, dtls_protocol_version) of
	undefined ->
	    lists:map(Fun, supported_protocol_versions([]));
	{ok, []} ->
	    lists:map(Fun, supported_protocol_versions([]));
	{ok, Vsns} when is_list(Vsns) ->
	    supported_protocol_versions(lists:map(Fun, Vsns));
	{ok, Vsn} ->
	    supported_protocol_versions([Fun(Vsn)])
     end.

supported_protocol_versions([]) ->
    Vsns = case sufficient_dtlsv1_2_crypto_support() of
	       true ->
		   ?ALL_DATAGRAM_SUPPORTED_VERSIONS;
	       false ->
		   ?MIN_DATAGRAM_SUPPORTED_VERSIONS
	   end,
    application:set_env(ssl, dtls_protocol_version, Vsns),
    Vsns;

supported_protocol_versions([_|_] = Vsns) ->
    case sufficient_dtlsv1_2_crypto_support() of
	true ->
	    Vsns;
	false ->
	    case Vsns -- ['dtlsv1.2'] of
		[] ->
		    ?MIN_SUPPORTED_VERSIONS;
		NewVsns ->
		    NewVsns
	    end
    end.

%%--------------------------------------------------------------------
-spec is_acceptable_version(dtls_version(), Supported :: [dtls_version()]) -> boolean().
%%
%% Description: ssl version 2 is not acceptable security risks are too big.
%%
%%--------------------------------------------------------------------
is_acceptable_version(Version, Versions) ->
    lists:member(Version, Versions).


%%--------------------------------------------------------------------
-spec init_connection_state_seq(dtls_version(), ssl_record:connection_states()) ->
				       ssl_record:connection_state().
%%
%% Description: Copy the read sequence number to the write sequence number
%% This is only valid for DTLS in the first client_hello
%%--------------------------------------------------------------------
init_connection_state_seq({254, _},
			  #{current_read := #{epoch := 0} = Read,
			    current_write := #{epoch := 0} = Write} = CS0) ->
    Seq = maps:get(sequence_number, Read),
    CS0#{current_write => Write#{sequence_number => Seq}};
init_connection_state_seq(_, CS) ->
    CS.

%%--------------------------------------------------------
-spec current_connection_state_epoch(ssl_record:connection_states(), read | write) ->
					    integer().
%%
%% Description: Returns the epoch the connection_state record
%% that is currently defined as the current conection state.
%%--------------------------------------------------------------------
current_connection_state_epoch(#{current_read := Current},
			       read) ->
    maps:get(epoch, Current);
current_connection_state_epoch(#{current_write := Current},
			       write) ->
    maps:get(epoch, Current).

%%--------------------------------------------------------------------

-spec connection_state_by_epoch(ssl_record:connection_states(), integer(), read | write) ->
				       ssl_record:connection_state().
%%
%% Description: Returns the instance of the connection_state record
%% that is defined by the Epoch.
%%--------------------------------------------------------------------
connection_state_by_epoch(#{current_read := #{epoch := Epoch}} = CS, Epoch, read) ->
    CS;
connection_state_by_epoch(#{pending_read := #{epoch := Epoch}} = CS, Epoch, read) ->
    CS;
connection_state_by_epoch(#{current_write := #{epoch := Epoch}} = CS, Epoch, write) ->
    CS;
connection_state_by_epoch(#{pending_write := #{epoch := Epoch}} = CS, Epoch, write) ->
    CS.
%%--------------------------------------------------------------------
-spec set_connection_state_by_epoch(ssl_record:connection_states(),
				    ssl_record:connection_state(), read | write)
				   -> ssl_record:connection_states().
%%
%% Description: Returns the instance of the connection_state record
%% that is defined by the Epoch.
%%--------------------------------------------------------------------
set_connection_state_by_epoch(#{current_read := #{epoch := Epoch}} = ConnectionStates0,
                              NewCS = #{epoch := Epoch}, read) ->
    ConnectionStates0#{current_read => NewCS};
set_connection_state_by_epoch(#{pending_read := #{epoch := Epoch}} = ConnectionStates0,
			      NewCS = #{epoch := Epoch}, read) ->
    ConnectionStates0#{pending_read => NewCS};
set_connection_state_by_epoch(#{current_write := #{epoch := Epoch}} = ConnectionStates0,
			      NewCS = #{epoch := Epoch}, write) ->
    ConnectionStates0#{current_write => NewCS};
set_connection_state_by_epoch(#{pending_write := #{epoch := Epoch}} = ConnectionStates0,
NewCS = #{epoch := Epoch}, write) ->
    ConnectionStates0#{pending_write => NewCS}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initial_connection_state(ConnectionEnd, BeastMitigation) ->
    #{security_parameters =>
	  ssl_record:initial_security_params(ConnectionEnd),
      epoch => 0,
      sequence_number => 1,
      beast_mitigation => BeastMitigation,
      compression_state  => undefined,
      cipher_state  => undefined,
      mac_secret  => undefined,
      secure_renegotiation => undefined,
      client_verify_data => undefined,
      server_verify_data => undefined
     }.

lowest_list_protocol_version(Ver, []) ->
    Ver;
lowest_list_protocol_version(Ver1,  [Ver2 | Rest]) ->
    lowest_list_protocol_version(lowest_protocol_version(Ver1, Ver2), Rest).

highest_list_protocol_version(Ver, []) ->
    Ver;
highest_list_protocol_version(Ver1,  [Ver2 | Rest]) ->
    highest_list_protocol_version(highest_protocol_version(Ver1, Ver2), Rest).

encode_tls_cipher_text(Type, {MajVer, MinVer}, Epoch, Seq, Fragment) ->
    Length = erlang:iolist_size(Fragment),
    [<<?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer), ?UINT16(Epoch),
       ?UINT48(Seq), ?UINT16(Length)>>, Fragment].

calc_mac_hash(#{mac_secret := MacSecret,
		security_parameters := #security_parameters{mac_algorithm = MacAlg}},
	      Type, Version, Epoch, SeqNo, Fragment) ->
    Length = erlang:iolist_size(Fragment),
    NewSeq = (Epoch bsl 48) + SeqNo,
    mac_hash(Version, MacAlg, MacSecret, NewSeq, Type,
	     Length, Fragment).

highest_protocol_version() ->
    highest_protocol_version(supported_protocol_versions()).

lowest_protocol_version() ->
    lowest_protocol_version(supported_protocol_versions()).

sufficient_dtlsv1_2_crypto_support() ->
    CryptoSupport = crypto:supports(),
    proplists:get_bool(sha256, proplists:get_value(hashs, CryptoSupport)).

mac_hash(Version, MacAlg, MacSecret, SeqNo, Type, Length, Fragment) ->
    dtls_v1:mac_hash(Version, MacAlg, MacSecret, SeqNo, Type,
		     Length, Fragment).

calc_aad(Type, {MajVer, MinVer}, Epoch, SeqNo) ->
    NewSeq = (Epoch bsl 48) + SeqNo,
    <<NewSeq:64/integer, ?BYTE(Type), ?BYTE(MajVer), ?BYTE(MinVer)>>.
