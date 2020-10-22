%%%-------------------------------------------------------------------
%%% Created : 22 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(message).

-behaviour(gen_server).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("message.hrl").

%% Ercloud
-include_lib("erlcloud/include/erlcloud_cloudformation.hrl").
%% For LOG purposes
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% gen_server exports
-export([init/1,
         start_link/0,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% APIs that are NOT using the gen_server
-export([create_stack/0,
         delete_stack/0,
         status_stack/0,
         send/1]).

%% APIs that are using the gen_server
-export([config_lambda/1]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%% SLEEP for invalid read message server
-define(INV_SERVER_SLEEP, 1000).

%% Max burst of messages to read
-define(MAX_BURST_MSG, 100).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  logger:set_module_level(?MODULE, debug),
  %% Enable trapping exits
  process_flag(trap_exit, true),

  %% Initialise environmental variables
  application:set_env(erlcloud, aws_access_key_id, ?AWS_ACCESS_KEY_ID),
  application:set_env(erlcloud, aws_secret_access_key, ?AWS_SECRET_ACCESS_KEY),
  application:set_env(erlcloud, aws_region, ?AWS_REGION),

  %% Check if the stack is already created and running
  ?LOG_INFO("Stack Status: ~p", [status_stack()]),

  %% Send message to itself to start the process of receiving messages
  %% from AWS SQS
  erlang:send(?MODULE, receive_aws_sqs_msg),
  {ok,#{msg_counter => 0, lambda_event => true}}.

%% Do Nothing, the lambda function will consume the AWS SQS messages
handle_info(receive_aws_sqs_msg, State = #{lambda_event := true}) ->
  %% Restart the cycle
  erlang:send(?MODULE, receive_aws_sqs_msg),
  {noreply, State};

%% Lambda is disabled, we have to read the messages
handle_info(receive_aws_sqs_msg, State = #{msg_counter := MsgCounter}) ->
  %% Read the maximum number of messages passed
  RcvMsg = request_aws_sqs_message(?MAX_BURST_MSG),
  %% Restart the cycle
  erlang:send(?MODULE, receive_aws_sqs_msg),
  {noreply, State#{msg_counter => MsgCounter + RcvMsg}};

handle_info(_Info, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call( {config_lambda, Enable}, _From, State ) ->
  Res = config_lambda_priv(Enable),
  case Res of
    {ok, _} -> % Update State
               {reply, Res, State#{lambda_event => Enable}};
    _       -> %Keep State
               {reply, Res, State}
  end;

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Public Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This function creates the stack using CloudFormation.template
%%
%% @end
%%--------------------------------------------------------------------
-spec create_stack() -> ok.
create_stack() ->
  %% Read File
  { ok, Bin } = file:read_file(?AWS_STACK_LOC),
  TemplateBody = erlang:binary_to_list(Bin),
  %% Create the Stack
  case erlcloud_cloudformation:create_stack( #cloudformation_create_stack_input{
                                             capabilities = ["CAPABILITY_NAMED_IAM"],
                                             stack_name = ?AWS_STACK_NAME,
                                             template_body = TemplateBody }) of
    {ok, Info}   -> ?LOG_INFO("Stack created with success: ~p", [Info]);
    {error, Err} -> ?LOG_ERROR("Error during stack creation: ~p", [Err])
  end,
  ok.

%%--------------------------------------------------------------------
%% @doc This function deletes the stack
%%
%% @endSLEEP_RETRY_LAMBDA_EVENT
%%--------------------------------------------------------------------
-spec delete_stack() -> ok.
delete_stack() ->
  %% Delete the Stack
  case erlcloud_cloudformation:delete_stack( #cloudformation_delete_stack_input{
                                             stack_name = ?AWS_STACK_NAME }) of
    ok -> ?LOG_INFO("Stack deleted with success");
    R  -> ?LOG_ERROR("Error during stack deletion: ~p", [R])
  end,
  ok.

%%--------------------------------------------------------------------
%% @doc This checks the stack status
%%
%% @end
%%--------------------------------------------------------------------
-spec status_stack() -> { ok | error , string() | atom() }.
status_stack() ->
  %% Check if the stack is already created and running
  StackList = case erlcloud_cloudformation:describe_stacks([]) of
    {ok, [[{stacks,[[{member,MemberList}]]},_]], _} ->
        MemberList;
    _ ->
        error
  end,
  %% Search for our crypto stack
  ListFilter = lists:filtermap(
    fun(Elem) ->
       case proplists:get_value(stack_name, Elem) of
         ?AWS_STACK_NAME -> {true, proplists:get_value(stack_status, Elem)};
                       _ -> false
          end
    end,
    StackList),

  %% Return results
  case {StackList, ListFilter} of
    { [], _  }      -> {ok, no_stack_available};
    { _, []  }      -> {ok, crypto_stack_not_created};
    { _, [Status] } -> {ok, Status};
    _               ->
        ?LOG_ERROR("Stack is not available or we don't have access"),
        {error, access_error}
  end.

%%--------------------------------------------------------------------
%% @doc Enable or Disable Lambda function to read messages
%%
%% @param Enable Enable or Disable the lambda configuration
%% @end
%%--------------------------------------------------------------------
-spec config_lambda_priv(boolean()) -> { ok|error , proplists:proplist() | atom() }.
config_lambda_priv(Enable) ->
  %% Check Our Stack is created
  case status_stack() of
    {ok,"CREATE_COMPLETE"} ->  update_lambda_event_source(Enable);
     _ ->                      {error, crypto_stack_not_created}
  end.

update_lambda_event_source(Enable) ->
  %% Retrieve all stack resource information
  [[{summaries,[[{member, Resources }]] } ]] =
    erlcloud_cloudformation:list_stack_resources_all([], ?AWS_STACK_NAME),
  %% Retrieve UUID of the Event Source for lambda function
  [Uuid] = lists:filtermap(
    fun(X) ->
      case proplists:get_value(logical_resource_id, X) of
        ?AWS_EVT_SOURCE_NAME -> {true,
                                 proplists:get_value(physical_resource_id, X)};
         _ -> false
      end
    end,
    Resources),
  %% Convert Uuid to binary
  UuidBin = erlang:list_to_binary(Uuid),
  %% Check current state and update if needed
  case { Enable, lambda_event_state(UuidBin) } of
    { true,  <<"Enabled">> }   -> {ok, already_enabled};
    { false, <<"Disabled">> }  -> {ok, already_disabled};
    { _, <<"Enabling">> }      -> {error, wait_it_is_enabling};
    { _, <<"Disabling">> }     -> {error, wait_it_is_disabling};
    _ -> %% Update the event lambda status
         erlcloud_lambda:update_event_source_mapping( UuidBin, 10, Enable,
                                                <<?AWS_LAMBDA_FUNCTION_NAME>>)
  end.

-spec lambda_event_state(binary()) -> binary().
lambda_event_state(Uuid) ->
  {ok, List } = erlcloud_lambda:get_event_source_mapping(Uuid),
  proplists:get_value(<<"State">>, List).

%%--------------------------------------------------------------------
%% @doc This function sends a message to the AWS SQS
%%
%% @param String Message to be sent
%% @end
%%--------------------------------------------------------------------
-spec send(string()) -> ok.
send(Msg) ->
  [{message_id,_}, {md5_of_message_body,_}] =
    erlcloud_sqs:send_message(?AWS_CRYPTO_SQS_NAME, Msg),
  ok.

%%--------------------------------------------------------------------
%% @doc Enable or Disable Lambda function to read messages
%%
%% @param Enable Enable or Disable the lambda configuration
%% @end
%%--------------------------------------------------------------------
-spec config_lambda(boolean()) -> { ok|error , proplists:proplist() | atom() }.
config_lambda(Enable) ->
  gen_server:call(?MODULE, {config_lambda, Enable}).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc This function request messages from AWS SQS and start a new gen_server
%%      to handle the validation
%%
%% @param MaxMsgServers Current maximum number of supported messages by this
%%        application and if greater than 10, it assigns the maximum supported
%%        by aws sqs that is 10.
%% @end
%%--------------------------------------------------------------------
-spec request_aws_sqs_message(integer()) -> integer().
request_aws_sqs_message(MaxMsgServers)
  when MaxMsgServers > ?AWS_SQS_MAX_READ_MSGS ->
  request_aws_sqs_message(?AWS_SQS_MAX_READ_MSGS );

request_aws_sqs_message(MaxMsgServers)
  when MaxMsgServers =< ?AWS_SQS_MAX_READ_MSGS ->
  %% read messages (If no server is available, an exception is raised)
  MessageList = try erlcloud_sqs:receive_message(?AWS_CRYPTO_SQS_NAME,
                                                 all,
                                                 MaxMsgServers) of
    [{messages, List}] -> List
  catch
    _:?AWS_SQS_INVALID_REQUEST(_) ->
        ?LOG_ERROR("The AWQ server is not accessible"),
        timer:sleep(?INV_SERVER_SLEEP),
        %% return no messages available
        []
  end,

  %% Check body message is ok, and create servers to handling it
  lists:foreach(
    fun([ {body, Body}, {md5_of_body, RcvMd5}, _,
          {receipt_handle,HandleMsg}, _, _ ]) ->
      %% Check message integrity
      case util:md5sum(Body) of
        RcvMd5 -> %% Decode the body message to a map.
                  ?LOG_DEBUG("Message: ~p", [Body]),
                  %% TODO: Process the message
                  %% The message was processed, delete it
                  ok =
                   erlcloud_sqs:delete_message(?AWS_CRYPTO_SQS_NAME, HandleMsg);
        _ ->      % Corrupted message, discard
                  none
      end
    end,
    MessageList),
  length(MessageList).
