%%%-------------------------------------------------------------------
%%% Created : 22 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc Definitions for the whole application
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(message).
-define(message, true).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% AWS Credential definitions
-define(AWS_ACCESS_KEY_ID,     "XXXXXXXXX").
-define(AWS_SECRET_ACCESS_KEY, "XXXXXXXXX").
-define(AWS_REGION,            "sa-east-1").
%% AWS resource names
-define(AWS_STACK_NAME,           "CryptoStack").
-define(AWS_CRYPTO_SQS_NAME,      "CryptoSQS").
-define(AWS_LAMBDA_FUNCTION_NAME, "ProcessMsgs").
-define(AWS_EVT_SOURCE_NAME,      "LambdaFunctionSQSEventSourceMapping").
%% Stack template location
-define(AWS_STACK_LOC,            "aws/CloudFormation.template").

%% AWS SQS Definitions
-define(AWS_SQS_MAX_READ_MSGS,  10).
-define(AWS_SQS_INVALID_REQUEST(X), {_,{http_error,400,"Bad Request",X}} ).

%% AWS Errors
-define(AWS_RESOURCE_IN_USE,        {http_error,400,"Bad Request",
  <<"{\"Type\":\"User\",\"Message\":\"Cannot update the event source mapping because it is in use.\"}">>}).

-endif. %% message
