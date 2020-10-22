# AWS solution
This code is going to show how to implement and test the following AWS items:

 * How to use/configure server side encryption for AWS SQS;
 * How to produce and consume messages for AWS SQS using erlcloud
 * How to use Lambda Function to consume messages, including enable/diable on the fly;
 * How to create and delete aws stack using cloudformation files/templates.

## Getting started ##
You need to clone the repository and download rebar/rebar3 (if it's not already available in your path).
```
git clone https://github.com/thiagoesteves/aws_sse_sqs.git
cd aws_sse_sqs
```
To compile and run
```
make
```

### Security Credentials

you can provide them via `erlcloud` application environment variables at message.hrl file
```erlang
%% AWS Credential definitions
-define(AWS_ACCESS_KEY_ID,     "XXXXXXXX").
-define(AWS_SECRET_ACCESS_KEY, "XXXXXXXX").
-define(AWS_REGION,            "XXXXXXXX").
```
### CloudFormation file

you can find the template for the this project inside the folder aws. If you want to create/delete the stack by command line you can use the following commands:
```erlang
1> message:create_stack().
ok
2> message:delete_stack().
ok
```

### Project description
![Proposed Exercise](/doc/aws_sqs.png)

The image above shows the example solution, where the AWS SQS is receiving the messages and encrypting it. Once the message is received,
it can be redirect to the lambda function or to the Erlang code that is periodically checking the SQS Queue. The user can change the
message destination using the following function:

```erlang
1> message:config_lambda(true). % Enable lambda function destination (Default)
```

In order to send messages, you can use this API:
```erlang
1> message:send("Hello World!").
```

To check that the message is being processed by lambda function, the user can have a look in the CloudWatch log:
![Proposed Exercise](/doc/CloudWatchMsgs.png)

The lambda function can be disabled and the message will be handled by the Erlang code:

```erlang
1> message:config_lambda(false).
```

You must wait until the configuration is done, you can use the same command the check:
```erlang
1> message:config_lambda(false).
{ok,already_disabled}
```

Now, you can send a message and check:
```erlang
1> message:send("Hello World!").
ok
2> XXXXXXXXXXXXXXX debug: Message: "Hello World!"
```





