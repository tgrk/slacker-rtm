# slacker-rtm
Erlang [Real Time Messaging API client][2] for [Slack][1].

## Dependencies

* Erlang (>= R17) - because `maps`
* [rebar][8]

## Fetch dependencies and compile

```
$ rebar get-deps compile
```

### Authentication
The Slack RTM API uses custom implementation of oAuth 2.0 for authentiaction.
Authorization process is not part of this library and you have to manually
[register][4] your application and obtain valid [token][5].


## Quick start

Include library in your `rebar.conf`.
```erlang
{'slacker-rtm', "",
   {git, "https://github.com/tgrk/slacker-rtm.git", {branch, "master"}}}
```
Connect to API using obtained [token][5]:
```erlang
{ok, Pid} = sr:connect(BinToken).
```

## Sending messages

Slack RTM API supports only plain text messages. Rich formatted messages (aka attachmets) using JSON structures that follow specific [formatting][6] are not supported. Please refer to [Sending messages][2] section of oficial Slack RTM API documentation. For sending rich formatted messages (aka attachments) you have to use [slacker][9] - Slack Web API client.

Simple message can be created using following helper:
```erlang
JsonMsg = sr:format_message(1, <<"foo">>, <<"Hello world!">>).
ok = sr:send(Pid, JsonMsg).
```


## Testing
You have to setup API token file in order to use tests. Please add your
valid [token][5] into template (replace .template) file `test/api_token.term`.

Tests are executed as usual:
```bash
$ rebar co eunit skip_deps=true
```

[1]: http://www.slack.com/
[2]: https://api.slack.com/rtm
[3]: https://api.slack.com/web
[4]: https://api.slack.com/applications
[5]: https://api.slack.com/tokens
[6]: https://api.slack.com/docs/formatting
[7]: https://api.slack.com/methods/rtm.start
[8]: https://github.com/rebar/rebar
[9]: https://github.com/julienXX/slacker
