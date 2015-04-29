# slacker-rtm
Erlang [Real Time Messaging API client][2] for [Slack][1].

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

## Message formatting

Slack messages are JSON structures that follow specific [formatting][6].

Simple message with bot icon and colored left border:
```erlang
Msg = sr_message:format().
```

Simple message with table, bot icon and colored left border:
```erlang
Fields = [[{title, "Col"}, {value, 1}]].
Msg = sr_message:format_table().
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