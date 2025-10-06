#  Erdis — a Minimal Redis Clone in Erlang #

Erdis is a simplified re-implementation of [Redis](https://redis.io) written in **pure Erlang**.  
It supports the basic [RESP](https://redis.io/docs/latest/develop/reference/protocol-spec/) wire protocol and can be used directly with the official `redis-cli`.

### 1. Prerequisites

- Erlang/OTP **25+** (works on 27)
- `rebar3` build tool

Verify installation:

```bash
erl -version
rebar3 version
```

### Funcionalities ###

- TCP server compatible with `redis-cli`
- RESP protocol encoding/decoding
- In-memory key–value store using `gen_server`
- Commands:
  - `SET Milicah Krmpotich`
  - `GET Milicah`
  - `DEL Milicah`
  - `COMMAND`
  
- Compatible with Redis CLI on port **6379**
