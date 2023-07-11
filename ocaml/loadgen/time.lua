-- Helper functions

local ffi = require "ffi"
ffi.cdef [[
    typedef long time_t;
    typedef int clockid_t;

    struct timespec {
        time_t tv_sec;
        long   tv_nsec;
    };

    int clock_gettime(clockid_t clockid, struct timespec *tp);
]]

-- distributed trace needs a clock that is comparable with other systems
local CLOCK_REALTIME = 0
-- local CLOCK_MONOTONIC = 1

-- cache namespace, see http://luajit.org/ext_ffi_tutorial.html
local C = ffi.C
local timespec = ffi.typeof("struct timespec[?]")

-- SCRIPTING says each thread has its own environment, so using globals here should be fine
local timespecptr = assert(timespec(1))

local clock_gettime

function clock_gettime()
  if (C.clock_gettime(CLOCK_REALTIME, timespecptr) < 0) then
    local errno = ffi.errno()
    error(string.format("clock_gettime: errno %d", errno))
  end

  return timespecptr[0]
end

local seconds

--- @return number
function seconds(ts)
  assert(ts)
  return tonumber(ts.tv_sec) + tonumber(ts.tv_nsec) / 1e9
end

-- wrk/wrk2 API hooks

---@type table<number, thread>
local threads = {}

-- global callbacks

-- global vars set in setup below,
-- not strictly needed, but helps with lua-language-server

---@type number
ThreadId = nil

-- IP addresses have been resolved, all threads initialized but not yet started
-- called once for each thread
--- @param thread thread
setup = function(thread)
  -- save the thread in the global threads table, so we can read back thread specific value in 'done'
  -- note that global variables accessed during 'init'/'request'/'response' will actually be variable on 'thread' here
  thread:set("ThreadId", #threads)
  table.insert(threads, thread)
end

---@param str string
---@return string
local csv_escape = function(str)
  return '"' .. string.gsub(str, '"', '""') .. '"'
end

---prints a CSV line with fields ordered according to [csv_header]
---@param csv_header table<number, string>
---@param data table<string,any>
local print_csv_line = function(csv_header, data)
  local line = {}
  for i, header in ipairs(csv_header) do
    local entry = data[header]
    if entry then
      line[i] = csv_escape(entry)
    else
      line[i] = ''
    end
  end
  io.write(table.concat(line, ','), '\n')
end

---@param trace_id string
---@param thread_id number
---@param request_id number
---@return string
local format_traceparent = function(trace_id, thread_id, request_id)
  return string.format("00-%s-%08x%08x-00", trace_id, thread_id, request_id)
end

done = function(_, _, _)
  local time_stop = seconds(clock_gettime())
  local file
  local test_start = nil
  local trace_id = nil

  local first_span = nil
  for _, thread in ipairs(threads) do
    local time_start = thread:get("Time_start")
    if not test_start or time_start < test_start then
      test_start = time_start
    end
    local output = thread:get("Output")
    if output and not file then
      ---@cast output string
      file = io.open(output, "a")
      if not file then
        error(string.format("Cannot open file to save report: %s", file))
      end
      print(string.format("Saving timings report to %s", output))
      io.output(file)
    end

    local id = thread:get("Trace_id")
    if id then
      trace_id = id
    end
  end
  local span_threads = {}
  for j, thread in ipairs(threads) do
    local n = thread:get("Response_counter")
    local spans = {}
    local default_span = thread:get("Default_span")
    for i = 1, n do
      local span = {}
      for k, v in pairs(default_span) do
          span[k] = v
      end
      local spans_http_response_body_size = thread:get("Spans_http_response_body_size")
      local spans_start_time = thread:get("Spans_start_time")
      local spans_finish_time = thread:get("Spans_finish_time")
      local spans_status_code = thread:get("Spans_status_code")
      local spans_body = thread:get("Spans_body")
      local spans_http_request_body_size = thread:get("Spans_http_request_body_size")
      local spans_traceparent = thread:get("Spans_traceparent")
      local spans_http_request_method = thread:get("Spans_http_request_method")
      span["http.response.status_code"] = spans_status_code[i]
      span["http.response.body_size"] = spans_http_response_body_size[i]
      span["start_time"] = spans_start_time[i]
      span["finish_time"] = spans_finish_time[i]
      span["_body"] = spans_body[i]
      span["http.request.body.size"] = spans_http_request_body_size[i]
      span["traceparent"] = spans_traceparent[i]
      span["http.request.method"] = spans_http_request_method[i]
      spans[i] = span
    end
    first_span = spans[1]
    span_threads[j] = spans
  end

  if not trace_id then
    error("No Trace_id found in threads")
  end
  io.flush()
  print("Here")
  io.flush()
  for k, v in pairs(first_span) do
    print(">>", k, v)
  end
  io.flush()

  -- pairs iteration order is undefined, store headers in a table with numeric index
  local csv_header = {}
  local hi = 0
  for k, _ in pairs(first_span) do
    csv_header[hi] = k
    hi = hi + 1
  end
  csv_header[hi] = 'thread'
  io.write(table.concat(csv_header, ','), "\n")
  io.flush()

  ---@cast trace_id string
  local traceparent = format_traceparent(trace_id, 0x7fffffff, 0)
  print_csv_line(csv_header,    {
      start_time = test_start
      ,
      finish_time = time_stop
      ,
      traceparent = traceparent
      ,
      thread = "PROCESS"
    }
  )

  for i, thread in ipairs(threads) do
    local Time_start = thread:get("Time_start")
    local ThreadId = thread:get("ThreadId")
    print_csv_line(csv_header,
      {
        start_time = Time_start
        ,
        finish_time = time_stop
        ,
        traceparent = traceparent
        ,
        thread = ThreadId
      })

    local spans = span_threads[i]
    for _, span in ipairs(spans) do
      print_csv_line(csv_header, span)
    end
  end

  if file then
    io.close(file)
  end
end

---@type table<number, string>
local Requests = {}

---@type string
local Trace_id = nil

-- Set span attributes following semantic conventions
-- See https://github.com/open-telemetry/semantic-conventions/blob/main/specification/http/http-spans.md

local User_agent = 'wrk'

---@type nil|string
local Rpc_system

---@type table<number, number>
Spans_http_response_body_size = {}

---@type table<number, number>
Spans_start_time = {}

---@type table<number, number>
Spans_finish_time = {}

--@type table<number, number>
Spans_status_code = {}

---@type table<number, string>
Spans_body = {}

--@type table<number, int>
Spans_http_request_body_size = {}

--@type table<number, string>
Spans_traceparent = {}

--@type table<number, method>
Spans_http_request_method = {}

--@type table<string, string>
Default_span = {}


---@param n number
---@param status_code number
---@param response_body nil|string
---@param timestamp number
local set_response = function(n, status_code, response_body, timestamp)
  if response_body then
    Spans_http_response_body_size[n] = #response_body
    if status_code >= 400 then
      -- will decode in python/ocaml post-processing code
      Spans_body[n] = response_body
    end
  end
  Spans_status_code[n] = status_code
  Spans_finish_time[n] = timestamp
end

---@param n number
---@param method string
---@param path string
---@param body nil|string
local build_request = function(n, method, path, body)
  local headers = {}
  local len = nil
  local traceparent = format_traceparent(Trace_id, ThreadId, n)

  headers['Host'] = wrk.host
  headers['traceparent'] = traceparent
  if body then
    len = #body
    headers['Content-Length'] = tostring(len)
    Spans_http_request_body_size[n] = len
  end
  Spans_traceparent[n] = traceparent
  Spans_start_time[n] = 0
  Spans_finish_time[n] = 0

  Spans_http_request_method[n] = method
  Default_span["user_agent.original"] = User_agent
  Default_span["server.address"] = tostring(wrk.thread.addr)
  local portstr = ""
  if wrk.port then
    Default_span["server.port"] = wrk.port
    portstr = string.format(":%d", wrk.port)
  end
  Default_span["url.full"] = string.format("%s://%s%s%s", wrk.scheme, wrk.host, portstr, wrk.path)
  if Rpc_system then
    Default_span["rpc.system"] = Rpc_system
    if Rpc_system == "jsonrpc" then
      Default_span["rpc.jsonrpc.request_id"] = n
      Default_span["rpc.jsonrpc.version"] = "1.0"
    end
    Default_span["rpc.service"] = "xapi"
  end
  -- preallocate response
  set_response(n, 0, nil, 0)

  Requests[n] = wrk.format(method, path, headers, body)
end

-- per thread callbacks

init = function(args)
  Request_counter = 1
  Response_counter = 1
  wrk.headers['User-Agent'] = User_agent
  wrk.headers['Host'] = wrk.thread.addr

  if #args >= 2 then
    Output = args[2]
  end
  if #args >= 3 then
    Trace_id = args[3]
    print("Got trace id", Trace_id)
    wrk.thread:set("Trace_id", Trace_id)
  end

  if #args >= 1 then
    local inputfile = args[1]
    local file = io.open(inputfile, "r")
    if not file then
      error(string.format("Cannot open file to read requests: %s", inputfile))
    else
      print(string.format("Reading requests from %s", inputfile))
      local request = file:read()
      local method, path = string.match(request, "(%g+) (%g+)")
      wrk.method = method
      wrk.path = path
      if path == "/jsonrpc" then
        Rpc_system = "jsonrpc"
      elseif path == "/RPC2" then
        Rpc_system = "xmlrpc"
      end
      local i = 1
      for body in file:lines() do
        build_request(i, method, path, body)
        i = i + 1
      end
    end
  else
    for i = 1, 99 do
      build_request(i, "GET", "/", nil)
    end
  end
  -- print(Requests[1])
  Time_start = seconds(clock_gettime())
end

-- per request callbacks
-- keep code here minimal, since this is on the fast-path
-- all requests should be pre-created in init as needed

request = function()
  if Request_counter > #Spans_start_time then
    wrk.thread:stop()
  end
  Spans_start_time[Request_counter] = seconds(clock_gettime())
  local packet = Requests[Request_counter]
  Request_counter = Request_counter + 1
  return packet
end

response = function(status, _, body)
  local timestamp = seconds(clock_gettime())
  Response_counter = Response_counter + 1
  set_response(Response_counter, status, body, timestamp)
end
