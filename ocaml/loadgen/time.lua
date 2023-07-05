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
  return '"' .. string.gsub(str, '"', '""') .. '""'
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
  io.write(table.concat(line), '\n')
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

    io.flush()
    print("getting spans")
    io.flush()
    local spans = thread:get("Spans")
    print("got spans")
    io.flush()
    if spans then
      ---@cast spans table
      first_span = spans
    end
  end
  if not trace_id then
    error("No Trace_id found in threads")
  end
  io.flush()
  print("Here")
  io.flush()
  print(first_span)
  io.flush()

  -- pairs iteration order is undefined, store headers in a table with numeric index
  local csv_header = {}
  local hi = 0
  for k, _ in pairs(first_span) do
    print("Here2")
    io.flush()
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

  for thread in threads do
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

    local spans = thread:get("Spans")

    for span in spans do
      print_csv_line(csv_header, span)
    end
  end

  if file then
    io.close(file)
  end
end

---@type table<number, table>
Spans = {}

---@type table<number, string>
local Requests = {}

---@type string
local Trace_id = nil

-- Set span attributes following semantic conventions
-- See https://github.com/open-telemetry/semantic-conventions/blob/main/specification/http/http-spans.md

local User_agent = 'wrk'

---@type nil|string
local Rpc_system

---@param n number
---@param status_code number
---@param response_body nil|string
---@param timestamp number
local set_response = function(n, status_code, response_body, timestamp)
  local span = Spans[n]
  if response_body then
    span["http.response.body.size"] = #response_body
    if status_code >= 400 then
      -- will decode in python/ocaml post-processing code
      span["_body"] = response_body
    end
  end
  span.finish_time = timestamp
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
  local span = {}
  if body then
    len = #body
    headers['Content-Length'] = tostring(len)
    span["http.request.body.size"] = len
  end
  span.traceparent = traceparent

  span["http.request.method"] = method
  span["user_agent.original"] = User_agent
  span["server.address"] = tostring(wrk.thread.addr)
  local portstr = ""
  if wrk.port then
    span["server.port"] = wrk.port
    portstr = string.format(":%d", wrk.port)
  end
  span["url.full"] = string.format("%s://%s%s%s", wrk.scheme, wrk.host, portstr, wrk.path)
  if Rpc_system then
    span["rpc.system"] = Rpc_system
    if Rpc_system == "jsonrpc" then
      span["rpc.jsonrpc.request_id"] = n
      span["rpc.jsonrpc.version"] = "1.0"
    end
    span["rpc.service"] = "xapi"
  end
  span.start_time = 0
  Spans[n] = span
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
    for i in 1, 99 do
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
  if Request_counter > #Spans then
    wrk.thread:stop()
  end
  Spans[Request_counter].start_time = seconds(clock_gettime())
  local packet = Requests[Request_counter]
  Request_counter = Request_counter + 1
  return packet
end

response = function(status, _, body)
  local timestamp = seconds(clock_gettime())
  Response_counter = Response_counter + 1
  set_response(Response_counter, status, body, timestamp)
end
