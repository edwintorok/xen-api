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

-- IP addresses have been resolved, all threads initialized but not yet started
-- called once for each thread

setup = function(thread)
  -- save the thread in the global threads table, so we can read back thread specific value in 'done'
  -- note that global variables accessed during 'init'/'request'/'response' will actually be variable on 'thread' here
  thread:set("ThreadId", #threads)
  thread:set("Addr", tostring(thread.addr))
  table.insert(threads, thread)
end

local format_traceparent = function(trace_id, thread_id, request_id)
  return string.format("00-%s-%08x%08x-00", trace_id, thread_id, request_id)
end

done = function(_, _, _)
  local time_stop = seconds(clock_gettime())
  local file
  local test_start = nil

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

    Trace_id = thread:get("Trace_id")
  end

  io.write("thread_id,request_id,traceparent,begin,end\n")
  local traceparent = format_traceparent(Trace_id, 0x7fffffff, 0)
  io.write(string.format("-1,PROCESS,%s,%.9f,%.9f\n", traceparent, test_start, time_stop))

  for index, thread in ipairs(threads) do
    local time_start = thread:get("Time_start")
    local thread_id = thread:get("ThreadId")
    io.write(string.format("%d,THREAD,,%.9f,%.9f\n", index, time_start, time_stop))

    local request_begin = thread:get("Request_begin")
    local request_end = thread:get("Request_end")

    if request_end then
      ---@cast request_begin table<number, number>
      for idx, start in ipairs(request_begin) do
        local traceparent = format_traceparent(Trace_id, thread_id, idx)
        local stop = request_end[idx]
        if stop then
          io.write(string.format("%d,%d,%s,%.9f,%.9f\n", index, idx, traceparent, start, stop))
        else
          io.write(string.format("%d,%d,%s,%.9f,DNF\n", index, idx, traceparent, start))
        end
      end
    end

  end

  if file then
    io.close(file)
  end
end

Request_begin = {}
-- per thread callbacks
Requests = {}

init = function(args)
  Time_start = seconds(clock_gettime())
  Request_end = {}
  Request_counter = 1
  Response_counter = 1
  wrk.headers['User-Agent'] = 'wrk'
  wrk.headers['Host'] = Addr

  if #args >= 2 then
    Output = args[2]
  end
  if #args >= 3 then
    Trace_id = args[3]
    print("Got trace id", Trace_id)
  end

  if #args >= 1 then
      local inputfile = args[1]
      local file = io.open(inputfile, "r")
      if not file then
        error(string.format("Cannot open file to read requests: %s", inputfile))
      else
        print(string.format("Reading requests from %s", inputfile))
        local request = file:read()
        local method, path = string.match(request,"(%g+) (%g+)")
        wrk.method = method
        wrk.path = path
        local i = 1
        for body in file:lines() do
          local h = {}
          h['Content-Length'] = tostring(#body)
          h['traceparent'] = format_traceparent(Trace_id, ThreadId, i)
          Requests[i] = wrk.format(method, path, h, body)
          i = i + 1
        end
      end
  else
    for i in 1,99 do
      local h = {}
      h['traceparent'] = format_traceparent(Trace_id, ThreadId, i)
      Requests[i] = wrk.format(nil, "/", h, nil)
    end
  end
  -- print(Requests[1])
end

-- per request callbacks
-- keep code here minimal, since this is on the fast-path
-- all requests should be pre-created in init as needed


request = function()
  Request_begin[Request_counter] = tostring(seconds(clock_gettime()))
  local req = Requests[Request_counter]
  Request_counter = Request_counter + 1
  return req
end

response = function(status, headers, body)
  Request_end[Response_counter] = seconds(clock_gettime())
  Response_counter = Response_counter + 1
--  print(status)
--  print(headers)
--  print(body)
end
