---@meta

-- from SCRIPTING in wrk and wrk2,
-- adapted so that the documentation shows up in lua-language-server

---@class wrk
---@field thread userdata
local wrk = {
    scheme  = "http",
    host    = "localhost",
    port    = nil,
    method  = "GET",
    path    = "/",
    headers = {},
    body    = nil,
}

---@alias http_request userdata

---wrk.format returns a HTTP request string containing the passed parameters merged with values from the wrk table.
---@return http_request
function wrk.format(method, path, headers, body)
end

---wrk.lookup returns a table containing all known addresses for the host and service pair. This corresponds to the POSIX getaddrinfo() function.
---@return table
function wrk.lookup(host, service)
end

---wrk.connect returns true if the address can be connected to, otherwise it returns false. The address must be one returned from wrk.lookup().
---@return boolean
function wrk.connect(addr)
end

---@class thread : userdata
---@field addr string get or set the thread's server address
local thread

---get the value of a global in the thread's env
---@param name string
---@return (nil | boolean | number | string | table)
function thread:get(name)
end

---set the value of a global in the thread's env
---@param name string
---@param value (nil | boolean | number | string | table)
function thread:set(name, value)
end

---stop the thread
function thread:stop()
end

---called during thread setup
---The setup phase begins after the target IP address has been resolved and all threads have been initialized but not yet started.
---setup() is called once for each thread and receives a userdata object representing the thread.
---@param thr thread
function setup(thr)
end

---called when the thread is starting
---The running phase begins with a single call to init(), followed by a call to request() and response() for each request cycle.
---The init() function receives any extra command line arguments for the script which must be separated from wrk arguments with "--".
---@param args string[]
function init(args)
end

---called to generate the HTTP request
---request() returns a string containing the HTTP request. Building a new request each time is expensive, when testing a high performance server one solution is to pre-generate all requests in init() and do a quick lookup in request().
---@return http_request
function request()
end

---called with HTTP response data
---response() is called with the HTTP response status, headers, and body. Parsing the headers and body is expensive, so if the response global is nil after the call to init() wrk will ignore the headers and body.
function response(status, headers, body)
end

---@class errors
---@field connect number total socket connection errors
---@field read number total socket read errors
---@field write number total socket write errors
---@field status number total HTTP status codes > 399
---@field timeout number total request timeouts

---@class summary
---@field duration number run duration in microseconds
---@field requests number total completed requests
---@field bytes number total bytes received
---@field errors errors

---@class latency
---@field min number minimum value seen
---@field max number maximum value seen
---@field mean number average value seen
---@field stdev number standard deviation

---@param p number p-th percentile value
---@return number
function latency:percentile(p)
end

---@param i number
---@return table raw value and count
function latency(i)
end

---called with results of run
---The done() function receives a table containing result data, and two statistics objects representing the per-request latency and per-thread request rate. Duration and latency are microsecond values and rate is measured in requests per second.
---@param summary summary
---@param latency latency
---@param requests userdata TODO
function done(summary, latency, requests)
end

return wrk
