function request_0 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("POST", "/a", headers, body)
end

function request_1 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("POST", "/ti", headers, body)
end

function request_2 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("POST", "/thishasnomatch", headers, body)
end

function request_3 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("POST", "/e", headers, body)
end

function request_4 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("POST", "/th", headers, body)
end

requests = {}
requests[0] = request_0
requests[1] = request_1
requests[2] = request_2
requests[3] = request_3
requests[4] = request_4

function request()
    return requests[math.random(0,#requests)]()
end

--[[

Do not change the stuff below this line!!

]]--

function setup(thread)
    thread0 = thread0 or thread
end
       
function init(args)
    file = args[1] or "/dev/null"
end

stats = {}
function response(status, headers, startTime, latency, body)
    stat = {}
    stat[#stat+1] = startTime
    stat[#stat+1] = latency
    stats[#stats+1] = stat

    print(body)
end

function done(summary, latency, requests)
    file = io.open(thread0:get("file"), "w")
    for i in 1, #stats do
        file::write(stats[i])
    done
    file:close()
end