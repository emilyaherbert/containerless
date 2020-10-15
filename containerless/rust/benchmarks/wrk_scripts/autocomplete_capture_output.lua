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

request = function()
    return requests[math.random(0,4)]()
end