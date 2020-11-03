function request_0 ()
    --headers = {}
    --headers["Content-Type"] = "application/json"
    --body  = '{}'
    return wrk.format("GET", "/a", headers)
end

function request_1 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("GET", "/ti", headers)
end

function request_2 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("GET", "/thishasnomatch", headers)
end

function request_3 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("GET", "/e", headers)
end

function request_4 ()
    headers = {}
    headers["Content-Type"] = "application/json"
    body  = '{}'
    return wrk.format("GET", "/th", headers)
end

requests = {}
requests[0] = request_0
requests[1] = request_1
requests[2] = request_2
requests[3] = request_3
requests[4] = request_4

function request()
    req = requests[math.random(0,#requests)]()
    print(req)
    return req
end

function response(status, headers, startTime, latency)
    print(body)
end