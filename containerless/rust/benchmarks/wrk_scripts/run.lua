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