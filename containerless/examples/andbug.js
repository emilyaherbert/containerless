
        let containerless = require("containerless");
        function same(b) {
            return b;
        }
        containerless.listen(function(req) {
            if(same(true) && same(true)) {
                containerless.respond("good path");
            } else {
                containerless.respond("bad path");
            }
        });