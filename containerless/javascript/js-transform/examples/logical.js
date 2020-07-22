let containerless = require("../../javascript/containerless");
let foo = 10;
containerless.get("https://emilyaherbert.github.io/authorize.txt", function(resp) {
    if(resp.username === "emily" && resp.password === "herbert") {
        console.log("Login successful!");
    }
});