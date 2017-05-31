import chai = require("chai");
import chai_string = require("chai-string");

// global hooks such as before and after
// HINT: we extend chai here
before(() => {
    chai.use(chai_string);
});
