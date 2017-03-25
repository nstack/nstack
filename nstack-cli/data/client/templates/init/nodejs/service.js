"use strict";
// {{ name }} service
let nstack = require('./nstack');

// create each service as either an ES6 class or an object of functions
class Default extends nstack.Service {
    constructor() {
        super();
        // empty
    }

    add(x, y){
        let res = x + y;
        return Promise.resolve(res);
    }
}

// export the services here
module.exports = {
    Default : new Default()
};
