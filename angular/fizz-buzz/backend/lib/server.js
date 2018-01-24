"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const express = require("express");
const fizz_buzz_control_1 = require("./src/fizz-buzz-control");
const app = express();
const router = express.Router();
const baseUrl = process.env.npm_package_config_baseUrl;
const port = process.env.npm_package_config_port;
const message = (message) => ({ message: message });
const control = new fizz_buzz_control_1.FizzBuzzControl();
router.get('/fizz-buzz/:limit', (request, response) => control
    .calculateUpTo(request.params.limit)
    .subscribe(value => response.json(value), (error) => response.status(400).json({ message: error.message, name: error.name })));
app.use(baseUrl, router);
app.use((request, response) => {
    response
        .status(404)
        .send(message('unknown command'));
});
console.log(`listening to port ${port}... (press Ctrl+C to cancel)`);
app.listen(port);
//# sourceMappingURL=server.js.map