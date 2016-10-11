var Greeter = (function () {
    function Greeter() {
    }

    Greeter.greet = function (name) {
        name = name || "World";
        return "Hello, " + name + "!";
    };
    return Greeter;
}());
//# sourceMappingURL=greeter.js.map