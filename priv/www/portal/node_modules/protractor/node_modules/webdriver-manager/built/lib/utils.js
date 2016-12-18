"use strict";
var child_process = require("child_process");
var fs = require("fs");
var os = require("os");
function spawnFactory(sync) {
    return function (cmd, args, stdio, opts) {
        if ((os.type() === 'Windows_NT') && (cmd.slice(-4) !== '.exe')) {
            if (fs.existsSync(cmd + '.exe')) {
                cmd += '.exe';
            }
            else {
                args = ['/c'].concat([cmd], args);
                cmd = 'cmd';
            }
        }
        if (stdio) {
            opts = opts || {};
            opts.stdio = stdio;
        }
        if (sync) {
            return child_process.spawnSync(cmd, args, opts);
        }
        else {
            return child_process.spawn(cmd, args, opts);
        }
    };
}
exports.spawn = spawnFactory(false);
exports.spawnSync = spawnFactory(true);
//# sourceMappingURL=utils.js.map