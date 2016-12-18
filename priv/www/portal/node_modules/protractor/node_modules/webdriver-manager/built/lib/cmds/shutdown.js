"use strict";
var http = require("http");
var minimist = require("minimist");
var cli_1 = require("../cli");
var Opt = require("./");
var opts_1 = require("./opts");
var logger = new cli_1.Logger('shutdown');
var prog = new cli_1.Program()
    .command('shutdown', 'shut down the selenium server')
    .action(shutdown)
    .addOption(opts_1.Opts[Opt.SELENIUM_PORT]);
exports.program = prog;
// stand alone runner
var argv = minimist(process.argv.slice(2), prog.getMinimistOptions());
if (argv._[0] === 'shutdown-run') {
    prog.run(JSON.parse(JSON.stringify(argv)));
}
else if (argv._[0] === 'shutdown-help') {
    prog.printHelp();
}
/**
 * Parses the options and starts the selenium standalone server.
 * @param options
 */
function shutdown(options) {
    logger.info('Attempting to shut down selenium nicely');
    http.get('http://localhost:' + options[Opt.SELENIUM_PORT].getString() +
        '/selenium-server/driver/?cmd=shutDownSeleniumServer');
}
//# sourceMappingURL=shutdown.js.map