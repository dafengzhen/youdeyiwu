const childProcess = require("child_process");

childProcess.exec("git diff HEAD^ HEAD --quiet -- ./web/", (error, stdout, stderr) => {
    if (error) {
        process.exit(1);
    } else {
        process.exit(0);
    }
});
