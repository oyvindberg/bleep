const { execFileSync } = require("child_process");
const path = require("path");

const extractScript = path.join(__dirname, "extract-snippets.js");

module.exports = function snippetExtractorPlugin() {
  return {
    name: "bleep-snippet-extractor",
    async loadContent() {
      execFileSync(process.execPath, [extractScript], { stdio: "inherit" });
    },
  };
};
