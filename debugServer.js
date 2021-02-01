/* eslint-disable @typescript-eslint/no-var-requires */
/* eslint-disable @typescript-eslint/no-require-imports */
const esbuild = require("esbuild");

esbuild.build({
  entryPoints: ["source/main.ts"],
  bundle: true,
  outfile: "out.js",
});
