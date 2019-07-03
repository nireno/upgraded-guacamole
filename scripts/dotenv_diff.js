let {red, yellow} = require("ansi-colors");
let path = require('path');
let fs = require('fs');
let dotenv = require('dotenv');

let env = fs.readFileSync(path.resolve(process.cwd(), '.env'))
let envTemplate = fs.readFileSync(path.resolve(process.cwd(), '.env.template'))

let envConfig = dotenv.parse(env);
let envTemplateConfig = dotenv.parse(envTemplate);

let envKeys = Object.keys(envConfig);
let envTemplateKeys = Object.keys(envTemplateConfig);

let missingKeys = envTemplateKeys.filter(key => !envKeys.some(envKey => envKey == key));
// ensure all keys are defined
if(envKeys.join() !== envTemplateKeys.join()){
  console.error(red(".env and .env.template are out of sync "));
  console.error(yellow("Add the following variables to your .env file: "), missingKeys);
  process.exit(1);
}
