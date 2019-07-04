let { red, yellow } = require("ansi-colors");
let path = require('path');
let fs = require('fs-extra');
let dotenv = require('dotenv');

// Ensure .env exists
if (!(fs.existsSync(path.resolve(process.cwd(), ".env")))) {
  console.error(red("`.env` file required. See `.env.template`."));
  process.exit(1);
};

let env = fs.readFileSync(path.resolve(process.cwd(), '.env'))
let envTemplate = fs.readFileSync(path.resolve(process.cwd(), '.env.template'))

let envConfig = dotenv.parse(env);
let envTemplateConfig = dotenv.parse(envTemplate);

let envKeys = Object.keys(envConfig);
let envTemplateKeys = Object.keys(envTemplateConfig);

// Ensure all environment variables are defined
let missingKeys = envTemplateKeys.filter(key => !envKeys.some(envKey => envKey == key));
if (envKeys.join() !== envTemplateKeys.join()) {
  console.error(red(".env and .env.template are out of sync "));
  console.error(yellow("Add the following variables to your .env file: "), missingKeys);
  process.exit(1);
}

// Ensure NODE_ENV=production
dotenv.config();
if (!(process.env.NODE_ENV == "production")) {
  console.error(red("Expected NODE_ENV=production but got NODE_ENV=" + process.env.NODE_ENV));
  process.exit(1);
};
