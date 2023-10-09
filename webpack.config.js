const { green, yellow } = require('ansi-colors');
const packageJson = require('./package.json');
require('dotenv').config();

if (process.env.NODE_ENV === "production") {
  console.log(green("Webpack is optimizing for production"));
}
else {
  console.log(yellow("Webpack is building in development mode."));
  console.log(yellow("Set NODE_ENV=production in .env to optimize the build."));
};

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const TerserPlugin = require('terser-webpack-plugin');
const webpack = require('webpack');
const outputDir = path.join(__dirname, 'build/');

const isProd = process.env.NODE_ENV === 'production';


module.exports = {
  entry: {
    "Index.js": './src/Index.bs.js',
  },
  mode: isProd ? 'production' : 'development',
  output: {
    path: outputDir,
    publicPath: '/',
    filename: '[name]'
  },
  plugins: [
    new webpack.ProvidePlugin({
      process: "process",
    }),
    new HtmlWebpackPlugin({
      template: 'src/index.html',
      inject: false,
      allfoursVersion: packageJson.version,
    }),
    new webpack.EnvironmentPlugin([
      'NODE_ENV',
      'allfours_base_url',
      'allfours_feedback_url',
      'allfours_help_url',
      'allfours_rules_url']),
  ],
  devServer: {
    compress: true,
    contentBase: outputDir,
    port: process.env.PORT || 8000,
    historyApiFallback: true
  },
  watchOptions: {
    ignored: /node_modules/,
  },
  optimization: {
    minimizer: [
      new TerserPlugin({
        terserOptions: {
          compress: {
            drop_console: true,
          },
        },
      }),
    ],
  },
};
