const {green, yellow} = require('ansi-colors');
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
    new HtmlWebpackPlugin({
      template: 'src/index.html',
      inject: false
    }),
    new webpack.EnvironmentPlugin([
      'NODE_ENV', 
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
  optimization: {
    minimizer: [
      new TerserPlugin({
        sourceMap: false, // Must be set to true if using source-maps in production
        terserOptions: {
          compress: {
            drop_console: true,
          },
        },
      }),
    ],
  },
};
