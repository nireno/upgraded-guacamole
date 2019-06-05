const purgecss = require('@fullhuman/postcss-purgecss')({
  // Specify the paths to all of the template files in your project purgecss
  // will scan the following "content" files to determine which styles need to
  // be preserved. See
  // https://tailwindcss.com/docs/controlling-file-size/#writing-purgeable-html
  // for recommendations on writing purgeable tailwind classes.
  content: [
    './src/**/*.re',
    './src/**/*.html',
    // './src/**/*.jsx',
    // etc.
  ],

  // Include any special characters you're using in this regular expression
  defaultExtractor: content => content.match(/[A-Za-z0-9-_:/]+/g) || []
})

module.exports = {
  plugins: [
    require('precss'),
    require('tailwindcss')('./tailwind.js'),
    require('autoprefixer'),
    ...process.env.NODE_ENV === 'production'
    ? [purgecss, require('cssnano')]
    : []
  ],
}
