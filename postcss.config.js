let productionPlugins =
  process.env.NODE_ENV === "production"
    ? {
        cssnano: {},
      }
    : {};

module.exports = {
  plugins: {
    tailwindcss: {},
    autoprefixer: {},
    ...productionPlugins,
  },
};
