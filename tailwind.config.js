const colors = require('tailwindcss/colors')

module.exports = {
  mode: 'jit',
  purge: [
    'src/**/*.purs',
    'src/**/*.js',
    '**/*.html'
  ],
  darkMode: false, // or 'media' or 'class'
  theme: {
    colors,
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
