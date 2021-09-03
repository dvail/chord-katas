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
    fontFamily: {
      sans: ['Nunito', 'sans-serif'],
      serif: ['serif'],
      display: ['"Ceviche One"', 'serif'],
    },
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
}
