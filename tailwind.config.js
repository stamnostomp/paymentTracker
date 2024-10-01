// tailwind.config.js
module.exports = {
  content: [
    './src/**/*.{html,js,elm}',
  ],
  theme: {
    extend: {
      colors: {
        everblush: {
          bg: '#141B1E',
          text: '#D9E0EE',
          primary: '#EE64AC',
          secondary: '#8bd5ca',
          accent: '#e5c890',
          red: '#f0c6c6',
          white: '#F5E0DC',
        },
      },
    },
  },
};
