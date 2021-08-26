'use strict';

require('./src/Client/Main.purs').main();
require('./src/index.css');

if (module.hot) {
  module.hot.accept();
}

console.log('app starting');