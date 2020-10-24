'use strict';

require('./src/Client/Main.purs').main();
require('./index.css');

if (module.hot) {
  module.hot.accept();
}

console.log('app starting');