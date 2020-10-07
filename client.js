'use strict';

require('./src/Client/Main.purs').main();

if (module.hot) {
  module.hot.accept();
}

console.log('app starting');