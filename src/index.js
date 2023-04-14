'use strict';

import "./tailwind.css";
require("./styles.scss");
const config = require('./actions.yaml').default;

config.fight.actions = Object.fromEntries(config.fight.actions.entries());

const flags = JSON.stringify(config)

const {Elm} = require('./Main');
var app = Elm.Main.init({flags});

window.addEventListener('message', (message) => {
    console.log('bundlejs/onMessage', message.data);

    if (message.data.to === 'ext') {
        switch(message.data.type) {
            case 'ROLL20_TURN_ORDER_CHANGE':
                console.log(message.data.payload);
                break;
            default:
                console.warn('Unhandled message type', message.data.type);
        }
    }
});

app.ports.toJs.subscribe(data => {
    console.log(data);
})
// Use ES2015 syntax and let Babel compile it for you
var testFn = (inp) => {
    let a = inp + 1;
    return a;
}
