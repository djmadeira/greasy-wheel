console.log('Greasy Wheel: page script');

const init = () => {
    console.log('Greasy Wheel: init');

    window.Campaign.on('change:turnorder', (Campaign) => {
        window.postMessage({
            to: 'ext',
            type: 'ROLL20_TURN_ORDER_CHANGE',
            payload: Campaign.get('turnorder')
        }, '*');
    });
};

const checkInit = () => {
    if (window.Campaign) {
        return init()
    }

    setTimeout(checkInit, 10)
}

checkInit();
