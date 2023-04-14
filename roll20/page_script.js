console.log('Greasy Wheel: page script');

const init = () => {
    console.log('Greasy Wheel: init');

    window.Campaign.on('change:turnorder', () => {
        const characters = [];
        const turnOrder = JSON.parse(window.Campaign.get('turnorder'));

        turnOrder.forEach((turnOrderItem) => {
            const { id, _pageid } = turnOrderItem;
            const page = window.Campaign.pages._byId[_pageid];

            if (!page) {
                console.error('page not found', _pageid, id);
                return;
            }

            const token = page.thegraphics._byId[id];

            if (!token || !token.character) {
                console.error('token not found', _pageid, id);
                return;
            }

            characters.push({
                tokenId: id,
                characterId: token.character.id,
                characterName: token.character.attributes.name
            });
        });

        console.log('roll20/page_script/change:turnorder', characters);

        window.postMessage({
            to: 'ext',
            type: 'ROLL20_TURN_ORDER_CHANGE',
            payload: characters
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
