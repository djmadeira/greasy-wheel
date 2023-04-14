const ROLL20_URL = "*://app.roll20.net/editor/";

const openTabIfNotExist = async () => {
    const url = 'https://djmadeira.github.io/';

    const getTab = async () => {
        const [tab] = await chrome.tabs.query({ url });

        if (tab) {
            return tab;
        }

        return chrome.tabs.create({ url });
    };

    return getTab();
};

const handleTurnOrderChange = async (payload) => {
    const tab = await openTabIfNotExist();
    await chrome.tabs.sendMessage(tab.id, { to: 'ext', type: 'ROLL20_TURN_ORDER_CHANGE', payload });
};

chrome.action.onClicked.addListener(async () => {
    openTabIfNotExist();
});

chrome.runtime.onMessage.addListener(({ message }) => {
    console.log('background/chromeMessage', message);

    const { type, payload } = message;
    switch(type) {
        case 'ROLL20_TURN_ORDER_CHANGE':
            handleTurnOrderChange(payload);
            break;
        default:
            console.error('Unhandled event', type, payload);
    }
});
