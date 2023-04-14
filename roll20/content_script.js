const script = document.createElement('script');
script.setAttribute('type', 'text/javascript');
script.src = chrome.runtime.getURL('roll20/page_script.js');
document.body.appendChild(script);

window.addEventListener('message', (message) => {
    console.log('roll20/content_script/onMessage', message.data);

    if (message.data.to = 'ext') {
        chrome.runtime.sendMessage({ message: message.data });
    }
});

chrome.runtime.onMessage.addListener((message) => {
    console.log('roll20/content_script/chromeOnMessage', message);

    if (message.to = 'roll20') {
        window.postMessage(message, '*');
    }
});
