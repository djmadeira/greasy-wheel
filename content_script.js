document.write(`<!doctype html><html><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width,initial-scale=1"><meta name="author" content="simon"><meta http-equiv="expires" content="0"><link rel="preconnect" href="https://fonts.googleapis.com"><link rel="preconnect" href="https://fonts.gstatic.com" crossorigin><link href="https://fonts.googleapis.com/css2?family=PT+Serif:ital,wght@0,400;0,700;1,400;1,700&display=swap" rel="stylesheet"><title>Elm hotloading dev environment (replaced by Elm)</title></head><body></body></html>`);
document.close();

const link = document.createElement('link');
link.setAttribute('rel', 'stylesheet');
link.setAttribute('href',  chrome.runtime.getURL('dist/main.css'));
document.head.appendChild(link);

const script = document.createElement('script');
script.setAttribute('type', 'text/javascript');
script.src = chrome.runtime.getURL('dist/bundle.js');
document.body.appendChild(script);

window.addEventListener('message', ({ data }) => {
    console.log('content_script/onMessage', data);

    if (data.to === 'roll20') {
        chrome.runtime.sendMessage({ message: data });
    }
});

chrome.runtime.onMessage.addListener((message) => {
    console.log('content_script/chromeMessage', message);

    if (message.to === 'ext') {
        window.postMessage(message, '*');
    }
});
