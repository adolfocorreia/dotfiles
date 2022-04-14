// ==UserScript==
// @name Dark Mode Whitelist
// @match http://www.cachedpages.com/
// ==/UserScript==

// Reference: https://github.com/qutebrowser/qutebrowser/issues/5542#issuecomment-782040210
const meta = document.createElement('meta');
meta.name = "color-scheme";
meta.content = "dark light";
document.head.appendChild(meta);
