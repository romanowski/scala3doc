const findRef = (searchBy, element = document) =>
  element.querySelector(searchBy);

const findRefs = (searchBy, element = document) =>
  element.querySelectorAll(searchBy);

const withEvent = (element, listener, callback) => {
  element && element.addEventListener(listener, callback);
  return () => element && element.removeEventListener(listener, callback);
};

const init = (cb) => window.addEventListener("DOMContentLoaded", cb);

const attachDOM = (element, html) => {
  if (element) {
    element.innerHTML = htmlToString(html);
  }
};

const getText = (element) => (element ? element.textContent : "");

const ifVisible = (condition, element, displayStyles) =>
  (element.style.display = condition ? displayStyles : "none");

const toggleVisibility = (condition, element) =>
  (element.dataset.visibility = condition ? "true" : "false");

const startsWith = (str, character) => str.charAt(0) === character;

const htmlToString = (html) => {
  if (Array.isArray(html)) {
    return html.join(" ");
  }
  return String(html);
};
