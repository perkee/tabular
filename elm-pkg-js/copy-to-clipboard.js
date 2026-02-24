/* elm-pkg-js */
exports.init = function (app) {
  customElements.define(
    "copy-button",
    class extends HTMLElement {
      connectedCallback() {
        if (this.querySelector("button")) return;
        var b = document.createElement("button");
        b.className = "copy-btn";
        b.textContent = "Copy";
        var self = this;
        b.onclick = function () {
          var el = document.getElementById(self.getAttribute("target"));
          if (el) {
            navigator.clipboard.writeText(el.value).then(function () {
              b.textContent = "Copied!";
              setTimeout(function () {
                b.textContent = "Copy";
              }, 1500);
            });
          }
        };
        this.appendChild(b);
      }
    }
  );
};
