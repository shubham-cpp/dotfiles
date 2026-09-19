const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const test = require("node:test");
const list = vm.createContext({});
vm.runInContext(fs.readFileSync("Common/PopupGeometry.js", "utf8").replace(/^\.pragma library\s*/, ""), list);

test("popup is centered under the icon and clamped at either output edge", () => {
    assert.equal(list.popupX(1000, 380, 1920), 810);
    assert.equal(list.popupX(10, 380, 1920), 8);
    assert.equal(list.popupX(1900, 380, 1920), 1532);
    assert.equal(list.popupX(80, 144, 160), 8);
});
