(function() {
/* START: test___fixtures___concat___modules___Page___Foo_js_js */
function test___fixtures___concat___modules___Page___Foo_js_js(require, module, exports) {
var moo = require('./moo');
moo(4, 2);
} /* END: test___fixtures___concat___modules___Page___Foo_js_js */
/* START: test___fixtures___concat___sources___Page___Moo_js_js */
function test___fixtures___concat___sources___Page___Moo_js_js(require, module, exports) {
module.exports = function(a, b) {
  console.log(a + b + "");
};
} /* END: test___fixtures___concat___sources___Page___Moo_js_js */

test___fixtures___concat___modules___Page___Foo_js_js();
})();
