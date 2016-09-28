var _user$project$Native_DomUtils = function() {
  var rAF = typeof requestAnimationFrame !== 'undefined'
  	? requestAnimationFrame
  	: function(callback) { callback(); };

  var Scheduler = _elm_lang$core$Native_Scheduler;
  var Utils = _elm_lang$core$Native_Utils.Tuple0;

  function _withNodeBySelector(selector, doStuff) {
  	return Scheduler.nativeBinding(function(callback) {
  		rAF(function() {
  			var node = document.querySelector(selector);

        if (node === null) {
  				callback(Scheduler.fail({ ctor: 'NotFound', _0: selector }));
  				return;
  			}

        callback(Scheduler.succeed(doStuff(node)));
  		});
  	});
  }

  function _createFakeElem(value) {
    var isRightToLeft = document.documentElement.getAttribute('dir') == 'rtl';
    var fakeElem = document.createElement('textarea');

    fakeElem.setAttribute('readonly', '');
    fakeElem.value = value;

    // Prevent zooming on iOS
    fakeElem.style.fontSize = '12pt';

    // Reset box model
    fakeElem.style.border = '0';
    fakeElem.style.padding = '0';
    fakeElem.style.margin = '0';

    // Move element out of screen horizontally and vertically
    fakeElem.style.position = 'absolute';
    fakeElem.style[ isRightToLeft ? 'right' : 'left' ] = '-9999px';
    fakeElem.style.top =
      (window.pageYOffset || document.documentElement.scrollTop) + 'px';

    return fakeElem;
  }

  function _selectElem(elem) {
    elem.focus();
    elem.setSelectionRange(0, elem.value.length);
  }

  function _copySelectedText() {
    try {
      return document.execCommand('copy');
    } catch (err) {
      return false;
    }
  }

  function _copyToClipboard(textToCopy) {
    var isSucceeded;
    var fakeElem = _createFakeElem(textToCopy);
    document.body.appendChild(fakeElem);
    _selectElem(fakeElem);
    isSucceeded = _copySelectedText();
    document.body.removeChild(fakeElem);
    return isSucceeded;
  }

  function focusSelector(selector) {
  	return _withNodeBySelector(selector, function(node) {
  		node.focus();
  		return Utils.Tuple0;
  	});
  }

  function blurSelector(selector) {
  	return _withNodeBySelector(selector, function(node) {
  		node.blurSelector();
  		return Utils.Tuple0;
  	});
  }

  function copyToClipboard(textToCopy) {
  	return Scheduler.nativeBinding(function(callback) {
			var isSucceeded = _copyToClipboard(textToCopy);

      if (isSucceeded) {
        callback(Scheduler.succeed(Utils.Tuple0));
      } else {
				callback(Scheduler.fail({ ctor: 'CopyFailed' }));
				return;
			}
  	});
  }

  return {
  	blurSelector: blurSelector,
    focusSelector: focusSelector,
    copyToClipboard: copyToClipboard
  };
}();
