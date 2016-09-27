var _user$project$Native_DomUtils = function() {
  var rAF = typeof requestAnimationFrame !== 'undefined'
  	? requestAnimationFrame
  	: function(callback) { callback(); };

  var Scheduler = _elm_lang$core$Native_Scheduler;
  var EmptyTuple = _elm_lang$core$Native_Utils.Tuple0;

  function withNodeBySelector(selector, doStuff) {
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

  function focusSelector(selector) {
  	return withNodeBySelector(selector, function(node) {
  		node.focus();
  		return EmptyTuple;
  	});
  }

  function blurSelector(selector) {
  	return withNodeBySelector(selector, function(node) {
  		node.blurSelector();
  		return EmptyTuple;
  	});
  }

  return {
  	blurSelector: blurSelector,
    focusSelector: focusSelector
  };
}();
