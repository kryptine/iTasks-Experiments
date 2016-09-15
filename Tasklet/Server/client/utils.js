isArray = function(o){
	return (o instanceof Array);
}

isNumber = function(o){
	return (typeof o == "number");
}

isFloat = function(n){
  return n===+n && n!==(n|0);
}

isInteger = function(n){
  return n===+n && n===(n|0);
}

isString = function(o){
	return (typeof o == "string");
}

isBoolean = function(o){
	return (typeof o == "boolean");
}

isFunction = function(o){
	return (typeof o == "function");
}

isObject = function(o){
	return (typeof (o) == "object");
}

String.prototype.trim = function () {
    return this.replace(/^\s*/, "").replace(/\s*$/, "");
}

String.prototype.startsWith = function(str){
    return (this.indexOf(str) === 0);
}

String.prototype.endsWith = function(str){
    var lastIndex = this.lastIndexOf(str);
    return (lastIndex != -1) && (lastIndex + str.length == this.length);
}

String.prototype.capitalize = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
}

String.prototype.lpad = function(padString, length) {
	var str = this;
    while (str.length < length)
        str = padString + str;
    return str;
}
 
String.prototype.rpad = function(padString, length) {
	var str = this;
    while (str.length < length)
        str = str + padString;
    return str;
}

//This prototype is provided by the Mozilla foundation and
//is distributed under the MIT license.
//http://www.ibiblio.org/pub/Linux/LICENSES/mit.license
if (!Array.prototype.map) {
  Array.prototype.map = function(fun /*, thisp*/) {
    var len = this.length;
    if (typeof fun != "function")
      throw new TypeError();

    var res = new Array(len);
    var thisp = arguments[1];
    for (var i = 0; i < len; i++)
    {
      if (i in this)
        res[i] = fun.call(thisp, this[i], i, this);
    }

    return res;
  };
}

//This prototype is provided by the Mozilla foundation and
//is distributed under the MIT license.
//http://www.ibiblio.org/pub/Linux/LICENSES/mit.license

if (!Array.prototype.some) {
  Array.prototype.some = function(fun /*, thisp*/) {
    var len = this.length;
    if (typeof fun != "function")
      throw new TypeError();

    var thisp = arguments[1];
    for (var i = 0; i < len; i++)
    {
      if (i in this &&
          fun.call(thisp, this[i], i, this))
        return true;
    }

    return false;
  };
}

if (!Array.prototype.any){
  Array.prototype.any = function(element /*, thisp*/){
    return this.some(function(a){return a==element;})
  };
}

(evalScript = function(e){
	var	h = evalScript.node,
		s = document.createElement("script");
	s.type = "text/javascript";
	/*@cc_on if(!(s.text=e))@*/
	s.appendChild(document.createTextNode(e));
	h.appendChild(s);
	h.removeChild(s);
}).node = document.getElementsByTagName("head")[0] || document.getElementsByTagName("*")[0];

// Generic equality from: http://documentcloud.github.com/underscore/underscore.js
function geq(a, b) {
	return _geq(a,b,[],[]);
}

function _has(obj, key) {
    return Object.prototype.hasOwnProperty.call(obj, key);
}

// Internal recursive comparison function.
function _geq(a, b, aStack, bStack) {
    // Identical objects are equal. `0 === -0`, but they aren't identical.
    // See the Harmony `egal` proposal: http://wiki.ecmascript.org/doku.php?id=harmony:egal.
    if (a === b) return a !== 0 || 1 / a == 1 / b;
    // A strict comparison is necessary because `null == undefined`.
    if (a == null || b == null) return a === b;
    // Compare `[[Class]]` names.
    var className = toString.call(a);
    if (className != toString.call(b)) return false;
    switch (className) {
      // Strings, numbers, dates, and booleans are compared by value.
      case '[object String]':
        // Primitives and their corresponding object wrappers are equivalent; thus, `"5"` is
        // equivalent to `new String("5")`.
        return a == String(b);
      case '[object Number]':
        // `NaN`s are equivalent, but non-reflexive. An `egal` comparison is performed for
        // other numeric values.
        return a != +a ? b != +b : (a == 0 ? 1 / a == 1 / b : a == +b);
      case '[object Date]':
      case '[object Boolean]':
        // Coerce dates and booleans to numeric primitive values. Dates are compared by their
        // millisecond representations. Note that invalid dates with millisecond representations
        // of `NaN` are not equivalent.
        return +a == +b;
      // RegExps are compared by their source patterns and flags.
      case '[object RegExp]':
        return a.source == b.source &&
               a.global == b.global &&
               a.multiline == b.multiline &&
               a.ignoreCase == b.ignoreCase;
    }
    if (typeof a != 'object' || typeof b != 'object') return false;
    // Assume equality for cyclic structures. The algorithm for detecting cyclic
    // structures is adapted from ES 5.1 section 15.12.3, abstract operation `JO`.
    var length = aStack.length;
    while (length--) {
      // Linear search. Performance is inversely proportional to the number of
      // unique nested structures.
      if (aStack[length] == a) return bStack[length] == b;
    }
    // Add the first object to the stack of traversed objects.
    aStack.push(a);
    bStack.push(b);
    var size = 0, result = true;
    // Recursively compare objects and arrays.
    if (className == '[object Array]') {
      // Compare array lengths to determine if a deep comparison is necessary.
      size = a.length;
      result = size == b.length;
      if (result) {
        // Deep compare the contents, ignoring non-numeric properties.
        while (size--) {
          if (!(result = _geq(a[size], b[size], aStack, bStack))) break;
        }
      }
    } else {
      // Objects with different constructors are not equivalent, but `Object`s
      // from different frames are.
      var aCtor = a.constructor, bCtor = b.constructor;
      if (aCtor !== bCtor && !(isFunction(aCtor) && (aCtor instanceof aCtor) &&
                               isFunction(bCtor) && (bCtor instanceof bCtor))) {
        return false;
      }
      // Deep compare objects.
      for (var key in a) {
        if (_has(a, key)) {
          // Count the expected number of properties.
          size++;
          // Deep compare each member.
          if (!(result = _has(b, key) && _geq(a[key], b[key], aStack, bStack))) break;
        }
      }
      // Ensure that both objects contain the same number of properties.
      if (result) {
        for (key in b) {
          if (_has(b, key) && !(size--)) break;
        }
        result = !size;
      }
    }
    // Remove the first object from the stack of traversed objects.
    aStack.pop();
    bStack.pop();
    return result;
}

function isIE(){
    return /msie/i.test(navigator.userAgent) && !/opera/i.test(navigator.userAgent);
}
    
 /* Only this way works for built in javascript objects */
function streval(obj, method){
	if(method!=null){
		var str = "obj."+method+"(";
	}else{
		var str = "new "+obj+"(";
	}
		
    // Creating a closure
    var eventHandler = function(expr){
		
		var h = function(param){
			expr[1].push(param);
			Sapl.feval(expr);
		};
		
		return h;
    }
	    
    for(var i=2; i<arguments.length; i++){
	// if an argument is a function, we handle it as an event handler
        if(isArray(arguments[i]) && isFunction(arguments[i][0])){
	      arguments[i] = eventHandler(arguments[i]);
	}
	    
        if(i>2) str += ",";
        str += "arguments["+i+"]";
    }
    
    str = str + ")";
    var value = eval(str);
    
    return value;
}

