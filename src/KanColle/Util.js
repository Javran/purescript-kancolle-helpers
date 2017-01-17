"use strict";

// module KanColle.Util

exports.peekSTArrayUnsafe = function (xs) {
      return function (i) {
        return function () {
          return xs[i];
        };
      };
};

exports.pokeSTArrayUnsafe = function (xs) {
  return function (i) {
    return function (a) {
      return function () {
          xs [i]=a;
          return {};
      };
    };
  };
};

exports.jsonStringify = function (x) {
    return JSON.stringify(x);
};

// level = 0 or 1
exports.consoleMessage = function(level) {
    return function (x) {
        return function (k) {
            if (level === 0) {
                console.log(x);
            } else if (level === 1) {
                console.warn(x);
            } else {
                console.warn("Unknown level: " +level);
                console.warn(x);
            }
            return k({});
        };
    };
};

exports.throwWith = function(o) {
    throw o;
};

exports.unsafeToFixed = function (scale) {
    return function (number) {
        return function () {
            return number.toFixed(scale);
        }
    }
};
