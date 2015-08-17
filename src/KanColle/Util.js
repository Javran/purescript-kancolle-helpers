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
