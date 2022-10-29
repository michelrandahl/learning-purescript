"use strict";

//var crypto = require('crypto');
import crypto from 'crypto';

//exports._createHash = function(algorithm) {
export function _createHash(algorithm) {
  return function() {
    return crypto.createHash(algorithm);
  }
}

//exports.update = function(hash) {
export function update(hash){
  return function(buffer) {
    return function() {
      return hash.update(buffer);
    }
  }
}

//exports.digest = function(hash) {
export function digest(hash){
  return function() {
    return hash.digest();
  }
}
