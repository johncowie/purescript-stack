"use strict";

var twilio = require('twilio')

exports._validateRequest = function(authToken) {
  return function(signature) {
    return function(url) {
      return function(body) {
        return twilio.validateRequest(authToken, signature, url, body)
      }
    }
  }
}
