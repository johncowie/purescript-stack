"use strict";

var twilio = require('twilio');

exports._messagingResponse = function(to) {
  return function(from) {
    return function(message) {
      var twiml = new twilio.twiml.MessagingResponse();
      twiml.message({to: to, from: from}, message);
      return twiml.toString();
    }
  }
}

exports._emptyMessagingResponse = (new twilio.twiml.MessagingResponse()).toString();
