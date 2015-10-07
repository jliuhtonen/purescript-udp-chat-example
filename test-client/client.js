var dgram = require('dgram')

var socket = dgram.createSocket('udp4')

setInterval(function() {
    var msg = JSON.stringify({
        nick: "Archie"
    })
    socket.send(msg, 0, msg.length, 62111, '127.0.0.1')
}, 1000)
