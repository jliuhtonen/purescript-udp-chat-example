var dgram = require('dgram')

var socket = dgram.createSocket('udp4')

var joinMsg = JSON.stringify({
    nick: "Archie"
})

socket.send(joinMsg, 0, joinMsg.length, 62111, '127.0.0.1', function() {
    var msg = JSON.stringify({
        chatMsg: "Hello, is there anybody in there?"
    })
    setInterval(function() {
        socket.send(msg, 0, msg.length, 62111, '127.0.0.1')
    }, 1000)
})

