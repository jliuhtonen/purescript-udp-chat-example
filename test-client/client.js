var dgram = require('dgram')
var readline = require('readline')
var rl = readline.createInterface({
    input: process.stdin,
      output: process.stdout
})

var socket = dgram.createSocket('udp4')
var args = process.argv.slice(2);

var server = '127.0.0.1'
var serverPort = 62111

function sendToServer(obj) {
  var str = JSON.stringify(obj)
  var buf = new Buffer(str)
  socket.send(buf, 0, buf.length, serverPort, server)
}

function sendJoinMsg(nick) {
  sendToServer({
    nick: nick
  })
}

function sendChatMsg(msg) {
  if(!!msg) {
    sendToServer({
      chatMsg: msg
    })
  }
}

function startChat(nick) {
  socket.bind(function() {
    socket.on('message', function(msg) {
      var obj = JSON.parse(msg)
      if(!!obj.msg) {
        console.log("<" + obj.nick + "> " + obj.msg)
      } else {
        console.log(obj.nick + " joined the chat!") 
      }
    })
    sendJoinMsg(nick)
    
    rl.on('line', function(line) {
      sendChatMsg(line)
    })
  })

} 

if(args.length === 1) {
  startChat(args[0])
} else {
  console.log("Usage: chat <nick>")
  process.exit(1)
}
