function srvConnect(url) {
  let ws = new WebSocket(url)

  ws.onopen = function(e) {
    console.log("Connected.")
  }

  ws.onmessage = handleMessage

  ws.onclose = function(e) {
    console.log("Disconnected.")
  }

  ws.onerror = function(e) {
    console.log("error")
    console.log(e)
  }
}

var api = { newObject:          [ newObject, 1 ]
          , destroyObject:      [ destroyObject, 1 ]
          , setPosition:        [ setPosition, 2 ]
          , setRotation:        [ setRotation, 2 ]
          , setSize:            [ setSize, 2 ]
          , setBackgroundColor: [ setBackgroundColor, 2 ]
          , setBackground:      [ setBackground, 2 ]
          , setVisible:         [ setVisible, 2 ]
          }

function handleMessage(ev) {
  let msg = JSON.parse(ev.data)
  console.log(msg)
  let entry = api[msg[0]]
  if (!entry) return

  let fun  = entry[0]

  switch (entry[1]) {
    case 1: return fun(msg[1])
    case 2: let args = msg[1]
            return fun(args[0],args[1])
  }
}


