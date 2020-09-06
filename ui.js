let objects = {}

function getObject(id) {
  return objects[id]
}


function newObject(id) {
  let dom = document.createElement("div")
  let obj = { dom: dom
            , rotation: 0
            , scale: 1
            , keyEvents: {}
            }
  objects[id] = obj

  let style = dom.style
  style.position = "absolute"
  setVisible(id,false)
  dom.setAttribute("id",id)
  document.body.appendChild(dom)
}

function destroyObject(id) {
  let obj = getObject(id)
  delete objects[id]
  document.body.reomveChild(obj.dom)
}

function setPosition(id, pos) {
  let obj = getObject(id)
  let s = obj.dom.style
  s.left = pos.x + 'px'
  s.top = pos.y + 'px'
  s.zIndex = pos.z
}

function setSize(id, dim) {
  let obj = getObject(id)
  let s = obj.dom.style
  s.width  = dim.width  + 'px'
  s.height = dim.height + 'px'
}

function setVisible(id, yes) {
  getObject(id).dom.style.display = yes? "inline-block" : "none"
}

function setBackgroundColor(id, color) {
  let obj = getObject(id)
  obj.dom.style.backgroundColor = color
}

function setBackground(id,img) {
  let obj = getObject(id)
  let s = obj.dom.style
  let url = "url('" + encodeURI(img) + "')"
  s.backgroundImage = url
  s.backgroundSize = "contain"
}

function setRotation(id, deg) {
  let obj = getObject(id)
  obj.rotation = deg
  doTransform(obj)
}

function getRotation(id) {
  return getObject(id).rotation
}

function rotate(id,deg) {
  let obj = getObject(id)
  obj.rotation = obj.rotation + deg
  doTransform(obj)
}


function setScale(id, s) {
  let obj = getObject(id)
  obj.scale = s
  doTransform(obj)
}

function getScale(id) {
  return getObject(id).scale
}

// private
function doTransform(obj) {
  let transform = "scale(" + obj.scale + ") rotate(" + obj.rotation + "deg)"
  obj.dom.style.transform = transform
}



// XXX: for clipping
function setShape(id, s) {
}

function setClipPath(id, path) {
  let obj = getObject(id)
  let name = "svg-paths"
  let svg  = document.getElementById(name)
  let svgNS = "http://www.w3.org/2000/svg"
  if (!svg) {
    svg = document.createElementNS(svgNS, "svg");
    svg.dataset.nextPathId = 0
    document.body.appendChild(svg)
  }
  let pid = svg.dataset.nextPathId
  svg.dataset.nextPathId = pid + 1

  let pathId = "svg-path" + pid
  let cp = document.createElementNS(svgNS,"clipPath")
  cp.setAttribute("id",pathId)
  cp.setAttribute("clipPathUnits","objectBoundingBox")
  let p = document.createElementNS(svgNS,"path")
  p.setAttribute("d",path)
  cp.appendChild(p)
  svg.appendChild(cp)

  let s = obj.dom.style
  s.clipPath = "url(#" + pathId + ")"
}


// -----------------------------------------------------------------------------


// The x y are relative to the bounding rectangle of the object which
// is not terribly useful, but it matters for body
function setClickable(id,ws) {
  let dom = id == "body"
          ? document.getElementsByTagName("body")[0]
          : getObject(id).dom
    dom.addEventListener("click",
    function(ev) { ws.send(JSON.stringify({id:id,event:'click',
                                          x: ev.clientX, y: ev.clientY})) }
  )
}

function setOnClickLocal(id,fun) {
  getObject(id).dom.addEventListener("click",fun)
}

function setOnEnterLocal(id,fun) {
  getObject(id).dom.addEventListener("mouseenter",fun)
}

function setOnExitLocal(id,fun) {
  getObject(id).dom.addEventListener("mouseleave",fun)
}

function setOnKeyPressLocal(id,key,fun) {
  let obj = getObject(id)
  let dom = obj.dom
  dom.setAttribute("tabindex","0")
  obj.keyEvents[key] = fun
  dom.addEventListener("keypress",function(ev) {
    let handler = obj.keyEvents[ev.key]
    if (handler) handler()
  })
}







