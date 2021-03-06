(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({"/Users/anders/dev/ps/halo/ajax/node_modules/global/document.js":[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":"/usr/local/lib/node_modules/pulp/node_modules/browser-resolve/empty.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/is-object/index.js":[function(require,module,exports){
"use strict";

module.exports = function isObject(x) {
	return typeof x === "object" && x !== null;
};

},{}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/create-element.js":[function(require,module,exports){
var createElement = require("./vdom/create-element.js")

module.exports = createElement

},{"./vdom/create-element.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/create-element.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/diff.js":[function(require,module,exports){
var diff = require("./vtree/diff.js")

module.exports = diff

},{"./vtree/diff.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vtree/diff.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/patch.js":[function(require,module,exports){
var patch = require("./vdom/patch.js")

module.exports = patch

},{"./vdom/patch.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/patch.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/apply-properties.js":[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook.js")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, propName, propValue, previous);
        } else if (isHook(propValue)) {
            removeProperty(node, propName, propValue, previous)
            if (propValue.hook) {
                propValue.hook(node,
                    propName,
                    previous ? previous[propName] : undefined)
            }
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, propName, propValue, previous) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        } else if (previousValue.unhook) {
            previousValue.unhook(node, propName, propValue)
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"../vnode/is-vhook.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vhook.js","is-object":"/Users/anders/dev/ps/halo/ajax/node_modules/is-object/index.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/create-element.js":[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("../vnode/is-vnode.js")
var isVText = require("../vnode/is-vtext.js")
var isWidget = require("../vnode/is-widget.js")
var handleThunk = require("../vnode/handle-thunk.js")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"../vnode/handle-thunk.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/handle-thunk.js","../vnode/is-vnode.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vnode.js","../vnode/is-vtext.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vtext.js","../vnode/is-widget.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-widget.js","./apply-properties":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/apply-properties.js","global/document":"/Users/anders/dev/ps/halo/ajax/node_modules/global/document.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/dom-index.js":[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/patch-op.js":[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("../vnode/is-widget.js")
var VPatch = require("../vnode/vpatch.js")

var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = renderOptions.render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = renderOptions.render(vText, renderOptions)

        if (parentNode && newNode !== domNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    var updating = updateWidget(leftVNode, widget)
    var newNode

    if (updating) {
        newNode = widget.update(leftVNode, domNode) || domNode
    } else {
        newNode = renderOptions.render(widget, renderOptions)
    }

    var parentNode = domNode.parentNode

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    if (!updating) {
        destroyWidget(domNode, leftVNode)
    }

    return newNode
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = renderOptions.render(vNode, renderOptions)

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, moves) {
    var childNodes = domNode.childNodes
    var keyMap = {}
    var node
    var remove
    var insert

    for (var i = 0; i < moves.removes.length; i++) {
        remove = moves.removes[i]
        node = childNodes[remove.from]
        if (remove.key) {
            keyMap[remove.key] = node
        }
        domNode.removeChild(node)
    }

    var length = childNodes.length
    for (var j = 0; j < moves.inserts.length; j++) {
        insert = moves.inserts[j]
        node = keyMap[insert.key]
        // this is the weirdest bug i've ever seen in webkit
        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to])
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"../vnode/is-widget.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-widget.js","../vnode/vpatch.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/vpatch.js","./apply-properties":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/apply-properties.js","./update-widget":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/update-widget.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/patch.js":[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var render = require("./create-element")
var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches, renderOptions) {
    renderOptions = renderOptions || {}
    renderOptions.patch = renderOptions.patch && renderOptions.patch !== patch
        ? renderOptions.patch
        : patchRecursive
    renderOptions.render = renderOptions.render || render

    return renderOptions.patch(rootNode, patches, renderOptions)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions.document && ownerDocument !== document) {
        renderOptions.document = ownerDocument
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./create-element":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/create-element.js","./dom-index":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/dom-index.js","./patch-op":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/patch-op.js","global/document":"/Users/anders/dev/ps/halo/ajax/node_modules/global/document.js","x-is-array":"/Users/anders/dev/ps/halo/ajax/node_modules/x-is-array/index.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vdom/update-widget.js":[function(require,module,exports){
var isWidget = require("../vnode/is-widget.js")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"../vnode/is-widget.js":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-widget.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/virtual-hyperscript/hooks/soft-set-hook.js":[function(require,module,exports){
'use strict';

module.exports = SoftSetHook;

function SoftSetHook(value) {
    if (!(this instanceof SoftSetHook)) {
        return new SoftSetHook(value);
    }

    this.value = value;
}

SoftSetHook.prototype.hook = function (node, propertyName) {
    if (node[propertyName] !== this.value) {
        node[propertyName] = this.value;
    }
};

},{}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/handle-thunk.js":[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-thunk.js","./is-vnode":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vnode.js","./is-vtext":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vtext.js","./is-widget":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-widget.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-thunk.js":[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vhook.js":[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook &&
      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
}

},{}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vnode.js":[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/version.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vtext.js":[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/version.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-widget.js":[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/version.js":[function(require,module,exports){
module.exports = "2"

},{}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/vnode.js":[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var hasThunks = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property) && property.unhook) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!hasThunks && child.hasThunks) {
                hasThunks = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        } else if (!hasThunks && isThunk(child)) {
            hasThunks = true;
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hasThunks = hasThunks
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-thunk":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-thunk.js","./is-vhook":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vhook.js","./is-vnode":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vnode.js","./is-widget":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-widget.js","./version":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/version.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/vpatch.js":[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/version.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/vtext.js":[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/version.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vtree/diff-props.js":[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook")

module.exports = diffProps

function diffProps(a, b) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (aValue === bValue) {
            continue
        } else if (isObject(aValue) && isObject(bValue)) {
            if (getPrototype(bValue) !== getPrototype(aValue)) {
                diff = diff || {}
                diff[aKey] = bValue
            } else if (isHook(bValue)) {
                 diff = diff || {}
                 diff[aKey] = bValue
            } else {
                var objectDiff = diffProps(aValue, bValue)
                if (objectDiff) {
                    diff = diff || {}
                    diff[aKey] = objectDiff
                }
            }
        } else {
            diff = diff || {}
            diff[aKey] = bValue
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
  if (Object.getPrototypeOf) {
    return Object.getPrototypeOf(value)
  } else if (value.__proto__) {
    return value.__proto__
  } else if (value.constructor) {
    return value.constructor.prototype
  }
}

},{"../vnode/is-vhook":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vhook.js","is-object":"/Users/anders/dev/ps/halo/ajax/node_modules/is-object/index.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vtree/diff.js":[function(require,module,exports){
var isArray = require("x-is-array")

var VPatch = require("../vnode/vpatch")
var isVNode = require("../vnode/is-vnode")
var isVText = require("../vnode/is-vtext")
var isWidget = require("../vnode/is-widget")
var isThunk = require("../vnode/is-thunk")
var handleThunk = require("../vnode/handle-thunk")

var diffProps = require("./diff-props")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        return
    }

    var apply = patch[index]
    var applyClear = false

    if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (b == null) {

        // If a is a widget we will add a remove patch for it
        // Otherwise any child widgets/hooks must be destroyed.
        // This prevents adding two remove patches for a widget.
        if (!isWidget(a)) {
            clearState(a, patch, index)
            apply = patch[index]
        }

        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                applyClear = true
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            applyClear = true
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            applyClear = true
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        if (!isWidget(a)) {
            applyClear = true
        }

        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))
    }

    if (apply) {
        patch[index] = apply
    }

    if (applyClear) {
        clearState(a, patch, index)
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var orderedSet = reorder(aChildren, b.children)
    var bChildren = orderedSet.children

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (orderedSet.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(
            VPatch.ORDER,
            a,
            orderedSet.moves
        ))
    }

    return apply
}

function clearState(vNode, patch, index) {
    // TODO: Make this a single walk, not two
    unhook(vNode, patch, index)
    destroyWidgets(vNode, patch, index)
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(VPatch.REMOVE, vNode, null)
            )
        }
    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b)
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true
        }
    }

    return false
}

// Execute hooks when two nodes are identical
function unhook(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(
                    VPatch.PROPS,
                    vNode,
                    undefinedKeys(vNode.hooks)
                )
            )
        }

        if (vNode.descendantHooks || vNode.hasThunks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                unhook(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

function undefinedKeys(obj) {
    var result = {}

    for (var key in obj) {
        result[key] = undefined
    }

    return result
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {
    // O(M) time, O(M) memory
    var bChildIndex = keyIndex(bChildren)
    var bKeys = bChildIndex.keys
    var bFree = bChildIndex.free

    if (bFree.length === bChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(N) time, O(N) memory
    var aChildIndex = keyIndex(aChildren)
    var aKeys = aChildIndex.keys
    var aFree = aChildIndex.free

    if (aFree.length === aChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(MAX(N, M)) memory
    var newChildren = []

    var freeIndex = 0
    var freeCount = bFree.length
    var deletedItems = 0

    // Iterate through a and match a node in b
    // O(N) time,
    for (var i = 0 ; i < aChildren.length; i++) {
        var aItem = aChildren[i]
        var itemIndex

        if (aItem.key) {
            if (bKeys.hasOwnProperty(aItem.key)) {
                // Match up the old keys
                itemIndex = bKeys[aItem.key]
                newChildren.push(bChildren[itemIndex])

            } else {
                // Remove old keyed items
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        } else {
            // Match the item in a with the next free item in b
            if (freeIndex < freeCount) {
                itemIndex = bFree[freeIndex++]
                newChildren.push(bChildren[itemIndex])
            } else {
                // There are no free items in b to match with
                // the free items in a, so the extra free nodes
                // are deleted.
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        }
    }

    var lastFreeIndex = freeIndex >= bFree.length ?
        bChildren.length :
        bFree[freeIndex]

    // Iterate through b and append any new keys
    // O(M) time
    for (var j = 0; j < bChildren.length; j++) {
        var newItem = bChildren[j]

        if (newItem.key) {
            if (!aKeys.hasOwnProperty(newItem.key)) {
                // Add any new keyed items
                // We are adding new items to the end and then sorting them
                // in place. In future we should insert new items in place.
                newChildren.push(newItem)
            }
        } else if (j >= lastFreeIndex) {
            // Add any leftover non-keyed items
            newChildren.push(newItem)
        }
    }

    var simulate = newChildren.slice()
    var simulateIndex = 0
    var removes = []
    var inserts = []
    var simulateItem

    for (var k = 0; k < bChildren.length;) {
        var wantedItem = bChildren[k]
        simulateItem = simulate[simulateIndex]

        // remove items
        while (simulateItem === null && simulate.length) {
            removes.push(remove(simulate, simulateIndex, null))
            simulateItem = simulate[simulateIndex]
        }

        if (!simulateItem || simulateItem.key !== wantedItem.key) {
            // if we need a key in this position...
            if (wantedItem.key) {
                if (simulateItem && simulateItem.key) {
                    // if an insert doesn't put this key in place, it needs to move
                    if (bKeys[simulateItem.key] !== k + 1) {
                        removes.push(remove(simulate, simulateIndex, simulateItem.key))
                        simulateItem = simulate[simulateIndex]
                        // if the remove didn't put the wanted item in place, we need to insert it
                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
                            inserts.push({key: wantedItem.key, to: k})
                        }
                        // items are matching, so skip ahead
                        else {
                            simulateIndex++
                        }
                    }
                    else {
                        inserts.push({key: wantedItem.key, to: k})
                    }
                }
                else {
                    inserts.push({key: wantedItem.key, to: k})
                }
                k++
            }
            // a key in simulate has no matching wanted key, remove it
            else if (simulateItem && simulateItem.key) {
                removes.push(remove(simulate, simulateIndex, simulateItem.key))
            }
        }
        else {
            simulateIndex++
            k++
        }
    }

    // remove all the remaining nodes from simulate
    while(simulateIndex < simulate.length) {
        simulateItem = simulate[simulateIndex]
        removes.push(remove(simulate, simulateIndex, simulateItem && simulateItem.key))
    }

    // If the only moves we have are deletes then we can just
    // let the delete patch remove these items.
    if (removes.length === deletedItems && !inserts.length) {
        return {
            children: newChildren,
            moves: null
        }
    }

    return {
        children: newChildren,
        moves: {
            removes: removes,
            inserts: inserts
        }
    }
}

function remove(arr, index, key) {
    arr.splice(index, 1)

    return {
        from: index,
        key: key
    }
}

function keyIndex(children) {
    var keys = {}
    var free = []
    var length = children.length

    for (var i = 0; i < length; i++) {
        var child = children[i]

        if (child.key) {
            keys[child.key] = i
        } else {
            free.push(i)
        }
    }

    return {
        keys: keys,     // A hash of key name to index
        free: free      // An array of unkeyed item indices
    }
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"../vnode/handle-thunk":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/handle-thunk.js","../vnode/is-thunk":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-thunk.js","../vnode/is-vnode":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vnode.js","../vnode/is-vtext":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-vtext.js","../vnode/is-widget":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/is-widget.js","../vnode/vpatch":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/vpatch.js","./diff-props":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vtree/diff-props.js","x-is-array":"/Users/anders/dev/ps/halo/ajax/node_modules/x-is-array/index.js"}],"/Users/anders/dev/ps/halo/ajax/node_modules/x-is-array/index.js":[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Alt = function (__superclass_Prelude$dotFunctor_0, alt) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.alt = alt;
};
var altArray = new Alt(function () {
    return Prelude.functorArray;
}, Prelude.append(Prelude.semigroupArray));
var alt = function (dict) {
    return dict.alt;
};
var $less$bar$greater = function (dictAlt) {
    return alt(dictAlt);
};
module.exports = {
    Alt: Alt, 
    "<|>": $less$bar$greater, 
    alt: alt, 
    altArray: altArray
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Plus = require("Control.Plus");
var Alternative = function (__superclass_Control$dotPlus$dotPlus_1, __superclass_Prelude$dotApplicative_0) {
    this["__superclass_Control.Plus.Plus_1"] = __superclass_Control$dotPlus$dotPlus_1;
    this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
};
var alternativeArray = new Alternative(function () {
    return Control_Plus.plusArray;
}, function () {
    return Prelude.applicativeArray;
});
module.exports = {
    Alternative: Alternative, 
    alternativeArray: alternativeArray
};

},{"Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Apply/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var $less$times = function (dictApply) {
    return function (a) {
        return function (b) {
            return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Prelude["const"])(a))(b);
        };
    };
};
var $times$greater = function (dictApply) {
    return function (a) {
        return function (b) {
            return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Prelude["const"](Prelude.id(Prelude.categoryFn)))(a))(b);
        };
    };
};
var lift5 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(f)(a))(b))(c))(d))(e);
                        };
                    };
                };
            };
        };
    };
};
var lift4 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(f)(a))(b))(c))(d);
                    };
                };
            };
        };
    };
};
var lift3 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Prelude["<*>"](dictApply)(Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(f)(a))(b))(c);
                };
            };
        };
    };
};
var lift2 = function (dictApply) {
    return function (f) {
        return function (a) {
            return function (b) {
                return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(f)(a))(b);
            };
        };
    };
};
module.exports = {
    lift5: lift5, 
    lift4: lift4, 
    lift3: lift3, 
    lift2: lift2, 
    "*>": $times$greater, 
    "<*": $less$times
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Biapplicative/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Biapply = require("Control.Biapply");
var Biapplicative = function (__superclass_Control$dotBiapply$dotBiapply_0, bipure) {
    this["__superclass_Control.Biapply.Biapply_0"] = __superclass_Control$dotBiapply$dotBiapply_0;
    this.bipure = bipure;
};
var bipure = function (dict) {
    return dict.bipure;
};
module.exports = {
    Biapplicative: Biapplicative, 
    bipure: bipure
};

},{"Control.Biapply":"/Users/anders/dev/ps/halo/ajax/output/Control.Biapply/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Biapply/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Bifunctor = require("Data.Bifunctor");
var Biapply = function (__superclass_Data$dotBifunctor$dotBifunctor_0, biapply) {
    this["__superclass_Data.Bifunctor.Bifunctor_0"] = __superclass_Data$dotBifunctor$dotBifunctor_0;
    this.biapply = biapply;
};
var $less$less$dollar$greater$greater = Prelude.id(Prelude.categoryFn);
var biapply = function (dict) {
    return dict.biapply;
};
var $less$less$times$greater$greater = function (dictBiapply) {
    return biapply(dictBiapply);
};
var bilift2 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return $less$less$times$greater$greater(dictBiapply)($less$less$dollar$greater$greater(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g))(a))(b);
                };
            };
        };
    };
};
var bilift3 = function (dictBiapply) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return $less$less$times$greater$greater(dictBiapply)($less$less$times$greater$greater(dictBiapply)($less$less$dollar$greater$greater(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g))(a))(b))(c);
                    };
                };
            };
        };
    };
};
var $times$greater$greater = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return $less$less$times$greater$greater(dictBiapply)($less$less$dollar$greater$greater(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(Prelude["const"](Prelude.id(Prelude.categoryFn)))(Prelude["const"](Prelude.id(Prelude.categoryFn))))(a))(b);
        };
    };
};
var $less$less$times = function (dictBiapply) {
    return function (a) {
        return function (b) {
            return $less$less$times$greater$greater(dictBiapply)($less$less$dollar$greater$greater(Data_Bifunctor.bimap(dictBiapply["__superclass_Data.Bifunctor.Bifunctor_0"]())(Prelude["const"])(Prelude["const"]))(a))(b);
        };
    };
};
module.exports = {
    Biapply: Biapply, 
    bilift3: bilift3, 
    bilift2: bilift2, 
    "<<*": $less$less$times, 
    "*>>": $times$greater$greater, 
    "<<*>>": $less$less$times$greater$greater, 
    biapply: biapply, 
    "<<$>>": $less$less$dollar$greater$greater
};

},{"Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Bind/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var $greater$eq$greater = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return Prelude[">>="](dictBind)(f(a))(g);
            };
        };
    };
};
var $eq$less$less = function (dictBind) {
    return function (f) {
        return function (m) {
            return Prelude[">>="](dictBind)(m)(f);
        };
    };
};
var $less$eq$less = function (dictBind) {
    return function (f) {
        return function (g) {
            return function (a) {
                return $eq$less$less(dictBind)(f)(g(a));
            };
        };
    };
};
var join = function (dictBind) {
    return function (m) {
        return Prelude[">>="](dictBind)(m)(Prelude.id(Prelude.categoryFn));
    };
};
var ifM = function (dictBind) {
    return function (cond) {
        return function (t) {
            return function (f) {
                return Prelude[">>="](dictBind)(cond)(function (condʹ) {
                    if (condʹ) {
                        return t;
                    };
                    if (!condʹ) {
                        return f;
                    };
                    throw new Error("Failed pattern match at Control.Bind line 44, column 1 - line 45, column 1: " + [ condʹ.constructor.name ]);
                });
            };
        };
    };
};
module.exports = {
    ifM: ifM, 
    join: join, 
    "<=<": $less$eq$less, 
    ">=>": $greater$eq$greater, 
    "=<<": $eq$less$less
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Extend = require("Control.Extend");
var Comonad = function (__superclass_Control$dotExtend$dotExtend_0, extract) {
    this["__superclass_Control.Extend.Extend_0"] = __superclass_Control$dotExtend$dotExtend_0;
    this.extract = extract;
};
var extract = function (dict) {
    return dict.extract;
};
module.exports = {
    Comonad: Comonad, 
    extract: extract
};

},{"Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine.Aff/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Either = require("Data.Either");
var Data_Functor = require("Data.Functor");
var Control_Coroutine = require("Control.Coroutine");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("Control.Monad.Aff.AVar");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Monad_Free_Trans = require("Control.Monad.Free.Trans");
var produce = function (recv) {
    return Prelude.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorEmit)(Control_Monad_Aff.monadAff))(Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorEmit))(Control_Monad_Aff.monadAff)(Control_Monad_Aff_AVar.makeVar))(function (v) {
        return Prelude.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorEmit)(Control_Monad_Aff.monadAff))(Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorEmit))(Control_Monad_Aff.monadAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(recv(function ($2) {
            return Control_Monad_Aff.runAff(Prelude["const"](Prelude["return"](Control_Monad_Eff.applicativeEff)(Prelude.unit)))(Prelude["return"](Control_Monad_Eff.applicativeEff))(Control_Monad_Aff_AVar.putVar(v)($2));
        }))))(function () {
            return Control_Coroutine.producer(Control_Monad_Aff.monadAff)(Control_Monad_Aff_AVar.takeVar(v));
        });
    });
};
module.exports = {
    produce: produce
};

},{"Control.Coroutine":"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine/index.js","Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Aff.AVar":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/index.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Free.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free.Trans/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Functor":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine.Stalling/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Coroutine = require("Control.Coroutine");
var Control_Monad_Free_Trans = require("Control.Monad.Free.Trans");
var Control_Monad_Maybe_Trans = require("Control.Monad.Maybe.Trans");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Bind = require("Control.Bind");
var Control_Plus = require("Control.Plus");
var Data_Functor = require("Data.Functor");
var Data_Bifunctor = require("Data.Bifunctor");
var Data_Either = require("Data.Either");
var Data_Identity = require("Data.Identity");
var Data_Maybe = require("Data.Maybe");
var Data_Tuple = require("Data.Tuple");
var Emit = (function () {
    function Emit(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Emit.create = function (value0) {
        return function (value1) {
            return new Emit(value0, value1);
        };
    };
    return Emit;
})();
var Stall = (function () {
    function Stall(value0) {
        this.value0 = value0;
    };
    Stall.create = function (value0) {
        return new Stall(value0);
    };
    return Stall;
})();
var stallF = function (e) {
    return function (s) {
        return function (q) {
            if (q instanceof Emit) {
                return e(q.value0)(q.value1);
            };
            if (q instanceof Stall) {
                return s(q.value0);
            };
            throw new Error("Failed pattern match at Control.Coroutine.Stalling line 40, column 1 - line 46, column 1: " + [ q.constructor.name ]);
        };
    };
};
var runStallingProcess = function (dictMonadRec) {
    return function ($28) {
        return Control_Monad_Maybe_Trans.runMaybeT(Control_Monad_Free_Trans.runFreeT(Data_Maybe.functorMaybe)(Control_Monad_Maybe_Trans.monadRecMaybeT(dictMonadRec))(Data_Maybe.maybe(Control_Plus.empty(Control_Monad_Maybe_Trans.plusMaybeT(dictMonadRec["__superclass_Prelude.Monad_0"]())))(Prelude.pure(Control_Monad_Maybe_Trans.applicativeMaybeT(dictMonadRec["__superclass_Prelude.Monad_0"]()))))(Control_Monad_Free_Trans.hoistFreeT(Data_Maybe.functorMaybe)(Control_Monad_Maybe_Trans.functorMaybeT(dictMonadRec["__superclass_Prelude.Monad_0"]()))(function ($29) {
            return Control_Monad_Maybe_Trans.MaybeT(Prelude.map((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe.Just.create)($29));
        })($28)));
    };
};
var producerToStallingProducer = function (dictFunctor) {
    return Control_Monad_Free_Trans.interpret(Control_Coroutine.functorEmit)(dictFunctor)(function (v) {
        return new Emit(v.value0, v.value1);
    });
};
var processToStallingProcess = function (dictFunctor) {
    return Control_Monad_Free_Trans.interpret(Data_Identity.functorIdentity)(dictFunctor)(function ($30) {
        return Data_Maybe.Just.create(Data_Identity.runIdentity($30));
    });
};
var bifunctorStallF = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (q) {
            if (q instanceof Emit) {
                return new Emit(f(q.value0), g(q.value1));
            };
            if (q instanceof Stall) {
                return new Stall(g(q.value0));
            };
            throw new Error("Failed pattern match at Control.Coroutine.Stalling line 51, column 1 - line 57, column 1: " + [ q.constructor.name ]);
        };
    };
});
var functorStallF = new Prelude.Functor(function (f) {
    return Data_Bifunctor.rmap(bifunctorStallF)(f);
});
var emit = function (dictMonad) {
    return function ($31) {
        return Control_Monad_Free_Trans.liftFreeT(functorStallF)(dictMonad)(Prelude.flip(Emit.create)(Prelude.unit)($31));
    };
};
var catMaybes = function (dictMonadRec) {
    return Control_Monad_Rec_Class.tailRecM(Control_Monad_Free_Trans.monadRecFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(Control_Bind[">=>"](Control_Monad_Free_Trans.bindFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(Prelude[">>>"](Prelude.semigroupoidFn)(Control_Monad_Free_Trans.resume(functorStallF)(dictMonadRec))(Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(functorStallF))(dictMonadRec["__superclass_Prelude.Monad_0"]())))(Data_Either.either(Prelude[">>>"](Prelude.semigroupoidFn)(Data_Either.Right.create)(Prelude.pure(Control_Monad_Free_Trans.applicativeFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))))(stallF(function (mo) {
        return function (t) {
            return Data_Functor["$>"](Control_Monad_Free_Trans.functorFreeT(functorStallF)((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(Data_Maybe.maybe(Prelude.pure(Control_Monad_Free_Trans.applicativeFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(Prelude.unit))(emit(dictMonadRec["__superclass_Prelude.Monad_0"]()))(mo))(new Data_Either.Left(t));
        };
    })(Prelude[">>>"](Prelude.semigroupoidFn)(Data_Either.Left.create)(Prelude.pure(Control_Monad_Free_Trans.applicativeFreeT(functorStallF)(dictMonadRec["__superclass_Prelude.Monad_0"]())))))));
};
var stall = function (dictMonad) {
    return Control_Monad_Free_Trans.liftFreeT(functorStallF)(dictMonad)(new Stall(Prelude.unit));
};
var $dollar$dollar$qmark = function (dictMonadRec) {
    return Control_Coroutine.fuseWith(functorStallF)(Control_Coroutine.functorAwait)(Data_Maybe.functorMaybe)(dictMonadRec)(function (f) {
        return function (q) {
            return function (v) {
                if (q instanceof Emit) {
                    return new Data_Maybe.Just(f(q.value1)(v(q.value0)));
                };
                if (q instanceof Stall) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Control.Coroutine.Stalling line 80, column 1 - line 86, column 1: " + [ q.constructor.name ]);
            };
        };
    });
};
var mapStallingProducer = function (dictFunctor) {
    return function ($32) {
        return Control_Monad_Free_Trans.interpret(functorStallF)(dictFunctor)(Data_Bifunctor.lmap(bifunctorStallF)($32));
    };
};
var filter = function (dictMonadRec) {
    return function (p) {
        return function ($33) {
            return catMaybes(dictMonadRec)(mapStallingProducer((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function (x) {
                var $27 = p(x);
                if ($27) {
                    return new Data_Maybe.Just(x);
                };
                if (!$27) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Control.Coroutine.Stalling line 143, column 1 - line 149, column 1: " + [ $27.constructor.name ]);
            })($33));
        };
    };
};
module.exports = {
    Emit: Emit, 
    Stall: Stall, 
    filter: filter, 
    catMaybes: catMaybes, 
    mapStallingProducer: mapStallingProducer, 
    "$$?": $dollar$dollar$qmark, 
    runStallingProcess: runStallingProcess, 
    processToStallingProcess: processToStallingProcess, 
    producerToStallingProducer: producerToStallingProducer, 
    stallF: stallF, 
    stall: stall, 
    emit: emit, 
    bifunctorStallF: bifunctorStallF, 
    functorStallF: functorStallF
};

},{"Control.Bind":"/Users/anders/dev/ps/halo/ajax/output/Control.Bind/index.js","Control.Coroutine":"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine/index.js","Control.Monad.Free.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free.Trans/index.js","Control.Monad.Maybe.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Maybe.Trans/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Functor":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Tuple = require("Data.Tuple");
var Data_Either = require("Data.Either");
var Data_Identity = require("Data.Identity");
var Data_Functor = require("Data.Functor");
var Data_Bifunctor = require("Data.Bifunctor");
var Data_Profunctor = require("Data.Profunctor");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Monad_Free_Trans = require("Control.Monad.Free.Trans");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Transform = function (x) {
    return x;
};
var Emit = (function () {
    function Emit(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Emit.create = function (value0) {
        return function (value1) {
            return new Emit(value0, value1);
        };
    };
    return Emit;
})();
var Await = function (x) {
    return x;
};
var runProcess = function (dictMonadRec) {
    return Control_Monad_Free_Trans.runFreeT(Data_Identity.functorIdentity)(dictMonadRec)(function ($104) {
        return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(Data_Identity.runIdentity($104));
    });
};
var profunctorAwait = new Data_Profunctor.Profunctor(function (f) {
    return function (g) {
        return function (v) {
            return Data_Profunctor.dimap(Data_Profunctor.profunctorFn)(f)(g)(v);
        };
    };
});
var loop = function (dictFunctor) {
    return function (dictMonad) {
        return function (me) {
            return Control_Monad_Rec_Class.tailRecM(Control_Monad_Free_Trans.monadRecFreeT(dictFunctor)(dictMonad))(function (v) {
                return Prelude.map(Control_Monad_Free_Trans.functorFreeT(dictFunctor)(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(Data_Maybe.maybe(new Data_Either.Left(Prelude.unit))(Data_Either.Right.create))(me);
            })(Prelude.unit);
        };
    };
};
var fuseWith = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictFunctor2) {
            return function (dictMonadRec) {
                return function (zap) {
                    return function (fs) {
                        return function (gs) {
                            var go = function (v) {
                                return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(Control_Monad_Free_Trans.resume(dictFunctor1)(dictMonadRec)(v.value1))(function (v1) {
                                    return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(Control_Monad_Free_Trans.resume(dictFunctor)(dictMonadRec)(v.value0))(function (v2) {
                                        var $49 = Prelude["<*>"](Data_Either.applyEither)(Prelude["<$>"](Data_Either.functorEither)(zap(Data_Tuple.Tuple.create))(v2))(v1);
                                        if ($49 instanceof Data_Either.Left) {
                                            return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left($49.value0));
                                        };
                                        if ($49 instanceof Data_Either.Right) {
                                            return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(Prelude.map(dictFunctor2)(function (t) {
                                                return Control_Monad_Free_Trans.freeT(function (v3) {
                                                    return go(t);
                                                });
                                            })($49.value0)));
                                        };
                                        throw new Error("Failed pattern match at Control.Coroutine line 49, column 1 - line 54, column 1: " + [ $49.constructor.name ]);
                                    });
                                });
                            };
                            return Control_Monad_Free_Trans.freeT(function (v) {
                                return go(new Data_Tuple.Tuple(fs, gs));
                            });
                        };
                    };
                };
            };
        };
    };
};
var functorAwait = new Prelude.Functor(Data_Profunctor.rmap(profunctorAwait));
var $bslash$div = function (dictMonadRec) {
    return fuseWith(functorAwait)(functorAwait)(functorAwait)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return function (v2) {
                    return f(v(v2.value0))(v1(v2.value1));
                };
            };
        };
    });
};
var bifunctorTransform = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return function ($105) {
                return Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(f)(g)(v($105));
            };
        };
    };
});
var functorTransform = new Prelude.Functor(Data_Bifunctor.rmap(bifunctorTransform));
var transform = function (dictMonad) {
    return function (f) {
        return Control_Monad_Free_Trans.liftFreeT(functorTransform)(dictMonad)(function (i) {
            return new Data_Tuple.Tuple(f(i), Prelude.unit);
        });
    };
};
var $tilde$dollar = function (dictMonadRec) {
    return fuseWith(functorTransform)(functorAwait)(functorAwait)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return function (i) {
                    var $66 = v(i);
                    return f($66.value1)(v1($66.value0));
                };
            };
        };
    });
};
var $tilde$tilde = function (dictMonadRec) {
    return fuseWith(functorTransform)(functorTransform)(functorTransform)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return function (i) {
                    var $71 = v(i);
                    var $72 = v1($71.value0);
                    return new Data_Tuple.Tuple($72.value0, f($71.value1)($72.value1));
                };
            };
        };
    });
};
var bifunctorEmit = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return new Emit(f(v.value0), g(v.value1));
        };
    };
});
var functorEmit = new Prelude.Functor(Data_Bifunctor.rmap(bifunctorEmit));
var emit = function (dictMonad) {
    return function (o) {
        return Control_Monad_Free_Trans.liftFreeT(functorEmit)(dictMonad)(new Emit(o, Prelude.unit));
    };
};
var producer = function (dictMonad) {
    return function (recv) {
        return loop(functorEmit)(dictMonad)(Prelude.bind(Control_Monad_Free_Trans.bindFreeT(functorEmit)(dictMonad))(Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(functorEmit))(dictMonad)(recv))(function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Functor["$>"](Control_Monad_Free_Trans.functorFreeT(functorEmit)(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(emit(dictMonad)(v.value0))(Data_Maybe.Nothing.value);
            };
            if (v instanceof Data_Either.Right) {
                return Prelude["return"](Control_Monad_Free_Trans.applicativeFreeT(functorEmit)(dictMonad))(new Data_Maybe.Just(v.value0));
            };
            throw new Error("Failed pattern match at Control.Coroutine line 83, column 1 - line 84, column 1: " + [ v.constructor.name ]);
        }));
    };
};
var $dollar$dollar = function (dictMonadRec) {
    return fuseWith(functorEmit)(functorAwait)(Data_Identity.functorIdentity)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return f(v.value1)(v1(v.value0));
            };
        };
    });
};
var $dollar$tilde = function (dictMonadRec) {
    return fuseWith(functorEmit)(functorTransform)(functorEmit)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                var $92 = v1(v.value0);
                return new Emit($92.value0, f(v.value1)($92.value1));
            };
        };
    });
};
var $div$bslash = function (dictMonadRec) {
    return fuseWith(functorEmit)(functorEmit)(functorEmit)(dictMonadRec)(function (f) {
        return function (v) {
            return function (v1) {
                return new Emit(new Data_Tuple.Tuple(v.value0, v1.value0), f(v.value1)(v1.value1));
            };
        };
    });
};
var await = function (dictMonad) {
    return Control_Monad_Free_Trans.liftFreeT(functorAwait)(dictMonad)(Prelude.id(Prelude.categoryFn));
};
var consumer = function (dictMonad) {
    return function (send) {
        return loop(functorAwait)(dictMonad)(Prelude.bind(Control_Monad_Free_Trans.bindFreeT(functorAwait)(dictMonad))(await(dictMonad))(function (v) {
            return Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(functorAwait))(dictMonad)(send(v));
        }));
    };
};
module.exports = {
    Transform: Transform, 
    Await: Await, 
    Emit: Emit, 
    "\\/": $bslash$div, 
    "/\\": $div$bslash, 
    "~~": $tilde$tilde, 
    "~$": $tilde$dollar, 
    "$~": $dollar$tilde, 
    "$$": $dollar$dollar, 
    transform: transform, 
    consumer: consumer, 
    await: await, 
    producer: producer, 
    emit: emit, 
    fuseWith: fuseWith, 
    runProcess: runProcess, 
    loop: loop, 
    bifunctorEmit: bifunctorEmit, 
    functorEmit: functorEmit, 
    profunctorAwait: profunctorAwait, 
    functorAwait: functorAwait, 
    bifunctorTransform: bifunctorTransform, 
    functorTransform: functorTransform
};

},{"Control.Monad.Free.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free.Trans/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Functor":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Profunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Profunctor/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Extend = function (__superclass_Prelude$dotFunctor_0, extend) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.extend = extend;
};
var extendFn = function (dictSemigroup) {
    return new Extend(function () {
        return Prelude.functorFn;
    }, function (f) {
        return function (g) {
            return function (w) {
                return f(function (wʹ) {
                    return g(Prelude["<>"](dictSemigroup)(w)(wʹ));
                });
            };
        };
    });
};
var extend = function (dict) {
    return dict.extend;
};
var $less$less$eq = function (dictExtend) {
    return extend(dictExtend);
};
var $eq$less$eq = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return f($less$less$eq(dictExtend)(g)(w));
            };
        };
    };
};
var $eq$greater$eq = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return g($less$less$eq(dictExtend)(f)(w));
            };
        };
    };
};
var $eq$greater$greater = function (dictExtend) {
    return function (w) {
        return function (f) {
            return $less$less$eq(dictExtend)(f)(w);
        };
    };
};
var duplicate = function (dictExtend) {
    return extend(dictExtend)(Prelude.id(Prelude.categoryFn));
};
module.exports = {
    Extend: Extend, 
    duplicate: duplicate, 
    "=<=": $eq$less$eq, 
    "=>=": $eq$greater$eq, 
    "=>>": $eq$greater$greater, 
    "<<=": $less$less$eq, 
    extend: extend, 
    extendFn: extendFn
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Lazy/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Lazy = function (defer) {
    this.defer = defer;
};
var defer = function (dict) {
    return dict.defer;
};
var fix = function (dictLazy) {
    return function (f) {
        return defer(dictLazy)(function (v) {
            return f(fix(dictLazy)(f));
        });
    };
};
module.exports = {
    Lazy: Lazy, 
    fix: fix, 
    defer: defer
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Aff.AVar

exports._makeVar = function (nonCanceler) {
  return function(success, error) {
    try {
      success({
        consumers: [],
        producers: [],
        error: undefined 
      });
    } catch (e) {
      error(e);
    }

    return nonCanceler;
  }
}

exports._takeVar = function (nonCanceler, avar) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      var producer = avar.producers.shift();

      producer(success, error);
    } else {
      avar.consumers.push({success: success, error: error});
    }

    return nonCanceler;
  } 
}

exports._putVar = function (nonCanceler, avar, a) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.consumers.length === 0) {
      avar.producers.push(function(success, error) {
        try {
          success(a);
        } catch (e) {
          error(e);
        }
      });

      success({});
    } else {
      var consumer = avar.consumers.shift();

      try {
        consumer.success(a);
      } catch (e) {
        error(e);

        return;                  
      }

      success({});
    }

    return nonCanceler;
  }
}

exports._killVar = function (nonCanceler, avar, e) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else {
      var errors = [];

      avar.error = e;

      while (avar.consumers.length > 0) {
        var consumer = avar.consumers.shift();

        try {
          consumer.error(e);
        } catch (e) {
          errors.push(e);              
        }
      }

      if (errors.length > 0) error(errors[0]);
      else success({});
    }

    return nonCanceler;
  }
}

},{}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Function = require("Data.Function");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Eff_Exception = require("Control.Monad.Eff.Exception");
var takeVar = function (q) {
    return $foreign._takeVar(Control_Monad_Aff.nonCanceler, q);
};
var putVar = function (q) {
    return function (a) {
        return $foreign._putVar(Control_Monad_Aff.nonCanceler, q, a);
    };
};
var modifyVar = function (f) {
    return function (v) {
        return Prelude[">>="](Control_Monad_Aff.bindAff)(takeVar(v))(Prelude[">>>"](Prelude.semigroupoidFn)(f)(putVar(v)));
    };
};
var makeVar = $foreign._makeVar(Control_Monad_Aff.nonCanceler);
var makeVar$prime = function (a) {
    return Prelude.bind(Control_Monad_Aff.bindAff)(makeVar)(function (v) {
        return Prelude.bind(Control_Monad_Aff.bindAff)(putVar(v)(a))(function () {
            return Prelude["return"](Control_Monad_Aff.applicativeAff)(v);
        });
    });
};
var killVar = function (q) {
    return function (e) {
        return $foreign._killVar(Control_Monad_Aff.nonCanceler, q, e);
    };
};
module.exports = {
    takeVar: takeVar, 
    putVar: putVar, 
    modifyVar: modifyVar, 
    "makeVar'": makeVar$prime, 
    makeVar: makeVar, 
    killVar: killVar
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/foreign.js","Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Eff.Exception":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/index.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Cont_Trans = require("Control.Monad.Cont.Trans");
var Control_Monad_Except_Trans = require("Control.Monad.Except.Trans");
var Control_Monad_List_Trans = require("Control.Monad.List.Trans");
var Control_Monad_Maybe_Trans = require("Control.Monad.Maybe.Trans");
var Control_Monad_Reader_Trans = require("Control.Monad.Reader.Trans");
var Control_Monad_RWS_Trans = require("Control.Monad.RWS.Trans");
var Control_Monad_State_Trans = require("Control.Monad.State.Trans");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Monad_Writer_Trans = require("Control.Monad.Writer.Trans");
var Data_Monoid = require("Data.Monoid");
var MonadAff = function (liftAff) {
    this.liftAff = liftAff;
};
var monadAffAff = new MonadAff(Prelude.id(Prelude.categoryFn));
var liftAff = function (dict) {
    return dict.liftAff;
};
var monadAffContT = function (dictMonad) {
    return function (dictMonadAff) {
        return new MonadAff(function ($18) {
            return Control_Monad_Trans.lift(Control_Monad_Cont_Trans.monadTransContT)(dictMonad)(liftAff(dictMonadAff)($18));
        });
    };
};
var monadAffExceptT = function (dictMonad) {
    return function (dictMonadAff) {
        return new MonadAff(function ($19) {
            return Control_Monad_Trans.lift(Control_Monad_Except_Trans.monadTransExceptT)(dictMonad)(liftAff(dictMonadAff)($19));
        });
    };
};
var monadAffListT = function (dictMonad) {
    return function (dictMonadAff) {
        return new MonadAff(function ($20) {
            return Control_Monad_Trans.lift(Control_Monad_List_Trans.monadTransListT)(dictMonad)(liftAff(dictMonadAff)($20));
        });
    };
};
var monadAffMaybe = function (dictMonad) {
    return function (dictMonadAff) {
        return new MonadAff(function ($21) {
            return Control_Monad_Trans.lift(Control_Monad_Maybe_Trans.monadTransMaybeT)(dictMonad)(liftAff(dictMonadAff)($21));
        });
    };
};
var monadAffRWS = function (dictMonad) {
    return function (dictMonoid) {
        return function (dictMonadAff) {
            return new MonadAff(function ($22) {
                return Control_Monad_Trans.lift(Control_Monad_RWS_Trans.monadTransRWST(dictMonoid))(dictMonad)(liftAff(dictMonadAff)($22));
            });
        };
    };
};
var monadAffReader = function (dictMonad) {
    return function (dictMonadAff) {
        return new MonadAff(function ($23) {
            return Control_Monad_Trans.lift(Control_Monad_Reader_Trans.monadTransReaderT)(dictMonad)(liftAff(dictMonadAff)($23));
        });
    };
};
var monadAffState = function (dictMonad) {
    return function (dictMonadAff) {
        return new MonadAff(function ($24) {
            return Control_Monad_Trans.lift(Control_Monad_State_Trans.monadTransStateT)(dictMonad)(liftAff(dictMonadAff)($24));
        });
    };
};
var monadAffWriter = function (dictMonad) {
    return function (dictMonoid) {
        return function (dictMonadAff) {
            return new MonadAff(function ($25) {
                return Control_Monad_Trans.lift(Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid))(dictMonad)(liftAff(dictMonadAff)($25));
            });
        };
    };
};
module.exports = {
    MonadAff: MonadAff, 
    liftAff: liftAff, 
    monadAffAff: monadAffAff, 
    monadAffContT: monadAffContT, 
    monadAffExceptT: monadAffExceptT, 
    monadAffListT: monadAffListT, 
    monadAffMaybe: monadAffMaybe, 
    monadAffReader: monadAffReader, 
    monadAffRWS: monadAffRWS, 
    monadAffState: monadAffState, 
    monadAffWriter: monadAffWriter
};

},{"Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Cont.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Trans/index.js","Control.Monad.Except.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Except.Trans/index.js","Control.Monad.List.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.List.Trans/index.js","Control.Monad.Maybe.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Maybe.Trans/index.js","Control.Monad.RWS.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.RWS.Trans/index.js","Control.Monad.Reader.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Trans/index.js","Control.Monad.State.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Trans/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Monad.Writer.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Trans/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.Par/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("Control.Monad.Aff.AVar");
var Data_Either = require("Data.Either");
var Data_Monoid = require("Data.Monoid");
var Control_Apply = require("Control.Apply");
var Control_Alt = require("Control.Alt");
var Control_Plus = require("Control.Plus");
var Control_Alternative = require("Control.Alternative");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Par = function (x) {
    return x;
};
var runPar = function (v) {
    return v;
};
var functorPar = new Prelude.Functor(function (f) {
    return function (v) {
        return Prelude["<$>"](Control_Monad_Aff.functorAff)(f)(v);
    };
});
var applyPar = new Prelude.Apply(function () {
    return functorPar;
}, function (v) {
    return function (v1) {
        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.makeVar)(function (v2) {
            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.makeVar)(function (v3) {
                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Prelude[">>="](Control_Monad_Aff.bindAff)(v)(Control_Monad_Aff_AVar.putVar(v2))))(function (v4) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Prelude[">>="](Control_Monad_Aff.bindAff)(v1)(Control_Monad_Aff_AVar.putVar(v3))))(function (v5) {
                        return Control_Monad_Aff.cancelWith(Prelude["<*>"](Control_Monad_Aff.applyAff)(Control_Monad_Aff_AVar.takeVar(v2))(Control_Monad_Aff_AVar.takeVar(v3)))(Prelude["<>"](Control_Monad_Aff.semigroupCanceler)(v4)(v5));
                    });
                });
            });
        });
    };
});
var semigroupPar = function (dictSemigroup) {
    return new Prelude.Semigroup(function (a) {
        return function (b) {
            return Prelude["<*>"](applyPar)(Prelude["<$>"](functorPar)(Prelude.append(dictSemigroup))(a))(b);
        };
    });
};
var applicativePar = new Prelude.Applicative(function () {
    return applyPar;
}, function (v) {
    return Prelude.pure(Control_Monad_Aff.applicativeAff)(v);
});
var monoidPar = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupPar(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Prelude.pure(applicativePar)(Data_Monoid.mempty(dictMonoid)));
};
var altPar = new Control_Alt.Alt(function () {
    return functorPar;
}, function (v) {
    return function (v1) {
        var maybeKill = function (va) {
            return function (ve) {
                return function (err) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(ve))(function (v2) {
                        return Prelude.bind(Control_Monad_Aff.bindAff)((function () {
                            var $29 = v2 === 1;
                            if ($29) {
                                return Control_Monad_Aff_AVar.killVar(va)(err);
                            };
                            if (!$29) {
                                return Prelude["return"](Control_Monad_Aff.applicativeAff)(Prelude.unit);
                            };
                            throw new Error("Failed pattern match at Control.Monad.Aff.Par line 51, column 11 - line 55, column 7: " + [ $29.constructor.name ]);
                        })())(function () {
                            return Control_Monad_Aff_AVar.putVar(ve)(v2 + 1 | 0);
                        });
                    });
                };
            };
        };
        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.makeVar)(function (v2) {
            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar["makeVar'"](0))(function (v3) {
                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Prelude[">>="](Control_Monad_Aff.bindAff)(Control_Monad_Aff.attempt(v))(Data_Either.either(maybeKill(v2)(v3))(Control_Monad_Aff_AVar.putVar(v2)))))(function (v4) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Prelude[">>="](Control_Monad_Aff.bindAff)(Control_Monad_Aff.attempt(v1))(Data_Either.either(maybeKill(v2)(v3))(Control_Monad_Aff_AVar.putVar(v2)))))(function (v5) {
                        return Control_Monad_Aff.cancelWith(Control_Monad_Aff_AVar.takeVar(v2))(Prelude["<>"](Control_Monad_Aff.semigroupCanceler)(v4)(v5));
                    });
                });
            });
        });
    };
});
var plusPar = new Control_Plus.Plus(function () {
    return altPar;
}, Control_Plus.empty(Control_Monad_Aff.plusAff));
var alternativePar = new Control_Alternative.Alternative(function () {
    return plusPar;
}, function () {
    return applicativePar;
});
module.exports = {
    Par: Par, 
    runPar: runPar, 
    semigroupPar: semigroupPar, 
    monoidPar: monoidPar, 
    functorPar: functorPar, 
    applyPar: applyPar, 
    applicativePar: applicativePar, 
    altPar: altPar, 
    plusPar: plusPar, 
    alternativePar: alternativePar
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Apply":"/Users/anders/dev/ps/halo/ajax/output/Control.Apply/index.js","Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Aff.AVar":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Aff

exports._cancelWith = function (nonCanceler, aff, canceler1) {
  return function(success, error) {
    var canceler2 = aff(success, error);

    return function(e) {
      return function(success, error) {
        var cancellations = 0;
        var result        = false;
        var errored       = false;

        var s = function(bool) {
          cancellations = cancellations + 1;
          result        = result || bool;

          if (cancellations === 2 && !errored) {
            try {
              success(result);
            } catch (e) {
              error(e);
            }
          }
        };

        var f = function(err) {
          if (!errored) {
            errored = true;

            error(err);
          }
        };

        canceler2(e)(s, f);
        canceler1(e)(s, f);

        return nonCanceler;
      };
    };
  };
}

exports._setTimeout = function (nonCanceler, millis, aff) {
  var set = setTimeout, clear = clearTimeout;
  if (millis <= 0 && typeof setImmediate === "function") {
    set = setImmediate;
    clear = clearImmediate;
  }
  return function(success, error) {
    var canceler;

    var timeout = set(function() {
      canceler = aff(success, error);
    }, millis);

    return function(e) {
      return function(s, f) {
        if (canceler !== undefined) {
          return canceler(e)(s, f);
        } else {
          clear(timeout);

          try {
            s(true);
          } catch (e) {
            f(e);
          }

          return nonCanceler;
        }
      };
    };
  };
}

exports._unsafeInterleaveAff = function (aff) {
  return aff;
}

exports._forkAff = function (nonCanceler, aff) {
  var voidF = function(){};

  return function(success, error) {
    var canceler = aff(voidF, voidF);

    try {
      success(canceler);
    } catch (e) {
      error(e);
    }

    return nonCanceler;
  };
}

exports._makeAff = function (cb) {
  return function(success, error) {
    return cb(function(e) {
      return function() {
        error(e);
      };
    })(function(v) {
      return function() {
        try {
          success(v);
        } catch (e) {
          error(e);
        }
      };
    })();
  }
}

exports._pure = function (nonCanceler, v) {
  return function(success, error) {
    try {
      success(v);
    } catch (e) {
      error(e);
    }

    return nonCanceler;
  };
}

exports._throwError = function (nonCanceler, e) {
  return function(success, error) {
    error(e);

    return nonCanceler;
  };
}

exports._fmap = function (f, aff) {
  return function(success, error) {
    return aff(function(v) {
      try {
        success(f(v));
      } catch (e) {
        error(e);
      }
    }, error);
  };
}

exports._bind = function (alwaysCanceler, aff, f) {
  return function(success, error) {
    var canceler1, canceler2;

    var isCanceled    = false;
    var requestCancel = false;

    var onCanceler = function(){};

    canceler1 = aff(function(v) {
      if (requestCancel) {
        isCanceled = true;

        return alwaysCanceler;
      } else {
        canceler2 = f(v)(success, error);

        onCanceler(canceler2);

        return canceler2;
      }
    }, error);

    return function(e) {
      return function(s, f) {
        requestCancel = true;

        if (canceler2 !== undefined) {
          return canceler2(e)(s, f);
        } else {
          return canceler1(e)(function(bool) {
            if (bool || isCanceled) {
              try {
                s(true);
              } catch (e) {
                f(e);
              }
            } else {
              onCanceler = function(canceler) {
                canceler(e)(s, f);
              };
            }
          }, f);
        }
      };
    };
  };
}

exports._attempt = function (Left, Right, aff) {
  return function(success, error) {
    return aff(function(v) {
      try {
        success(Right(v));
      } catch (e) {
        error(e);
      }
    }, function(e) {
      try {
        success(Left(e));
      } catch (e) {
        error(e);
      }
    });
  };
}

exports._runAff = function (errorT, successT, aff) {
  return function() {
    return aff(function(v) {
      try {
        successT(v)();
      } catch (e) {
        errorT(e)();
      }
    }, function(e) {
      errorT(e)();
    });
  };
}

exports._liftEff = function (nonCanceler, e) {
  return function(success, error) {
    try {
      success(e());
    } catch (e) {
      error(e);
    }

    return nonCanceler;
  };
}

},{}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Monad_Cont_Class = require("Control.Monad.Cont.Class");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("Control.Monad.Eff.Exception");
var Control_Monad_Eff_Unsafe = require("Control.Monad.Eff.Unsafe");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var Data_Either = require("Data.Either");
var Data_Function = require("Data.Function");
var Data_Monoid = require("Data.Monoid");
var Canceler = function (x) {
    return x;
};
var runAff = function (ex) {
    return function (f) {
        return function (aff) {
            return $foreign._runAff(ex, f, aff);
        };
    };
};
var makeAff$prime = function (h) {
    return $foreign._makeAff(h);
};
var launchAff = function ($19) {
    return runAff(Control_Monad_Eff_Exception.throwException)(Prelude["const"](Prelude.pure(Control_Monad_Eff.applicativeEff)(Prelude.unit)))($foreign._unsafeInterleaveAff($19));
};
var functorAff = new Prelude.Functor(function (f) {
    return function (fa) {
        return $foreign._fmap(f, fa);
    };
});
var cancel = function (v) {
    return v;
};
var attempt = function (aff) {
    return $foreign._attempt(Data_Either.Left.create, Data_Either.Right.create, aff);
};
var apathize = function (a) {
    return Prelude["<$>"](functorAff)(Prelude["const"](Prelude.unit))(attempt(a));
};
var applyAff = new Prelude.Apply(function () {
    return functorAff;
}, function (ff) {
    return function (fa) {
        return $foreign._bind(alwaysCanceler, ff, function (f) {
            return Prelude["<$>"](functorAff)(f)(fa);
        });
    };
});
var applicativeAff = new Prelude.Applicative(function () {
    return applyAff;
}, function (v) {
    return $foreign._pure(nonCanceler, v);
});
var nonCanceler = Prelude["const"](Prelude.pure(applicativeAff)(false));
var alwaysCanceler = Prelude["const"](Prelude.pure(applicativeAff)(true));
var cancelWith = function (aff) {
    return function (c) {
        return $foreign._cancelWith(nonCanceler, aff, c);
    };
};
var forkAff = function (aff) {
    return $foreign._forkAff(nonCanceler, aff);
};
var later$prime = function (n) {
    return function (aff) {
        return $foreign._setTimeout(nonCanceler, n, aff);
    };
};
var later = later$prime(0);
var liftEff$prime = function (eff) {
    return attempt($foreign._unsafeInterleaveAff($foreign._liftEff(nonCanceler, eff)));
};
var makeAff = function (h) {
    return makeAff$prime(function (e) {
        return function (a) {
            return Prelude["<$>"](Control_Monad_Eff.functorEff)(Prelude["const"](nonCanceler))(h(e)(a));
        };
    });
};
var semigroupAff = function (dictSemigroup) {
    return new Prelude.Semigroup(function (a) {
        return function (b) {
            return Prelude["<*>"](applyAff)(Prelude["<$>"](functorAff)(Prelude["<>"](dictSemigroup))(a))(b);
        };
    });
};
var monoidAff = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupAff(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Prelude.pure(applicativeAff)(Data_Monoid.mempty(dictMonoid)));
};
var semigroupCanceler = new Prelude.Semigroup(function (v) {
    return function (v1) {
        return function (e) {
            return Prelude["<*>"](applyAff)(Prelude["<$>"](functorAff)(Prelude["||"](Prelude.booleanAlgebraBoolean))(v(e)))(v1(e));
        };
    };
});
var monoidCanceler = new Data_Monoid.Monoid(function () {
    return semigroupCanceler;
}, Prelude["const"](Prelude.pure(applicativeAff)(true)));
var bindAff = new Prelude.Bind(function () {
    return applyAff;
}, function (fa) {
    return function (f) {
        return $foreign._bind(alwaysCanceler, fa, f);
    };
});
var monadAff = new Prelude.Monad(function () {
    return applicativeAff;
}, function () {
    return bindAff;
});
var monadContAff = new Control_Monad_Cont_Class.MonadCont(function () {
    return monadAff;
}, function (f) {
    return makeAff(function (eb) {
        return function (cb) {
            return runAff(eb)(cb)(f(function (a) {
                return makeAff(function (v) {
                    return function (v1) {
                        return cb(a);
                    };
                });
            }));
        };
    });
});
var monadEffAff = new Control_Monad_Eff_Class.MonadEff(function () {
    return monadAff;
}, function (eff) {
    return $foreign._liftEff(nonCanceler, eff);
});
var monadErrorAff = new Control_Monad_Error_Class.MonadError(function () {
    return monadAff;
}, function (aff) {
    return function (ex) {
        return Prelude[">>="](bindAff)(attempt(aff))(Data_Either.either(ex)(Prelude.pure(applicativeAff)));
    };
}, function (e) {
    return $foreign._throwError(nonCanceler, e);
});
var $$finally = function (aff1) {
    return function (aff2) {
        return Prelude.bind(bindAff)(attempt(aff1))(function (v) {
            return Prelude.bind(bindAff)(aff2)(function () {
                return Data_Either.either(Control_Monad_Error_Class.throwError(monadErrorAff))(Prelude.pure(applicativeAff))(v);
            });
        });
    };
};
var monadRecAff = new Control_Monad_Rec_Class.MonadRec(function () {
    return monadAff;
}, function (f) {
    return function (a) {
        var go = function (size) {
            return function (f1) {
                return function (a1) {
                    return Prelude.bind(bindAff)(f1(a1))(function (v) {
                        if (v instanceof Data_Either.Left) {
                            if (size < 100) {
                                return go(size + 1 | 0)(f1)(v.value0);
                            };
                            if (Prelude.otherwise) {
                                return later(Control_Monad_Rec_Class.tailRecM(monadRecAff)(f1)(v.value0));
                            };
                        };
                        if (v instanceof Data_Either.Right) {
                            return Prelude.pure(applicativeAff)(v.value0);
                        };
                        throw new Error("Failed pattern match: " + [ v.constructor.name ]);
                    });
                };
            };
        };
        return go(0)(f)(a);
    };
});
var altAff = new Control_Alt.Alt(function () {
    return functorAff;
}, function (a1) {
    return function (a2) {
        return Prelude[">>="](bindAff)(attempt(a1))(Data_Either.either(Prelude["const"](a2))(Prelude.pure(applicativeAff)));
    };
});
var plusAff = new Control_Plus.Plus(function () {
    return altAff;
}, Control_Monad_Error_Class.throwError(monadErrorAff)(Control_Monad_Eff_Exception.error("Always fails")));
var alternativeAff = new Control_Alternative.Alternative(function () {
    return plusAff;
}, function () {
    return applicativeAff;
});
var monadPlusAff = new Control_MonadPlus.MonadPlus(function () {
    return alternativeAff;
}, function () {
    return monadAff;
});
module.exports = {
    Canceler: Canceler, 
    runAff: runAff, 
    nonCanceler: nonCanceler, 
    "makeAff'": makeAff$prime, 
    makeAff: makeAff, 
    "liftEff'": liftEff$prime, 
    launchAff: launchAff, 
    "later'": later$prime, 
    later: later, 
    forkAff: forkAff, 
    "finally": $$finally, 
    cancelWith: cancelWith, 
    cancel: cancel, 
    attempt: attempt, 
    apathize: apathize, 
    semigroupAff: semigroupAff, 
    monoidAff: monoidAff, 
    functorAff: functorAff, 
    applyAff: applyAff, 
    applicativeAff: applicativeAff, 
    bindAff: bindAff, 
    monadAff: monadAff, 
    monadEffAff: monadEffAff, 
    monadErrorAff: monadErrorAff, 
    altAff: altAff, 
    plusAff: plusAff, 
    alternativeAff: alternativeAff, 
    monadPlusAff: monadPlusAff, 
    monadRecAff: monadRecAff, 
    monadContAff: monadContAff, 
    semigroupCanceler: semigroupCanceler, 
    monoidCanceler: monoidCanceler
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/foreign.js","Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Monad.Cont.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Class/index.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Eff.Exception":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/index.js","Control.Monad.Eff.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Unsafe/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var MonadCont = function (__superclass_Prelude$dotMonad_0, callCC) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.callCC = callCC;
};
var callCC = function (dict) {
    return dict.callCC;
};
module.exports = {
    MonadCont: MonadCont, 
    callCC: callCC
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Cont_Class = require("Control.Monad.Cont.Class");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var ContT = function (x) {
    return x;
};
var runContT = function (v) {
    return function (k) {
        return v(k);
    };
};
var withContT = function (f) {
    return function (m) {
        return function (k) {
            return runContT(m)(f(k));
        };
    };
};
var monadTransContT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return function (k) {
            return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(m)(k);
        };
    };
});
var mapContT = function (f) {
    return function (m) {
        return function (k) {
            return f(runContT(m)(k));
        };
    };
};
var functorContT = function (dictMonad) {
    return new Prelude.Functor(function (f) {
        return function (m) {
            return function (k) {
                return runContT(m)(function (a) {
                    return k(f(a));
                });
            };
        };
    });
};
var applyContT = function (dictMonad) {
    return new Prelude.Apply(function () {
        return functorContT(dictMonad);
    }, function (f) {
        return function (v) {
            return function (k) {
                return runContT(f)(function (g) {
                    return runContT(v)(function (a) {
                        return k(g(a));
                    });
                });
            };
        };
    });
};
var bindContT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyContT(dictMonad);
    }, function (m) {
        return function (k) {
            return function (kʹ) {
                return runContT(m)(function (a) {
                    return runContT(k(a))(kʹ);
                });
            };
        };
    });
};
var applicativeContT = function (dictMonad) {
    return new Prelude.Applicative(function () {
        return applyContT(dictMonad);
    }, function (a) {
        return function (k) {
            return k(a);
        };
    });
};
var monadContT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeContT(dictMonad);
    }, function () {
        return bindContT(dictMonad);
    });
};
var monadContContT = function (dictMonad) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadContT(dictMonad);
    }, function (f) {
        return function (k) {
            return runContT(f(function (a) {
                return function (v) {
                    return k(a);
                };
            }))(k);
        };
    });
};
var monadEffContT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadContT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($17) {
        return Control_Monad_Trans.lift(monadTransContT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($17));
    });
};
var monadReaderContT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadContT(dictMonadReader["__superclass_Prelude.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransContT)(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return function (c) {
            return function (k) {
                return Prelude.bind((dictMonadReader["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(Control_Monad_Reader_Class.ask(dictMonadReader))(function (v) {
                    return Control_Monad_Reader_Class.local(dictMonadReader)(f)(runContT(c)(function ($18) {
                        return Control_Monad_Reader_Class.local(dictMonadReader)(Prelude["const"](v))(k($18));
                    }));
                });
            };
        };
    });
};
var monadStateContT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadContT(dictMonadState["__superclass_Prelude.Monad_0"]());
    }, function ($19) {
        return Control_Monad_Trans.lift(monadTransContT)(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)($19));
    });
};
module.exports = {
    ContT: ContT, 
    withContT: withContT, 
    mapContT: mapContT, 
    runContT: runContT, 
    monadContContT: monadContContT, 
    functorContT: functorContT, 
    applyContT: applyContT, 
    applicativeContT: applicativeContT, 
    bindContT: bindContT, 
    monadContT: monadContT, 
    monadTransContT: monadTransContT, 
    monadEffContT: monadEffContT, 
    monadReaderContT: monadReaderContT, 
    monadStateContT: monadStateContT
};

},{"Control.Monad.Cont.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Class/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Reader.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var MonadEff = function (__superclass_Prelude$dotMonad_0, liftEff) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.liftEff = liftEff;
};
var monadEffEff = new MonadEff(function () {
    return Control_Monad_Eff.monadEff;
}, Prelude.id(Prelude.categoryFn));
var liftEff = function (dict) {
    return dict.liftEff;
};
module.exports = {
    MonadEff: MonadEff, 
    liftEff: liftEff, 
    monadEffEff: monadEffEff
};

},{"Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff.Exception

exports.showErrorImpl = function (err) {
  return err.stack || err.toString();
};

exports.error = function (msg) {
  return new Error(msg);
};

exports.message = function (e) {
  return e.message;
};

exports.throwException = function (e) {
  return function () {
    throw e;
  };
};

exports.catchException = function (c) {
  return function (t) {
    return function () {
      try {
        return t();
      } catch (e) {
        if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
          return c(e)();
        } else {
          return c(new Error(e.toString()))();
        }
      }
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var showError = new Prelude.Show($foreign.showErrorImpl);
module.exports = {
    showError: showError, 
    catchException: $foreign.catchException, 
    throwException: $foreign.throwException, 
    message: $foreign.message, 
    error: $foreign.error
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Ref/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff.Ref

exports.newRef = function (val) {
  return function () {
    return { value: val };
  };
};

exports.readRef = function (ref) {
  return function () {
    return ref.value;
  };
};

exports["modifyRef'"] = function (ref) {
  return function (f) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

exports.writeRef = function (ref) {
  return function (val) {
    return function () {
      ref.value = val;
      return {};
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Ref/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var modifyRef = function (ref) {
    return function (f) {
        return $foreign["modifyRef'"](ref)(function (s) {
            return {
                state: f(s), 
                value: Prelude.unit
            };
        });
    };
};
module.exports = {
    modifyRef: modifyRef, 
    writeRef: $foreign.writeRef, 
    "modifyRef'": $foreign["modifyRef'"], 
    readRef: $foreign.readRef, 
    newRef: $foreign.newRef
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Ref/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Unsafe/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff.Unsafe

exports.unsafeInterleaveEff = function (f) {
  return f;
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Unsafe/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
module.exports = {
    unsafeInterleaveEff: $foreign.unsafeInterleaveEff
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Unsafe/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.Eff

exports.returnE = function (a) {
  return function () {
    return a;
  };
};

exports.bindE = function (a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.runPure = function (f) {
  return f();
};

exports.untilE = function (f) {
  return function () {
    while (!f());
    return {};
  };
};

exports.whileE = function (f) {
  return function (a) {
    return function () {
      while (f()) {
        a();
      }
      return {};
    };
  };
};

exports.forE = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};

exports.foreachE = function (as) {
  return function (f) {
    return function () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var monadEff = new Prelude.Monad(function () {
    return applicativeEff;
}, function () {
    return bindEff;
});
var bindEff = new Prelude.Bind(function () {
    return applyEff;
}, $foreign.bindE);
var applyEff = new Prelude.Apply(function () {
    return functorEff;
}, Prelude.ap(monadEff));
var applicativeEff = new Prelude.Applicative(function () {
    return applyEff;
}, $foreign.returnE);
var functorEff = new Prelude.Functor(Prelude.liftA1(applicativeEff));
module.exports = {
    functorEff: functorEff, 
    applyEff: applyEff, 
    applicativeEff: applicativeEff, 
    bindEff: bindEff, 
    monadEff: monadEff, 
    foreachE: $foreign.foreachE, 
    forE: $foreign.forE, 
    whileE: $foreign.whileE, 
    untilE: $foreign.untilE, 
    runPure: $foreign.runPure
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/foreign.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Either = require("Data.Either");
var MonadError = function (__superclass_Prelude$dotMonad_0, catchError, throwError) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.catchError = catchError;
    this.throwError = throwError;
};
var throwError = function (dict) {
    return dict.throwError;
};
var monadErrorMaybe = new MonadError(function () {
    return Data_Maybe.monadMaybe;
}, function (v) {
    return function (f) {
        if (v instanceof Data_Maybe.Nothing) {
            return f(Prelude.unit);
        };
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class line 51, column 1 - line 54, column 32: " + [ v.constructor.name, f.constructor.name ]);
    };
}, Prelude["const"](Data_Maybe.Nothing.value));
var monadErrorEither = new MonadError(function () {
    return Data_Either.monadEither;
}, function (v) {
    return function (h) {
        if (v instanceof Data_Either.Left) {
            return h(v.value0);
        };
        if (v instanceof Data_Either.Right) {
            return new Data_Either.Right(v.value0);
        };
        throw new Error("Failed pattern match at Control.Monad.Error.Class line 46, column 1 - line 51, column 1: " + [ v.constructor.name, h.constructor.name ]);
    };
}, Data_Either.Left.create);
var catchError = function (dict) {
    return dict.catchError;
};
var catchJust = function (dictMonadError) {
    return function (p) {
        return function (act) {
            return function (handler) {
                var handle = function (e) {
                    var $10 = p(e);
                    if ($10 instanceof Data_Maybe.Nothing) {
                        return throwError(dictMonadError)(e);
                    };
                    if ($10 instanceof Data_Maybe.Just) {
                        return handler($10.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Error.Class line 41, column 3 - line 46, column 1: " + [ $10.constructor.name ]);
                };
                return catchError(dictMonadError)(act)(handle);
            };
        };
    };
};
module.exports = {
    MonadError: MonadError, 
    catchJust: catchJust, 
    catchError: catchError, 
    throwError: throwError, 
    monadErrorEither: monadErrorEither, 
    monadErrorMaybe: monadErrorMaybe
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Except.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Tuple = require("Data.Tuple");
var Data_Either = require("Data.Either");
var Data_Monoid = require("Data.Monoid");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Control_Monad_Cont_Class = require("Control.Monad.Cont.Class");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var Control_Monad_RWS_Class = require("Control.Monad.RWS.Class");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var ExceptT = function (x) {
    return x;
};
var runExceptT = function (v) {
    return v;
};
var withExceptT = function (dictFunctor) {
    return function (f) {
        var mapLeft = function (f1) {
            return function (v) {
                if (v instanceof Data_Either.Right) {
                    return new Data_Either.Right(v.value0);
                };
                if (v instanceof Data_Either.Left) {
                    return new Data_Either.Left(f1(v.value0));
                };
                throw new Error("Failed pattern match at Control.Monad.Except.Trans line 43, column 3 - line 44, column 3: " + [ f1.constructor.name, v.constructor.name ]);
            };
        };
        return function ($66) {
            return ExceptT(Prelude["<$>"](dictFunctor)(mapLeft(f))(runExceptT($66)));
        };
    };
};
var monadTransExceptT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return ExceptT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
            return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(v));
        }));
    };
});
var mapExceptT = function (f) {
    return function (m) {
        return f(runExceptT(m));
    };
};
var functorExceptT = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return mapExceptT(Prelude["<$>"](dictFunctor)(Prelude["<$>"](Data_Either.functorEither)(f)));
    });
};
var applyExceptT = function (dictApply) {
    return new Prelude.Apply(function () {
        return functorExceptT(dictApply["__superclass_Prelude.Functor_0"]());
    }, function (v) {
        return function (v1) {
            var fʹ = Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Prelude["<*>"](Data_Either.applyEither))(v);
            var xʹ = Prelude["<*>"](dictApply)(fʹ)(v1);
            return xʹ;
        };
    });
};
var bindExceptT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyExceptT((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]());
    }, function (m) {
        return function (k) {
            return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runExceptT(m))(Data_Either.either(function ($67) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Either.Left.create($67));
            })(function ($68) {
                return runExceptT(k($68));
            }));
        };
    });
};
var applicativeExceptT = function (dictApplicative) {
    return new Prelude.Applicative(function () {
        return applyExceptT(dictApplicative["__superclass_Prelude.Apply_0"]());
    }, function ($69) {
        return ExceptT(Prelude.pure(dictApplicative)(Data_Either.Right.create($69)));
    });
};
var monadExceptT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeExceptT(dictMonad["__superclass_Prelude.Applicative_0"]());
    }, function () {
        return bindExceptT(dictMonad);
    });
};
var monadContExceptT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadExceptT(dictMonadCont["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return ExceptT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
            return runExceptT(f(function (a) {
                return ExceptT(c(new Data_Either.Right(a)));
            }));
        }));
    });
};
var monadEffExceptT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadExceptT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($70) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($70));
    });
};
var monadErrorExceptT = function (dictMonad) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadExceptT(dictMonad);
    }, function (m) {
        return function (handler) {
            return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runExceptT(m))(Data_Either.either(function ($71) {
                return runExceptT(handler($71));
            })(function ($72) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Either.Right.create($72));
            }));
        };
    }, function ($73) {
        return ExceptT(Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Either.Left.create($73)));
    });
};
var monadReaderExceptT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadExceptT(dictMonadReader["__superclass_Prelude.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransExceptT)(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapExceptT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecExceptT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadExceptT(dictMonadRec["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return function ($74) {
            return ExceptT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runExceptT(f(a)))(function (v) {
                    return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                        if (v instanceof Data_Either.Left) {
                            return new Data_Either.Right(new Data_Either.Left(v.value0));
                        };
                        if (v instanceof Data_Either.Right && v.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(v.value0.value0);
                        };
                        if (v instanceof Data_Either.Right && v.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Either.Right(v.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Except.Trans line 68, column 1 - line 76, column 1: " + [ v.constructor.name ]);
                    })());
                });
            })($74));
        };
    });
};
var monadStateExceptT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadExceptT(dictMonadState["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadWriterExceptT = function (dictMonadWriter) {
    return new Control_Monad_Writer_Class.MonadWriter(function () {
        return monadExceptT(dictMonadWriter["__superclass_Prelude.Monad_0"]());
    }, mapExceptT(function (m) {
        return Prelude.bind((dictMonadWriter["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
            return Prelude["return"]((dictMonadWriter["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(Prelude["<$>"](Data_Either.functorEither)(function (r) {
                return new Data_Tuple.Tuple(r, v.value1);
            })(v.value0));
        });
    }), mapExceptT(function (m) {
        return Control_Monad_Writer_Class.pass(dictMonadWriter)(Prelude.bind((dictMonadWriter["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(m)(function (v) {
            return Prelude["return"]((dictMonadWriter["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                if (v instanceof Data_Either.Left) {
                    return new Data_Tuple.Tuple(new Data_Either.Left(v.value0), Prelude.id(Prelude.categoryFn));
                };
                if (v instanceof Data_Either.Right) {
                    return new Data_Tuple.Tuple(new Data_Either.Right(v.value0.value0), v.value0.value1);
                };
                throw new Error("Failed pattern match at Control.Monad.Except.Trans line 116, column 1 - line 127, column 1: " + [ v.constructor.name ]);
            })());
        }));
    }), function (wd) {
        return Control_Monad_Trans.lift(monadTransExceptT)(dictMonadWriter["__superclass_Prelude.Monad_0"]())(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
    });
};
var monadRWSExceptT = function (dictMonoid) {
    return function (dictMonadRWS) {
        return new Control_Monad_RWS_Class.MonadRWS(function () {
            return monadReaderExceptT(dictMonadRWS["__superclass_Control.Monad.Reader.Class.MonadReader_1"]());
        }, function () {
            return monadStateExceptT(dictMonadRWS["__superclass_Control.Monad.State.Class.MonadState_3"]());
        }, function () {
            return monadWriterExceptT(dictMonadRWS["__superclass_Control.Monad.Writer.Class.MonadWriter_2"]());
        }, function () {
            return dictMonoid;
        });
    };
};
var altExceptT = function (dictSemigroup) {
    return function (dictMonad) {
        return new Control_Alt.Alt(function () {
            return functorExceptT(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
        }, function (m) {
            return function (n) {
                return ExceptT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runExceptT(m))(function (v) {
                    if (v instanceof Data_Either.Right) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(v.value0));
                    };
                    if (v instanceof Data_Either.Left) {
                        return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runExceptT(n))(function (v1) {
                            if (v1 instanceof Data_Either.Right) {
                                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(v1.value0));
                            };
                            if (v1 instanceof Data_Either.Left) {
                                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(Prelude["<>"](dictSemigroup)(v.value0)(v1.value0)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Except.Trans line 76, column 1 - line 87, column 1: " + [ v1.constructor.name ]);
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.Except.Trans line 76, column 1 - line 87, column 1: " + [ v.constructor.name ]);
                }));
            };
        });
    };
};
var plusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Plus.Plus(function () {
            return altExceptT(dictMonoid["__superclass_Prelude.Semigroup_0"]())(dictMonad);
        }, Control_Monad_Error_Class.throwError(monadErrorExceptT(dictMonad))(Data_Monoid.mempty(dictMonoid)));
    };
};
var alternativeExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Alternative.Alternative(function () {
            return plusExceptT(dictMonoid)(dictMonad);
        }, function () {
            return applicativeExceptT(dictMonad["__superclass_Prelude.Applicative_0"]());
        });
    };
};
var monadPlusExceptT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_MonadPlus.MonadPlus(function () {
            return alternativeExceptT(dictMonoid)(dictMonad);
        }, function () {
            return monadExceptT(dictMonad);
        });
    };
};
module.exports = {
    ExceptT: ExceptT, 
    mapExceptT: mapExceptT, 
    withExceptT: withExceptT, 
    runExceptT: runExceptT, 
    functorExceptT: functorExceptT, 
    applyExceptT: applyExceptT, 
    applicativeExceptT: applicativeExceptT, 
    bindExceptT: bindExceptT, 
    monadExceptT: monadExceptT, 
    monadRecExceptT: monadRecExceptT, 
    altExceptT: altExceptT, 
    plusExceptT: plusExceptT, 
    alternativeExceptT: alternativeExceptT, 
    monadPlusExceptT: monadPlusExceptT, 
    monadTransExceptT: monadTransExceptT, 
    monadEffExceptT: monadEffExceptT, 
    monadContExceptT: monadContExceptT, 
    monadErrorExceptT: monadErrorExceptT, 
    monadReaderExceptT: monadReaderExceptT, 
    monadStateExceptT: monadStateExceptT, 
    monadWriterExceptT: monadWriterExceptT, 
    monadRWSExceptT: monadRWSExceptT
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Monad.Cont.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Class/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","Control.Monad.RWS.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.RWS.Class/index.js","Control.Monad.Reader.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Tuple = require("Data.Tuple");
var Data_Exists = require("Data.Exists");
var Data_Either = require("Data.Either");
var Data_Bifunctor = require("Data.Bifunctor");
var Control_Bind = require("Control.Bind");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Bound = (function () {
    function Bound(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Bound.create = function (value0) {
        return function (value1) {
            return new Bound(value0, value1);
        };
    };
    return Bound;
})();
var FreeT = (function () {
    function FreeT(value0) {
        this.value0 = value0;
    };
    FreeT.create = function (value0) {
        return new FreeT(value0);
    };
    return FreeT;
})();
var Bind = (function () {
    function Bind(value0) {
        this.value0 = value0;
    };
    Bind.create = function (value0) {
        return new Bind(value0);
    };
    return Bind;
})();
var monadTransFreeT = function (dictFunctor) {
    return new Control_Monad_Trans.MonadTrans(function (dictMonad) {
        return function (ma) {
            return new FreeT(function (v) {
                return Prelude.map(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Left.create)(ma);
            });
        };
    });
};
var freeT = FreeT.create;
var bound = function (m) {
    return function (f) {
        return new Bind(Data_Exists.mkExists(new Bound(m, f)));
    };
};
var functorFreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return new Prelude.Functor(function (f) {
            return function (v) {
                if (v instanceof FreeT) {
                    return new FreeT(function (v1) {
                        return Prelude.map(dictFunctor1)(Data_Bifunctor.bimap(Data_Either.bifunctorEither)(f)(Prelude.map(dictFunctor)(Prelude.map(functorFreeT(dictFunctor)(dictFunctor1))(f))))(v.value0(Prelude.unit));
                    });
                };
                if (v instanceof Bind) {
                    return Data_Exists.runExists(function (v1) {
                        return bound(v1.value0)(function ($98) {
                            return Prelude.map(functorFreeT(dictFunctor)(dictFunctor1))(f)(v1.value1($98));
                        });
                    })(v.value0);
                };
                throw new Error("Failed pattern match: " + [ f.constructor.name, v.constructor.name ]);
            };
        });
    };
};
var bimapFreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (nf) {
            return function (nm) {
                return function (v) {
                    if (v instanceof Bind) {
                        return Data_Exists.runExists(function (v1) {
                            return bound(function ($99) {
                                return bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm)(v1.value0($99));
                            })(function ($100) {
                                return bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm)(v1.value1($100));
                            });
                        })(v.value0);
                    };
                    if (v instanceof FreeT) {
                        return new FreeT(function (v1) {
                            return Prelude["<$>"](dictFunctor1)(Prelude.map(Data_Either.functorEither)(function ($101) {
                                return nf(Prelude.map(dictFunctor)(bimapFreeT(dictFunctor)(dictFunctor1)(nf)(nm))($101));
                            }))(nm(v.value0(Prelude.unit)));
                        });
                    };
                    throw new Error("Failed pattern match: " + [ nf.constructor.name, nm.constructor.name, v.constructor.name ]);
                };
            };
        };
    };
};
var hoistFreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return bimapFreeT(dictFunctor)(dictFunctor1)(Prelude.id(Prelude.categoryFn));
    };
};
var interpret = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (nf) {
            return bimapFreeT(dictFunctor)(dictFunctor1)(nf)(Prelude.id(Prelude.categoryFn));
        };
    };
};
var monadFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Prelude.Monad(function () {
            return applicativeFreeT(dictFunctor)(dictMonad);
        }, function () {
            return bindFreeT(dictFunctor)(dictMonad);
        });
    };
};
var bindFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Prelude.Bind(function () {
            return applyFreeT(dictFunctor)(dictMonad);
        }, function (v) {
            return function (f) {
                if (v instanceof Bind) {
                    return Data_Exists.runExists(function (v1) {
                        return bound(v1.value0)(function (x) {
                            return bound(function (v2) {
                                return v1.value1(x);
                            })(f);
                        });
                    })(v.value0);
                };
                return bound(function (v1) {
                    return v;
                })(f);
            };
        });
    };
};
var applyFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Prelude.Apply(function () {
            return functorFreeT(dictFunctor)(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
        }, Prelude.ap(monadFreeT(dictFunctor)(dictMonad)));
    };
};
var applicativeFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Prelude.Applicative(function () {
            return applyFreeT(dictFunctor)(dictMonad);
        }, function (a) {
            return new FreeT(function (v) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(a));
            });
        });
    };
};
var liftFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return function (fa) {
            return new FreeT(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(Prelude.map(dictFunctor)(Prelude.pure(applicativeFreeT(dictFunctor)(dictMonad)))(fa)));
            });
        };
    };
};
var resume = function (dictFunctor) {
    return function (dictMonadRec) {
        var go = function (v) {
            if (v instanceof FreeT) {
                return Prelude.map((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Right.create)(v.value0(Prelude.unit));
            };
            if (v instanceof Bind) {
                return Data_Exists.runExists(function (v1) {
                    var $77 = v1.value0(Prelude.unit);
                    if ($77 instanceof FreeT) {
                        return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())($77.value0(Prelude.unit))(function (v2) {
                            if (v2 instanceof Data_Either.Left) {
                                return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(v1.value1(v2.value0)));
                            };
                            if (v2 instanceof Data_Either.Right) {
                                return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(new Data_Either.Right(Prelude.map(dictFunctor)(function (h) {
                                    return Prelude[">>="](bindFreeT(dictFunctor)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(h)(v1.value1);
                                })(v2.value0))));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Free.Trans line 45, column 3 - line 46, column 3: " + [ v2.constructor.name ]);
                        });
                    };
                    if ($77 instanceof Bind) {
                        return Data_Exists.runExists(function (v2) {
                            return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(Prelude.bind(bindFreeT(dictFunctor)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(v2.value0(Prelude.unit))(function (z) {
                                return Prelude[">>="](bindFreeT(dictFunctor)(dictMonadRec["__superclass_Prelude.Monad_0"]()))(v2.value1(z))(v1.value1);
                            })));
                        })($77.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Free.Trans line 45, column 3 - line 46, column 3: " + [ $77.constructor.name ]);
                })(v.value0);
            };
            throw new Error("Failed pattern match at Control.Monad.Free.Trans line 45, column 3 - line 46, column 3: " + [ v.constructor.name ]);
        };
        return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
    };
};
var runFreeT = function (dictFunctor) {
    return function (dictMonadRec) {
        return function (interp) {
            var go = function (v) {
                if (v instanceof Data_Either.Left) {
                    return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Right(v.value0));
                };
                if (v instanceof Data_Either.Right) {
                    return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(interp(v.value0))(function (v1) {
                        return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(new Data_Either.Left(v1));
                    });
                };
                throw new Error("Failed pattern match at Control.Monad.Free.Trans line 105, column 3 - line 106, column 3: " + [ v.constructor.name ]);
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(Control_Bind["<=<"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(go)(resume(dictFunctor)(dictMonadRec)));
        };
    };
};
var monadRecFreeT = function (dictFunctor) {
    return function (dictMonad) {
        return new Control_Monad_Rec_Class.MonadRec(function () {
            return monadFreeT(dictFunctor)(dictMonad);
        }, function (f) {
            var go = function (s) {
                return Prelude.bind(bindFreeT(dictFunctor)(dictMonad))(f(s))(function (v) {
                    if (v instanceof Data_Either.Left) {
                        return go(v.value0);
                    };
                    if (v instanceof Data_Either.Right) {
                        return Prelude["return"](applicativeFreeT(dictFunctor)(dictMonad))(v.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Free.Trans line 75, column 1 - line 85, column 1: " + [ v.constructor.name ]);
                });
            };
            return go;
        });
    };
};
module.exports = {
    runFreeT: runFreeT, 
    resume: resume, 
    bimapFreeT: bimapFreeT, 
    interpret: interpret, 
    hoistFreeT: hoistFreeT, 
    liftFreeT: liftFreeT, 
    freeT: freeT, 
    functorFreeT: functorFreeT, 
    applyFreeT: applyFreeT, 
    applicativeFreeT: applicativeFreeT, 
    bindFreeT: bindFreeT, 
    monadFreeT: monadFreeT, 
    monadTransFreeT: monadTransFreeT, 
    monadRecFreeT: monadRecFreeT
};

},{"Control.Bind":"/Users/anders/dev/ps/halo/ajax/output/Control.Bind/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Exists":"/Users/anders/dev/ps/halo/ajax/output/Data.Exists/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Data_CatList = require("Data.CatList");
var Data_Either = require("Data.Either");
var Data_Identity = require("Data.Identity");
var Data_Inject = require("Data.Inject");
var Data_Maybe = require("Data.Maybe");
var Data_NaturalTransformation = require("Data.NaturalTransformation");
var Data_Tuple = require("Data.Tuple");
var Unsafe_Coerce = require("Unsafe.Coerce");
var ExpF = function (x) {
    return x;
};
var Free = (function () {
    function Free(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Free.create = function (value0) {
        return function (value1) {
            return new Free(value0, value1);
        };
    };
    return Free;
})();
var Return = (function () {
    function Return(value0) {
        this.value0 = value0;
    };
    Return.create = function (value0) {
        return new Return(value0);
    };
    return Return;
})();
var Bind = (function () {
    function Bind(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Bind.create = function (value0) {
        return function (value1) {
            return new Bind(value0, value1);
        };
    };
    return Bind;
})();
var toView = function (__copy_v) {
    var v = __copy_v;
    tco: while (true) {
        var runExpF = function (v2) {
            return v2;
        };
        var concatF = function (v2) {
            return function (r) {
                return new Free(v2.value0, Prelude["<>"](Data_CatList.semigroupCatList)(v2.value1)(r));
            };
        };
        if (v.value0 instanceof Return) {
            var $19 = Data_CatList.uncons(v.value1);
            if ($19 instanceof Data_Maybe.Nothing) {
                return new Return(Unsafe_Coerce.unsafeCoerce(v.value0.value0));
            };
            if ($19 instanceof Data_Maybe.Just) {
                var __tco_v = Unsafe_Coerce.unsafeCoerce(concatF(runExpF($19.value0.value0)(v.value0.value0))($19.value0.value1));
                v = __tco_v;
                continue tco;
            };
            throw new Error("Failed pattern match: " + [ $19.constructor.name ]);
        };
        if (v.value0 instanceof Bind) {
            return new Bind(v.value0.value0, function (a) {
                return Unsafe_Coerce.unsafeCoerce(concatF(v.value0.value1(a))(v.value1));
            });
        };
        throw new Error("Failed pattern match: " + [ v.value0.constructor.name ]);
    };
};
var runFreeM = function (dictFunctor) {
    return function (dictMonadRec) {
        return function (k) {
            var go = function (f) {
                var $28 = toView(f);
                if ($28 instanceof Return) {
                    return Prelude["<$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Right.create)(Prelude.pure((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())($28.value0));
                };
                if ($28 instanceof Bind) {
                    return Prelude["<$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Left.create)(k(Prelude["<$>"](dictFunctor)($28.value1)($28.value0)));
                };
                throw new Error("Failed pattern match at Control.Monad.Free line 123, column 3 - line 124, column 3: " + [ $28.constructor.name ]);
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
        };
    };
};
var runFree = function (dictFunctor) {
    return function (k) {
        return function ($40) {
            return Data_Identity.runIdentity(runFreeM(dictFunctor)(Control_Monad_Rec_Class.monadRecIdentity)(function ($41) {
                return Data_Identity.Identity(k($41));
            })($40));
        };
    };
};
var fromView = function (f) {
    return new Free(Unsafe_Coerce.unsafeCoerce(f), Data_CatList.empty);
};
var suspendF = function (dictApplicative) {
    return function (f) {
        return fromView(new Bind(Unsafe_Coerce.unsafeCoerce(Prelude.pure(dictApplicative)(f)), function ($42) {
            return Prelude.id(Prelude.categoryFn)(Unsafe_Coerce.unsafeCoerce($42));
        }));
    };
};
var freeMonad = new Prelude.Monad(function () {
    return freeApplicative;
}, function () {
    return freeBind;
});
var freeFunctor = new Prelude.Functor(function (k) {
    return function (f) {
        return Prelude[">>="](freeBind)(f)(function ($43) {
            return Prelude["return"](freeApplicative)(k($43));
        });
    };
});
var freeBind = new Prelude.Bind(function () {
    return freeApply;
}, function (v) {
    return function (k) {
        return new Free(v.value0, Data_CatList.snoc(v.value1)(Unsafe_Coerce.unsafeCoerce(k)));
    };
});
var freeApply = new Prelude.Apply(function () {
    return freeFunctor;
}, Prelude.ap(freeMonad));
var freeApplicative = new Prelude.Applicative(function () {
    return freeApply;
}, function ($44) {
    return fromView(Return.create($44));
});
var freeMonadRec = new Control_Monad_Rec_Class.MonadRec(function () {
    return freeMonad;
}, function (k) {
    return function (a) {
        return Prelude[">>="](freeBind)(k(a))(Data_Either.either(Control_Monad_Rec_Class.tailRecM(freeMonadRec)(k))(Prelude.pure(freeApplicative)));
    };
});
var liftF = function (f) {
    return fromView(new Bind(Unsafe_Coerce.unsafeCoerce(f), function ($45) {
        return Prelude.pure(freeApplicative)(Unsafe_Coerce.unsafeCoerce($45));
    }));
};
var freeMonadTrans = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return liftF;
});
var liftFI = function (dictInject) {
    return function (fa) {
        return liftF(Data_Inject.inj(dictInject)(fa));
    };
};
var foldFree = function (dictMonadRec) {
    return function (k) {
        var go = function (f) {
            var $36 = toView(f);
            if ($36 instanceof Return) {
                return Prelude["<$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Either.Right.create)(Prelude.pure((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())($36.value0));
            };
            if ($36 instanceof Bind) {
                return Prelude["<$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($46) {
                    return Data_Either.Left.create($36.value1($46));
                })(k($36.value0));
            };
            throw new Error("Failed pattern match at Control.Monad.Free line 108, column 3 - line 109, column 3: " + [ $36.constructor.name ]);
        };
        return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
    };
};
var mapF = function (k) {
    return foldFree(freeMonadRec)(function ($47) {
        return liftF(k($47));
    });
};
var injF = function (dictInject) {
    return mapF(Data_Inject.inj(dictInject));
};
module.exports = {
    runFreeM: runFreeM, 
    runFree: runFree, 
    foldFree: foldFree, 
    injF: injF, 
    mapF: mapF, 
    liftFI: liftFI, 
    liftF: liftF, 
    suspendF: suspendF, 
    freeFunctor: freeFunctor, 
    freeBind: freeBind, 
    freeApplicative: freeApplicative, 
    freeApply: freeApply, 
    freeMonad: freeMonad, 
    freeMonadTrans: freeMonadTrans, 
    freeMonadRec: freeMonadRec
};

},{"Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Data.CatList":"/Users/anders/dev/ps/halo/ajax/output/Data.CatList/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Inject":"/Users/anders/dev/ps/halo/ajax/output/Data.Inject/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.NaturalTransformation":"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js","Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.List.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Plus = require("Control.Plus");
var Data_Lazy = require("Data.Lazy");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var Data_Tuple = require("Data.Tuple");
var Data_Unfoldable = require("Data.Unfoldable");
var Yield = (function () {
    function Yield(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Yield.create = function (value0) {
        return function (value1) {
            return new Yield(value0, value1);
        };
    };
    return Yield;
})();
var Skip = (function () {
    function Skip(value0) {
        this.value0 = value0;
    };
    Skip.create = function (value0) {
        return new Skip(value0);
    };
    return Skip;
})();
var Done = (function () {
    function Done() {

    };
    Done.value = new Done();
    return Done;
})();
var ListT = (function () {
    function ListT(value0) {
        this.value0 = value0;
    };
    ListT.create = function (value0) {
        return new ListT(value0);
    };
    return ListT;
})();
var wrapLazy = function (dictApplicative) {
    return function (v) {
        return ListT.create(Prelude.pure(dictApplicative)(new Skip(v)));
    };
};
var wrapEffect = function (dictFunctor) {
    return function (v) {
        return ListT.create(Prelude["<$>"](dictFunctor)(function ($159) {
            return Skip.create(Data_Lazy.defer(Prelude["const"]($159)));
        })(v));
    };
};
var unfold = function (dictMonad) {
    return function (f) {
        return function (z) {
            var g = function (v) {
                if (v instanceof Data_Maybe.Just) {
                    return new Yield(v.value0.value1, Data_Lazy.defer(function (v1) {
                        return unfold(dictMonad)(f)(v.value0.value0);
                    }));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 116, column 3 - line 117, column 3: " + [ v.constructor.name ]);
            };
            return ListT.create(Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(g)(f(z)));
        };
    };
};
var runListT = function (v) {
    return v.value0;
};
var scanl = function (dictMonad) {
    return function (f) {
        return function (b) {
            return function (l) {
                var g = function (v) {
                    var h = function (v1) {
                        if (v1 instanceof Yield) {
                            var bʹ = f(v.value0)(v1.value0);
                            return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(bʹ, Data_Lazy.force(v1.value1)), bʹ));
                        };
                        if (v1 instanceof Skip) {
                            return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0, Data_Lazy.force(v1.value0)), v.value0));
                        };
                        if (v1 instanceof Done) {
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match at Control.Monad.List.Trans line 215, column 5 - line 217, column 5: " + [ v1.constructor.name ]);
                    };
                    return Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(h)(runListT(v.value1));
                };
                return unfold(dictMonad)(g)(new Data_Tuple.Tuple(b, l));
            };
        };
    };
};
var stepMap = function (dictFunctor) {
    return function (f) {
        return function (l) {
            return ListT.create(Prelude["<$>"](dictFunctor)(f)(runListT(l)));
        };
    };
};
var takeWhile = function (dictApplicative) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var $88 = f(v.value0);
                if ($88) {
                    return new Yield(v.value0, Prelude["<$>"](Data_Lazy.functorLazy)(takeWhile(dictApplicative)(f))(v.value1));
                };
                if (!$88) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 140, column 3 - line 141, column 3: " + [ $88.constructor.name ]);
            };
            if (v instanceof Skip) {
                return Skip.create(Prelude["<$>"](Data_Lazy.functorLazy)(takeWhile(dictApplicative)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 140, column 3 - line 141, column 3: " + [ v.constructor.name ]);
        };
        return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(g);
    };
};
var uncons = function (dictMonad) {
    return function (l) {
        var g = function (v) {
            if (v instanceof Yield) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Just.create(new Data_Tuple.Tuple(v.value0, Data_Lazy.force(v.value1))));
            };
            if (v instanceof Skip) {
                return uncons(dictMonad)(Data_Lazy.force(v.value0));
            };
            if (v instanceof Done) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Nothing.value);
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 181, column 3 - line 182, column 3: " + [ v.constructor.name ]);
        };
        return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runListT(l))(g);
    };
};
var tail = function (dictMonad) {
    return function (l) {
        return Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.snd))(uncons(dictMonad)(l));
    };
};
var prepend$prime = function (dictApplicative) {
    return function (h) {
        return function (t) {
            return ListT.create(Prelude.pure(dictApplicative)(new Yield(h, t)));
        };
    };
};
var prepend = function (dictApplicative) {
    return function (h) {
        return function (t) {
            return prepend$prime(dictApplicative)(h)(Data_Lazy.defer(Prelude["const"](t)));
        };
    };
};
var nil = function (dictApplicative) {
    return ListT.create(Prelude.pure(dictApplicative)(Done.value));
};
var singleton = function (dictApplicative) {
    return function (a) {
        return prepend(dictApplicative)(a)(nil(dictApplicative));
    };
};
var take = function (dictApplicative) {
    return function (v) {
        return function (fa) {
            if (v === 0) {
                return nil(dictApplicative);
            };
            var f = function (v1) {
                if (v1 instanceof Yield) {
                    return new Yield(v1.value0, Prelude["<$>"](Data_Lazy.functorLazy)(take(dictApplicative)(v - 1))(v1.value1));
                };
                if (v1 instanceof Skip) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(take(dictApplicative)(v))(v1.value0));
                };
                if (v1 instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 133, column 3 - line 134, column 3: " + [ v1.constructor.name ]);
            };
            return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(f)(fa);
        };
    };
};
var zipWith$prime = function (dictMonad) {
    return function (f) {
        var g = function (v) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(nil(dictMonad["__superclass_Prelude.Applicative_0"]()));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(nil(dictMonad["__superclass_Prelude.Applicative_0"]()));
                };
                if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Just) {
                    return Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude.flip(prepend$prime(dictMonad["__superclass_Prelude.Applicative_0"]()))(Data_Lazy.defer(function (v2) {
                        return zipWith$prime(dictMonad)(f)(v.value0.value1)(v1.value0.value1);
                    })))(f(v.value0.value0)(v1.value0.value0));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 223, column 3 - line 228, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        var loop = function (fa) {
            return function (fb) {
                return wrapEffect(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(uncons(dictMonad)(fa))(function (v) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(uncons(dictMonad)(fb))(function (v1) {
                        return g(v)(v1);
                    });
                }));
            };
        };
        return loop;
    };
};
var zipWith = function (dictMonad) {
    return function (f) {
        var g = function (a) {
            return function (b) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(f(a)(b));
            };
        };
        return zipWith$prime(dictMonad)(g);
    };
};
var mapMaybe = function (dictFunctor) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                return Data_Maybe.fromMaybe(Skip.create)(Prelude["<$>"](Data_Maybe.functorMaybe)(Yield.create)(f(v.value0)))(Prelude["<$>"](Data_Lazy.functorLazy)(mapMaybe(dictFunctor)(f))(v.value1));
            };
            if (v instanceof Skip) {
                return Skip.create(Prelude["<$>"](Data_Lazy.functorLazy)(mapMaybe(dictFunctor)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 169, column 3 - line 170, column 3: " + [ v.constructor.name ]);
        };
        return stepMap(dictFunctor)(g);
    };
};
var iterate = function (dictMonad) {
    return function (f) {
        return function (a) {
            var g = function (a1) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Maybe.Just(new Data_Tuple.Tuple(f(a1), a1)));
            };
            return unfold(dictMonad)(g)(a);
        };
    };
};
var repeat = function (dictMonad) {
    return iterate(dictMonad)(Prelude.id(Prelude.categoryFn));
};
var head = function (dictMonad) {
    return function (l) {
        return Prelude["<$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.fst))(uncons(dictMonad)(l));
    };
};
var functorListT = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                return new Yield(f(v.value0), Prelude["<$>"](Data_Lazy.functorLazy)(Prelude["<$>"](functorListT(dictFunctor))(f))(v.value1));
            };
            if (v instanceof Skip) {
                return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(Prelude["<$>"](functorListT(dictFunctor))(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 246, column 5 - line 247, column 5: " + [ v.constructor.name ]);
        };
        return stepMap(dictFunctor)(g);
    });
};
var fromEffect = function (dictApplicative) {
    return function (fa) {
        return ListT.create(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function (v) {
            return new Yield(v, Data_Lazy.defer(function (v1) {
                return nil(dictApplicative);
            }));
        })(fa));
    };
};
var monadTransListT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return fromEffect(dictMonad["__superclass_Prelude.Applicative_0"]());
});
var foldl$prime = function (dictMonad) {
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(b);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(f(b)(v.value0.value0))(Prelude.flip(loop)(v.value0.value1));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans line 198, column 5 - line 199, column 5: " + [ v.constructor.name ]);
                };
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(uncons(dictMonad)(l))(g);
            };
        };
        return loop;
    };
};
var foldl = function (dictMonad) {
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(b);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return loop(f(b)(v.value0.value0))(v.value0.value1);
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans line 206, column 5 - line 207, column 5: " + [ v.constructor.name ]);
                };
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(uncons(dictMonad)(l))(g);
            };
        };
        return loop;
    };
};
var filter = function (dictFunctor) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var sʹ = Prelude["<$>"](Data_Lazy.functorLazy)(filter(dictFunctor)(f))(v.value1);
                var $131 = f(v.value0);
                if ($131) {
                    return new Yield(v.value0, sʹ);
                };
                if (!$131) {
                    return new Skip(sʹ);
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 162, column 3 - line 163, column 3: " + [ $131.constructor.name ]);
            };
            if (v instanceof Skip) {
                var sʹ = Prelude["<$>"](Data_Lazy.functorLazy)(filter(dictFunctor)(f))(v.value0);
                return new Skip(sʹ);
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 162, column 3 - line 163, column 3: " + [ v.constructor.name ]);
        };
        return stepMap(dictFunctor)(g);
    };
};
var dropWhile = function (dictApplicative) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var $136 = f(v.value0);
                if ($136) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(dropWhile(dictApplicative)(f))(v.value1));
                };
                if (!$136) {
                    return new Yield(v.value0, v.value1);
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 155, column 3 - line 156, column 3: " + [ $136.constructor.name ]);
            };
            if (v instanceof Skip) {
                return Skip.create(Prelude["<$>"](Data_Lazy.functorLazy)(dropWhile(dictApplicative)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans line 155, column 3 - line 156, column 3: " + [ v.constructor.name ]);
        };
        return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(g);
    };
};
var drop = function (dictApplicative) {
    return function (v) {
        return function (fa) {
            if (v === 0) {
                return fa;
            };
            var f = function (v1) {
                if (v1 instanceof Yield) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(drop(dictApplicative)(v - 1))(v1.value1));
                };
                if (v1 instanceof Skip) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(drop(dictApplicative)(v))(v1.value0));
                };
                if (v1 instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 148, column 3 - line 149, column 3: " + [ v1.constructor.name ]);
            };
            return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(f)(fa);
        };
    };
};
var cons = function (dictApplicative) {
    return function (lh) {
        return function (t) {
            return ListT.create(Prelude.pure(dictApplicative)(new Yield(Data_Lazy.force(lh), t)));
        };
    };
};
var unfoldableListT = function (dictMonad) {
    return new Data_Unfoldable.Unfoldable(function (f) {
        return function (b) {
            var go = function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return nil(dictMonad["__superclass_Prelude.Applicative_0"]());
                };
                if (v instanceof Data_Maybe.Just) {
                    return cons(dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude.pure(Data_Lazy.applicativeLazy)(v.value0.value0))(Data_Lazy.defer(function (v1) {
                        return go(f(v.value0.value1));
                    }));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 250, column 1 - line 255, column 1: " + [ v.constructor.name ]);
            };
            return go(f(b));
        };
    });
};
var semigroupListT = function (dictApplicative) {
    return new Prelude.Semigroup(concat(dictApplicative));
};
var concat = function (dictApplicative) {
    return function (x) {
        return function (y) {
            var f = function (v) {
                if (v instanceof Yield) {
                    return new Yield(v.value0, Prelude["<$>"](Data_Lazy.functorLazy)(function (v1) {
                        return Prelude["<>"](semigroupListT(dictApplicative))(v1)(y);
                    })(v.value1));
                };
                if (v instanceof Skip) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(function (v1) {
                        return Prelude["<>"](semigroupListT(dictApplicative))(v1)(y);
                    })(v.value0));
                };
                if (v instanceof Done) {
                    return new Skip(Data_Lazy.defer(Prelude["const"](y)));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 92, column 3 - line 93, column 3: " + [ v.constructor.name ]);
            };
            return stepMap((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(f)(x);
        };
    };
};
var monoidListT = function (dictApplicative) {
    return new Data_Monoid.Monoid(function () {
        return semigroupListT(dictApplicative);
    }, nil(dictApplicative));
};
var catMaybes = function (dictFunctor) {
    return mapMaybe(dictFunctor)(Prelude.id(Prelude.categoryFn));
};
var monadListT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeListT(dictMonad);
    }, function () {
        return bindListT(dictMonad);
    });
};
var bindListT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyListT(dictMonad);
    }, function (fa) {
        return function (f) {
            var g = function (v) {
                if (v instanceof Yield) {
                    var h = function (s1) {
                        return Prelude["<>"](semigroupListT(dictMonad["__superclass_Prelude.Applicative_0"]()))(f(v.value0))(Prelude[">>="](bindListT(dictMonad))(s1)(f));
                    };
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(h)(v.value1));
                };
                if (v instanceof Skip) {
                    return new Skip(Prelude["<$>"](Data_Lazy.functorLazy)(function (v1) {
                        return Prelude[">>="](bindListT(dictMonad))(v1)(f);
                    })(v.value0));
                };
                if (v instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans line 263, column 5 - line 266, column 5: " + [ v.constructor.name ]);
            };
            return stepMap(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(g)(fa);
        };
    });
};
var applyListT = function (dictMonad) {
    return new Prelude.Apply(function () {
        return functorListT(((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
    }, Prelude.ap(monadListT(dictMonad)));
};
var applicativeListT = function (dictMonad) {
    return new Prelude.Applicative(function () {
        return applyListT(dictMonad);
    }, singleton(dictMonad["__superclass_Prelude.Applicative_0"]()));
};
var monadEffListT = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadListT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($160) {
        return Control_Monad_Trans.lift(monadTransListT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($160));
    });
};
var altListT = function (dictApplicative) {
    return new Control_Alt.Alt(function () {
        return functorListT((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
    }, concat(dictApplicative));
};
var plusListT = function (dictMonad) {
    return new Control_Plus.Plus(function () {
        return altListT(dictMonad["__superclass_Prelude.Applicative_0"]());
    }, nil(dictMonad["__superclass_Prelude.Applicative_0"]()));
};
var alternativeListT = function (dictMonad) {
    return new Control_Alternative.Alternative(function () {
        return plusListT(dictMonad);
    }, function () {
        return applicativeListT(dictMonad);
    });
};
var monadPlusListT = function (dictMonad) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeListT(dictMonad);
    }, function () {
        return monadListT(dictMonad);
    });
};
module.exports = {
    "zipWith'": zipWith$prime, 
    zipWith: zipWith, 
    wrapLazy: wrapLazy, 
    wrapEffect: wrapEffect, 
    unfold: unfold, 
    uncons: uncons, 
    takeWhile: takeWhile, 
    take: take, 
    tail: tail, 
    singleton: singleton, 
    scanl: scanl, 
    repeat: repeat, 
    "prepend'": prepend$prime, 
    prepend: prepend, 
    nil: nil, 
    mapMaybe: mapMaybe, 
    iterate: iterate, 
    head: head, 
    fromEffect: fromEffect, 
    "foldl'": foldl$prime, 
    foldl: foldl, 
    filter: filter, 
    dropWhile: dropWhile, 
    drop: drop, 
    cons: cons, 
    catMaybes: catMaybes, 
    semigroupListT: semigroupListT, 
    monoidListT: monoidListT, 
    functorListT: functorListT, 
    unfoldableListT: unfoldableListT, 
    applyListT: applyListT, 
    applicativeListT: applicativeListT, 
    bindListT: bindListT, 
    monadListT: monadListT, 
    monadTransListT: monadTransListT, 
    altListT: altListT, 
    plusListT: plusListT, 
    alternativeListT: alternativeListT, 
    monadPlusListT: monadPlusListT, 
    monadEffListT: monadEffListT
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Lazy":"/Users/anders/dev/ps/halo/ajax/output/Data.Lazy/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Data.Unfoldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Unfoldable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Maybe.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Maybe = require("Data.Maybe");
var Data_Tuple = require("Data.Tuple");
var Data_Monoid = require("Data.Monoid");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Monad = require("Control.Monad");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Cont_Class = require("Control.Monad.Cont.Class");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_RWS_Class = require("Control.Monad.RWS.Class");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var MaybeT = function (x) {
    return x;
};
var runMaybeT = function (v) {
    return v;
};
var monadTransMaybeT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function ($46) {
        return MaybeT(Prelude.liftM1(dictMonad)(Data_Maybe.Just.create)($46));
    };
});
var mapMaybeT = function (f) {
    return function ($47) {
        return MaybeT(f(runMaybeT($47)));
    };
};
var monadMaybeT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeMaybeT(dictMonad);
    }, function () {
        return bindMaybeT(dictMonad);
    });
};
var functorMaybeT = function (dictMonad) {
    return new Prelude.Functor(Prelude.liftA1(applicativeMaybeT(dictMonad)));
};
var bindMaybeT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyMaybeT(dictMonad);
    }, function (x) {
        return function (f) {
            return MaybeT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runMaybeT(x))(function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Nothing.value);
                };
                if (v instanceof Data_Maybe.Just) {
                    return runMaybeT(f(v.value0));
                };
                throw new Error("Failed pattern match: " + [ v.constructor.name ]);
            }));
        };
    });
};
var applyMaybeT = function (dictMonad) {
    return new Prelude.Apply(function () {
        return functorMaybeT(dictMonad);
    }, Prelude.ap(monadMaybeT(dictMonad)));
};
var applicativeMaybeT = function (dictMonad) {
    return new Prelude.Applicative(function () {
        return applyMaybeT(dictMonad);
    }, function ($48) {
        return MaybeT(Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Just.create($48)));
    });
};
var monadContMaybeT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadMaybeT(dictMonadCont["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return MaybeT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
            return runMaybeT(f(function (a) {
                return MaybeT(c(new Data_Maybe.Just(a)));
            }));
        }));
    });
};
var monadEffMaybe = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadMaybeT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($49) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($49));
    });
};
var monadErrorMaybeT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadMaybeT(dictMonadError["__superclass_Prelude.Monad_0"]());
    }, function (m) {
        return function (h) {
            return MaybeT(Control_Monad_Error_Class.catchError(dictMonadError)(runMaybeT(m))(function ($50) {
                return runMaybeT(h($50));
            }));
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
    });
};
var monadReaderMaybeT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadMaybeT(dictMonadReader["__superclass_Prelude.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapMaybeT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecMaybeT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadMaybeT(dictMonadRec["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return function ($51) {
            return MaybeT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runMaybeT(f(a)))(function (v) {
                    return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                        if (v instanceof Data_Maybe.Nothing) {
                            return new Data_Either.Right(Data_Maybe.Nothing.value);
                        };
                        if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(v.value0.value0);
                        };
                        if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Maybe.Just(v.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Maybe.Trans line 79, column 1 - line 87, column 1: " + [ v.constructor.name ]);
                    })());
                });
            })($51));
        };
    });
};
var monadStateMaybeT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadMaybeT(dictMonadState["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadWriterMaybeT = function (dictMonad) {
    return function (dictMonadWriter) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadMaybeT(dictMonad);
        }, mapMaybeT(function (m) {
            return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude["<$>"](Data_Maybe.functorMaybe)(function (r) {
                    return new Data_Tuple.Tuple(r, v.value1);
                })(v.value0));
            });
        }), mapMaybeT(function (m) {
            return Control_Monad_Writer_Class.pass(dictMonadWriter)(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())((function () {
                    if (v instanceof Data_Maybe.Nothing) {
                        return new Data_Tuple.Tuple(Data_Maybe.Nothing.value, Prelude.id(Prelude.categoryFn));
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return new Data_Tuple.Tuple(new Data_Maybe.Just(v.value0.value0), v.value0.value1);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Maybe.Trans line 104, column 1 - line 115, column 1: " + [ v.constructor.name ]);
                })());
            }));
        }), function (wd) {
            return Control_Monad_Trans.lift(monadTransMaybeT)(dictMonad)(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
        });
    };
};
var monadRWSMaybeT = function (dictMonoid) {
    return function (dictMonadRWS) {
        return new Control_Monad_RWS_Class.MonadRWS(function () {
            return monadReaderMaybeT(dictMonadRWS["__superclass_Control.Monad.Reader.Class.MonadReader_1"]());
        }, function () {
            return monadStateMaybeT(dictMonadRWS["__superclass_Control.Monad.State.Class.MonadState_3"]());
        }, function () {
            return monadWriterMaybeT((dictMonadRWS["__superclass_Control.Monad.State.Class.MonadState_3"]())["__superclass_Prelude.Monad_0"]())(dictMonadRWS["__superclass_Control.Monad.Writer.Class.MonadWriter_2"]());
        }, function () {
            return dictMonoid;
        });
    };
};
var altMaybeT = function (dictMonad) {
    return new Control_Alt.Alt(function () {
        return functorMaybeT(dictMonad);
    }, function (m1) {
        return function (m2) {
            return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runMaybeT(m1))(function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return runMaybeT(m2);
                };
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(v);
            });
        };
    });
};
var plusMaybeT = function (dictMonad) {
    return new Control_Plus.Plus(function () {
        return altMaybeT(dictMonad);
    }, Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(Data_Maybe.Nothing.value));
};
var alternativeMaybeT = function (dictMonad) {
    return new Control_Alternative.Alternative(function () {
        return plusMaybeT(dictMonad);
    }, function () {
        return applicativeMaybeT(dictMonad);
    });
};
var monadPlusMaybeT = function (dictMonad) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeMaybeT(dictMonad);
    }, function () {
        return monadMaybeT(dictMonad);
    });
};
module.exports = {
    MaybeT: MaybeT, 
    mapMaybeT: mapMaybeT, 
    runMaybeT: runMaybeT, 
    functorMaybeT: functorMaybeT, 
    applyMaybeT: applyMaybeT, 
    applicativeMaybeT: applicativeMaybeT, 
    bindMaybeT: bindMaybeT, 
    monadMaybeT: monadMaybeT, 
    monadTransMaybeT: monadTransMaybeT, 
    altMaybeT: altMaybeT, 
    plusMaybeT: plusMaybeT, 
    alternativeMaybeT: alternativeMaybeT, 
    monadPlusMaybeT: monadPlusMaybeT, 
    monadRecMaybeT: monadRecMaybeT, 
    monadEffMaybe: monadEffMaybe, 
    monadContMaybeT: monadContMaybeT, 
    monadErrorMaybeT: monadErrorMaybeT, 
    monadReaderMaybeT: monadReaderMaybeT, 
    monadStateMaybeT: monadStateMaybeT, 
    monadWriterMaybeT: monadWriterMaybeT, 
    monadRWSMaybeT: monadRWSMaybeT
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Monad":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad/index.js","Control.Monad.Cont.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Class/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","Control.Monad.RWS.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.RWS.Class/index.js","Control.Monad.Reader.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.RWS.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Monoid = require("Data.Monoid");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var MonadRWS = function (__superclass_Control$dotMonad$dotReader$dotClass$dotMonadReader_1, __superclass_Control$dotMonad$dotState$dotClass$dotMonadState_3, __superclass_Control$dotMonad$dotWriter$dotClass$dotMonadWriter_2, __superclass_Data$dotMonoid$dotMonoid_0) {
    this["__superclass_Control.Monad.Reader.Class.MonadReader_1"] = __superclass_Control$dotMonad$dotReader$dotClass$dotMonadReader_1;
    this["__superclass_Control.Monad.State.Class.MonadState_3"] = __superclass_Control$dotMonad$dotState$dotClass$dotMonadState_3;
    this["__superclass_Control.Monad.Writer.Class.MonadWriter_2"] = __superclass_Control$dotMonad$dotWriter$dotClass$dotMonadWriter_2;
    this["__superclass_Data.Monoid.Monoid_0"] = __superclass_Data$dotMonoid$dotMonoid_0;
};
module.exports = {
    MonadRWS: MonadRWS
};

},{"Control.Monad.Reader.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.RWS.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Monoid = require("Data.Monoid");
var Data_Tuple = require("Data.Tuple");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Control_Monad_RWS_Class = require("Control.Monad.RWS.Class");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var RWSResult = (function () {
    function RWSResult(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    RWSResult.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new RWSResult(value0, value1, value2);
            };
        };
    };
    return RWSResult;
})();
var RWST = function (x) {
    return x;
};
var runRWST = function (v) {
    return v;
};
var withRWST = function (f) {
    return function (m) {
        return function (r) {
            return function (s) {
                return Data_Tuple.uncurry(runRWST(m))(f(r)(s));
            };
        };
    };
};
var monadTransRWST = function (dictMonoid) {
    return new Control_Monad_Trans.MonadTrans(function (dictMonad) {
        return function (m) {
            return function (v) {
                return function (s) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (a) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                    });
                };
            };
        };
    });
};
var mapRWST = function (f) {
    return function (m) {
        return function (r) {
            return function (s) {
                return f(runRWST(m)(r)(s));
            };
        };
    };
};
var functorRWST = function (dictFunctor) {
    return function (dictMonoid) {
        return new Prelude.Functor(function (f) {
            return function (m) {
                return function (r) {
                    return function (s) {
                        return Prelude["<$>"](dictFunctor)(function (v) {
                            return new RWSResult(v.value0, f(v.value1), v.value2);
                        })(runRWST(m)(r)(s));
                    };
                };
            };
        });
    };
};
var execRWST = function (dictMonad) {
    return function (m) {
        return function (r) {
            return function (s) {
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runRWST(m)(r)(s))(function (v) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v.value0, v.value2));
                });
            };
        };
    };
};
var evalRWST = function (dictMonad) {
    return function (m) {
        return function (r) {
            return function (s) {
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runRWST(m)(r)(s))(function (v) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v.value1, v.value2));
                });
            };
        };
    };
};
var applyRWST = function (dictBind) {
    return function (dictMonoid) {
        return new Prelude.Apply(function () {
            return functorRWST((dictBind["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(dictMonoid);
        }, function (f) {
            return function (m) {
                return function (r) {
                    return function (s) {
                        return Prelude[">>="](dictBind)(runRWST(f)(r)(s))(function (v) {
                            return Prelude["<#>"]((dictBind["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(runRWST(m)(r)(v.value0))(function (v1) {
                                return new RWSResult(v1.value0, v.value1(v1.value1), Prelude["++"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value2)(v1.value2));
                            });
                        });
                    };
                };
            };
        });
    };
};
var bindRWST = function (dictBind) {
    return function (dictMonoid) {
        return new Prelude.Bind(function () {
            return applyRWST(dictBind)(dictMonoid);
        }, function (m) {
            return function (f) {
                return function (r) {
                    return function (s) {
                        return Prelude[">>="](dictBind)(runRWST(m)(r)(s))(function (v) {
                            return Prelude["<#>"]((dictBind["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(runRWST(f(v.value1))(r)(v.value0))(function (v1) {
                                return new RWSResult(v1.value0, v1.value1, Prelude["++"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value2)(v1.value2));
                            });
                        });
                    };
                };
            };
        });
    };
};
var applicativeRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Prelude.Applicative(function () {
            return applyRWST(dictMonad["__superclass_Prelude.Bind_1"]())(dictMonoid);
        }, function (a) {
            return function (v) {
                return function (s) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                };
            };
        });
    };
};
var monadRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Prelude.Monad(function () {
            return applicativeRWST(dictMonad)(dictMonoid);
        }, function () {
            return bindRWST(dictMonad["__superclass_Prelude.Bind_1"]())(dictMonoid);
        });
    };
};
var monadEffRWS = function (dictMonad) {
    return function (dictMonoid) {
        return function (dictMonadEff) {
            return new Control_Monad_Eff_Class.MonadEff(function () {
                return monadRWST(dictMonad)(dictMonoid);
            }, function ($107) {
                return Control_Monad_Trans.lift(monadTransRWST(dictMonoid))(dictMonad)(Control_Monad_Eff_Class.liftEff(dictMonadEff)($107));
            });
        };
    };
};
var monadErrorRWST = function (dictMonadError) {
    return function (dictMonoid) {
        return new Control_Monad_Error_Class.MonadError(function () {
            return monadRWST(dictMonadError["__superclass_Prelude.Monad_0"]())(dictMonoid);
        }, function (m) {
            return function (h) {
                return RWST(function (r) {
                    return function (s) {
                        return Control_Monad_Error_Class.catchError(dictMonadError)(runRWST(m)(r)(s))(function (e) {
                            return runRWST(h(e))(r)(s);
                        });
                    };
                });
            };
        }, function (e) {
            return Control_Monad_Trans.lift(monadTransRWST(dictMonoid))(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
        });
    };
};
var monadReaderRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Control_Monad_Reader_Class.MonadReader(function () {
            return monadRWST(dictMonad)(dictMonoid);
        }, function (r) {
            return function (s) {
                return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(s, r, Data_Monoid.mempty(dictMonoid)));
            };
        }, function (f) {
            return function (m) {
                return function (r) {
                    return function (s) {
                        return runRWST(m)(f(r))(s);
                    };
                };
            };
        });
    };
};
var monadRecRWST = function (dictMonoid) {
    return function (dictMonadRec) {
        return new Control_Monad_Rec_Class.MonadRec(function () {
            return monadRWST(dictMonadRec["__superclass_Prelude.Monad_0"]())(dictMonoid);
        }, function (k) {
            return function (a) {
                var kʹ = function (r) {
                    return function (v) {
                        return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runRWST(k(v.value1))(r)(v.value0))(function (v1) {
                            return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                                if (v1.value1 instanceof Data_Either.Left) {
                                    return new Data_Either.Left(new RWSResult(v1.value0, v1.value1.value0, Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value2)(v1.value2)));
                                };
                                if (v1.value1 instanceof Data_Either.Right) {
                                    return new Data_Either.Right(new RWSResult(v1.value0, v1.value1.value0, Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value2)(v1.value2)));
                                };
                                throw new Error("Failed pattern match at Control.Monad.RWS.Trans line 98, column 5 - line 102, column 75: " + [ v1.value1.constructor.name ]);
                            })());
                        });
                    };
                };
                return function (r) {
                    return function (s) {
                        return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(kʹ(r))(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                    };
                };
            };
        });
    };
};
var monadStateRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Control_Monad_State_Class.MonadState(function () {
            return monadRWST(dictMonad)(dictMonoid);
        }, function (f) {
            return function (v) {
                return function (s) {
                    var $90 = f(s);
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult($90.value1, $90.value0, Data_Monoid.mempty(dictMonoid)));
                };
            };
        });
    };
};
var monadWriterRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadRWST(dictMonad)(dictMonoid);
        }, function (m) {
            return function (r) {
                return function (s) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runRWST(m)(r)(s))(function (v) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(v.value0, new Data_Tuple.Tuple(v.value1, v.value2), v.value2));
                    });
                };
            };
        }, function (m) {
            return function (r) {
                return function (s) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(runRWST(m)(r)(s))(function (v) {
                        return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(v.value0, v.value1.value0, v.value1.value1(v.value2)));
                    });
                };
            };
        }, function (v) {
            return function (v1) {
                return function (s) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(new RWSResult(s, v.value0, v.value1));
                };
            };
        });
    };
};
var monadRWSRWST = function (dictMonad) {
    return function (dictMonoid) {
        return new Control_Monad_RWS_Class.MonadRWS(function () {
            return monadReaderRWST(dictMonad)(dictMonoid);
        }, function () {
            return monadStateRWST(dictMonad)(dictMonoid);
        }, function () {
            return monadWriterRWST(dictMonad)(dictMonoid);
        }, function () {
            return dictMonoid;
        });
    };
};
module.exports = {
    RWST: RWST, 
    RWSResult: RWSResult, 
    withRWST: withRWST, 
    mapRWST: mapRWST, 
    execRWST: execRWST, 
    evalRWST: evalRWST, 
    runRWST: runRWST, 
    functorRWST: functorRWST, 
    applyRWST: applyRWST, 
    bindRWST: bindRWST, 
    applicativeRWST: applicativeRWST, 
    monadRWST: monadRWST, 
    monadTransRWST: monadTransRWST, 
    monadEffRWS: monadEffRWS, 
    monadReaderRWST: monadReaderRWST, 
    monadStateRWST: monadStateRWST, 
    monadWriterRWST: monadWriterRWST, 
    monadRWSRWST: monadRWSRWST, 
    monadErrorRWST: monadErrorRWST, 
    monadRecRWST: monadRecRWST
};

},{"Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","Control.Monad.RWS.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.RWS.Class/index.js","Control.Monad.Reader.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var MonadReader = function (__superclass_Prelude$dotMonad_0, ask, local) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.ask = ask;
    this.local = local;
};
var monadReaderFun = new MonadReader(function () {
    return Prelude.monadFn;
}, Prelude.id(Prelude.categoryFn), Prelude[">>>"](Prelude.semigroupoidFn));
var local = function (dict) {
    return dict.local;
};
var ask = function (dict) {
    return dict.ask;
};
var reader = function (dictMonadReader) {
    return function (f) {
        return Prelude[">>="]((dictMonadReader["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(ask(dictMonadReader))(function ($1) {
            return Prelude["return"]((dictMonadReader["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(f($1));
        });
    };
};
module.exports = {
    MonadReader: MonadReader, 
    reader: reader, 
    local: local, 
    ask: ask, 
    monadReaderFun: monadReaderFun
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Distributive = require("Data.Distributive");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Cont_Class = require("Control.Monad.Cont.Class");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var Data_Either = require("Data.Either");
var ReaderT = function (x) {
    return x;
};
var runReaderT = function (v) {
    return v;
};
var withReaderT = function (f) {
    return function (m) {
        return ReaderT(function ($27) {
            return runReaderT(m)(f($27));
        });
    };
};
var monadTransReaderT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function ($28) {
        return ReaderT(Prelude["const"]($28));
    };
});
var mapReaderT = function (f) {
    return function (m) {
        return ReaderT(function ($29) {
            return f(runReaderT(m)($29));
        });
    };
};
var functorReaderT = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return mapReaderT(Prelude["<$>"](dictFunctor)(f));
    });
};
var distributiveReaderT = function (dictDistributive) {
    return new Data_Distributive.Distributive(function () {
        return functorReaderT(dictDistributive["__superclass_Prelude.Functor_0"]());
    }, function (dictFunctor) {
        return function (f) {
            return function ($30) {
                return Data_Distributive.distribute(distributiveReaderT(dictDistributive))(dictFunctor)(Prelude.map(dictFunctor)(f)($30));
            };
        };
    }, function (dictFunctor) {
        return function (a) {
            return function (e) {
                return Data_Distributive.collect(dictDistributive)(dictFunctor)(Prelude.flip(runReaderT)(e))(a);
            };
        };
    });
};
var applyReaderT = function (dictApplicative) {
    return new Prelude.Apply(function () {
        return functorReaderT((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]());
    }, function (f) {
        return function (v) {
            return function (r) {
                return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(runReaderT(f)(r))(runReaderT(v)(r));
            };
        };
    });
};
var bindReaderT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyReaderT(dictMonad["__superclass_Prelude.Applicative_0"]());
    }, function (m) {
        return function (k) {
            return function (r) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runReaderT(m)(r))(function (v) {
                    return runReaderT(k(v))(r);
                });
            };
        };
    });
};
var applicativeReaderT = function (dictApplicative) {
    return new Prelude.Applicative(function () {
        return applyReaderT(dictApplicative);
    }, function ($31) {
        return ReaderT(Prelude["const"](Prelude.pure(dictApplicative)($31)));
    });
};
var monadReaderT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeReaderT(dictMonad["__superclass_Prelude.Applicative_0"]());
    }, function () {
        return bindReaderT(dictMonad);
    });
};
var monadContReaderT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadReaderT(dictMonadCont["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return ReaderT(function (r) {
            return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                return runReaderT(f(function (a) {
                    return ReaderT(Prelude["const"](c(a)));
                }))(r);
            });
        });
    });
};
var monadEffReader = function (dictMonadEff) {
    return new Control_Monad_Eff_Class.MonadEff(function () {
        return monadReaderT(dictMonadEff["__superclass_Prelude.Monad_0"]());
    }, function ($32) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadEff["__superclass_Prelude.Monad_0"]())(Control_Monad_Eff_Class.liftEff(dictMonadEff)($32));
    });
};
var monadErrorReaderT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadReaderT(dictMonadError["__superclass_Prelude.Monad_0"]());
    }, function (m) {
        return function (h) {
            return ReaderT(function (r) {
                return Control_Monad_Error_Class.catchError(dictMonadError)(runReaderT(m)(r))(function (e) {
                    return runReaderT(h(e))(r);
                });
            });
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
    });
};
var monadReaderReaderT = function (dictMonad) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadReaderT(dictMonad);
    }, Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]()), withReaderT);
};
var monadRecReaderT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadReaderT(dictMonadRec["__superclass_Prelude.Monad_0"]());
    }, function (k) {
        return function (a) {
            var kʹ = function (r) {
                return function (a1) {
                    return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runReaderT(k(a1))(r))(function (v) {
                        return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())(Data_Either.either(Data_Either.Left.create)(Data_Either.Right.create)(v));
                    });
                };
            };
            return function (r) {
                return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(kʹ(r))(a);
            };
        };
    });
};
var monadStateReaderT = function (dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadReaderT(dictMonadState["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return Control_Monad_Trans.lift(monadTransReaderT)(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
};
var monadWriterReaderT = function (dictMonad) {
    return function (dictMonadWriter) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadReaderT(dictMonad);
        }, mapReaderT(Control_Monad_Writer_Class.listen(dictMonadWriter)), mapReaderT(Control_Monad_Writer_Class.pass(dictMonadWriter)), function (wd) {
            return Control_Monad_Trans.lift(monadTransReaderT)(dictMonad)(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
        });
    };
};
var altReaderT = function (dictAlt) {
    return new Control_Alt.Alt(function () {
        return functorReaderT(dictAlt["__superclass_Prelude.Functor_0"]());
    }, function (m) {
        return function (n) {
            return function (r) {
                return Control_Alt["<|>"](dictAlt)(runReaderT(m)(r))(runReaderT(n)(r));
            };
        };
    });
};
var plusReaderT = function (dictPlus) {
    return new Control_Plus.Plus(function () {
        return altReaderT(dictPlus["__superclass_Control.Alt.Alt_0"]());
    }, Prelude["const"](Control_Plus.empty(dictPlus)));
};
var alternativeReaderT = function (dictAlternative) {
    return new Control_Alternative.Alternative(function () {
        return plusReaderT(dictAlternative["__superclass_Control.Plus.Plus_1"]());
    }, function () {
        return applicativeReaderT(dictAlternative["__superclass_Prelude.Applicative_0"]());
    });
};
var monadPlusReaderT = function (dictMonadPlus) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeReaderT(dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]());
    }, function () {
        return monadReaderT(dictMonadPlus["__superclass_Prelude.Monad_0"]());
    });
};
module.exports = {
    ReaderT: ReaderT, 
    mapReaderT: mapReaderT, 
    withReaderT: withReaderT, 
    runReaderT: runReaderT, 
    functorReaderT: functorReaderT, 
    applyReaderT: applyReaderT, 
    applicativeReaderT: applicativeReaderT, 
    altReaderT: altReaderT, 
    plusReaderT: plusReaderT, 
    alternativeReaderT: alternativeReaderT, 
    bindReaderT: bindReaderT, 
    monadReaderT: monadReaderT, 
    monadPlusReaderT: monadPlusReaderT, 
    monadTransReaderT: monadTransReaderT, 
    monadEffReader: monadEffReader, 
    monadContReaderT: monadContReaderT, 
    monadErrorReaderT: monadErrorReaderT, 
    monadReaderReaderT: monadReaderReaderT, 
    monadStateReaderT: monadStateReaderT, 
    monadWriterReaderT: monadWriterReaderT, 
    distributiveReaderT: distributiveReaderT, 
    monadRecReaderT: monadRecReaderT
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Monad.Cont.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Class/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","Control.Monad.Reader.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Distributive":"/Users/anders/dev/ps/halo/ajax/output/Data.Distributive/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_ST = require("Control.Monad.ST");
var Data_Either = require("Data.Either");
var Data_Functor = require("Data.Functor");
var Data_Identity = require("Data.Identity");
var Control_Monad_Eff_Unsafe = require("Control.Monad.Eff.Unsafe");
var Data_Either_Unsafe = require("Data.Either.Unsafe");
var MonadRec = function (__superclass_Prelude$dotMonad_0, tailRecM) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.tailRecM = tailRecM;
};
var tailRecM = function (dict) {
    return dict.tailRecM;
};
var tailRecM2 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return tailRecM(dictMonadRec)(function (o) {
                    return f(o.a)(o.b);
                })({
                    a: a, 
                    b: b
                });
            };
        };
    };
};
var tailRecM3 = function (dictMonadRec) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return tailRecM(dictMonadRec)(function (o) {
                        return f(o.a)(o.b)(o.c);
                    })({
                        a: a, 
                        b: b, 
                        c: c
                    });
                };
            };
        };
    };
};
var tailRecEff = function (f) {
    return function (a) {
        var fʹ = function ($18) {
            return Control_Monad_Eff_Unsafe.unsafeInterleaveEff(f($18));
        };
        return function __do() {
            var v = fʹ(a)();
            var v1 = {
                value: v
            };
            (function () {
                while (!(function __do() {
                    var v2 = v1.value;
                    return (function () {
                        if (v2 instanceof Data_Either.Left) {
                            return function __do() {
                                var v3 = fʹ(v2.value0)();
                                v1.value = v3;
                                return Prelude["return"](Control_Monad_Eff.applicativeEff)(false)();
                            };
                        };
                        if (v2 instanceof Data_Either.Right) {
                            return Prelude["return"](Control_Monad_Eff.applicativeEff)(true);
                        };
                        throw new Error("Failed pattern match at Control.Monad.Rec.Class line 75, column 1 - line 76, column 1: " + [ v2.constructor.name ]);
                    })()();
                })()) {

                };
                return {};
            })();
            return Prelude["<$>"](Control_Monad_Eff.functorEff)(Data_Either_Unsafe.fromRight)(Control_Monad_ST.readSTRef(v1))();
        };
    };
};
var tailRec = function (f) {
    return function (a) {
        var go = function (__copy_v) {
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Data_Either.Left) {
                    var __tco_v = f(v.value0);
                    v = __tco_v;
                    continue tco;
                };
                if (v instanceof Data_Either.Right) {
                    return v.value0;
                };
                throw new Error("Failed pattern match at Control.Monad.Rec.Class line 63, column 1 - line 64, column 1: " + [ v.constructor.name ]);
            };
        };
        return go(f(a));
    };
};
var monadRecIdentity = new MonadRec(function () {
    return Data_Identity.monadIdentity;
}, function (f) {
    return function ($19) {
        return Data_Identity.Identity(tailRec(function ($20) {
            return Data_Identity.runIdentity(f($20));
        })($19));
    };
});
var monadRecEff = new MonadRec(function () {
    return Control_Monad_Eff.monadEff;
}, tailRecEff);
var forever = function (dictMonadRec) {
    return function (ma) {
        return tailRecM(dictMonadRec)(function (u) {
            return Data_Functor["<$"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(new Data_Either.Left(u))(ma);
        })(Prelude.unit);
    };
};
module.exports = {
    MonadRec: MonadRec, 
    forever: forever, 
    tailRecM3: tailRecM3, 
    tailRecM2: tailRecM2, 
    tailRecM: tailRecM, 
    tailRec: tailRec, 
    monadRecIdentity: monadRecIdentity, 
    monadRecEff: monadRecEff
};

},{"Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Unsafe/index.js","Control.Monad.ST":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.ST/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Either.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.Either.Unsafe/index.js","Data.Functor":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.ST/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Control.Monad.ST

exports.newSTRef = function (val) {
  return function () {
    return { value: val };
  };
};

exports.readSTRef = function (ref) {
  return function () {
    return ref.value;
  };
};

exports.modifySTRef = function (ref) {
  return function (f) {
    return function () {
      /* jshint boss: true */
      return ref.value = f(ref.value);
    };
  };
};

exports.writeSTRef = function (ref) {
  return function (a) {
    return function () {
      /* jshint boss: true */
      return ref.value = a;
    };
  };
};

exports.runST = function (f) {
  return f;
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.ST/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var pureST = function (st) {
    return Control_Monad_Eff.runPure($foreign.runST(st));
};
module.exports = {
    pureST: pureST, 
    runST: $foreign.runST, 
    writeSTRef: $foreign.writeSTRef, 
    modifySTRef: $foreign.modifySTRef, 
    readSTRef: $foreign.readSTRef, 
    newSTRef: $foreign.newSTRef
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.ST/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Monoid = require("Data.Monoid");
var Data_Tuple = require("Data.Tuple");
var MonadState = function (__superclass_Prelude$dotMonad_0, state) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.state = state;
};
var state = function (dict) {
    return dict.state;
};
var put = function (dictMonadState) {
    return function (s) {
        return state(dictMonadState)(function (v) {
            return new Data_Tuple.Tuple(Prelude.unit, s);
        });
    };
};
var modify = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(Prelude.unit, f(s));
        });
    };
};
var gets = function (dictMonadState) {
    return function (f) {
        return state(dictMonadState)(function (s) {
            return new Data_Tuple.Tuple(f(s), s);
        });
    };
};
var get = function (dictMonadState) {
    return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(s, s);
    });
};
module.exports = {
    MonadState: MonadState, 
    modify: modify, 
    put: put, 
    gets: gets, 
    get: get, 
    state: state
};

},{"Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Tuple = require("Data.Tuple");
var Data_Either = require("Data.Either");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Lazy = require("Control.Lazy");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Cont_Class = require("Control.Monad.Cont.Class");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var StateT = function (x) {
    return x;
};
var runStateT = function (v) {
    return v;
};
var withStateT = function (f) {
    return function (s) {
        return StateT(function ($60) {
            return runStateT(s)(f($60));
        });
    };
};
var monadTransStateT = new Control_Monad_Trans.MonadTrans(function (dictMonad) {
    return function (m) {
        return function (s) {
            return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v, s));
            });
        };
    };
});
var mapStateT = function (f) {
    return function (m) {
        return StateT(function ($61) {
            return f(runStateT(m)($61));
        });
    };
};
var lazyStateT = new Control_Lazy.Lazy(function (f) {
    return StateT(function (s) {
        return runStateT(f(Prelude.unit))(s);
    });
});
var execStateT = function (dictApply) {
    return function (m) {
        return function (s) {
            return Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Data_Tuple.snd)(runStateT(m)(s));
        };
    };
};
var evalStateT = function (dictApply) {
    return function (m) {
        return function (s) {
            return Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Data_Tuple.fst)(runStateT(m)(s));
        };
    };
};
var monadStateT = function (dictMonad) {
    return new Prelude.Monad(function () {
        return applicativeStateT(dictMonad);
    }, function () {
        return bindStateT(dictMonad);
    });
};
var functorStateT = function (dictMonad) {
    return new Prelude.Functor(Prelude.liftM1(monadStateT(dictMonad)));
};
var bindStateT = function (dictMonad) {
    return new Prelude.Bind(function () {
        return applyStateT(dictMonad);
    }, function (v) {
        return function (f) {
            return function (s) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(v(s))(function (v1) {
                    return runStateT(f(v1.value0))(v1.value1);
                });
            };
        };
    });
};
var applyStateT = function (dictMonad) {
    return new Prelude.Apply(function () {
        return functorStateT(dictMonad);
    }, Prelude.ap(monadStateT(dictMonad)));
};
var applicativeStateT = function (dictMonad) {
    return new Prelude.Applicative(function () {
        return applyStateT(dictMonad);
    }, function (a) {
        return StateT(function (s) {
            return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(a, s));
        });
    });
};
var monadContStateT = function (dictMonadCont) {
    return new Control_Monad_Cont_Class.MonadCont(function () {
        return monadStateT(dictMonadCont["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return StateT(function (s) {
            return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                return runStateT(f(function (a) {
                    return StateT(function (sʹ) {
                        return c(new Data_Tuple.Tuple(a, sʹ));
                    });
                }))(s);
            });
        });
    });
};
var monadEffState = function (dictMonad) {
    return function (dictMonadEff) {
        return new Control_Monad_Eff_Class.MonadEff(function () {
            return monadStateT(dictMonad);
        }, function ($62) {
            return Control_Monad_Trans.lift(monadTransStateT)(dictMonad)(Control_Monad_Eff_Class.liftEff(dictMonadEff)($62));
        });
    };
};
var monadErrorStateT = function (dictMonadError) {
    return new Control_Monad_Error_Class.MonadError(function () {
        return monadStateT(dictMonadError["__superclass_Prelude.Monad_0"]());
    }, function (m) {
        return function (h) {
            return StateT(function (s) {
                return Control_Monad_Error_Class.catchError(dictMonadError)(runStateT(m)(s))(function (e) {
                    return runStateT(h(e))(s);
                });
            });
        };
    }, function (e) {
        return Control_Monad_Trans.lift(monadTransStateT)(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
    });
};
var monadReaderStateT = function (dictMonadReader) {
    return new Control_Monad_Reader_Class.MonadReader(function () {
        return monadStateT(dictMonadReader["__superclass_Prelude.Monad_0"]());
    }, Control_Monad_Trans.lift(monadTransStateT)(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
        return mapStateT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
    });
};
var monadRecStateT = function (dictMonadRec) {
    return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadStateT(dictMonadRec["__superclass_Prelude.Monad_0"]());
    }, function (f) {
        return function (a) {
            var fʹ = function (v) {
                return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runStateT(f(v.value0))(v.value1))(function (v1) {
                    return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                        if (v1.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(new Data_Tuple.Tuple(v1.value0.value0, v1.value1));
                        };
                        if (v1.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(new Data_Tuple.Tuple(v1.value0.value0, v1.value1));
                        };
                        throw new Error("Failed pattern match at Control.Monad.State.Trans line 83, column 5 - line 89, column 1: " + [ v1.value0.constructor.name ]);
                    })());
                });
            };
            return function (s) {
                return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(fʹ)(new Data_Tuple.Tuple(a, s));
            };
        };
    });
};
var monadStateStateT = function (dictMonad) {
    return new Control_Monad_State_Class.MonadState(function () {
        return monadStateT(dictMonad);
    }, function (f) {
        return StateT(function ($63) {
            return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(f($63));
        });
    });
};
var monadWriterStateT = function (dictMonad) {
    return function (dictMonadWriter) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadStateT(dictMonad);
        }, function (m) {
            return StateT(function (s) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(Control_Monad_Writer_Class.listen(dictMonadWriter)(runStateT(m)(s)))(function (v) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
                });
            });
        }, function (m) {
            return StateT(function (s) {
                return Control_Monad_Writer_Class.pass(dictMonadWriter)(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runStateT(m)(s))(function (v) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
                }));
            });
        }, function (wd) {
            return Control_Monad_Trans.lift(monadTransStateT)(dictMonad)(Control_Monad_Writer_Class.writer(dictMonadWriter)(wd));
        });
    };
};
var altStateT = function (dictMonad) {
    return function (dictAlt) {
        return new Control_Alt.Alt(function () {
            return functorStateT(dictMonad);
        }, function (x) {
            return function (y) {
                return StateT(function (s) {
                    return Control_Alt["<|>"](dictAlt)(runStateT(x)(s))(runStateT(y)(s));
                });
            };
        });
    };
};
var plusStateT = function (dictMonad) {
    return function (dictPlus) {
        return new Control_Plus.Plus(function () {
            return altStateT(dictMonad)(dictPlus["__superclass_Control.Alt.Alt_0"]());
        }, StateT(function (v) {
            return Control_Plus.empty(dictPlus);
        }));
    };
};
var alternativeStateT = function (dictMonad) {
    return function (dictAlternative) {
        return new Control_Alternative.Alternative(function () {
            return plusStateT(dictMonad)(dictAlternative["__superclass_Control.Plus.Plus_1"]());
        }, function () {
            return applicativeStateT(dictMonad);
        });
    };
};
var monadPlusStateT = function (dictMonadPlus) {
    return new Control_MonadPlus.MonadPlus(function () {
        return alternativeStateT(dictMonadPlus["__superclass_Prelude.Monad_0"]())(dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]());
    }, function () {
        return monadStateT(dictMonadPlus["__superclass_Prelude.Monad_0"]());
    });
};
module.exports = {
    StateT: StateT, 
    withStateT: withStateT, 
    mapStateT: mapStateT, 
    execStateT: execStateT, 
    evalStateT: evalStateT, 
    runStateT: runStateT, 
    functorStateT: functorStateT, 
    applyStateT: applyStateT, 
    applicativeStateT: applicativeStateT, 
    altStateT: altStateT, 
    plusStateT: plusStateT, 
    alternativeStateT: alternativeStateT, 
    bindStateT: bindStateT, 
    monadStateT: monadStateT, 
    monadRecStateT: monadRecStateT, 
    monadPlusStateT: monadPlusStateT, 
    monadTransStateT: monadTransStateT, 
    lazyStateT: lazyStateT, 
    monadEffState: monadEffState, 
    monadContStateT: monadContStateT, 
    monadErrorStateT: monadErrorStateT, 
    monadReaderStateT: monadReaderStateT, 
    monadStateStateT: monadStateStateT, 
    monadWriterStateT: monadWriterStateT
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Lazy":"/Users/anders/dev/ps/halo/ajax/output/Control.Lazy/index.js","Control.Monad.Cont.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Class/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","Control.Monad.Reader.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Identity = require("Data.Identity");
var Data_Tuple = require("Data.Tuple");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_State_Trans = require("Control.Monad.State.Trans");
var withState = Control_Monad_State_Trans.withStateT;
var runState = function (s) {
    return function ($0) {
        return Data_Identity.runIdentity(Control_Monad_State_Trans.runStateT(s)($0));
    };
};
var mapState = function (f) {
    return Control_Monad_State_Trans.mapStateT(function ($1) {
        return Data_Identity.Identity(f(Data_Identity.runIdentity($1)));
    });
};
var execState = function (m) {
    return function (s) {
        return Data_Tuple.snd(runState(m)(s));
    };
};
var evalState = function (m) {
    return function (s) {
        return Data_Tuple.fst(runState(m)(s));
    };
};
module.exports = {
    withState: withState, 
    mapState: mapState, 
    execState: execState, 
    evalState: evalState, 
    runState: runState
};

},{"Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.State.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Trans/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var MonadTrans = function (lift) {
    this.lift = lift;
};
var lift = function (dict) {
    return dict.lift;
};
module.exports = {
    MonadTrans: MonadTrans, 
    lift: lift
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Monoid = require("Data.Monoid");
var Data_Tuple = require("Data.Tuple");
var MonadWriter = function (__superclass_Prelude$dotMonad_0, listen, pass, writer) {
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
    this.listen = listen;
    this.pass = pass;
    this.writer = writer;
};
var writer = function (dict) {
    return dict.writer;
};
var tell = function (dictMonoid) {
    return function (dictMonad) {
        return function (dictMonadWriter) {
            return function (w) {
                return writer(dictMonadWriter)(new Data_Tuple.Tuple(Prelude.unit, w));
            };
        };
    };
};
var pass = function (dict) {
    return dict.pass;
};
var listen = function (dict) {
    return dict.listen;
};
var listens = function (dictMonoid) {
    return function (dictMonad) {
        return function (dictMonadWriter) {
            return function (f) {
                return function (m) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(listen(dictMonadWriter)(m))(function (v) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v.value0, f(v.value1)));
                    });
                };
            };
        };
    };
};
var censor = function (dictMonoid) {
    return function (dictMonad) {
        return function (dictMonadWriter) {
            return function (f) {
                return function (m) {
                    return pass(dictMonadWriter)(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v, f));
                    }));
                };
            };
        };
    };
};
module.exports = {
    MonadWriter: MonadWriter, 
    censor: censor, 
    listens: listens, 
    tell: tell, 
    pass: pass, 
    listen: listen, 
    writer: writer
};

},{"Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Trans/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Monoid = require("Data.Monoid");
var Data_Tuple = require("Data.Tuple");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Cont_Class = require("Control.Monad.Cont.Class");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Control_Monad_Reader_Class = require("Control.Monad.Reader.Class");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var WriterT = function (x) {
    return x;
};
var runWriterT = function (v) {
    return v;
};
var monadTransWriterT = function (dictMonoid) {
    return new Control_Monad_Trans.MonadTrans(function (dictMonad) {
        return function (m) {
            return WriterT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v, Data_Monoid.mempty(dictMonoid)));
            }));
        };
    });
};
var mapWriterT = function (f) {
    return function (m) {
        return WriterT(f(runWriterT(m)));
    };
};
var functorWriterT = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return mapWriterT(Prelude["<$>"](dictFunctor)(function (v) {
            return new Data_Tuple.Tuple(f(v.value0), v.value1);
        }));
    });
};
var execWriterT = function (dictApply) {
    return function (m) {
        return Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(Data_Tuple.snd)(runWriterT(m));
    };
};
var applyWriterT = function (dictSemigroup) {
    return function (dictApply) {
        return new Prelude.Apply(function () {
            return functorWriterT(dictApply["__superclass_Prelude.Functor_0"]());
        }, function (f) {
            return function (v) {
                return WriterT((function () {
                    var k = function (v1) {
                        return function (v2) {
                            return new Data_Tuple.Tuple(v1.value0(v2.value0), Prelude["<>"](dictSemigroup)(v1.value1)(v2.value1));
                        };
                    };
                    return Prelude["<*>"](dictApply)(Prelude["<$>"](dictApply["__superclass_Prelude.Functor_0"]())(k)(runWriterT(f)))(runWriterT(v));
                })());
            };
        });
    };
};
var bindWriterT = function (dictSemigroup) {
    return function (dictMonad) {
        return new Prelude.Bind(function () {
            return applyWriterT(dictSemigroup)((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]());
        }, function (m) {
            return function (k) {
                return WriterT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runWriterT(m))(function (v) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runWriterT(k(v.value0)))(function (v1) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v1.value0, Prelude["<>"](dictSemigroup)(v.value1)(v1.value1)));
                    });
                }));
            };
        });
    };
};
var applicativeWriterT = function (dictMonoid) {
    return function (dictApplicative) {
        return new Prelude.Applicative(function () {
            return applyWriterT(dictMonoid["__superclass_Prelude.Semigroup_0"]())(dictApplicative["__superclass_Prelude.Apply_0"]());
        }, function (a) {
            return WriterT(Prelude.pure(dictApplicative)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
        });
    };
};
var monadWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Prelude.Monad(function () {
            return applicativeWriterT(dictMonoid)(dictMonad["__superclass_Prelude.Applicative_0"]());
        }, function () {
            return bindWriterT(dictMonoid["__superclass_Prelude.Semigroup_0"]())(dictMonad);
        });
    };
};
var monadContWriterT = function (dictMonoid) {
    return function (dictMonadCont) {
        return new Control_Monad_Cont_Class.MonadCont(function () {
            return monadWriterT(dictMonoid)(dictMonadCont["__superclass_Prelude.Monad_0"]());
        }, function (f) {
            return WriterT(Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                return runWriterT(f(function (a) {
                    return WriterT(c(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
                }));
            }));
        });
    };
};
var monadEffWriter = function (dictMonad) {
    return function (dictMonoid) {
        return function (dictMonadEff) {
            return new Control_Monad_Eff_Class.MonadEff(function () {
                return monadWriterT(dictMonoid)(dictMonad);
            }, function ($78) {
                return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonad)(Control_Monad_Eff_Class.liftEff(dictMonadEff)($78));
            });
        };
    };
};
var monadErrorWriterT = function (dictMonoid) {
    return function (dictMonadError) {
        return new Control_Monad_Error_Class.MonadError(function () {
            return monadWriterT(dictMonoid)(dictMonadError["__superclass_Prelude.Monad_0"]());
        }, function (m) {
            return function (h) {
                return WriterT(Control_Monad_Error_Class.catchError(dictMonadError)(runWriterT(m))(function (e) {
                    return runWriterT(h(e));
                }));
            };
        }, function (e) {
            return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadError["__superclass_Prelude.Monad_0"]())(Control_Monad_Error_Class.throwError(dictMonadError)(e));
        });
    };
};
var monadReaderWriterT = function (dictMonoid) {
    return function (dictMonadReader) {
        return new Control_Monad_Reader_Class.MonadReader(function () {
            return monadWriterT(dictMonoid)(dictMonadReader["__superclass_Prelude.Monad_0"]());
        }, Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadReader["__superclass_Prelude.Monad_0"]())(Control_Monad_Reader_Class.ask(dictMonadReader)), function (f) {
            return mapWriterT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
        });
    };
};
var monadRecWriterT = function (dictMonoid) {
    return function (dictMonadRec) {
        return new Control_Monad_Rec_Class.MonadRec(function () {
            return monadWriterT(dictMonoid)(dictMonadRec["__superclass_Prelude.Monad_0"]());
        }, function (f) {
            return function (a) {
                var fʹ = function (v) {
                    return Prelude.bind((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())(runWriterT(f(v.value0)))(function (v1) {
                        return Prelude["return"]((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Applicative_0"]())((function () {
                            if (v1.value0 instanceof Data_Either.Left) {
                                return new Data_Either.Left(new Data_Tuple.Tuple(v1.value0.value0, Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value1)(v1.value1)));
                            };
                            if (v1.value0 instanceof Data_Either.Right) {
                                return new Data_Either.Right(new Data_Tuple.Tuple(v1.value0.value0, Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.value1)(v1.value1)));
                            };
                            throw new Error("Failed pattern match at Control.Monad.Writer.Trans line 78, column 5 - line 84, column 1: " + [ v1.value0.constructor.name ]);
                        })());
                    });
                };
                return WriterT(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(fʹ)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid))));
            };
        });
    };
};
var monadStateWriterT = function (dictMonoid) {
    return function (dictMonadState) {
        return new Control_Monad_State_Class.MonadState(function () {
            return monadWriterT(dictMonoid)(dictMonadState["__superclass_Prelude.Monad_0"]());
        }, function (f) {
            return Control_Monad_Trans.lift(monadTransWriterT(dictMonoid))(dictMonadState["__superclass_Prelude.Monad_0"]())(Control_Monad_State_Class.state(dictMonadState)(f));
        });
    };
};
var monadWriterWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return new Control_Monad_Writer_Class.MonadWriter(function () {
            return monadWriterT(dictMonoid)(dictMonad);
        }, function (m) {
            return WriterT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runWriterT(m))(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0, v.value1), v.value1));
            }));
        }, function (m) {
            return WriterT(Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(runWriterT(m))(function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Data_Tuple.Tuple(v.value0.value0, v.value0.value1(v.value1)));
            }));
        }, function ($79) {
            return WriterT(Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())($79));
        });
    };
};
var altWriterT = function (dictAlt) {
    return new Control_Alt.Alt(function () {
        return functorWriterT(dictAlt["__superclass_Prelude.Functor_0"]());
    }, function (m) {
        return function (n) {
            return WriterT(Control_Alt["<|>"](dictAlt)(runWriterT(m))(runWriterT(n)));
        };
    });
};
var plusWriterT = function (dictPlus) {
    return new Control_Plus.Plus(function () {
        return altWriterT(dictPlus["__superclass_Control.Alt.Alt_0"]());
    }, Control_Plus.empty(dictPlus));
};
var alternativeWriterT = function (dictMonoid) {
    return function (dictAlternative) {
        return new Control_Alternative.Alternative(function () {
            return plusWriterT(dictAlternative["__superclass_Control.Plus.Plus_1"]());
        }, function () {
            return applicativeWriterT(dictMonoid)(dictAlternative["__superclass_Prelude.Applicative_0"]());
        });
    };
};
var monadPlusWriterT = function (dictMonoid) {
    return function (dictMonadPlus) {
        return new Control_MonadPlus.MonadPlus(function () {
            return alternativeWriterT(dictMonoid)(dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]());
        }, function () {
            return monadWriterT(dictMonoid)(dictMonadPlus["__superclass_Prelude.Monad_0"]());
        });
    };
};
module.exports = {
    WriterT: WriterT, 
    mapWriterT: mapWriterT, 
    execWriterT: execWriterT, 
    runWriterT: runWriterT, 
    functorWriterT: functorWriterT, 
    applyWriterT: applyWriterT, 
    applicativeWriterT: applicativeWriterT, 
    altWriterT: altWriterT, 
    plusWriterT: plusWriterT, 
    alternativeWriterT: alternativeWriterT, 
    bindWriterT: bindWriterT, 
    monadWriterT: monadWriterT, 
    monadRecWriterT: monadRecWriterT, 
    monadPlusWriterT: monadPlusWriterT, 
    monadTransWriterT: monadTransWriterT, 
    monadEffWriter: monadEffWriter, 
    monadContWriterT: monadContWriterT, 
    monadErrorWriterT: monadErrorWriterT, 
    monadReaderWriterT: monadReaderWriterT, 
    monadStateWriterT: monadStateWriterT, 
    monadWriterWriterT: monadWriterWriterT
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Monad.Cont.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Cont.Class/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","Control.Monad.Reader.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Reader.Class/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Identity = require("Data.Identity");
var Data_Monoid = require("Data.Monoid");
var Data_Tuple = require("Data.Tuple");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var Control_Monad_Writer_Trans = require("Control.Monad.Writer.Trans");
var runWriter = function ($0) {
    return Data_Identity.runIdentity(Control_Monad_Writer_Trans.runWriterT($0));
};
var mapWriter = function (f) {
    return Control_Monad_Writer_Trans.mapWriterT(function ($1) {
        return Data_Identity.Identity(f(Data_Identity.runIdentity($1)));
    });
};
var execWriter = function (m) {
    return Data_Tuple.snd(runWriter(m));
};
module.exports = {
    mapWriter: mapWriter, 
    execWriter: execWriter, 
    runWriter: runWriter
};

},{"Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Control.Monad.Writer.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Trans/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Monad/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var when = function (dictMonad) {
    return function (v) {
        return function (m) {
            if (v) {
                return m;
            };
            if (!v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude.unit);
            };
            throw new Error("Failed pattern match at Control.Monad line 8, column 1 - line 9, column 1: " + [ v.constructor.name, m.constructor.name ]);
        };
    };
};
var unless = function (dictMonad) {
    return function (v) {
        return function (m) {
            if (!v) {
                return m;
            };
            if (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Prelude.unit);
            };
            throw new Error("Failed pattern match at Control.Monad line 13, column 1 - line 14, column 1: " + [ v.constructor.name, m.constructor.name ]);
        };
    };
};
module.exports = {
    unless: unless, 
    when: when
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Alternative = require("Control.Alternative");
var Control_Plus = require("Control.Plus");
var MonadPlus = function (__superclass_Control$dotAlternative$dotAlternative_1, __superclass_Prelude$dotMonad_0) {
    this["__superclass_Control.Alternative.Alternative_1"] = __superclass_Control$dotAlternative$dotAlternative_1;
    this["__superclass_Prelude.Monad_0"] = __superclass_Prelude$dotMonad_0;
};
var monadPlusArray = new MonadPlus(function () {
    return Control_Alternative.alternativeArray;
}, function () {
    return Prelude.monadArray;
});
var guard = function (dictMonadPlus) {
    return function (v) {
        if (v) {
            return Prelude["return"]((dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]())["__superclass_Prelude.Applicative_0"]())(Prelude.unit);
        };
        if (!v) {
            return Control_Plus.empty((dictMonadPlus["__superclass_Control.Alternative.Alternative_1"]())["__superclass_Control.Plus.Plus_1"]());
        };
        throw new Error("Failed pattern match at Control.MonadPlus line 35, column 1 - line 36, column 1: " + [ v.constructor.name ]);
    };
};
module.exports = {
    MonadPlus: MonadPlus, 
    guard: guard, 
    monadPlusArray: monadPlusArray
};

},{"Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Plus = function (__superclass_Control$dotAlt$dotAlt_0, empty) {
    this["__superclass_Control.Alt.Alt_0"] = __superclass_Control$dotAlt$dotAlt_0;
    this.empty = empty;
};
var plusArray = new Plus(function () {
    return Control_Alt.altArray;
}, [  ]);
var empty = function (dict) {
    return dict.empty;
};
module.exports = {
    Plus: Plus, 
    empty: empty, 
    plusArray: plusArray
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.EventPhase/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Enum = require("Data.Enum");
var Data_Maybe = require("Data.Maybe");
var None = (function () {
    function None() {

    };
    None.value = new None();
    return None;
})();
var Capturing = (function () {
    function Capturing() {

    };
    Capturing.value = new Capturing();
    return Capturing;
})();
var AtTarget = (function () {
    function AtTarget() {

    };
    AtTarget.value = new AtTarget();
    return AtTarget;
})();
var Bubbling = (function () {
    function Bubbling() {

    };
    Bubbling.value = new Bubbling();
    return Bubbling;
})();
var toEnumEventPhase = function (v) {
    if (v === 0) {
        return new Data_Maybe.Just(None.value);
    };
    if (v === 1) {
        return new Data_Maybe.Just(Capturing.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(AtTarget.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(Bubbling.value);
    };
    return Data_Maybe.Nothing.value;
};
var fromEnumEventPhase = function (v) {
    if (v instanceof None) {
        return 0;
    };
    if (v instanceof Capturing) {
        return 1;
    };
    if (v instanceof AtTarget) {
        return 2;
    };
    if (v instanceof Bubbling) {
        return 3;
    };
    throw new Error("Failed pattern match at DOM.Event.EventPhase line 44, column 1 - line 45, column 1: " + [ v.constructor.name ]);
};
var eqEventPhase = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof None && v1 instanceof None) {
            return true;
        };
        if (v instanceof Capturing && v1 instanceof Capturing) {
            return true;
        };
        if (v instanceof AtTarget && v1 instanceof AtTarget) {
            return true;
        };
        if (v instanceof Bubbling && v1 instanceof Bubbling) {
            return true;
        };
        return false;
    };
});
var ordEventPhase = new Prelude.Ord(function () {
    return eqEventPhase;
}, function (x) {
    return function (y) {
        return Prelude.compare(Prelude.ordInt)(fromEnumEventPhase(x))(fromEnumEventPhase(y));
    };
});
var boundedEventPhase = new Prelude.Bounded(None.value, Bubbling.value);
var boundedOrdEventPhase = new Prelude.BoundedOrd(function () {
    return boundedEventPhase;
}, function () {
    return ordEventPhase;
});
var enumEventPhase = new Data_Enum.Enum(function () {
    return boundedEventPhase;
}, 4, fromEnumEventPhase, Data_Enum.defaultPred(toEnumEventPhase)(fromEnumEventPhase), Data_Enum.defaultSucc(toEnumEventPhase)(fromEnumEventPhase), toEnumEventPhase);
module.exports = {
    None: None, 
    Capturing: Capturing, 
    AtTarget: AtTarget, 
    Bubbling: Bubbling, 
    eqEventPhase: eqEventPhase, 
    ordEventPhase: ordEventPhase, 
    boundedEventPhase: boundedEventPhase, 
    boundedOrdEventPhase: boundedOrdEventPhase, 
    enumEventPhase: enumEventPhase
};

},{"Data.Enum":"/Users/anders/dev/ps/halo/ajax/output/Data.Enum/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.EventTarget/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module DOM.Event.EventTarget

exports.eventListener = function (fn) {
  return function (event) {
    return fn(event)();
  };
};

exports.addEventListener = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          target.addEventListener(type, listener, useCapture);
          return {};
        };
      };
    };
  };
};

exports.removeEventListener = function (type) {
  return function (listener) {
    return function (useCapture) {
      return function (target) {
        return function () {
          target.removeEventListener(type, listener, useCapture);
          return {};
        };
      };
    };
  };
};

exports.dispatchEvent = function (event) {
  return function (target) {
    return function () {
      return target.dispatchEvent(event);
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.EventTarget/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Exception = require("Control.Monad.Eff.Exception");
var DOM = require("DOM");
var DOM_Event_Types = require("DOM.Event.Types");
module.exports = {
    dispatchEvent: $foreign.dispatchEvent, 
    removeEventListener: $foreign.removeEventListener, 
    addEventListener: $foreign.addEventListener, 
    eventListener: $foreign.eventListener
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.EventTarget/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Exception":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","DOM.Event.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.Types/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.EventTypes/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var DOM_Event_Types = require("DOM.Event.Types");
var wheel = "wheel";
var waiting = "waiting";
var volumechange = "volumechange";
var visibilitychange = "visibilitychange";
var unload = "unload";
var transitionend = "transitionend";
var touchstart = "touchstart";
var touchmove = "touchmove";
var touchleave = "touchleave";
var touchenter = "touchenter";
var touchend = "touchend";
var touchcancel = "touchcancel";
var timeupdate = "timeupdate";
var timeout = "timeout";
var suspend = "suspend";
var submit = "submit";
var stalled = "stalled";
var show = "show";
var select = "select";
var seeking = "seeking";
var seeked = "seeked";
var scroll = "scroll";
var resize = "resize";
var reset = "reset";
var readystatechange = "readystatechange";
var ratechange = "ratechange";
var progress = "progress";
var popstate = "popstate";
var playing = "playing";
var play = "play";
var pause = "pause";
var paste = "paste";
var pageshow = "pageshow";
var pagehide = "pagehide";
var open = "open";
var mouseup = "mouseup";
var mouseover = "mouseover";
var mouseout = "mouseout";
var mousemove = "mousemove";
var mouseleave = "mouseleave";
var mouseenter = "mouseenter";
var mousedown = "mousedown";
var message = "message";
var loadstart = "loadstart";
var loadend = "loadend";
var loadedmetadata = "loadedmetadata";
var loadeddata = "loadeddata";
var load = "load";
var keyup = "keyup";
var keypress = "keypress";
var keydown = "keydown";
var invalid = "invalid";
var input = "input";
var hashchange = "hashchange";
var fullscreenerror = "fullscreenerror";
var fullscreenchange = "fullscreenchange";
var focus = "focus";
var error = "error";
var ended = "ended";
var emptied = "emptied";
var durationchange = "durationchange";
var drop = "drop";
var dragstart = "dragstart";
var dragover = "dragover";
var dragleave = "dragleave";
var dragenter = "dragenter";
var dragend = "dragend";
var drag = "drag";
var dblclick = "dblclick";
var cut = "cut";
var copy = "copy";
var contextmenu = "contextmenu";
var compositionupdate = "compositionupdate";
var compositionstart = "compositionstart";
var compositionend = "compositionend";
var complete = "complete";
var click = "click";
var change = "change";
var canplaythrough = "canplaythrough";
var canplay = "canplay";
var blur = "blur";
var beforeunload = "beforeunload";
var beforeprint = "beforeprint";
var audioprocess = "audioprocess";
var animationstart = "animationstart";
var animationiteration = "animationiteration";
var animationend = "animationend";
var abort = "abort";
module.exports = {
    wheel: wheel, 
    waiting: waiting, 
    volumechange: volumechange, 
    visibilitychange: visibilitychange, 
    unload: unload, 
    transitionend: transitionend, 
    touchstart: touchstart, 
    touchmove: touchmove, 
    touchleave: touchleave, 
    touchenter: touchenter, 
    touchend: touchend, 
    touchcancel: touchcancel, 
    timeupdate: timeupdate, 
    timeout: timeout, 
    suspend: suspend, 
    submit: submit, 
    stalled: stalled, 
    show: show, 
    select: select, 
    seeking: seeking, 
    seeked: seeked, 
    scroll: scroll, 
    resize: resize, 
    reset: reset, 
    readystatechange: readystatechange, 
    ratechange: ratechange, 
    progress: progress, 
    popstate: popstate, 
    playing: playing, 
    play: play, 
    pause: pause, 
    paste: paste, 
    pageshow: pageshow, 
    pagehide: pagehide, 
    open: open, 
    mouseup: mouseup, 
    mouseover: mouseover, 
    mouseout: mouseout, 
    mousemove: mousemove, 
    mouseleave: mouseleave, 
    mouseenter: mouseenter, 
    mousedown: mousedown, 
    message: message, 
    loadstart: loadstart, 
    loadend: loadend, 
    loadedmetadata: loadedmetadata, 
    loadeddata: loadeddata, 
    load: load, 
    keyup: keyup, 
    keypress: keypress, 
    keydown: keydown, 
    invalid: invalid, 
    input: input, 
    hashchange: hashchange, 
    fullscreenerror: fullscreenerror, 
    fullscreenchange: fullscreenchange, 
    focus: focus, 
    error: error, 
    ended: ended, 
    emptied: emptied, 
    durationchange: durationchange, 
    drop: drop, 
    dragstart: dragstart, 
    dragover: dragover, 
    dragleave: dragleave, 
    dragenter: dragenter, 
    dragend: dragend, 
    drag: drag, 
    dblclick: dblclick, 
    cut: cut, 
    copy: copy, 
    contextmenu: contextmenu, 
    compositionupdate: compositionupdate, 
    compositionstart: compositionstart, 
    compositionend: compositionend, 
    complete: complete, 
    click: click, 
    change: change, 
    canplaythrough: canplaythrough, 
    canplay: canplay, 
    blur: blur, 
    beforeunload: beforeunload, 
    beforeprint: beforeprint, 
    audioprocess: audioprocess, 
    animationstart: animationstart, 
    animationiteration: animationiteration, 
    animationend: animationend, 
    abort: abort
};

},{"DOM.Event.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.Types/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.Types/foreign.js":[function(require,module,exports){
/* global exports, EventTarget */
"use strict";

// module DOM.Event.Types

exports._readEventTarget = function (left) {
  return function (right) {
    return function (foreign) {
      return foreign instanceof EventTarget ? left("Value is not an EventTarget") : right(foreign);
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.Types/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Data_Either = require("Data.Either");
var Data_Foreign = require("Data.Foreign");
var Data_Foreign_Class = require("Data.Foreign.Class");
var Data_Maybe_Unsafe = require("Data.Maybe.Unsafe");
var Unsafe_Coerce = require("Unsafe.Coerce");
var DOM = require("DOM");
var DOM_Event_EventPhase = require("DOM.Event.EventPhase");
var EventType = function (x) {
    return x;
};
var unsafeToEvent = Unsafe_Coerce.unsafeCoerce;
var userProximityEventToEvent = unsafeToEvent;
var wheelEventToEvent = unsafeToEvent;
var uiEventToEvent = unsafeToEvent;
var transitionEventToEvent = unsafeToEvent;
var touchEventToEvent = unsafeToEvent;
var timeEventToEvent = unsafeToEvent;
var svgZoomEventToEvent = unsafeToEvent;
var svgEventToEvent = unsafeToEvent;
var storageEventToEvent = unsafeToEvent;
var sensorEventToEvent = unsafeToEvent;
var rtcPeerConnectionIceEventToEvent = unsafeToEvent;
var rtcIdentityEventToEvent = unsafeToEvent;
var rtcIdentityErrorEventToEvent = unsafeToEvent;
var rtcDataChannelEventToEvent = unsafeToEvent;
var relatedEventToEvent = unsafeToEvent;
var readWheelEvent = Data_Foreign.unsafeReadTagged("WheelEvent");
var readUserProximityEvent = Data_Foreign.unsafeReadTagged("UserProximityEvent");
var readUIEvent = Data_Foreign.unsafeReadTagged("UIEvent");
var readTransitionEvent = Data_Foreign.unsafeReadTagged("TransitionEvent");
var readTouchEvent = Data_Foreign.unsafeReadTagged("TouchEvent");
var readTimeEvent = Data_Foreign.unsafeReadTagged("TimeEvent");
var readStorageEvent = Data_Foreign.unsafeReadTagged("StorageEvent");
var readSensorEvent = Data_Foreign.unsafeReadTagged("SensorEvent");
var readSVGZoomEvent = Data_Foreign.unsafeReadTagged("SVGZoomEvent");
var readSVGEvent = Data_Foreign.unsafeReadTagged("SVGEvent");
var readRelatedEvent = Data_Foreign.unsafeReadTagged("RelatedEvent");
var readRTCPeerConnectionIceEvent = Data_Foreign.unsafeReadTagged("RTCPeerConnectionIceEvent");
var readRTCIdentityEvent = Data_Foreign.unsafeReadTagged("RTCIdentityEvent");
var readRTCIdentityErrorEvent = Data_Foreign.unsafeReadTagged("RTCIdentityErrorEvent");
var readRTCDataChannelEvent = Data_Foreign.unsafeReadTagged("RTCDataChannelEvent");
var readProgressEvent = Data_Foreign.unsafeReadTagged("ProgressEvent");
var readPopStateEvent = Data_Foreign.unsafeReadTagged("PopStateEvent");
var readPointerEvent = Data_Foreign.unsafeReadTagged("PointerEvent");
var readPageTransitionEvent = Data_Foreign.unsafeReadTagged("PageTransitionEvent");
var readOfflineAudioCompletionEvent = Data_Foreign.unsafeReadTagged("OfflineAudioCompletionEvent");
var readMutationEvent = Data_Foreign.unsafeReadTagged("MutationEvent");
var readMouseEvent = Data_Foreign.unsafeReadTagged("MouseEvent");
var readMessageEvent = Data_Foreign.unsafeReadTagged("MessageEvent");
var readMediaStreamEvent = Data_Foreign.unsafeReadTagged("MediaStreamEvent");
var readKeyboardEvent = Data_Foreign.unsafeReadTagged("KeyboardEvent");
var readInputEvent = Data_Foreign.unsafeReadTagged("InputEvent");
var readIDBVersionChangeEvent = Data_Foreign.unsafeReadTagged("IDBVersionChangeEvent");
var readHashChangeEvent = Data_Foreign.unsafeReadTagged("HashChangeEvent");
var readGamepadEvent = Data_Foreign.unsafeReadTagged("GamepadEvent");
var readFocusEvent = Data_Foreign.unsafeReadTagged("FocusEvent");
var readFetchEvent = Data_Foreign.unsafeReadTagged("FetchEvent");
var readEventTarget = $foreign._readEventTarget(Data_Either.Left.create)(Data_Either.Right.create);
var readErrorEvent = Data_Foreign.unsafeReadTagged("ErrorEvent");
var readEditingBeforeInputEvent = Data_Foreign.unsafeReadTagged("EditingBeforeInputEvent");
var readDragEvent = Data_Foreign.unsafeReadTagged("DragEvent");
var readDeviceProximityEvent = Data_Foreign.unsafeReadTagged("DeviceProximityEvent");
var readDeviceOrientationEvent = Data_Foreign.unsafeReadTagged("DeviceOrientationEvent");
var readDeviceMotionEvent = Data_Foreign.unsafeReadTagged("DeviceMotionEvent");
var readDeviceLightEvent = Data_Foreign.unsafeReadTagged("DeviceLightEvent");
var readDOMTransactionEvent = Data_Foreign.unsafeReadTagged("DOMTransactionEvent");
var readCustomEvent = Data_Foreign.unsafeReadTagged("CustomEvent");
var readCompositionEvent = Data_Foreign.unsafeReadTagged("CompositionEvent");
var readCloseEvent = Data_Foreign.unsafeReadTagged("CloseEvent");
var readClipboardEvent = Data_Foreign.unsafeReadTagged("ClipboardEvent");
var readCSSFontFaceLoadEvent = Data_Foreign.unsafeReadTagged("CSSFontFaceLoadEvent");
var readBlobEvent = Data_Foreign.unsafeReadTagged("BlobEvent");
var readBeforeUnloadEvent = Data_Foreign.unsafeReadTagged("BeforeUnloadEvent");
var readBeforeInputEvent = Data_Foreign.unsafeReadTagged("BeforeInputEvent");
var readAudioProcessingEvent = Data_Foreign.unsafeReadTagged("AudioProcessingEvent");
var readAnimationEvent = Data_Foreign.unsafeReadTagged("AnimationEvent");
var progressEventToEvent = unsafeToEvent;
var popStateEventToEvent = unsafeToEvent;
var pointerEventToEvent = unsafeToEvent;
var pageTransitionEventToEvent = unsafeToEvent;
var offlineAudioCompletionEventToEvent = unsafeToEvent;
var mutationEventToEvent = unsafeToEvent;
var mouseEventToEvent = unsafeToEvent;
var messageEventToEvent = unsafeToEvent;
var mediaStreamEventToEvent = unsafeToEvent;
var keyboardEventToEvent = unsafeToEvent;
var isForeignWheelEvent = new Data_Foreign_Class.IsForeign(readWheelEvent);
var isForeignUserProximityEvent = new Data_Foreign_Class.IsForeign(readUserProximityEvent);
var isForeignUIEvent = new Data_Foreign_Class.IsForeign(readUIEvent);
var isForeignTransitionEvent = new Data_Foreign_Class.IsForeign(readTransitionEvent);
var isForeignTouchEvent = new Data_Foreign_Class.IsForeign(readTouchEvent);
var isForeignTimeEvent = new Data_Foreign_Class.IsForeign(readTimeEvent);
var isForeignStorageEvent = new Data_Foreign_Class.IsForeign(readStorageEvent);
var isForeignSensorEvent = new Data_Foreign_Class.IsForeign(readSensorEvent);
var isForeignSVGZoomEvent = new Data_Foreign_Class.IsForeign(readSVGZoomEvent);
var isForeignSVGEvent = new Data_Foreign_Class.IsForeign(readSVGEvent);
var isForeignRelatedEvent = new Data_Foreign_Class.IsForeign(readRelatedEvent);
var isForeignRTCPeerConnectionIceEvent = new Data_Foreign_Class.IsForeign(readRTCPeerConnectionIceEvent);
var isForeignRTCIdentityEvent = new Data_Foreign_Class.IsForeign(readRTCIdentityEvent);
var isForeignRTCIdentityErrorEvent = new Data_Foreign_Class.IsForeign(readRTCIdentityErrorEvent);
var isForeignRTCDataChannelEvent = new Data_Foreign_Class.IsForeign(readRTCDataChannelEvent);
var isForeignProgressEvent = new Data_Foreign_Class.IsForeign(readProgressEvent);
var isForeignPopStateEvent = new Data_Foreign_Class.IsForeign(readPopStateEvent);
var isForeignPointerEvent = new Data_Foreign_Class.IsForeign(readPointerEvent);
var isForeignPageTransitionEvent = new Data_Foreign_Class.IsForeign(readPageTransitionEvent);
var isForeignOfflineAudioCompletionEvent = new Data_Foreign_Class.IsForeign(readOfflineAudioCompletionEvent);
var isForeignMutationEvent = new Data_Foreign_Class.IsForeign(readMutationEvent);
var isForeignMouseEvent = new Data_Foreign_Class.IsForeign(readMouseEvent);
var isForeignMessageEvent = new Data_Foreign_Class.IsForeign(readMessageEvent);
var isForeignMediaStreamEvent = new Data_Foreign_Class.IsForeign(readMediaStreamEvent);
var isForeignKeyboardEvent = new Data_Foreign_Class.IsForeign(readKeyboardEvent);
var isForeignInputEvent = new Data_Foreign_Class.IsForeign(readInputEvent);
var isForeignIDBVersionChangeEvent = new Data_Foreign_Class.IsForeign(readIDBVersionChangeEvent);
var isForeignHashChangeEvent = new Data_Foreign_Class.IsForeign(readHashChangeEvent);
var isForeignGamepadEvent = new Data_Foreign_Class.IsForeign(readGamepadEvent);
var isForeignFocusEvent = new Data_Foreign_Class.IsForeign(readFocusEvent);
var isForeignFetchEvent = new Data_Foreign_Class.IsForeign(readFetchEvent);
var isForeignEventTarget = new Data_Foreign_Class.IsForeign(readEventTarget);
var isForeignErrorEvent = new Data_Foreign_Class.IsForeign(readErrorEvent);
var isForeignEditingBeforeInputEvent = new Data_Foreign_Class.IsForeign(readEditingBeforeInputEvent);
var isForeignDragEvent = new Data_Foreign_Class.IsForeign(readDragEvent);
var isForeignDeviceProximityEvent = new Data_Foreign_Class.IsForeign(readDeviceProximityEvent);
var isForeignDeviceOrientationEvent = new Data_Foreign_Class.IsForeign(readDeviceOrientationEvent);
var isForeignDeviceMotionEvent = new Data_Foreign_Class.IsForeign(readDeviceMotionEvent);
var isForeignDeviceLightEvent = new Data_Foreign_Class.IsForeign(readDeviceLightEvent);
var isForeignDOMTransactionEvent = new Data_Foreign_Class.IsForeign(readDOMTransactionEvent);
var isForeignCustomEvent = new Data_Foreign_Class.IsForeign(readCustomEvent);
var isForeignCompositionEvent = new Data_Foreign_Class.IsForeign(readCompositionEvent);
var isForeignCloseEvent = new Data_Foreign_Class.IsForeign(readCloseEvent);
var isForeignClipboardEvent = new Data_Foreign_Class.IsForeign(readClipboardEvent);
var isForeignCSSFontFaceLoadEvent = new Data_Foreign_Class.IsForeign(readCSSFontFaceLoadEvent);
var isForeignBlobEvent = new Data_Foreign_Class.IsForeign(readBlobEvent);
var isForeignBeforeUnloadEvent = new Data_Foreign_Class.IsForeign(readBeforeUnloadEvent);
var isForeignBeforeInputEvent = new Data_Foreign_Class.IsForeign(readBeforeInputEvent);
var isForeignAudioProcessingEvent = new Data_Foreign_Class.IsForeign(readAudioProcessingEvent);
var isForeignAnimationEvent = new Data_Foreign_Class.IsForeign(readAnimationEvent);
var inputEventToEvent = unsafeToEvent;
var idbVersionChangeEventToEvent = unsafeToEvent;
var hashChangeEventToEvent = unsafeToEvent;
var gamepadEventToEvent = unsafeToEvent;
var focusEventToEvent = unsafeToEvent;
var fetchEventToEvent = unsafeToEvent;
var errorEventToEvent = unsafeToEvent;
var eqEventType = new Prelude.Eq(function (v) {
    return function (v1) {
        return Prelude["=="](Prelude.eqString)(v)(v1);
    };
});
var ordEventType = new Prelude.Ord(function () {
    return eqEventType;
}, function (v) {
    return function (v1) {
        return Prelude.compare(Prelude.ordString)(v)(v1);
    };
});
var editingBeforeInputEventToEvent = unsafeToEvent;
var dragEventToEvent = unsafeToEvent;
var domTransactionEventToEvent = unsafeToEvent;
var deviceProximityEventToEvent = unsafeToEvent;
var deviceOrientationEventToEvent = unsafeToEvent;
var deviceMotionEventToEvent = unsafeToEvent;
var deviceLightEventToEvent = unsafeToEvent;
var customEventToEvent = unsafeToEvent;
var cssFontFaceLoadEventToEvent = unsafeToEvent;
var compositionEventToEvent = unsafeToEvent;
var closeEventToEvent = unsafeToEvent;
var clipboardEventToEvent = unsafeToEvent;
var blobEventToEvent = unsafeToEvent;
var beforeUnloadEventToEvent = unsafeToEvent;
var beforeInputEventToEvent = unsafeToEvent;
var audioProcessingEventToEvent = unsafeToEvent;
var animationEventToEvent = unsafeToEvent;
module.exports = {
    EventType: EventType, 
    readWheelEvent: readWheelEvent, 
    wheelEventToEvent: wheelEventToEvent, 
    readUserProximityEvent: readUserProximityEvent, 
    userProximityEventToEvent: userProximityEventToEvent, 
    readUIEvent: readUIEvent, 
    uiEventToEvent: uiEventToEvent, 
    readTransitionEvent: readTransitionEvent, 
    transitionEventToEvent: transitionEventToEvent, 
    readTouchEvent: readTouchEvent, 
    touchEventToEvent: touchEventToEvent, 
    readTimeEvent: readTimeEvent, 
    timeEventToEvent: timeEventToEvent, 
    readSVGZoomEvent: readSVGZoomEvent, 
    svgZoomEventToEvent: svgZoomEventToEvent, 
    readSVGEvent: readSVGEvent, 
    svgEventToEvent: svgEventToEvent, 
    readStorageEvent: readStorageEvent, 
    storageEventToEvent: storageEventToEvent, 
    readSensorEvent: readSensorEvent, 
    sensorEventToEvent: sensorEventToEvent, 
    readRTCPeerConnectionIceEvent: readRTCPeerConnectionIceEvent, 
    rtcPeerConnectionIceEventToEvent: rtcPeerConnectionIceEventToEvent, 
    readRTCIdentityEvent: readRTCIdentityEvent, 
    rtcIdentityEventToEvent: rtcIdentityEventToEvent, 
    readRTCIdentityErrorEvent: readRTCIdentityErrorEvent, 
    rtcIdentityErrorEventToEvent: rtcIdentityErrorEventToEvent, 
    readRTCDataChannelEvent: readRTCDataChannelEvent, 
    rtcDataChannelEventToEvent: rtcDataChannelEventToEvent, 
    readRelatedEvent: readRelatedEvent, 
    relatedEventToEvent: relatedEventToEvent, 
    readProgressEvent: readProgressEvent, 
    progressEventToEvent: progressEventToEvent, 
    readPopStateEvent: readPopStateEvent, 
    popStateEventToEvent: popStateEventToEvent, 
    readPointerEvent: readPointerEvent, 
    pointerEventToEvent: pointerEventToEvent, 
    readPageTransitionEvent: readPageTransitionEvent, 
    pageTransitionEventToEvent: pageTransitionEventToEvent, 
    readOfflineAudioCompletionEvent: readOfflineAudioCompletionEvent, 
    offlineAudioCompletionEventToEvent: offlineAudioCompletionEventToEvent, 
    readMutationEvent: readMutationEvent, 
    mutationEventToEvent: mutationEventToEvent, 
    readMouseEvent: readMouseEvent, 
    mouseEventToEvent: mouseEventToEvent, 
    readMessageEvent: readMessageEvent, 
    messageEventToEvent: messageEventToEvent, 
    readMediaStreamEvent: readMediaStreamEvent, 
    mediaStreamEventToEvent: mediaStreamEventToEvent, 
    readKeyboardEvent: readKeyboardEvent, 
    keyboardEventToEvent: keyboardEventToEvent, 
    readInputEvent: readInputEvent, 
    inputEventToEvent: inputEventToEvent, 
    readIDBVersionChangeEvent: readIDBVersionChangeEvent, 
    idbVersionChangeEventToEvent: idbVersionChangeEventToEvent, 
    readHashChangeEvent: readHashChangeEvent, 
    hashChangeEventToEvent: hashChangeEventToEvent, 
    readGamepadEvent: readGamepadEvent, 
    gamepadEventToEvent: gamepadEventToEvent, 
    readFocusEvent: readFocusEvent, 
    focusEventToEvent: focusEventToEvent, 
    readFetchEvent: readFetchEvent, 
    fetchEventToEvent: fetchEventToEvent, 
    readErrorEvent: readErrorEvent, 
    errorEventToEvent: errorEventToEvent, 
    readEditingBeforeInputEvent: readEditingBeforeInputEvent, 
    editingBeforeInputEventToEvent: editingBeforeInputEventToEvent, 
    readDragEvent: readDragEvent, 
    dragEventToEvent: dragEventToEvent, 
    readDOMTransactionEvent: readDOMTransactionEvent, 
    domTransactionEventToEvent: domTransactionEventToEvent, 
    readDeviceProximityEvent: readDeviceProximityEvent, 
    deviceProximityEventToEvent: deviceProximityEventToEvent, 
    readDeviceOrientationEvent: readDeviceOrientationEvent, 
    deviceOrientationEventToEvent: deviceOrientationEventToEvent, 
    readDeviceMotionEvent: readDeviceMotionEvent, 
    deviceMotionEventToEvent: deviceMotionEventToEvent, 
    readDeviceLightEvent: readDeviceLightEvent, 
    deviceLightEventToEvent: deviceLightEventToEvent, 
    readCustomEvent: readCustomEvent, 
    customEventToEvent: customEventToEvent, 
    readCSSFontFaceLoadEvent: readCSSFontFaceLoadEvent, 
    cssFontFaceLoadEventToEvent: cssFontFaceLoadEventToEvent, 
    readCompositionEvent: readCompositionEvent, 
    compositionEventToEvent: compositionEventToEvent, 
    readCloseEvent: readCloseEvent, 
    closeEventToEvent: closeEventToEvent, 
    readClipboardEvent: readClipboardEvent, 
    clipboardEventToEvent: clipboardEventToEvent, 
    readBlobEvent: readBlobEvent, 
    blobEventToEvent: blobEventToEvent, 
    readBeforeUnloadEvent: readBeforeUnloadEvent, 
    beforeUnloadEventToEvent: beforeUnloadEventToEvent, 
    readBeforeInputEvent: readBeforeInputEvent, 
    beforeInputEventToEvent: beforeInputEventToEvent, 
    readAudioProcessingEvent: readAudioProcessingEvent, 
    audioProcessingEventToEvent: audioProcessingEventToEvent, 
    readAnimationEvent: readAnimationEvent, 
    animationEventToEvent: animationEventToEvent, 
    readEventTarget: readEventTarget, 
    eqEventType: eqEventType, 
    ordEventType: ordEventType, 
    isForeignEventTarget: isForeignEventTarget, 
    isForeignAnimationEvent: isForeignAnimationEvent, 
    isForeignAudioProcessingEvent: isForeignAudioProcessingEvent, 
    isForeignBeforeInputEvent: isForeignBeforeInputEvent, 
    isForeignBeforeUnloadEvent: isForeignBeforeUnloadEvent, 
    isForeignBlobEvent: isForeignBlobEvent, 
    isForeignClipboardEvent: isForeignClipboardEvent, 
    isForeignCloseEvent: isForeignCloseEvent, 
    isForeignCompositionEvent: isForeignCompositionEvent, 
    isForeignCSSFontFaceLoadEvent: isForeignCSSFontFaceLoadEvent, 
    isForeignCustomEvent: isForeignCustomEvent, 
    isForeignDeviceLightEvent: isForeignDeviceLightEvent, 
    isForeignDeviceMotionEvent: isForeignDeviceMotionEvent, 
    isForeignDeviceOrientationEvent: isForeignDeviceOrientationEvent, 
    isForeignDeviceProximityEvent: isForeignDeviceProximityEvent, 
    isForeignDOMTransactionEvent: isForeignDOMTransactionEvent, 
    isForeignDragEvent: isForeignDragEvent, 
    isForeignEditingBeforeInputEvent: isForeignEditingBeforeInputEvent, 
    isForeignErrorEvent: isForeignErrorEvent, 
    isForeignFetchEvent: isForeignFetchEvent, 
    isForeignFocusEvent: isForeignFocusEvent, 
    isForeignGamepadEvent: isForeignGamepadEvent, 
    isForeignHashChangeEvent: isForeignHashChangeEvent, 
    isForeignIDBVersionChangeEvent: isForeignIDBVersionChangeEvent, 
    isForeignInputEvent: isForeignInputEvent, 
    isForeignKeyboardEvent: isForeignKeyboardEvent, 
    isForeignMediaStreamEvent: isForeignMediaStreamEvent, 
    isForeignMessageEvent: isForeignMessageEvent, 
    isForeignMouseEvent: isForeignMouseEvent, 
    isForeignMutationEvent: isForeignMutationEvent, 
    isForeignOfflineAudioCompletionEvent: isForeignOfflineAudioCompletionEvent, 
    isForeignPageTransitionEvent: isForeignPageTransitionEvent, 
    isForeignPointerEvent: isForeignPointerEvent, 
    isForeignPopStateEvent: isForeignPopStateEvent, 
    isForeignProgressEvent: isForeignProgressEvent, 
    isForeignRelatedEvent: isForeignRelatedEvent, 
    isForeignRTCDataChannelEvent: isForeignRTCDataChannelEvent, 
    isForeignRTCIdentityErrorEvent: isForeignRTCIdentityErrorEvent, 
    isForeignRTCIdentityEvent: isForeignRTCIdentityEvent, 
    isForeignRTCPeerConnectionIceEvent: isForeignRTCPeerConnectionIceEvent, 
    isForeignSensorEvent: isForeignSensorEvent, 
    isForeignStorageEvent: isForeignStorageEvent, 
    isForeignSVGEvent: isForeignSVGEvent, 
    isForeignSVGZoomEvent: isForeignSVGZoomEvent, 
    isForeignTimeEvent: isForeignTimeEvent, 
    isForeignTouchEvent: isForeignTouchEvent, 
    isForeignTransitionEvent: isForeignTransitionEvent, 
    isForeignUIEvent: isForeignUIEvent, 
    isForeignUserProximityEvent: isForeignUserProximityEvent, 
    isForeignWheelEvent: isForeignWheelEvent
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.Types/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","DOM.Event.EventPhase":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.EventPhase/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Foreign.Class":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Class/index.js","Data.Maybe.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js","Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.File.Types/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
module.exports = {};

},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module DOM.HTML.Types

exports._readHTMLElement = function (failure) {
  return function (success) {
    return function (value) {
      var tag = Object.prototype.toString.call(value);
      if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
        return success(value);
      } else {
        return failure(tag);
      }
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Foreign = require("Data.Foreign");
var Data_Foreign_Class = require("Data.Foreign.Class");
var DOM_Event_Types = require("DOM.Event.Types");
var DOM_Node_Types = require("DOM.Node.Types");
var Unsafe_Coerce = require("Unsafe.Coerce");
var windowToEventTarget = Unsafe_Coerce.unsafeCoerce;
var readHTMLElement = $foreign._readHTMLElement(function ($0) {
    return Data_Either.Left.create(Data_Foreign.TypeMismatch.create("HTMLElement")($0));
})(Data_Either.Right.create);
var readHTMLDocument = Data_Foreign.unsafeReadTagged("HTMLDocument");
var isForeignHTMLElement = new Data_Foreign_Class.IsForeign(readHTMLElement);
var isForeignHTMLDocument = new Data_Foreign_Class.IsForeign(readHTMLDocument);
var htmlElementToParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToNode = Unsafe_Coerce.unsafeCoerce;
var htmlElementToEventTarget = Unsafe_Coerce.unsafeCoerce;
var htmlElementToElement = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToNode = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToEventTarget = Unsafe_Coerce.unsafeCoerce;
var htmlDocumentToDocument = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    readHTMLElement: readHTMLElement, 
    htmlElementToEventTarget: htmlElementToEventTarget, 
    htmlElementToNode: htmlElementToNode, 
    htmlElementToNonDocumentTypeChildNode: htmlElementToNonDocumentTypeChildNode, 
    htmlElementToParentNode: htmlElementToParentNode, 
    htmlElementToElement: htmlElementToElement, 
    readHTMLDocument: readHTMLDocument, 
    htmlDocumentToEventTarget: htmlDocumentToEventTarget, 
    htmlDocumentToNode: htmlDocumentToNode, 
    htmlDocumentToParentNode: htmlDocumentToParentNode, 
    htmlDocumentToNonElementParentNode: htmlDocumentToNonElementParentNode, 
    htmlDocumentToDocument: htmlDocumentToDocument, 
    windowToEventTarget: windowToEventTarget, 
    isForeignHTMLDocument: isForeignHTMLDocument, 
    isForeignHTMLElement: isForeignHTMLElement
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/foreign.js","DOM.Event.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.Types/index.js","DOM.Node.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Types/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Foreign.Class":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Class/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js","Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Window/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module DOM.HTML.Window

exports.document = function (window) {
  return function () {
    return window.document;
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Window/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("Control.Monad.Eff");
var DOM = require("DOM");
var DOM_HTML_Types = require("DOM.HTML.Types");
module.exports = {
    document: $foreign.document
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Window/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML/foreign.js":[function(require,module,exports){
/* global exports, window */
"use strict";

// module DOM.HTML

exports.window = function () {
  return window;
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("Control.Monad.Eff");
var DOM = require("DOM");
var DOM_HTML_Types = require("DOM.HTML.Types");
module.exports = {
    window: $foreign.window
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Node/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module DOM.Node.Node

var getEffProp = function (name) {
  return function (node) {
    return function () {
      return node[name];
    };
  };
};

exports.nodeTypeIndex = function (node) {
  return node.nodeType;
};

exports.nodeName = function (node) {
  return node.nodeName;
};

exports.baseURI = getEffProp("baseURI");

exports.ownerDocument = getEffProp("ownerDocument");

exports.parentNode = getEffProp("parentNode");

exports.parentElement = getEffProp("parentElement");

exports.hasChildNodes = function (node) {
  return function () {
    return node.hasChildNodes();
  };
};

exports.childNodes = getEffProp("childNodes");

exports.firstChild = getEffProp("firstChild");

exports.lastChild = getEffProp("lastChild");

exports.previousSibling = getEffProp("previousSibling");

exports.nextSibling = getEffProp("nextSibling");

exports.nodeValue = getEffProp("nodeValue");

exports.setNodeValue = function (value) {
  return function (node) {
    return function () {
      node.nodeValue = value;
      return {};
    };
  };
};

exports.textContent = getEffProp("textContent");

exports.setTextContent = function (value) {
  return function (node) {
    return function () {
      node.textContent = value;
      return {};
    };
  };
};

exports.normalize = function (node) {
  return function () {
    node.normalize();
    return {};
  };
};

exports.clone = function (node) {
  return function () {
    return node.clone(false);
  };
};

exports.deepClone = function (node) {
  return function () {
    return node.clone(false);
  };
};

exports.isEqualNode = function (node1) {
  return function (node2) {
    return function () {
      return node1.isEqualNode(node2);
    };
  };
};

exports.compareDocumentPositionBits = function (node1) {
  return function (node2) {
    return function () {
      return node1.compareDocumentPosition(node2);
    };
  };
};

exports.contains = function (node1) {
  return function (node2) {
    return function () {
      return node1.contains(node2);
    };
  };
};

exports.lookupPrefix = function (prefix) {
  return function (node) {
    return function () {
      return node.lookupPrefix(prefix);
    };
  };
};

exports.lookupNamespaceURI = function (ns) {
  return function (node) {
    return function () {
      return node.lookupNamespaceURI(ns);
    };
  };
};

exports.lookupNamespaceURI = function (ns) {
  return function (node) {
    return function () {
      return node.isDefaultNamespace(ns);
    };
  };
};

exports.insertBefore = function (node1) {
  return function (node2) {
    return function (parent) {
      return function () {
        return parent.insertBefore(node1, node2);
      };
    };
  };
};

exports.appendChild = function (node) {
  return function (parent) {
    return function () {
      return parent.appendChild(node);
    };
  };
};

exports.replaceChild = function (oldChild) {
  return function (newChild) {
    return function (parent) {
      return function () {
        return parent.replaceChild(oldChild, newChild);
      };
    };
  };
};

exports.removeChild = function (node) {
  return function (parent) {
    return function () {
      return parent.removeChild(node);
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Node/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Data_Enum = require("Data.Enum");
var Data_Nullable = require("Data.Nullable");
var Data_Maybe_Unsafe = require("Data.Maybe.Unsafe");
var DOM = require("DOM");
var DOM_Node_NodeType = require("DOM.Node.NodeType");
var DOM_Node_Types = require("DOM.Node.Types");
var nodeType = function ($0) {
    return Data_Maybe_Unsafe.fromJust(Data_Enum.toEnum(DOM_Node_NodeType.enumNodeType)($foreign.nodeTypeIndex($0)));
};
module.exports = {
    nodeType: nodeType, 
    removeChild: $foreign.removeChild, 
    replaceChild: $foreign.replaceChild, 
    appendChild: $foreign.appendChild, 
    insertBefore: $foreign.insertBefore, 
    isDefaultNamespace: $foreign.isDefaultNamespace, 
    lookupNamespaceURI: $foreign.lookupNamespaceURI, 
    lookupPrefix: $foreign.lookupPrefix, 
    contains: $foreign.contains, 
    compareDocumentPositionBits: $foreign.compareDocumentPositionBits, 
    isEqualNode: $foreign.isEqualNode, 
    deepClone: $foreign.deepClone, 
    clone: $foreign.clone, 
    normalize: $foreign.normalize, 
    setTextContent: $foreign.setTextContent, 
    textContent: $foreign.textContent, 
    setNodeValue: $foreign.setNodeValue, 
    nodeValue: $foreign.nodeValue, 
    nextSibling: $foreign.nextSibling, 
    previousSibling: $foreign.previousSibling, 
    lastChild: $foreign.lastChild, 
    firstChild: $foreign.firstChild, 
    childNodes: $foreign.childNodes, 
    hasChildNodes: $foreign.hasChildNodes, 
    parentElement: $foreign.parentElement, 
    parentNode: $foreign.parentNode, 
    ownerDocument: $foreign.ownerDocument, 
    baseURI: $foreign.baseURI, 
    nodeName: $foreign.nodeName, 
    nodeTypeIndex: $foreign.nodeTypeIndex
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Node/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","DOM.Node.NodeType":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.NodeType/index.js","DOM.Node.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Types/index.js","Data.Enum":"/Users/anders/dev/ps/halo/ajax/output/Data.Enum/index.js","Data.Maybe.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/index.js","Data.Nullable":"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.NodeType/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Enum = require("Data.Enum");
var ElementNode = (function () {
    function ElementNode() {

    };
    ElementNode.value = new ElementNode();
    return ElementNode;
})();
var AttributeNode = (function () {
    function AttributeNode() {

    };
    AttributeNode.value = new AttributeNode();
    return AttributeNode;
})();
var TextNode = (function () {
    function TextNode() {

    };
    TextNode.value = new TextNode();
    return TextNode;
})();
var CDATASectionNode = (function () {
    function CDATASectionNode() {

    };
    CDATASectionNode.value = new CDATASectionNode();
    return CDATASectionNode;
})();
var EntityReferenceNode = (function () {
    function EntityReferenceNode() {

    };
    EntityReferenceNode.value = new EntityReferenceNode();
    return EntityReferenceNode;
})();
var EntityNode = (function () {
    function EntityNode() {

    };
    EntityNode.value = new EntityNode();
    return EntityNode;
})();
var ProcessingInstructionNode = (function () {
    function ProcessingInstructionNode() {

    };
    ProcessingInstructionNode.value = new ProcessingInstructionNode();
    return ProcessingInstructionNode;
})();
var CommentNode = (function () {
    function CommentNode() {

    };
    CommentNode.value = new CommentNode();
    return CommentNode;
})();
var DocumentNode = (function () {
    function DocumentNode() {

    };
    DocumentNode.value = new DocumentNode();
    return DocumentNode;
})();
var DocumentTypeNode = (function () {
    function DocumentTypeNode() {

    };
    DocumentTypeNode.value = new DocumentTypeNode();
    return DocumentTypeNode;
})();
var DocumentFragmentNode = (function () {
    function DocumentFragmentNode() {

    };
    DocumentFragmentNode.value = new DocumentFragmentNode();
    return DocumentFragmentNode;
})();
var NotationNode = (function () {
    function NotationNode() {

    };
    NotationNode.value = new NotationNode();
    return NotationNode;
})();
var toEnumNodeType = function (v) {
    if (v === 1) {
        return new Data_Maybe.Just(ElementNode.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(AttributeNode.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(TextNode.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(CDATASectionNode.value);
    };
    if (v === 5) {
        return new Data_Maybe.Just(EntityReferenceNode.value);
    };
    if (v === 6) {
        return new Data_Maybe.Just(EntityNode.value);
    };
    if (v === 7) {
        return new Data_Maybe.Just(ProcessingInstructionNode.value);
    };
    if (v === 8) {
        return new Data_Maybe.Just(CommentNode.value);
    };
    if (v === 9) {
        return new Data_Maybe.Just(DocumentNode.value);
    };
    if (v === 10) {
        return new Data_Maybe.Just(DocumentTypeNode.value);
    };
    if (v === 11) {
        return new Data_Maybe.Just(DocumentFragmentNode.value);
    };
    if (v === 12) {
        return new Data_Maybe.Just(NotationNode.value);
    };
    return Data_Maybe.Nothing.value;
};
var fromEnumNodeType = function (v) {
    if (v instanceof ElementNode) {
        return 1;
    };
    if (v instanceof AttributeNode) {
        return 2;
    };
    if (v instanceof TextNode) {
        return 3;
    };
    if (v instanceof CDATASectionNode) {
        return 4;
    };
    if (v instanceof EntityReferenceNode) {
        return 5;
    };
    if (v instanceof EntityNode) {
        return 6;
    };
    if (v instanceof ProcessingInstructionNode) {
        return 7;
    };
    if (v instanceof CommentNode) {
        return 8;
    };
    if (v instanceof DocumentNode) {
        return 9;
    };
    if (v instanceof DocumentTypeNode) {
        return 10;
    };
    if (v instanceof DocumentFragmentNode) {
        return 11;
    };
    if (v instanceof NotationNode) {
        return 12;
    };
    throw new Error("Failed pattern match at DOM.Node.NodeType line 67, column 1 - line 68, column 1: " + [ v.constructor.name ]);
};
var eqNodeType = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof ElementNode && v1 instanceof ElementNode) {
            return true;
        };
        if (v instanceof AttributeNode && v1 instanceof AttributeNode) {
            return true;
        };
        if (v instanceof TextNode && v1 instanceof TextNode) {
            return true;
        };
        if (v instanceof CDATASectionNode && v1 instanceof CDATASectionNode) {
            return true;
        };
        if (v instanceof EntityReferenceNode && v1 instanceof EntityReferenceNode) {
            return true;
        };
        if (v instanceof EntityNode && v1 instanceof EntityNode) {
            return true;
        };
        if (v instanceof ProcessingInstructionNode && v1 instanceof ProcessingInstructionNode) {
            return true;
        };
        if (v instanceof CommentNode && v1 instanceof CommentNode) {
            return true;
        };
        if (v instanceof DocumentNode && v1 instanceof DocumentNode) {
            return true;
        };
        if (v instanceof DocumentTypeNode && v1 instanceof DocumentTypeNode) {
            return true;
        };
        if (v instanceof DocumentFragmentNode && v1 instanceof DocumentFragmentNode) {
            return true;
        };
        if (v instanceof NotationNode && v1 instanceof NotationNode) {
            return true;
        };
        return false;
    };
});
var ordNodeType = new Prelude.Ord(function () {
    return eqNodeType;
}, function (x) {
    return function (y) {
        return Prelude.compare(Prelude.ordInt)(fromEnumNodeType(x))(fromEnumNodeType(y));
    };
});
var boundedNodeType = new Prelude.Bounded(ElementNode.value, NotationNode.value);
var boundedOrdNodeType = new Prelude.BoundedOrd(function () {
    return boundedNodeType;
}, function () {
    return ordNodeType;
});
var enumNodeType = new Data_Enum.Enum(function () {
    return boundedNodeType;
}, 4, fromEnumNodeType, Data_Enum.defaultPred(toEnumNodeType)(fromEnumNodeType), Data_Enum.defaultSucc(toEnumNodeType)(fromEnumNodeType), toEnumNodeType);
module.exports = {
    ElementNode: ElementNode, 
    AttributeNode: AttributeNode, 
    TextNode: TextNode, 
    CDATASectionNode: CDATASectionNode, 
    EntityReferenceNode: EntityReferenceNode, 
    EntityNode: EntityNode, 
    ProcessingInstructionNode: ProcessingInstructionNode, 
    CommentNode: CommentNode, 
    DocumentNode: DocumentNode, 
    DocumentTypeNode: DocumentTypeNode, 
    DocumentFragmentNode: DocumentFragmentNode, 
    NotationNode: NotationNode, 
    eqNodeType: eqNodeType, 
    ordNodeType: ordNodeType, 
    boundedNodeType: boundedNodeType, 
    boundedOrdNodeType: boundedOrdNodeType, 
    enumNodeType: enumNodeType
};

},{"Data.Enum":"/Users/anders/dev/ps/halo/ajax/output/Data.Enum/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.ParentNode/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module DOM.Node.ParentNode

var getEffProp = function (name) {
  return function (node) {
    return function () {
      return node[name];
    };
  };
};

exports.children = getEffProp("children");

exports.firstElementChild = getEffProp("firstElementChild");

exports.lastElementChild = getEffProp("lastElementChild");

exports.childElementCount = getEffProp("childElementCount");

exports.querySelector = function (selector) {
  return function (node) {
    return function () {
      return node.querySelector(selector);
    };
  };
};

exports.querySelectorAll = function (selector) {
  return function (node) {
    return function () {
      return node.querySelectorAll(selector);
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.ParentNode/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Data_Nullable = require("Data.Nullable");
var DOM = require("DOM");
var DOM_Node_Types = require("DOM.Node.Types");
module.exports = {
    querySelectorAll: $foreign.querySelectorAll, 
    querySelector: $foreign.querySelector, 
    childElementCount: $foreign.childElementCount, 
    lastElementChild: $foreign.lastElementChild, 
    firstElementChild: $foreign.firstElementChild, 
    children: $foreign.children
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.ParentNode/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","DOM.Node.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Types/index.js","Data.Nullable":"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Types/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var DOM_Event_Types = require("DOM.Event.Types");
var Unsafe_Coerce = require("Unsafe.Coerce");
var ElementId = function (x) {
    return x;
};
var textToNode = Unsafe_Coerce.unsafeCoerce;
var processingInstructionToNode = Unsafe_Coerce.unsafeCoerce;
var elementToParentNode = Unsafe_Coerce.unsafeCoerce;
var elementToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var elementToNode = Unsafe_Coerce.unsafeCoerce;
var elementToEventTarget = Unsafe_Coerce.unsafeCoerce;
var documentTypeToNode = Unsafe_Coerce.unsafeCoerce;
var documentToParentNode = Unsafe_Coerce.unsafeCoerce;
var documentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var documentToNode = Unsafe_Coerce.unsafeCoerce;
var documentToEventTarget = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToParentNode = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToNonElementParentNode = Unsafe_Coerce.unsafeCoerce;
var documentFragmentToNode = Unsafe_Coerce.unsafeCoerce;
var commentToNode = Unsafe_Coerce.unsafeCoerce;
var characterDataToNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    ElementId: ElementId, 
    documentTypeToNode: documentTypeToNode, 
    documentFragmentToNode: documentFragmentToNode, 
    documentFragmentToParentNode: documentFragmentToParentNode, 
    documentFragmentToNonElementParentNode: documentFragmentToNonElementParentNode, 
    processingInstructionToNode: processingInstructionToNode, 
    commentToNode: commentToNode, 
    textToNode: textToNode, 
    characterDataToNonDocumentTypeChildNode: characterDataToNonDocumentTypeChildNode, 
    elementToEventTarget: elementToEventTarget, 
    elementToNode: elementToNode, 
    elementToNonDocumentTypeChildNode: elementToNonDocumentTypeChildNode, 
    elementToParentNode: elementToParentNode, 
    documentToEventTarget: documentToEventTarget, 
    documentToNode: documentToNode, 
    documentToParentNode: documentToParentNode, 
    documentToNonElementParentNode: documentToNonElementParentNode
};

},{"DOM.Event.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.Types/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js","Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/DOM.XHR.Types/index.js":[function(require,module,exports){
arguments[4]["/Users/anders/dev/ps/halo/ajax/output/DOM.File.Types/index.js"][0].apply(exports,arguments)
},{}],"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js":[function(require,module,exports){
arguments[4]["/Users/anders/dev/ps/halo/ajax/output/DOM.File.Types/index.js"][0].apply(exports,arguments)
},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Array.ST/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Array.ST

exports.runSTArray = function (f) {
  return f;
};

exports.emptySTArray = function () {
  return [];
};

exports.peekSTArrayImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return function () {
          return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
        };
      };
    };
  };
};

exports.pokeSTArray = function (xs) {
  return function (i) {
    return function (a) {
      return function () {
        var ret = i >= 0 && i < xs.length;
        if (ret) xs[i] = a;
        return ret;
      };
    };
  };
};

exports.pushAllSTArray = function (xs) {
  return function (as) {
    return function () {
      return xs.push.apply(xs, as);
    };
  };
};

exports.spliceSTArray = function (xs) {
  return function (i) {
    return function (howMany) {
      return function (bs) {
        return function () {
          return xs.splice.apply(xs, [i, howMany].concat(bs));
        };
      };
    };
  };
};

exports.copyImpl = function (xs) {
  return function () {
    return xs.slice();
  };
};

exports.toAssocArray = function (xs) {
  return function () {
    var n = xs.length;
    var as = new Array(n);
    for (var i = 0; i < n; i++) as[i] = { value: xs[i], index: i };
    return as;
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Array.ST/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_ST = require("Control.Monad.ST");
var Data_Maybe = require("Data.Maybe");
var thaw = $foreign.copyImpl;
var pushSTArray = function (arr) {
    return function (a) {
        return $foreign.pushAllSTArray(arr)([ a ]);
    };
};
var peekSTArray = $foreign.peekSTArrayImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var freeze = $foreign.copyImpl;
module.exports = {
    thaw: thaw, 
    freeze: freeze, 
    pushSTArray: pushSTArray, 
    peekSTArray: peekSTArray, 
    toAssocArray: $foreign.toAssocArray, 
    spliceSTArray: $foreign.spliceSTArray, 
    pushAllSTArray: $foreign.pushAllSTArray, 
    pokeSTArray: $foreign.pokeSTArray, 
    emptySTArray: $foreign.emptySTArray, 
    runSTArray: $foreign.runSTArray
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Array.ST/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.ST":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.ST/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Array/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Array

//------------------------------------------------------------------------------
// Array creation --------------------------------------------------------------
//------------------------------------------------------------------------------

exports.range = function (start) {
  return function (end) {
    var step = start > end ? -1 : 1;
    var result = [];
    for (var i = start, n = 0; i !== end; i += step) {
      result[n++] = i;
    }
    result[n] = i;
    return result;
  };
};

exports.replicate = function (n) {
  return function (v) {
    if (n < 1) return [];
    var r = new Array(n);
    for (var i = 0; i < n; i++) r[i] = v;
    return r;
  };
};

//------------------------------------------------------------------------------
// Array size ------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.length = function (xs) {
  return xs.length;
};

//------------------------------------------------------------------------------
// Extending arrays ------------------------------------------------------------
//------------------------------------------------------------------------------

exports.cons = function (e) {
  return function (l) {
    return [e].concat(l);
  };
};

exports.snoc = function (l) {
  return function (e) {
    var l1 = l.slice();
    l1.push(e);
    return l1;
  };
};

//------------------------------------------------------------------------------
// Non-indexed reads -----------------------------------------------------------
//------------------------------------------------------------------------------

exports["uncons'"] = function (empty) {
  return function (next) {
    return function (xs) {
      return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
    };
  };
};

//------------------------------------------------------------------------------
// Indexed operations ----------------------------------------------------------
//------------------------------------------------------------------------------

exports.indexImpl = function (just) {
  return function (nothing) {
    return function (xs) {
      return function (i) {
        return i < 0 || i >= xs.length ? nothing :  just(xs[i]);
      };
    };
  };
};

exports.findIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = 0, l = xs.length; i < l; i++) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports.findLastIndexImpl = function (just) {
  return function (nothing) {
    return function (f) {
      return function (xs) {
        for (var i = xs.length - 1; i >= 0; i--) {
          if (f(xs[i])) return just(i);
        }
        return nothing;
      };
    };
  };
};

exports._insertAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i > l.length) return nothing;
          var l1 = l.slice();
          l1.splice(i, 0, a);
          return just(l1);
        };
      };
    };
  };
};

exports._deleteAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (l) {
        if (i < 0 || i >= l.length) return nothing;
        var l1 = l.slice();
        l1.splice(i, 1);
        return just(l1);
      };
    };
  };
};

exports._updateAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (a) {
        return function (l) {
          if (i < 0 || i >= l.length) return nothing;
          var l1 = l.slice();
          l1[i] = a;
          return just(l1);
        };
      };
    };
  };
};

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------

exports.reverse = function (l) {
  return l.slice().reverse();
};

exports.concat = function (xss) {
  var result = [];
  for (var i = 0, l = xss.length; i < l; i++) {
    var xs = xss[i];
    for (var j = 0, m = xs.length; j < m; j++) {
      result.push(xs[j]);
    }
  }
  return result;
};

exports.filter = function (f) {
  return function (xs) {
    return xs.filter(f);
  };
};

//------------------------------------------------------------------------------
// Sorting ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.sortImpl = function (f) {
  return function (l) {
    /* jshint maxparams: 2 */
    return l.slice().sort(function (x, y) {
      return f(x)(y);
    });
  };
};

//------------------------------------------------------------------------------
// Subarrays -------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.slice = function (s) {
  return function (e) {
    return function (l) {
      return l.slice(s, e);
    };
  };
};

exports.drop = function (n) {
  return function (l) {
    return n < 1 ? l : l.slice(n);
  };
};

//------------------------------------------------------------------------------
// Zipping ---------------------------------------------------------------------
//------------------------------------------------------------------------------

exports.zipWith = function (f) {
  return function (xs) {
    return function (ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(xs[i])(ys[i]);
      }
      return result;
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Array/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Lazy = require("Control.Lazy");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var Data_Foldable = require("Data.Foldable");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var Data_Traversable = require("Data.Traversable");
var Data_Tuple = require("Data.Tuple");
var Data_Maybe_Unsafe = require("Data.Maybe.Unsafe");
var $colon = $foreign.cons;
var $dot$dot = $foreign.range;
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = $foreign.zipWith(Data_Tuple.Tuple.create);
var updateAt = $foreign._updateAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unzip = $foreign["uncons'"](function (v) {
    return new Data_Tuple.Tuple([  ], [  ]);
})(function (v) {
    return function (ts) {
        var $34 = unzip(ts);
        return new Data_Tuple.Tuple($colon(v.value0)($34.value0), $colon(v.value1)($34.value1));
    };
});
var uncons = $foreign["uncons'"](Prelude["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
        return new Data_Maybe.Just({
            head: x, 
            tail: xs
        });
    };
});
var take = $foreign.slice(0);
var tail = $foreign["uncons'"](Prelude["const"](Data_Maybe.Nothing.value))(function (v) {
    return function (xs) {
        return new Data_Maybe.Just(xs);
    };
});
var span = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_xs) {
            var acc = __copy_acc;
            var xs = __copy_xs;
            tco: while (true) {
                var $40 = uncons(xs);
                if ($40 instanceof Data_Maybe.Just && p($40.value0.head)) {
                    var __tco_acc = $colon($40.value0.head)(acc);
                    acc = __tco_acc;
                    xs = $40.value0.tail;
                    continue tco;
                };
                return {
                    init: $foreign.reverse(acc), 
                    rest: xs
                };
            };
        };
    };
    return go([  ]);
};
var takeWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).init;
    };
};
var sortBy = function (comp) {
    return function (xs) {
        var compʹ = function (x) {
            return function (y) {
                var $44 = comp(x)(y);
                if ($44 instanceof Prelude.GT) {
                    return 1;
                };
                if ($44 instanceof Prelude.EQ) {
                    return 0;
                };
                if ($44 instanceof Prelude.LT) {
                    return -1;
                };
                throw new Error("Failed pattern match at Data.Array line 391, column 3 - line 396, column 1: " + [ $44.constructor.name ]);
            };
        };
        return $foreign.sortImpl(compʹ)(xs);
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Prelude.compare(dictOrd))(xs);
    };
};
var singleton = function (a) {
    return [ a ];
};
var replicateM = function (dictMonad) {
    return function (n) {
        return function (m) {
            if (n < 1) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())([  ]);
            };
            if (Prelude.otherwise) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictMonad["__superclass_Prelude.Applicative_0"]())($foreign.replicate(n)(m));
            };
            throw new Error("Failed pattern match at Data.Array line 118, column 1 - line 119, column 1: " + [ n.constructor.name, m.constructor.name ]);
        };
    };
};
var $$null = function (xs) {
    return $foreign.length(xs) === 0;
};
var nubBy = function (eq) {
    return function (xs) {
        var $47 = uncons(xs);
        if ($47 instanceof Data_Maybe.Just) {
            return $colon($47.value0.head)(nubBy(eq)($foreign.filter(function (y) {
                return !eq($47.value0.head)(y);
            })($47.value0.tail)));
        };
        if ($47 instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        throw new Error("Failed pattern match: " + [ $47.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Prelude.eq(dictEq));
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Prelude["<*>"]((dictAlternative["__superclass_Prelude.Applicative_0"]())["__superclass_Prelude.Apply_0"]())(Prelude["<$>"](((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())["__superclass_Prelude.Functor_0"]())($colon)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt["<|>"]((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())(some(dictAlternative)(dictLazy)(v))(Prelude.pure(dictAlternative["__superclass_Prelude.Applicative_0"]())([  ]));
        };
    };
};
var insertAt = $foreign._insertAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var init = function (xs) {
    if ($$null(xs)) {
        return Data_Maybe.Nothing.value;
    };
    if (Prelude.otherwise) {
        return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1)(xs));
    };
    throw new Error("Failed pattern match at Data.Array line 208, column 1 - line 209, column 1: " + [ xs.constructor.name ]);
};
var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var $bang$bang = index;
var last = function (xs) {
    return $bang$bang(xs)($foreign.length(xs) - 1);
};
var modifyAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                return updateAt(i)(f(x))(xs);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)($bang$bang(xs)(i));
        };
    };
};
var head = $foreign["uncons'"](Prelude["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (v) {
        return new Data_Maybe.Just(x);
    };
});
var groupBy = function (op) {
    var go = function (__copy_acc) {
        return function (__copy_xs) {
            var acc = __copy_acc;
            var xs = __copy_xs;
            tco: while (true) {
                var $52 = uncons(xs);
                if ($52 instanceof Data_Maybe.Just) {
                    var sp = span(op($52.value0.head))($52.value0.tail);
                    var __tco_acc = $colon($colon($52.value0.head)(sp.init))(acc);
                    acc = __tco_acc;
                    xs = sp.rest;
                    continue tco;
                };
                if ($52 instanceof Data_Maybe.Nothing) {
                    return $foreign.reverse(acc);
                };
                throw new Error("Failed pattern match at Data.Array line 458, column 1 - line 459, column 1: " + [ $52.constructor.name ]);
            };
        };
    };
    return go([  ]);
};
var group = function (dictEq) {
    return function (xs) {
        return groupBy(Prelude.eq(dictEq))(xs);
    };
};
var group$prime = function (dictOrd) {
    return function ($66) {
        return group(dictOrd["__superclass_Prelude.Eq_0"]())(sort(dictOrd)($66));
    };
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return $foreign["uncons'"](function (v) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(a);
            })(function (b) {
                return function (bs) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(f(a)(b))(function (aʹ) {
                        return foldM(dictMonad)(f)(aʹ)(bs);
                    });
                };
            });
        };
    };
};
var findLastIndex = $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var insertBy = function (cmp) {
    return function (x) {
        return function (ys) {
            var i = Data_Maybe.maybe(0)(function (v) {
                return v + 1 | 0;
            })(findLastIndex(function (y) {
                return Prelude["=="](Prelude.eqOrdering)(cmp(x)(y))(Prelude.GT.value);
            })(ys));
            return Data_Maybe_Unsafe.fromJust(insertAt(i)(x)(ys));
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Prelude.compare(dictOrd));
};
var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return $foreign.filter(function (x) {
                return Data_Maybe.isJust(findIndex(eq(x))(ys));
            })(xs);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Prelude.eq(dictEq));
};
var filterM = function (dictMonad) {
    return function (p) {
        return $foreign["uncons'"](function (v) {
            return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())([  ]);
        })(function (x) {
            return function (xs) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(p(x))(function (v) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(filterM(dictMonad)(p)(xs))(function (v1) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())((function () {
                            if (v) {
                                return $colon(x)(v1);
                            };
                            if (!v) {
                                return v1;
                            };
                            throw new Error("Failed pattern match: " + [ v.constructor.name ]);
                        })());
                    });
                });
            };
        });
    };
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Prelude["=="](dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Prelude["=="](dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).rest;
    };
};
var deleteAt = $foreign._deleteAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var deleteBy = function (eq) {
    return function (x) {
        return function (v) {
            if (v.length === 0) {
                return [  ];
            };
            return Data_Maybe.maybe(v)(function (i) {
                return Data_Maybe_Unsafe.fromJust(deleteAt(i)(v));
            })(findIndex(eq(x))(v));
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Prelude["++"](Prelude.semigroupArray)(xs)(Data_Foldable.foldl(Data_Foldable.foldableArray)(Prelude.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Prelude["=="](dictEq));
};
var $$delete = function (dictEq) {
    return deleteBy(Prelude.eq(dictEq));
};
var $bslash$bslash = function (dictEq) {
    return function (xs) {
        return function (ys) {
            if ($$null(xs)) {
                return [  ];
            };
            if (Prelude.otherwise) {
                return $foreign["uncons'"](Prelude["const"](xs))(function (y) {
                    return function (ys2) {
                        return $bslash$bslash(dictEq)($$delete(dictEq)(y)(xs))(ys2);
                    };
                })(ys);
            };
            throw new Error("Failed pattern match: " + [ xs.constructor.name, ys.constructor.name ]);
        };
    };
};
var concatMap = Prelude.flip(Prelude.bind(Prelude.bindArray));
var mapMaybe = function (f) {
    return concatMap(function ($67) {
        return Data_Maybe.maybe([  ])(singleton)(f($67));
    });
};
var catMaybes = mapMaybe(Prelude.id(Prelude.categoryFn));
var alterAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                var $64 = f(x);
                if ($64 instanceof Data_Maybe.Nothing) {
                    return deleteAt(i)(xs);
                };
                if ($64 instanceof Data_Maybe.Just) {
                    return updateAt(i)($64.value0)(xs);
                };
                throw new Error("Failed pattern match at Data.Array line 331, column 3 - line 340, column 1: " + [ $64.constructor.name ]);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)($bang$bang(xs)(i));
        };
    };
};
module.exports = {
    foldM: foldM, 
    unzip: unzip, 
    zip: zip, 
    zipWithA: zipWithA, 
    intersectBy: intersectBy, 
    intersect: intersect, 
    "\\\\": $bslash$bslash, 
    deleteBy: deleteBy, 
    "delete": $$delete, 
    unionBy: unionBy, 
    union: union, 
    nubBy: nubBy, 
    nub: nub, 
    groupBy: groupBy, 
    "group'": group$prime, 
    group: group, 
    span: span, 
    dropWhile: dropWhile, 
    takeWhile: takeWhile, 
    take: take, 
    sortBy: sortBy, 
    sort: sort, 
    catMaybes: catMaybes, 
    mapMaybe: mapMaybe, 
    filterM: filterM, 
    concatMap: concatMap, 
    alterAt: alterAt, 
    modifyAt: modifyAt, 
    updateAt: updateAt, 
    deleteAt: deleteAt, 
    insertAt: insertAt, 
    findLastIndex: findLastIndex, 
    findIndex: findIndex, 
    elemLastIndex: elemLastIndex, 
    elemIndex: elemIndex, 
    index: index, 
    "!!": $bang$bang, 
    uncons: uncons, 
    init: init, 
    tail: tail, 
    last: last, 
    head: head, 
    insertBy: insertBy, 
    insert: insert, 
    ":": $colon, 
    "null": $$null, 
    many: many, 
    some: some, 
    replicateM: replicateM, 
    "..": $dot$dot, 
    singleton: singleton, 
    zipWith: $foreign.zipWith, 
    drop: $foreign.drop, 
    slice: $foreign.slice, 
    filter: $foreign.filter, 
    concat: $foreign.concat, 
    reverse: $foreign.reverse, 
    snoc: $foreign.snoc, 
    cons: $foreign.cons, 
    length: $foreign.length, 
    replicate: $foreign.replicate, 
    range: $foreign.range
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Array/foreign.js","Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Lazy":"/Users/anders/dev/ps/halo/ajax/output/Control.Lazy/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.ArrayBuffer.Types/index.js":[function(require,module,exports){
arguments[4]["/Users/anders/dev/ps/halo/ajax/output/DOM.File.Types/index.js"][0].apply(exports,arguments)
},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Bifoldable/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Apply = require("Control.Apply");
var Data_Monoid = require("Data.Monoid");
var Data_Monoid_Disj = require("Data.Monoid.Disj");
var Data_Monoid_Conj = require("Data.Monoid.Conj");
var Data_Monoid_Endo = require("Data.Monoid.Endo");
var Data_Monoid_Dual = require("Data.Monoid.Dual");
var Bifoldable = function (bifoldMap, bifoldl, bifoldr) {
    this.bifoldMap = bifoldMap;
    this.bifoldl = bifoldl;
    this.bifoldr = bifoldr;
};
var bifoldr = function (dict) {
    return dict.bifoldr;
};
var bitraverse_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)(function ($18) {
                    return Control_Apply["*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(f($18));
                })(function ($19) {
                    return Control_Apply["*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(g($19));
                })(Prelude.pure(dictApplicative)(Prelude.unit));
            };
        };
    };
};
var bifor_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse_(dictBifoldable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
var bisequence_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return bitraverse_(dictBifoldable)(dictApplicative)(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn));
    };
};
var bifoldl = function (dict) {
    return dict.bifoldl;
};
var bifoldMapDefaultR = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return function (p) {
                    return bifoldr(dictBifoldable)(function ($20) {
                        return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f($20));
                    })(function ($21) {
                        return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(g($21));
                    })(Data_Monoid.mempty(dictMonoid))(p);
                };
            };
        };
    };
};
var bifoldMapDefaultL = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return function (p) {
                    return bifoldl(dictBifoldable)(function (m) {
                        return function (a) {
                            return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(m)(f(a));
                        };
                    })(function (m) {
                        return function (b) {
                            return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(m)(g(b));
                        };
                    })(Data_Monoid.mempty(dictMonoid))(p);
                };
            };
        };
    };
};
var bifoldMap = function (dict) {
    return dict.bifoldMap;
};
var bifoldlDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Monoid_Endo.runEndo(Data_Monoid_Dual.runDual(bifoldMap(dictBifoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo))(function ($22) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Prelude.flip(f)($22)));
                    })(function ($23) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Prelude.flip(g)($23)));
                    })(p)))(z);
                };
            };
        };
    };
};
var bifoldrDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Monoid_Endo.runEndo(bifoldMap(dictBifoldable)(Data_Monoid_Endo.monoidEndo)(function ($24) {
                        return Data_Monoid_Endo.Endo(f($24));
                    })(function ($25) {
                        return Data_Monoid_Endo.Endo(g($25));
                    })(p))(z);
                };
            };
        };
    };
};
var bifold = function (dictBifoldable) {
    return function (dictMonoid) {
        return bifoldMap(dictBifoldable)(dictMonoid)(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn));
    };
};
var biany = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($26) {
                    return Data_Monoid_Disj.runDisj(bifoldMap(dictBifoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra))(function ($27) {
                        return Data_Monoid_Disj.Disj(p($27));
                    })(function ($28) {
                        return Data_Monoid_Disj.Disj(q($28));
                    })($26));
                };
            };
        };
    };
};
var biall = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                return function ($29) {
                    return Data_Monoid_Conj.runConj(bifoldMap(dictBifoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra))(function ($30) {
                        return Data_Monoid_Conj.Conj(p($30));
                    })(function ($31) {
                        return Data_Monoid_Conj.Conj(q($31));
                    })($29));
                };
            };
        };
    };
};
module.exports = {
    Bifoldable: Bifoldable, 
    biall: biall, 
    biany: biany, 
    bisequence_: bisequence_, 
    bifor_: bifor_, 
    bitraverse_: bitraverse_, 
    bifold: bifold, 
    bifoldMapDefaultL: bifoldMapDefaultL, 
    bifoldMapDefaultR: bifoldMapDefaultR, 
    bifoldlDefault: bifoldlDefault, 
    bifoldrDefault: bifoldrDefault, 
    bifoldMap: bifoldMap, 
    bifoldl: bifoldl, 
    bifoldr: bifoldr
};

},{"Control.Apply":"/Users/anders/dev/ps/halo/ajax/output/Control.Apply/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Monoid.Conj":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Conj/index.js","Data.Monoid.Disj":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Disj/index.js","Data.Monoid.Dual":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Dual/index.js","Data.Monoid.Endo":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Endo/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Bifunctor = function (bimap) {
    this.bimap = bimap;
};
var bimap = function (dict) {
    return dict.bimap;
};
var lmap = function (dictBifunctor) {
    return function (f) {
        return bimap(dictBifunctor)(f)(Prelude.id(Prelude.categoryFn));
    };
};
var rmap = function (dictBifunctor) {
    return bimap(dictBifunctor)(Prelude.id(Prelude.categoryFn));
};
module.exports = {
    Bifunctor: Bifunctor, 
    rmap: rmap, 
    lmap: lmap, 
    bimap: bimap
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Bitraversable/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Bifoldable = require("Data.Bifoldable");
var Data_Bifunctor = require("Data.Bifunctor");
var Bitraversable = function (__superclass_Data$dotBifoldable$dotBifoldable_1, __superclass_Data$dotBifunctor$dotBifunctor_0, bisequence, bitraverse) {
    this["__superclass_Data.Bifoldable.Bifoldable_1"] = __superclass_Data$dotBifoldable$dotBifoldable_1;
    this["__superclass_Data.Bifunctor.Bifunctor_0"] = __superclass_Data$dotBifunctor$dotBifunctor_0;
    this.bisequence = bisequence;
    this.bitraverse = bitraverse;
};
var bitraverse = function (dict) {
    return dict.bitraverse;
};
var bisequenceDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return bitraverse(dictBitraversable)(dictApplicative)(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn))(t);
        };
    };
};
var bisequence = function (dict) {
    return dict.bisequence;
};
var bitraverseDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return function (t) {
                    return bisequence(dictBitraversable)(dictApplicative)(Data_Bifunctor.bimap(dictBitraversable["__superclass_Data.Bifunctor.Bifunctor_0"]())(f)(g)(t));
                };
            };
        };
    };
};
var bifor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse(dictBitraversable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
module.exports = {
    Bitraversable: Bitraversable, 
    bifor: bifor, 
    bisequenceDefault: bisequenceDefault, 
    bitraverseDefault: bitraverseDefault, 
    bisequence: bisequence, 
    bitraverse: bitraverse
};

},{"Data.Bifoldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifoldable/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.CatList/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var Data_Tuple = require("Data.Tuple");
var Data_CatQueue = require("Data.CatQueue");
var Data_List = require("Data.List");
var CatNil = (function () {
    function CatNil() {

    };
    CatNil.value = new CatNil();
    return CatNil;
})();
var CatCons = (function () {
    function CatCons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    CatCons.create = function (value0) {
        return function (value1) {
            return new CatCons(value0, value1);
        };
    };
    return CatCons;
})();
var showCatList = function (dictShow) {
    return new Prelude.Show(function (v) {
        if (v instanceof CatNil) {
            return "CatNil";
        };
        if (v instanceof CatCons) {
            return "CatList (" + (Prelude.show(dictShow)(v.value0) + (") (" + (Prelude.show(Data_CatQueue.showCatQueue(showCatList(dictShow)))(v.value1) + ")")));
        };
        throw new Error("Failed pattern match: " + [ v.constructor.name ]);
    });
};
var $$null = function (v) {
    if (v instanceof CatNil) {
        return true;
    };
    return false;
};
var link = function (v) {
    return function (cat) {
        if (v instanceof CatNil) {
            return cat;
        };
        if (v instanceof CatCons) {
            return new CatCons(v.value0, Data_CatQueue.snoc(v.value1)(cat));
        };
        throw new Error("Failed pattern match at Data.CatList line 88, column 1 - line 89, column 1: " + [ v.constructor.name, cat.constructor.name ]);
    };
};
var foldr = function (k) {
    return function (b) {
        return function (q) {
            var foldl = function (__copy_k1) {
                return function (__copy_b1) {
                    return function (__copy_v) {
                        var k1 = __copy_k1;
                        var b1 = __copy_b1;
                        var v = __copy_v;
                        tco: while (true) {
                            var b2 = b1;
                            if (v instanceof Data_List.Nil) {
                                return b2;
                            };
                            if (v instanceof Data_List.Cons) {
                                var __tco_k1 = k1;
                                var __tco_b1 = k1(b1)(v.value0);
                                var __tco_v = v.value1;
                                k1 = __tco_k1;
                                b1 = __tco_b1;
                                v = __tco_v;
                                continue tco;
                            };
                            throw new Error("Failed pattern match at Data.CatList line 95, column 1 - line 96, column 1: " + [ k1.constructor.name, b1.constructor.name, v.constructor.name ]);
                        };
                    };
                };
            };
            var go = function (__copy_xs) {
                return function (__copy_ys) {
                    var xs = __copy_xs;
                    var ys = __copy_ys;
                    tco: while (true) {
                        var $21 = Data_CatQueue.uncons(xs);
                        if ($21 instanceof Data_Maybe.Nothing) {
                            return foldl(function (x) {
                                return function (i) {
                                    return i(x);
                                };
                            })(b)(ys);
                        };
                        if ($21 instanceof Data_Maybe.Just) {
                            var __tco_ys = new Data_List.Cons(k($21.value0.value0), ys);
                            xs = $21.value0.value1;
                            ys = __tco_ys;
                            continue tco;
                        };
                        throw new Error("Failed pattern match at Data.CatList line 95, column 1 - line 96, column 1: " + [ $21.constructor.name ]);
                    };
                };
            };
            return go(q)(Data_List.Nil.value);
        };
    };
};
var uncons = function (v) {
    if (v instanceof CatNil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof CatCons) {
        return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, (function () {
            var $26 = Data_CatQueue["null"](v.value1);
            if ($26) {
                return CatNil.value;
            };
            if (!$26) {
                return foldr(link)(CatNil.value)(v.value1);
            };
            throw new Error("Failed pattern match at Data.CatList line 79, column 1 - line 80, column 1: " + [ $26.constructor.name ]);
        })()));
    };
    throw new Error("Failed pattern match at Data.CatList line 79, column 1 - line 80, column 1: " + [ v.constructor.name ]);
};
var empty = CatNil.value;
var append = function (v) {
    return function (v1) {
        if (v1 instanceof CatNil) {
            return v;
        };
        if (v instanceof CatNil) {
            return v1;
        };
        return link(v)(v1);
    };
};
var cons = function (a) {
    return function (cat) {
        return append(new CatCons(a, Data_CatQueue.empty))(cat);
    };
};
var semigroupCatList = new Prelude.Semigroup(append);
var monoidCatList = new Data_Monoid.Monoid(function () {
    return semigroupCatList;
}, CatNil.value);
var snoc = function (cat) {
    return function (a) {
        return append(cat)(new CatCons(a, Data_CatQueue.empty));
    };
};
module.exports = {
    CatNil: CatNil, 
    CatCons: CatCons, 
    uncons: uncons, 
    snoc: snoc, 
    cons: cons, 
    append: append, 
    "null": $$null, 
    empty: empty, 
    semigroupCatList: semigroupCatList, 
    monoidCatList: monoidCatList, 
    showCatList: showCatList
};

},{"Data.CatQueue":"/Users/anders/dev/ps/halo/ajax/output/Data.CatQueue/index.js","Data.List":"/Users/anders/dev/ps/halo/ajax/output/Data.List/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.CatQueue/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_List = require("Data.List");
var Data_Maybe = require("Data.Maybe");
var Data_Tuple = require("Data.Tuple");
var CatQueue = (function () {
    function CatQueue(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    CatQueue.create = function (value0) {
        return function (value1) {
            return new CatQueue(value0, value1);
        };
    };
    return CatQueue;
})();
var uncons = function (__copy_v) {
    var v = __copy_v;
    tco: while (true) {
        if (v.value0 instanceof Data_List.Nil && v.value1 instanceof Data_List.Nil) {
            return Data_Maybe.Nothing.value;
        };
        if (v.value0 instanceof Data_List.Nil) {
            var __tco_v = new CatQueue(Data_List.reverse(v.value1), Data_List.Nil.value);
            v = __tco_v;
            continue tco;
        };
        if (v.value0 instanceof Data_List.Cons) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
        };
        throw new Error("Failed pattern match: " + [ v.constructor.name ]);
    };
};
var snoc = function (v) {
    return function (a) {
        return new CatQueue(v.value0, new Data_List.Cons(a, v.value1));
    };
};
var showCatQueue = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "CatQueue (" + (Prelude.show(Data_List.showList(dictShow))(v.value0) + (") (" + (Prelude.show(Data_List.showList(dictShow))(v.value1) + ")")));
    });
};
var $$null = function (v) {
    if (v.value0 instanceof Data_List.Nil && v.value1 instanceof Data_List.Nil) {
        return true;
    };
    return false;
};
var empty = new CatQueue(Data_List.Nil.value, Data_List.Nil.value);
module.exports = {
    CatQueue: CatQueue, 
    uncons: uncons, 
    snoc: snoc, 
    "null": $$null, 
    empty: empty, 
    showCatQueue: showCatQueue
};

},{"Data.List":"/Users/anders/dev/ps/halo/ajax/output/Data.List/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Char/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Char

exports.toString = function (c) {
  return c;
};

exports.toCharCode = function (c) {
  return c.charCodeAt(0);
};

exports.fromCharCode = function (c) {
  return String.fromCharCode(c);
};

exports.toLower = function (c) {
  return c.toLowerCase();
};

exports.toUpper = function (c) {
  return c.toUpperCase();
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Char/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
module.exports = {
    toUpper: $foreign.toUpper, 
    toLower: $foreign.toLower, 
    toCharCode: $foreign.toCharCode, 
    fromCharCode: $foreign.fromCharCode, 
    toString: $foreign.toString
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Char/foreign.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Const/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Bifoldable = require("Data.Bifoldable");
var Data_Foldable = require("Data.Foldable");
var Data_Functor_Contravariant = require("Data.Functor.Contravariant");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Monoid = require("Data.Monoid");
var Data_Traversable = require("Data.Traversable");
var Const = function (x) {
    return x;
};
var showConst = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Const (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semigroupoidConst = new Prelude.Semigroupoid(function (v) {
    return function (v1) {
        return v1;
    };
});
var semigroupConst = function (dictSemigroup) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["<>"](dictSemigroup)(v)(v1);
        };
    });
};
var monoidConst = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConst(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var getConst = function (v) {
    return v;
};
var functorConst = new Prelude.Functor(function (v) {
    return function (v1) {
        return v1;
    };
});
var invariantConst = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorConst));
var foldableConst = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (v) {
        return function (v1) {
            return Data_Monoid.mempty(dictMonoid);
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            return z;
        };
    };
}, function (v) {
    return function (z) {
        return function (v1) {
            return z;
        };
    };
});
var traversableConst = new Data_Traversable.Traversable(function () {
    return foldableConst;
}, function () {
    return functorConst;
}, function (dictApplicative) {
    return function (v) {
        return Prelude.pure(dictApplicative)(v);
    };
}, function (dictApplicative) {
    return function (v) {
        return function (v1) {
            return Prelude.pure(dictApplicative)(v1);
        };
    };
});
var eqConst = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordConst = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqConst(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var contravariantConst = new Data_Functor_Contravariant.Contravariant(function (v) {
    return function (v1) {
        return v1;
    };
});
var boundedConst = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(dictBounded), Prelude.top(dictBounded));
};
var applyConst = function (dictSemigroup) {
    return new Prelude.Apply(function () {
        return functorConst;
    }, function (v) {
        return function (v1) {
            return Prelude["<>"](dictSemigroup)(v)(v1);
        };
    });
};
var bindConst = function (dictSemigroup) {
    return new Prelude.Bind(function () {
        return applyConst(dictSemigroup);
    }, function (v) {
        return function (v1) {
            return v;
        };
    });
};
var applicativeConst = function (dictMonoid) {
    return new Prelude.Applicative(function () {
        return applyConst(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, function (v) {
        return Data_Monoid.mempty(dictMonoid);
    });
};
module.exports = {
    Const: Const, 
    getConst: getConst, 
    eqConst: eqConst, 
    ordConst: ordConst, 
    boundedConst: boundedConst, 
    showConst: showConst, 
    semigroupoidConst: semigroupoidConst, 
    semigroupConst: semigroupConst, 
    monoidConst: monoidConst, 
    functorConst: functorConst, 
    invariantConst: invariantConst, 
    applyConst: applyConst, 
    bindConst: bindConst, 
    applicativeConst: applicativeConst, 
    contravariantConst: contravariantConst, 
    foldableConst: foldableConst, 
    traversableConst: traversableConst
};

},{"Data.Bifoldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifoldable/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Functor.Contravariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Contravariant/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Distributive/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Identity = require("Data.Identity");
var Distributive = function (__superclass_Prelude$dotFunctor_0, collect, distribute) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.collect = collect;
    this.distribute = distribute;
};
var distributiveIdentity = new Distributive(function () {
    return Data_Identity.functorIdentity;
}, function (dictFunctor) {
    return function (f) {
        return function ($7) {
            return Data_Identity.Identity(Prelude.map(dictFunctor)(function ($8) {
                return Data_Identity.runIdentity(f($8));
            })($7));
        };
    };
}, function (dictFunctor) {
    return function ($9) {
        return Data_Identity.Identity(Prelude.map(dictFunctor)(Data_Identity.runIdentity)($9));
    };
});
var distribute = function (dict) {
    return dict.distribute;
};
var distributiveFunction = new Distributive(function () {
    return Prelude.functorFn;
}, function (dictFunctor) {
    return function (f) {
        return function ($10) {
            return distribute(distributiveFunction)(dictFunctor)(Prelude.map(dictFunctor)(f)($10));
        };
    };
}, function (dictFunctor) {
    return function (a) {
        return function (e) {
            return Prelude.map(dictFunctor)(function (v) {
                return v(e);
            })(a);
        };
    };
});
var cotraverse = function (dictDistributive) {
    return function (dictFunctor) {
        return function (f) {
            return function ($11) {
                return Prelude.map(dictDistributive["__superclass_Prelude.Functor_0"]())(f)(distribute(dictDistributive)(dictFunctor)($11));
            };
        };
    };
};
var collect = function (dict) {
    return dict.collect;
};
module.exports = {
    Distributive: Distributive, 
    cotraverse: cotraverse, 
    collect: collect, 
    distribute: distribute, 
    distributiveIdentity: distributiveIdentity, 
    distributiveFunction: distributiveFunction
};

},{"Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Either.Unsafe/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Either.Unsafe

exports.unsafeThrow = function (msg) {
  throw new Error(msg);
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Either.Unsafe/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var fromRight = function (v) {
    if (v instanceof Data_Either.Right) {
        return v.value0;
    };
    return $foreign.unsafeThrow("Data.Either.Unsafe.fromRight called on Left value");
};
var fromLeft = function (v) {
    if (v instanceof Data_Either.Left) {
        return v.value0;
    };
    return $foreign.unsafeThrow("Data.Either.Unsafe.fromLeft called on Right value");
};
module.exports = {
    fromRight: fromRight, 
    fromLeft: fromLeft
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Either.Unsafe/foreign.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Extend = require("Control.Extend");
var Data_Bifoldable = require("Data.Bifoldable");
var Data_Bifunctor = require("Data.Bifunctor");
var Data_Bitraversable = require("Data.Bitraversable");
var Data_Foldable = require("Data.Foldable");
var Data_Monoid = require("Data.Monoid");
var Data_Traversable = require("Data.Traversable");
var Left = (function () {
    function Left(value0) {
        this.value0 = value0;
    };
    Left.create = function (value0) {
        return new Left(value0);
    };
    return Left;
})();
var Right = (function () {
    function Right(value0) {
        this.value0 = value0;
    };
    Right.create = function (value0) {
        return new Right(value0);
    };
    return Right;
})();
var showEither = function (dictShow) {
    return function (dictShow1) {
        return new Prelude.Show(function (v) {
            if (v instanceof Left) {
                return "Left (" + (Prelude.show(dictShow)(v.value0) + ")");
            };
            if (v instanceof Right) {
                return "Right (" + (Prelude.show(dictShow1)(v.value0) + ")");
            };
            throw new Error("Failed pattern match at Data.Either line 174, column 1 - line 181, column 1: " + [ v.constructor.name ]);
        });
    };
};
var functorEither = new Prelude.Functor(function (f) {
    return function (v) {
        if (v instanceof Left) {
            return new Left(v.value0);
        };
        if (v instanceof Right) {
            return new Right(f(v.value0));
        };
        throw new Error("Failed pattern match at Data.Either line 52, column 1 - line 56, column 1: " + [ f.constructor.name, v.constructor.name ]);
    };
});
var foldableEither = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Left) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Right) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 201, column 1 - line 209, column 1: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            if (v instanceof Left) {
                return z;
            };
            if (v instanceof Right) {
                return f(z)(v.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 201, column 1 - line 209, column 1: " + [ f.constructor.name, z.constructor.name, v.constructor.name ]);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            if (v instanceof Left) {
                return z;
            };
            if (v instanceof Right) {
                return f(v.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Either line 201, column 1 - line 209, column 1: " + [ f.constructor.name, z.constructor.name, v.constructor.name ]);
        };
    };
});
var traversableEither = new Data_Traversable.Traversable(function () {
    return foldableEither;
}, function () {
    return functorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Prelude.pure(dictApplicative)(new Left(v.value0));
        };
        if (v instanceof Right) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either line 217, column 1 - line 223, column 1: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            if (v instanceof Left) {
                return Prelude.pure(dictApplicative)(new Left(v.value0));
            };
            if (v instanceof Right) {
                return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Right.create)(f(v.value0));
            };
            throw new Error("Failed pattern match at Data.Either line 217, column 1 - line 223, column 1: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
});
var extendEither = new Control_Extend.Extend(function () {
    return functorEither;
}, function (f) {
    return function (v) {
        if (v instanceof Left) {
            return new Left(v.value0);
        };
        return new Right(f(v));
    };
});
var eqEither = function (dictEq) {
    return function (dictEq1) {
        return new Prelude.Eq(function (v) {
            return function (v1) {
                if (v instanceof Left && v1 instanceof Left) {
                    return Prelude["=="](dictEq)(v.value0)(v1.value0);
                };
                if (v instanceof Right && v1 instanceof Right) {
                    return Prelude["=="](dictEq1)(v.value0)(v1.value0);
                };
                return false;
            };
        });
    };
};
var ordEither = function (dictOrd) {
    return function (dictOrd1) {
        return new Prelude.Ord(function () {
            return eqEither(dictOrd["__superclass_Prelude.Eq_0"]())(dictOrd1["__superclass_Prelude.Eq_0"]());
        }, function (v) {
            return function (v1) {
                if (v instanceof Left && v1 instanceof Left) {
                    return Prelude.compare(dictOrd)(v.value0)(v1.value0);
                };
                if (v instanceof Right && v1 instanceof Right) {
                    return Prelude.compare(dictOrd1)(v.value0)(v1.value0);
                };
                if (v instanceof Left) {
                    return Prelude.LT.value;
                };
                if (v1 instanceof Left) {
                    return Prelude.GT.value;
                };
                throw new Error("Failed pattern match at Data.Either line 191, column 1 - line 197, column 1: " + [ v.constructor.name, v1.constructor.name ]);
            };
        });
    };
};
var either = function (f) {
    return function (g) {
        return function (v) {
            if (v instanceof Left) {
                return f(v.value0);
            };
            if (v instanceof Right) {
                return g(v.value0);
            };
            throw new Error("Failed pattern match at Data.Either line 28, column 1 - line 29, column 1: " + [ f.constructor.name, g.constructor.name, v.constructor.name ]);
        };
    };
};
var isLeft = either(Prelude["const"](true))(Prelude["const"](false));
var isRight = either(Prelude["const"](false))(Prelude["const"](true));
var boundedEither = function (dictBounded) {
    return function (dictBounded1) {
        return new Prelude.Bounded(new Left(Prelude.bottom(dictBounded)), new Right(Prelude.top(dictBounded1)));
    };
};
var bifunctorEither = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            if (v instanceof Left) {
                return new Left(f(v.value0));
            };
            if (v instanceof Right) {
                return new Right(g(v.value0));
            };
            throw new Error("Failed pattern match at Data.Either line 56, column 1 - line 92, column 1: " + [ f.constructor.name, g.constructor.name, v.constructor.name ]);
        };
    };
});
var bifoldableEither = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (f) {
        return function (g) {
            return function (v) {
                if (v instanceof Left) {
                    return f(v.value0);
                };
                if (v instanceof Right) {
                    return g(v.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 209, column 1 - line 217, column 1: " + [ f.constructor.name, g.constructor.name, v.constructor.name ]);
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                if (v instanceof Left) {
                    return f(z)(v.value0);
                };
                if (v instanceof Right) {
                    return g(z)(v.value0);
                };
                throw new Error("Failed pattern match at Data.Either line 209, column 1 - line 217, column 1: " + [ f.constructor.name, g.constructor.name, z.constructor.name, v.constructor.name ]);
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                if (v instanceof Left) {
                    return f(v.value0)(z);
                };
                if (v instanceof Right) {
                    return g(v.value0)(z);
                };
                throw new Error("Failed pattern match at Data.Either line 209, column 1 - line 217, column 1: " + [ f.constructor.name, g.constructor.name, z.constructor.name, v.constructor.name ]);
            };
        };
    };
});
var bitraversableEither = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableEither;
}, function () {
    return bifunctorEither;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Left) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Left.create)(v.value0);
        };
        if (v instanceof Right) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Right.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Either line 223, column 1 - line 229, column 1: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (v) {
                if (v instanceof Left) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Left.create)(f(v.value0));
                };
                if (v instanceof Right) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Right.create)(g(v.value0));
                };
                throw new Error("Failed pattern match at Data.Either line 223, column 1 - line 229, column 1: " + [ f.constructor.name, g.constructor.name, v.constructor.name ]);
            };
        };
    };
});
var applyEither = new Prelude.Apply(function () {
    return functorEither;
}, function (v) {
    return function (r) {
        if (v instanceof Left) {
            return new Left(v.value0);
        };
        if (v instanceof Right) {
            return Prelude["<$>"](functorEither)(v.value0)(r);
        };
        throw new Error("Failed pattern match at Data.Either line 92, column 1 - line 116, column 1: " + [ v.constructor.name, r.constructor.name ]);
    };
});
var bindEither = new Prelude.Bind(function () {
    return applyEither;
}, either(function (e) {
    return function (v) {
        return new Left(e);
    };
})(function (a) {
    return function (f) {
        return f(a);
    };
}));
var semigroupEither = function (dictSemigroup) {
    return new Prelude.Semigroup(function (x) {
        return function (y) {
            return Prelude["<*>"](applyEither)(Prelude["<$>"](functorEither)(Prelude.append(dictSemigroup))(x))(y);
        };
    });
};
var semiringEither = function (dictSemiring) {
    return new Prelude.Semiring(function (x) {
        return function (y) {
            return Prelude["<*>"](applyEither)(Prelude["<$>"](functorEither)(Prelude.add(dictSemiring))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyEither)(Prelude["<$>"](functorEither)(Prelude.mul(dictSemiring))(x))(y);
        };
    }, new Right(Prelude.one(dictSemiring)), new Right(Prelude.zero(dictSemiring)));
};
var applicativeEither = new Prelude.Applicative(function () {
    return applyEither;
}, Right.create);
var monadEither = new Prelude.Monad(function () {
    return applicativeEither;
}, function () {
    return bindEither;
});
var altEither = new Control_Alt.Alt(function () {
    return functorEither;
}, function (v) {
    return function (r) {
        if (v instanceof Left) {
            return r;
        };
        return v;
    };
});
module.exports = {
    Left: Left, 
    Right: Right, 
    isRight: isRight, 
    isLeft: isLeft, 
    either: either, 
    functorEither: functorEither, 
    bifunctorEither: bifunctorEither, 
    applyEither: applyEither, 
    applicativeEither: applicativeEither, 
    altEither: altEither, 
    bindEither: bindEither, 
    monadEither: monadEither, 
    extendEither: extendEither, 
    showEither: showEither, 
    eqEither: eqEither, 
    ordEither: ordEither, 
    boundedEither: boundedEither, 
    foldableEither: foldableEither, 
    bifoldableEither: bifoldableEither, 
    traversableEither: traversableEither, 
    bitraversableEither: bitraversableEither, 
    semiringEither: semiringEither, 
    semigroupEither: semigroupEither
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Bifoldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifoldable/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Data.Bitraversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Bitraversable/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Enum/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Char = require("Data.Char");
var Data_Either = require("Data.Either");
var Data_Maybe = require("Data.Maybe");
var Data_Maybe_Unsafe = require("Data.Maybe.Unsafe");
var Data_Tuple = require("Data.Tuple");
var Data_Unfoldable = require("Data.Unfoldable");
var Cardinality = function (x) {
    return x;
};
var Enum = function (__superclass_Prelude$dotBounded_0, cardinality, fromEnum, pred, succ, toEnum) {
    this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
    this.cardinality = cardinality;
    this.fromEnum = fromEnum;
    this.pred = pred;
    this.succ = succ;
    this.toEnum = toEnum;
};
var toEnum = function (dict) {
    return dict.toEnum;
};
var succ = function (dict) {
    return dict.succ;
};
var runCardinality = function (v) {
    return v;
};
var tupleCardinality = function (dictEnum) {
    return function (dictEnum1) {
        return function (l) {
            return function (r) {
                return Cardinality(runCardinality(l) * runCardinality(r) | 0);
            };
        };
    };
};
var tupleToEnum = function (dictEnum) {
    return function (dictEnum1) {
        return function (cardb) {
            return function (n) {
                return Prelude["<*>"](Data_Maybe.applyMaybe)(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(toEnum(dictEnum)(n / runCardinality(cardb) | 0)))(toEnum(dictEnum1)(n % runCardinality(cardb)));
            };
        };
    };
};
var pred = function (dict) {
    return dict.pred;
};
var maybeCardinality = function (dictEnum) {
    return function (c) {
        return Cardinality(1 + runCardinality(c) | 0);
    };
};
var maybeToEnum = function (dictEnum) {
    return function (carda) {
        return function (n) {
            if (n <= runCardinality(maybeCardinality(dictEnum)(carda))) {
                var $36 = n === 0;
                if ($36) {
                    return Data_Maybe.Just.create(Data_Maybe.Nothing.value);
                };
                if (!$36) {
                    return Data_Maybe.Just.create(toEnum(dictEnum)(n - 1));
                };
                throw new Error("Failed pattern match at Data.Enum line 138, column 1 - line 139, column 1: " + [ $36.constructor.name ]);
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var intStepFromTo = function (step) {
    return function (from) {
        return function (to) {
            return Data_Unfoldable.unfoldr(Data_Unfoldable.unfoldableArray)(function (e) {
                var $37 = e <= to;
                if ($37) {
                    return Data_Maybe.Just.create(new Data_Tuple.Tuple(e, e + step | 0));
                };
                if (!$37) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.Enum line 103, column 1 - line 104, column 1: " + [ $37.constructor.name ]);
            })(from);
        };
    };
};
var intFromTo = intStepFromTo(1);
var fromEnum = function (dict) {
    return dict.fromEnum;
};
var tupleFromEnum = function (dictEnum) {
    return function (dictEnum1) {
        return function (cardb) {
            return function (v) {
                return (fromEnum(dictEnum)(v.value0) * runCardinality(cardb) | 0) + fromEnum(dictEnum1)(v.value1) | 0;
            };
        };
    };
};
var enumFromTo = function (dictEnum) {
    return function (a) {
        return function (b) {
            var bʹ = fromEnum(dictEnum)(b);
            var aʹ = fromEnum(dictEnum)(a);
            return Prelude["<$>"](Prelude.functorArray)(Prelude[">>>"](Prelude.semigroupoidFn)(toEnum(dictEnum))(Data_Maybe_Unsafe.fromJust))(intFromTo(aʹ)(bʹ));
        };
    };
};
var enumFromThenTo = function (dictEnum) {
    return function (a) {
        return function (b) {
            return function (c) {
                var cʹ = fromEnum(dictEnum)(c);
                var bʹ = fromEnum(dictEnum)(b);
                var aʹ = fromEnum(dictEnum)(a);
                return Prelude["<$>"](Prelude.functorArray)(Prelude[">>>"](Prelude.semigroupoidFn)(toEnum(dictEnum))(Data_Maybe_Unsafe.fromJust))(intStepFromTo(bʹ - aʹ)(aʹ)(cʹ));
            };
        };
    };
};
var eitherFromEnum = function (dictEnum) {
    return function (dictEnum1) {
        return function (carda) {
            return function (v) {
                if (v instanceof Data_Either.Left) {
                    return fromEnum(dictEnum)(v.value0);
                };
                if (v instanceof Data_Either.Right) {
                    return fromEnum(dictEnum1)(v.value0) + runCardinality(carda) | 0;
                };
                throw new Error("Failed pattern match at Data.Enum line 197, column 1 - line 198, column 1: " + [ carda.constructor.name, v.constructor.name ]);
            };
        };
    };
};
var eitherCardinality = function (dictEnum) {
    return function (dictEnum1) {
        return function (l) {
            return function (r) {
                return Cardinality(runCardinality(l) + runCardinality(r) | 0);
            };
        };
    };
};
var eitherToEnum = function (dictEnum) {
    return function (dictEnum1) {
        return function (carda) {
            return function (cardb) {
                return function (n) {
                    var $46 = n >= 0 && n < runCardinality(carda);
                    if ($46) {
                        return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Either.Left.create)(toEnum(dictEnum)(n));
                    };
                    if (!$46) {
                        var $47 = n >= runCardinality(carda) && n < runCardinality(eitherCardinality(dictEnum)(dictEnum1)(carda)(cardb));
                        if ($47) {
                            return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Either.Right.create)(toEnum(dictEnum1)(n - runCardinality(carda)));
                        };
                        if (!$47) {
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match: " + [ $47.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Data.Enum line 189, column 1 - line 190, column 1: " + [ $46.constructor.name ]);
                };
            };
        };
    };
};
var defaultToEnum = function (succʹ) {
    return function (bottomʹ) {
        return function (n) {
            if (n < 0) {
                return Data_Maybe.Nothing.value;
            };
            if (n === 0) {
                return new Data_Maybe.Just(bottomʹ);
            };
            if (Prelude.otherwise) {
                return Prelude[">>="](Data_Maybe.bindMaybe)(defaultToEnum(succʹ)(bottomʹ)(n - 1))(succʹ);
            };
            throw new Error("Failed pattern match: " + [ succʹ.constructor.name, bottomʹ.constructor.name, n.constructor.name ]);
        };
    };
};
var defaultSucc = function (toEnumʹ) {
    return function (fromEnumʹ) {
        return function (a) {
            return toEnumʹ(fromEnumʹ(a) + 1 | 0);
        };
    };
};
var defaultPred = function (toEnumʹ) {
    return function (fromEnumʹ) {
        return function (a) {
            return toEnumʹ(fromEnumʹ(a) - 1);
        };
    };
};
var defaultFromEnum = function (predʹ) {
    return function (e) {
        return Data_Maybe.maybe(0)(function (prd) {
            return defaultFromEnum(predʹ)(prd) + 1 | 0;
        })(predʹ(e));
    };
};
var charToEnum = function (n) {
    if (n >= 0 && n <= 65535) {
        return Data_Maybe.Just.create(Data_Char.fromCharCode(n));
    };
    return Data_Maybe.Nothing.value;
};
var charFromEnum = Data_Char.toCharCode;
var enumChar = new Enum(function () {
    return Prelude.boundedChar;
}, 65536, charFromEnum, defaultPred(charToEnum)(charFromEnum), defaultSucc(charToEnum)(charFromEnum), charToEnum);
var cardinality = function (dict) {
    return dict.cardinality;
};
var enumEither = function (dictEnum) {
    return function (dictEnum1) {
        return new Enum(function () {
            return Data_Either.boundedEither(dictEnum["__superclass_Prelude.Bounded_0"]())(dictEnum1["__superclass_Prelude.Bounded_0"]());
        }, eitherCardinality(dictEnum)(dictEnum1)(cardinality(dictEnum))(cardinality(dictEnum1)), eitherFromEnum(dictEnum)(dictEnum1)(cardinality(dictEnum)), function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Maybe.maybe(Data_Maybe.Nothing.value)(function ($72) {
                    return Data_Maybe.Just.create(Data_Either.Left.create($72));
                })(pred(dictEnum)(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Maybe.maybe(Data_Maybe.Just.create(new Data_Either.Left(Prelude.top(dictEnum["__superclass_Prelude.Bounded_0"]()))))(function ($73) {
                    return Data_Maybe.Just.create(Data_Either.Right.create($73));
                })(pred(dictEnum1)(v.value0));
            };
            throw new Error("Failed pattern match at Data.Enum line 180, column 1 - line 189, column 1: " + [ v.constructor.name ]);
        }, function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Maybe.maybe(Data_Maybe.Just.create(new Data_Either.Right(Prelude.bottom(dictEnum1["__superclass_Prelude.Bounded_0"]()))))(function ($74) {
                    return Data_Maybe.Just.create(Data_Either.Left.create($74));
                })(succ(dictEnum)(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Maybe.maybe(Data_Maybe.Nothing.value)(function ($75) {
                    return Data_Maybe.Just.create(Data_Either.Right.create($75));
                })(succ(dictEnum1)(v.value0));
            };
            throw new Error("Failed pattern match at Data.Enum line 180, column 1 - line 189, column 1: " + [ v.constructor.name ]);
        }, eitherToEnum(dictEnum)(dictEnum1)(cardinality(dictEnum))(cardinality(dictEnum1)));
    };
};
var enumMaybe = function (dictEnum) {
    return new Enum(function () {
        return Data_Maybe.boundedMaybe(dictEnum["__superclass_Prelude.Bounded_0"]());
    }, maybeCardinality(dictEnum)(cardinality(dictEnum)), function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return 0;
        };
        if (v instanceof Data_Maybe.Just) {
            return fromEnum(dictEnum)(v.value0) + 1 | 0;
        };
        throw new Error("Failed pattern match at Data.Enum line 128, column 1 - line 138, column 1: " + [ v.constructor.name ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        if (v instanceof Data_Maybe.Just) {
            return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(pred(dictEnum)(v.value0));
        };
        throw new Error("Failed pattern match at Data.Enum line 128, column 1 - line 138, column 1: " + [ v.constructor.name ]);
    }, function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Just.create(Prelude.bottom(Data_Maybe.boundedMaybe(dictEnum["__superclass_Prelude.Bounded_0"]())));
        };
        if (v instanceof Data_Maybe.Just) {
            return Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Maybe.Just.create)(succ(dictEnum)(v.value0));
        };
        throw new Error("Failed pattern match at Data.Enum line 128, column 1 - line 138, column 1: " + [ v.constructor.name ]);
    }, maybeToEnum(dictEnum)(cardinality(dictEnum)));
};
var enumTuple = function (dictEnum) {
    return function (dictEnum1) {
        return new Enum(function () {
            return Data_Tuple.boundedTuple(dictEnum["__superclass_Prelude.Bounded_0"]())(dictEnum1["__superclass_Prelude.Bounded_0"]());
        }, tupleCardinality(dictEnum)(dictEnum1)(cardinality(dictEnum))(cardinality(dictEnum1)), tupleFromEnum(dictEnum)(dictEnum1)(cardinality(dictEnum1)), function (v) {
            return Data_Maybe.maybe(Prelude["<$>"](Data_Maybe.functorMaybe)(Prelude.flip(Data_Tuple.Tuple.create)(Prelude.bottom(dictEnum1["__superclass_Prelude.Bounded_0"]())))(pred(dictEnum)(v.value0)))(function ($76) {
                return Data_Maybe.Just.create(Data_Tuple.Tuple.create(v.value0)($76));
            })(pred(dictEnum1)(v.value1));
        }, function (v) {
            return Data_Maybe.maybe(Prelude["<$>"](Data_Maybe.functorMaybe)(Prelude.flip(Data_Tuple.Tuple.create)(Prelude.bottom(dictEnum1["__superclass_Prelude.Bounded_0"]())))(succ(dictEnum)(v.value0)))(function ($77) {
                return Data_Maybe.Just.create(Data_Tuple.Tuple.create(v.value0)($77));
            })(succ(dictEnum1)(v.value1));
        }, tupleToEnum(dictEnum)(dictEnum1)(cardinality(dictEnum1)));
    };
};
var booleanSucc = function (v) {
    if (!v) {
        return new Data_Maybe.Just(true);
    };
    return Data_Maybe.Nothing.value;
};
var booleanPred = function (v) {
    if (v) {
        return new Data_Maybe.Just(false);
    };
    return Data_Maybe.Nothing.value;
};
var enumBoolean = new Enum(function () {
    return Prelude.boundedBoolean;
}, 2, defaultFromEnum(booleanPred), booleanPred, booleanSucc, defaultToEnum(booleanSucc)(false));
module.exports = {
    Cardinality: Cardinality, 
    Enum: Enum, 
    enumFromThenTo: enumFromThenTo, 
    enumFromTo: enumFromTo, 
    intStepFromTo: intStepFromTo, 
    intFromTo: intFromTo, 
    defaultFromEnum: defaultFromEnum, 
    defaultToEnum: defaultToEnum, 
    defaultPred: defaultPred, 
    defaultSucc: defaultSucc, 
    toEnum: toEnum, 
    succ: succ, 
    runCardinality: runCardinality, 
    pred: pred, 
    fromEnum: fromEnum, 
    cardinality: cardinality, 
    enumChar: enumChar, 
    enumMaybe: enumMaybe, 
    enumBoolean: enumBoolean, 
    enumTuple: enumTuple, 
    enumEither: enumEither
};

},{"Data.Char":"/Users/anders/dev/ps/halo/ajax/output/Data.Char/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Data.Unfoldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Unfoldable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Exists/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Exists

exports.mkExists = function (fa) {
  return fa;
};

exports.runExists = function (f) {
  return function (fa) {
    return f(fa);
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Exists/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
module.exports = {
    runExists: $foreign.runExists, 
    mkExists: $foreign.mkExists
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Exists/foreign.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.ExistsR/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Unsafe_Coerce = require("Unsafe.Coerce");
var runExistsR = Unsafe_Coerce.unsafeCoerce;
var mkExistsR = Unsafe_Coerce.unsafeCoerce;
module.exports = {
    runExistsR: runExistsR, 
    mkExistsR: mkExistsR
};

},{"Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Foldable

exports.foldrArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};

exports.foldlArray = function (f) {
  return function (init) {
    return function (xs) {
      var acc = init;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Apply = require("Control.Apply");
var Data_Maybe = require("Data.Maybe");
var Data_Maybe_First = require("Data.Maybe.First");
var Data_Maybe_Last = require("Data.Maybe.Last");
var Data_Monoid = require("Data.Monoid");
var Data_Monoid_Additive = require("Data.Monoid.Additive");
var Data_Monoid_Conj = require("Data.Monoid.Conj");
var Data_Monoid_Disj = require("Data.Monoid.Disj");
var Data_Monoid_Dual = require("Data.Monoid.Dual");
var Data_Monoid_Endo = require("Data.Monoid.Endo");
var Data_Monoid_Multiplicative = require("Data.Monoid.Multiplicative");
var Foldable = function (foldMap, foldl, foldr) {
    this.foldMap = foldMap;
    this.foldl = foldl;
    this.foldr = foldr;
};
var foldr = function (dict) {
    return dict.foldr;
};
var traverse_ = function (dictApplicative) {
    return function (dictFoldable) {
        return function (f) {
            return foldr(dictFoldable)(function ($141) {
                return Control_Apply["*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(f($141));
            })(Prelude.pure(dictApplicative)(Prelude.unit));
        };
    };
};
var for_ = function (dictApplicative) {
    return function (dictFoldable) {
        return Prelude.flip(traverse_(dictApplicative)(dictFoldable));
    };
};
var sequence_ = function (dictApplicative) {
    return function (dictFoldable) {
        return traverse_(dictApplicative)(dictFoldable)(Prelude.id(Prelude.categoryFn));
    };
};
var foldl = function (dict) {
    return dict.foldl;
};
var intercalate = function (dictFoldable) {
    return function (dictMonoid) {
        return function (sep) {
            return function (xs) {
                var go = function (v) {
                    return function (x) {
                        if (v.init) {
                            return {
                                init: false, 
                                acc: x
                            };
                        };
                        return {
                            init: false, 
                            acc: Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(v.acc)(Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(sep)(x))
                        };
                    };
                };
                return (foldl(dictFoldable)(go)({
                    init: true, 
                    acc: Data_Monoid.mempty(dictMonoid)
                })(xs)).acc;
            };
        };
    };
};
var mconcat = function (dictFoldable) {
    return function (dictMonoid) {
        return foldl(dictFoldable)(Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]()))(Data_Monoid.mempty(dictMonoid));
    };
};
var product = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Prelude["*"](dictSemiring))(Prelude.one(dictSemiring));
    };
};
var sum = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Prelude["+"](dictSemiring))(Prelude.zero(dictSemiring));
    };
};
var foldableMultiplicative = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableMaybe = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v instanceof Data_Maybe.Just) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable line 99, column 1 - line 107, column 1: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v instanceof Data_Maybe.Just) {
                return f(z)(v.value0);
            };
            throw new Error("Failed pattern match at Data.Foldable line 99, column 1 - line 107, column 1: " + [ f.constructor.name, z.constructor.name, v.constructor.name ]);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return z;
            };
            if (v instanceof Data_Maybe.Just) {
                return f(v.value0)(z);
            };
            throw new Error("Failed pattern match at Data.Foldable line 99, column 1 - line 107, column 1: " + [ f.constructor.name, z.constructor.name, v.constructor.name ]);
        };
    };
});
var foldableDual = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableDisj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableConj = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldableAdditive = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var foldMapDefaultR = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (xs) {
                return foldr(dictFoldable)(function (x) {
                    return function (acc) {
                        return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f(x))(acc);
                    };
                })(Data_Monoid.mempty(dictMonoid))(xs);
            };
        };
    };
};
var foldableArray = new Foldable(function (dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
}, $foreign.foldlArray, $foreign.foldrArray);
var foldMapDefaultL = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (xs) {
                return foldl(dictFoldable)(function (acc) {
                    return function (x) {
                        return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f(x))(acc);
                    };
                })(Data_Monoid.mempty(dictMonoid))(xs);
            };
        };
    };
};
var foldMap = function (dict) {
    return dict.foldMap;
};
var foldableFirst = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldableLast = new Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return foldMap(foldableMaybe)(dictMonoid)(f)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldl(foldableMaybe)(f)(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return foldr(foldableMaybe)(f)(z)(v);
        };
    };
});
var foldlDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Monoid_Endo.runEndo(Data_Monoid_Dual.runDual(foldMap(dictFoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo))(function ($142) {
                    return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo(Prelude.flip(c)($142)));
                })(xs)))(u);
            };
        };
    };
};
var foldrDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Monoid_Endo.runEndo(foldMap(dictFoldable)(Data_Monoid_Endo.monoidEndo)(function ($143) {
                    return Data_Monoid_Endo.Endo(c($143));
                })(xs))(u);
            };
        };
    };
};
var fold = function (dictFoldable) {
    return function (dictMonoid) {
        return foldMap(dictFoldable)(dictMonoid)(Prelude.id(Prelude.categoryFn));
    };
};
var find = function (dictFoldable) {
    return function (p) {
        return foldl(dictFoldable)(function (r) {
            return function (x) {
                var $140 = p(x);
                if ($140) {
                    return new Data_Maybe.Just(x);
                };
                if (!$140) {
                    return r;
                };
                throw new Error("Failed pattern match at Data.Foldable line 229, column 1 - line 230, column 1: " + [ $140.constructor.name ]);
            };
        })(Data_Maybe.Nothing.value);
    };
};
var any = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function ($144) {
                return Data_Monoid_Disj.runDisj(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra))(function ($145) {
                    return Data_Monoid_Disj.Disj(p($145));
                })($144));
            };
        };
    };
};
var elem = function (dictFoldable) {
    return function (dictEq) {
        return function ($146) {
            return any(dictFoldable)(Prelude.booleanAlgebraBoolean)(Prelude["=="](dictEq)($146));
        };
    };
};
var notElem = function (dictFoldable) {
    return function (dictEq) {
        return function (x) {
            return function ($147) {
                return !elem(dictFoldable)(dictEq)(x)($147);
            };
        };
    };
};
var or = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return any(dictFoldable)(dictBooleanAlgebra)(Prelude.id(Prelude.categoryFn));
    };
};
var all = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function ($148) {
                return Data_Monoid_Conj.runConj(foldMap(dictFoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra))(function ($149) {
                    return Data_Monoid_Conj.Conj(p($149));
                })($148));
            };
        };
    };
};
var and = function (dictFoldable) {
    return function (dictBooleanAlgebra) {
        return all(dictFoldable)(dictBooleanAlgebra)(Prelude.id(Prelude.categoryFn));
    };
};
module.exports = {
    Foldable: Foldable, 
    find: find, 
    notElem: notElem, 
    elem: elem, 
    product: product, 
    sum: sum, 
    all: all, 
    any: any, 
    or: or, 
    and: and, 
    intercalate: intercalate, 
    mconcat: mconcat, 
    sequence_: sequence_, 
    for_: for_, 
    traverse_: traverse_, 
    fold: fold, 
    foldMapDefaultR: foldMapDefaultR, 
    foldMapDefaultL: foldMapDefaultL, 
    foldlDefault: foldlDefault, 
    foldrDefault: foldrDefault, 
    foldMap: foldMap, 
    foldl: foldl, 
    foldr: foldr, 
    foldableArray: foldableArray, 
    foldableMaybe: foldableMaybe, 
    foldableFirst: foldableFirst, 
    foldableLast: foldableLast, 
    foldableAdditive: foldableAdditive, 
    foldableDual: foldableDual, 
    foldableDisj: foldableDisj, 
    foldableConj: foldableConj, 
    foldableMultiplicative: foldableMultiplicative
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/foreign.js","Control.Apply":"/Users/anders/dev/ps/halo/ajax/output/Control.Apply/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.First":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.First/index.js","Data.Maybe.Last":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Last/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Monoid.Additive":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Additive/index.js","Data.Monoid.Conj":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Conj/index.js","Data.Monoid.Disj":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Disj/index.js","Data.Monoid.Dual":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Dual/index.js","Data.Monoid.Endo":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Endo/index.js","Data.Monoid.Multiplicative":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Multiplicative/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Class/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Array = require("Data.Array");
var Data_Either = require("Data.Either");
var Data_Foreign = require("Data.Foreign");
var Data_Foreign_Index = require("Data.Foreign.Index");
var Data_Foreign_Null = require("Data.Foreign.Null");
var Data_Foreign_NullOrUndefined = require("Data.Foreign.NullOrUndefined");
var Data_Foreign_Undefined = require("Data.Foreign.Undefined");
var Data_Int = require("Data.Int");
var Data_Traversable = require("Data.Traversable");
var IsForeign = function (read) {
    this.read = read;
};
var stringIsForeign = new IsForeign(Data_Foreign.readString);
var read = function (dict) {
    return dict.read;
};
var readJSON = function (dictIsForeign) {
    return function (json) {
        return Prelude[">>="](Data_Either.bindEither)(Data_Foreign.parseJSON(json))(read(dictIsForeign));
    };
};
var readWith = function (dictIsForeign) {
    return function (f) {
        return function (value) {
            return Data_Either.either(function ($8) {
                return Data_Either.Left.create(f($8));
            })(Data_Either.Right.create)(read(dictIsForeign)(value));
        };
    };
};
var readProp = function (dictIsForeign) {
    return function (dictIndex) {
        return function (prop) {
            return function (value) {
                return Prelude[">>="](Data_Either.bindEither)(Data_Foreign_Index["!"](dictIndex)(value)(prop))(readWith(dictIsForeign)(Data_Foreign_Index.errorAt(dictIndex)(prop)));
            };
        };
    };
};
var undefinedIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_Undefined.readUndefined(read(dictIsForeign)));
};
var numberIsForeign = new IsForeign(Data_Foreign.readNumber);
var nullOrUndefinedIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_NullOrUndefined.readNullOrUndefined(read(dictIsForeign)));
};
var nullIsForeign = function (dictIsForeign) {
    return new IsForeign(Data_Foreign_Null.readNull(read(dictIsForeign)));
};
var intIsForeign = new IsForeign(Data_Foreign.readInt);
var foreignIsForeign = new IsForeign(function (f) {
    return Prelude["return"](Data_Either.applicativeEither)(f);
});
var charIsForeign = new IsForeign(Data_Foreign.readChar);
var booleanIsForeign = new IsForeign(Data_Foreign.readBoolean);
var arrayIsForeign = function (dictIsForeign) {
    return new IsForeign(function (value) {
        var readElement = function (i) {
            return function (value1) {
                return readWith(dictIsForeign)(Data_Foreign.ErrorAtIndex.create(i))(value1);
            };
        };
        var readElements = function (arr) {
            return Data_Traversable.sequence(Data_Traversable.traversableArray)(Data_Either.applicativeEither)(Data_Array.zipWith(readElement)(Data_Array.range(0)(Data_Array.length(arr)))(arr));
        };
        return Prelude[">>="](Data_Either.bindEither)(Data_Foreign.readArray(value))(readElements);
    });
};
module.exports = {
    IsForeign: IsForeign, 
    readProp: readProp, 
    readWith: readWith, 
    readJSON: readJSON, 
    read: read, 
    foreignIsForeign: foreignIsForeign, 
    stringIsForeign: stringIsForeign, 
    charIsForeign: charIsForeign, 
    booleanIsForeign: booleanIsForeign, 
    numberIsForeign: numberIsForeign, 
    intIsForeign: intIsForeign, 
    arrayIsForeign: arrayIsForeign, 
    nullIsForeign: nullIsForeign, 
    undefinedIsForeign: undefinedIsForeign, 
    nullOrUndefinedIsForeign: nullOrUndefinedIsForeign
};

},{"Data.Array":"/Users/anders/dev/ps/halo/ajax/output/Data.Array/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Foreign.Index":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Index/index.js","Data.Foreign.Null":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Null/index.js","Data.Foreign.NullOrUndefined":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.NullOrUndefined/index.js","Data.Foreign.Undefined":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Undefined/index.js","Data.Int":"/Users/anders/dev/ps/halo/ajax/output/Data.Int/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Index/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Foreign.Index

// jshint maxparams: 4
exports.unsafeReadPropImpl = function (f, s, key, value) {
  return value == null ? f : s(value[key]);
};

// jshint maxparams: 2
exports.unsafeHasOwnProperty = function (prop, value) {
  return Object.prototype.hasOwnProperty.call(value, prop);
};

exports.unsafeHasProperty = function (prop, value) {
  return prop in value;
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Index/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Foreign = require("Data.Foreign");
var Data_Function = require("Data.Function");
var Data_Int = require("Data.Int");
var Index = function (errorAt, hasOwnProperty, hasProperty, ix) {
    this.errorAt = errorAt;
    this.hasOwnProperty = hasOwnProperty;
    this.hasProperty = hasProperty;
    this.ix = ix;
};
var unsafeReadProp = function (k) {
    return function (value) {
        return $foreign.unsafeReadPropImpl(new Data_Either.Left(new Data_Foreign.TypeMismatch("object", Data_Foreign.typeOf(value))), Prelude.pure(Data_Either.applicativeEither), k, value);
    };
};
var prop = unsafeReadProp;
var ix = function (dict) {
    return dict.ix;
};
var $bang = function (dictIndex) {
    return ix(dictIndex);
};
var index = unsafeReadProp;
var hasPropertyImpl = function (p) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return false;
        };
        if (Data_Foreign.isUndefined(value)) {
            return false;
        };
        if (Prelude["=="](Prelude.eqString)(Data_Foreign.typeOf(value))("object") || Prelude["=="](Prelude.eqString)(Data_Foreign.typeOf(value))("function")) {
            return $foreign.unsafeHasProperty(p, value);
        };
        return false;
    };
};
var hasProperty = function (dict) {
    return dict.hasProperty;
};
var hasOwnPropertyImpl = function (p) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return false;
        };
        if (Data_Foreign.isUndefined(value)) {
            return false;
        };
        if (Prelude["=="](Prelude.eqString)(Data_Foreign.typeOf(value))("object") || Prelude["=="](Prelude.eqString)(Data_Foreign.typeOf(value))("function")) {
            return $foreign.unsafeHasOwnProperty(p, value);
        };
        return false;
    };
};
var indexInt = new Index(Data_Foreign.ErrorAtIndex.create, hasOwnPropertyImpl, hasPropertyImpl, Prelude.flip(index));
var indexString = new Index(Data_Foreign.ErrorAtProperty.create, hasOwnPropertyImpl, hasPropertyImpl, Prelude.flip(prop));
var hasOwnProperty = function (dict) {
    return dict.hasOwnProperty;
};
var errorAt = function (dict) {
    return dict.errorAt;
};
module.exports = {
    Index: Index, 
    errorAt: errorAt, 
    hasOwnProperty: hasOwnProperty, 
    hasProperty: hasProperty, 
    "!": $bang, 
    ix: ix, 
    index: index, 
    prop: prop, 
    indexString: indexString, 
    indexInt: indexInt
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Index/foreign.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Data.Int":"/Users/anders/dev/ps/halo/ajax/output/Data.Int/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Null/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Foreign = require("Data.Foreign");
var Data_Either = require("Data.Either");
var Null = function (x) {
    return x;
};
var runNull = function (v) {
    return v;
};
var readNull = function (f) {
    return function (value) {
        if (Data_Foreign.isNull(value)) {
            return Prelude.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Prelude["<$>"](Data_Either.functorEither)(function ($4) {
            return Null(Data_Maybe.Just.create($4));
        })(f(value));
    };
};
module.exports = {
    Null: Null, 
    readNull: readNull, 
    runNull: runNull
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.NullOrUndefined/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Foreign = require("Data.Foreign");
var Data_Either = require("Data.Either");
var NullOrUndefined = function (x) {
    return x;
};
var runNullOrUndefined = function (v) {
    return v;
};
var readNullOrUndefined = function (f) {
    return function (value) {
        if (Data_Foreign.isNull(value) || Data_Foreign.isUndefined(value)) {
            return Prelude.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Prelude["<$>"](Data_Either.functorEither)(function ($4) {
            return NullOrUndefined(Data_Maybe.Just.create($4));
        })(f(value));
    };
};
module.exports = {
    NullOrUndefined: NullOrUndefined, 
    readNullOrUndefined: readNullOrUndefined, 
    runNullOrUndefined: runNullOrUndefined
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Undefined/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Foreign = require("Data.Foreign");
var Data_Either = require("Data.Either");
var Undefined = function (x) {
    return x;
};
var runUndefined = function (v) {
    return v;
};
var readUndefined = function (f) {
    return function (value) {
        if (Data_Foreign.isUndefined(value)) {
            return Prelude.pure(Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
        };
        return Prelude["<$>"](Data_Either.functorEither)(function ($4) {
            return Undefined(Data_Maybe.Just.create($4));
        })(f(value));
    };
};
module.exports = {
    Undefined: Undefined, 
    readUndefined: readUndefined, 
    runUndefined: runUndefined
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Foreign

// jshint maxparams: 3
exports.parseJSONImpl = function (left, right, str) {
  try {
    return right(JSON.parse(str));
  } catch (e) {
    return left(e.toString());
  }
};

// jshint maxparams: 1
exports.toForeign = function (value) {
  return value;
};

exports.unsafeFromForeign = function (value) {
  return value;
};

exports.typeOf = function (value) {
  return typeof value;
};

exports.tagOf = function (value) {
  return Object.prototype.toString.call(value).slice(8, -1);
};

exports.isNull = function (value) {
  return value === null;
};

exports.isUndefined = function (value) {
  return value === undefined;
};

exports.isArray = Array.isArray || function (value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Maybe = require("Data.Maybe");
var Data_Function = require("Data.Function");
var Data_Int = require("Data.Int");
var Data_String = require("Data.String");
var TypeMismatch = (function () {
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    return TypeMismatch;
})();
var ErrorAtIndex = (function () {
    function ErrorAtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtIndex.create = function (value0) {
        return function (value1) {
            return new ErrorAtIndex(value0, value1);
        };
    };
    return ErrorAtIndex;
})();
var ErrorAtProperty = (function () {
    function ErrorAtProperty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtProperty.create = function (value0) {
        return function (value1) {
            return new ErrorAtProperty(value0, value1);
        };
    };
    return ErrorAtProperty;
})();
var JSONError = (function () {
    function JSONError(value0) {
        this.value0 = value0;
    };
    JSONError.create = function (value0) {
        return new JSONError(value0);
    };
    return JSONError;
})();
var unsafeReadTagged = function (tag) {
    return function (value) {
        if (Prelude["=="](Prelude.eqString)($foreign.tagOf(value))(tag)) {
            return Prelude.pure(Data_Either.applicativeEither)($foreign.unsafeFromForeign(value));
        };
        return new Data_Either.Left(new TypeMismatch(tag, $foreign.tagOf(value)));
    };
};
var showForeignError = new Prelude.Show(function (v) {
    if (v instanceof TypeMismatch) {
        return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    };
    if (v instanceof ErrorAtIndex) {
        return "Error at array index " + (Prelude.show(Prelude.showInt)(v.value0) + (": " + Prelude.show(showForeignError)(v.value1)));
    };
    if (v instanceof ErrorAtProperty) {
        return "Error at property " + (Prelude.show(Prelude.showString)(v.value0) + (": " + Prelude.show(showForeignError)(v.value1)));
    };
    if (v instanceof JSONError) {
        return "JSON error: " + v.value0;
    };
    throw new Error("Failed pattern match: " + [ v.constructor.name ]);
});
var readString = unsafeReadTagged("String");
var readNumber = unsafeReadTagged("Number");
var readInt = function (value) {
    var error = Data_Either.Left.create(new TypeMismatch("Int", $foreign.tagOf(value)));
    var fromNumber = function ($30) {
        return Data_Maybe.maybe(error)(Prelude.pure(Data_Either.applicativeEither))(Data_Int.fromNumber($30));
    };
    return Data_Either.either(Prelude["const"](error))(fromNumber)(readNumber(value));
};
var readChar = function (value) {
    var error = Data_Either.Left.create(new TypeMismatch("Char", $foreign.tagOf(value)));
    var fromString = function ($31) {
        return Data_Maybe.maybe(error)(Prelude.pure(Data_Either.applicativeEither))(Data_String.toChar($31));
    };
    return Data_Either.either(Prelude["const"](error))(fromString)(readString(value));
};
var readBoolean = unsafeReadTagged("Boolean");
var readArray = function (value) {
    if ($foreign.isArray(value)) {
        return Prelude.pure(Data_Either.applicativeEither)($foreign.unsafeFromForeign(value));
    };
    return new Data_Either.Left(new TypeMismatch("array", $foreign.tagOf(value)));
};
var parseJSON = function (json) {
    return $foreign.parseJSONImpl(function ($32) {
        return Data_Either.Left.create(JSONError.create($32));
    }, Data_Either.Right.create, json);
};
var eqForeignError = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof TypeMismatch && v1 instanceof TypeMismatch) {
            return Prelude["=="](Prelude.eqString)(v.value0)(v1.value0) && Prelude["=="](Prelude.eqString)(v.value1)(v1.value1);
        };
        if (v instanceof ErrorAtIndex && v1 instanceof ErrorAtIndex) {
            return v.value0 === v1.value0 && Prelude["=="](eqForeignError)(v.value1)(v1.value1);
        };
        if (v instanceof ErrorAtProperty && v1 instanceof ErrorAtProperty) {
            return Prelude["=="](Prelude.eqString)(v.value0)(v1.value0) && Prelude["=="](eqForeignError)(v.value1)(v1.value1);
        };
        if (v instanceof JSONError && v1 instanceof JSONError) {
            return Prelude["=="](Prelude.eqString)(v.value0)(v1.value0);
        };
        return false;
    };
});
module.exports = {
    TypeMismatch: TypeMismatch, 
    ErrorAtIndex: ErrorAtIndex, 
    ErrorAtProperty: ErrorAtProperty, 
    JSONError: JSONError, 
    readArray: readArray, 
    readInt: readInt, 
    readNumber: readNumber, 
    readBoolean: readBoolean, 
    readChar: readChar, 
    readString: readString, 
    unsafeReadTagged: unsafeReadTagged, 
    parseJSON: parseJSON, 
    showForeignError: showForeignError, 
    eqForeignError: eqForeignError, 
    isArray: $foreign.isArray, 
    isUndefined: $foreign.isUndefined, 
    isNull: $foreign.isNull, 
    tagOf: $foreign.tagOf, 
    typeOf: $foreign.typeOf, 
    unsafeFromForeign: $foreign.unsafeFromForeign, 
    toForeign: $foreign.toForeign
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/foreign.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Data.Int":"/Users/anders/dev/ps/halo/ajax/output/Data.Int/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.String":"/Users/anders/dev/ps/halo/ajax/output/Data.String/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Function/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Function

exports.mkFn0 = function (fn) {
  return function () {
    return fn({});
  };
};

exports.mkFn1 = function (fn) {
  return function (a) {
    return fn(a);
  };
};

exports.mkFn2 = function (fn) {
  /* jshint maxparams: 2 */
  return function (a, b) {
    return fn(a)(b);
  };
};

exports.mkFn3 = function (fn) {
  /* jshint maxparams: 3 */
  return function (a, b, c) {
    return fn(a)(b)(c);
  };
};

exports.mkFn4 = function (fn) {
  /* jshint maxparams: 4 */
  return function (a, b, c, d) {
    return fn(a)(b)(c)(d);
  };
};

exports.mkFn5 = function (fn) {
  /* jshint maxparams: 5 */
  return function (a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};

exports.mkFn6 = function (fn) {
  /* jshint maxparams: 6 */
  return function (a, b, c, d, e, f) {
    return fn(a)(b)(c)(d)(e)(f);
  };
};

exports.mkFn7 = function (fn) {
  /* jshint maxparams: 7 */
  return function (a, b, c, d, e, f, g) {
    return fn(a)(b)(c)(d)(e)(f)(g);
  };
};

exports.mkFn8 = function (fn) {
  /* jshint maxparams: 8 */
  return function (a, b, c, d, e, f, g, h) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h);
  };
};

exports.mkFn9 = function (fn) {
  /* jshint maxparams: 9 */
  return function (a, b, c, d, e, f, g, h, i) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
  };
};

exports.mkFn10 = function (fn) {
  /* jshint maxparams: 10 */
  return function (a, b, c, d, e, f, g, h, i, j) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
  };
};

exports.runFn0 = function (fn) {
  return fn();
};

exports.runFn1 = function (fn) {
  return function (a) {
    return fn(a);
  };
};

exports.runFn2 = function (fn) {
  return function (a) {
    return function (b) {
      return fn(a, b);
    };
  };
};

exports.runFn3 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return fn(a, b, c);
      };
    };
  };
};

exports.runFn4 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return fn(a, b, c, d);
        };
      };
    };
  };
};

exports.runFn5 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return fn(a, b, c, d, e);
          };
        };
      };
    };
  };
};

exports.runFn6 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return fn(a, b, c, d, e, f);
            };
          };
        };
      };
    };
  };
};

exports.runFn7 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return fn(a, b, c, d, e, f, g);
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn8 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return fn(a, b, c, d, e, f, g, h);
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn9 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return fn(a, b, c, d, e, f, g, h, i);
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn10 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return fn(a, b, c, d, e, f, g, h, i, j);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var on = function (f) {
    return function (g) {
        return function (x) {
            return function (y) {
                return f(g(x))(g(y));
            };
        };
    };
};
module.exports = {
    on: on, 
    runFn10: $foreign.runFn10, 
    runFn9: $foreign.runFn9, 
    runFn8: $foreign.runFn8, 
    runFn7: $foreign.runFn7, 
    runFn6: $foreign.runFn6, 
    runFn5: $foreign.runFn5, 
    runFn4: $foreign.runFn4, 
    runFn3: $foreign.runFn3, 
    runFn2: $foreign.runFn2, 
    runFn1: $foreign.runFn1, 
    runFn0: $foreign.runFn0, 
    mkFn10: $foreign.mkFn10, 
    mkFn9: $foreign.mkFn9, 
    mkFn8: $foreign.mkFn8, 
    mkFn7: $foreign.mkFn7, 
    mkFn6: $foreign.mkFn6, 
    mkFn5: $foreign.mkFn5, 
    mkFn4: $foreign.mkFn4, 
    mkFn3: $foreign.mkFn3, 
    mkFn2: $foreign.mkFn2, 
    mkFn1: $foreign.mkFn1, 
    mkFn0: $foreign.mkFn0
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/foreign.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Contravariant/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Contravariant = function (cmap) {
    this.cmap = cmap;
};
var cmap = function (dict) {
    return dict.cmap;
};
var $greater$dollar$less = function (dictContravariant) {
    return cmap(dictContravariant);
};
var $greater$hash$less = function (dictContravariant) {
    return function (x) {
        return function (f) {
            return $greater$dollar$less(dictContravariant)(f)(x);
        };
    };
};
module.exports = {
    Contravariant: Contravariant, 
    ">#<": $greater$hash$less, 
    ">$<": $greater$dollar$less, 
    cmap: cmap
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Coproduct/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Foldable = require("Data.Foldable");
var Data_Traversable = require("Data.Traversable");
var Coproduct = function (x) {
    return x;
};
var runCoproduct = function (v) {
    return v;
};
var right = function ($11) {
    return Coproduct(Data_Either.Right.create($11));
};
var left = function ($12) {
    return Coproduct(Data_Either.Left.create($12));
};
var coproduct = function (f) {
    return function (g) {
        return function ($13) {
            return Data_Either.either(f)(g)(runCoproduct($13));
        };
    };
};
var foldableCoproduct = function (dictFoldable) {
    return function (dictFoldable1) {
        return new Data_Foldable.Foldable(function (dictMonoid) {
            return function (f) {
                return coproduct(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f))(Data_Foldable.foldMap(dictFoldable1)(dictMonoid)(f));
            };
        }, function (f) {
            return function (z) {
                return coproduct(Data_Foldable.foldl(dictFoldable)(f)(z))(Data_Foldable.foldl(dictFoldable1)(f)(z));
            };
        }, function (f) {
            return function (z) {
                return coproduct(Data_Foldable.foldr(dictFoldable)(f)(z))(Data_Foldable.foldr(dictFoldable1)(f)(z));
            };
        });
    };
};
var functorCoproduct = function (dictFunctor) {
    return function (dictFunctor1) {
        return new Prelude.Functor(function (f) {
            return function ($14) {
                return Coproduct(coproduct(function ($15) {
                    return Data_Either.Left.create(Prelude["<$>"](dictFunctor)(f)($15));
                })(function ($16) {
                    return Data_Either.Right.create(Prelude["<$>"](dictFunctor1)(f)($16));
                })($14));
            };
        });
    };
};
var traversableCoproduct = function (dictTraversable) {
    return function (dictTraversable1) {
        return new Data_Traversable.Traversable(function () {
            return foldableCoproduct(dictTraversable["__superclass_Data.Foldable.Foldable_1"]())(dictTraversable1["__superclass_Data.Foldable.Foldable_1"]());
        }, function () {
            return functorCoproduct(dictTraversable["__superclass_Prelude.Functor_0"]())(dictTraversable1["__superclass_Prelude.Functor_0"]());
        }, function (dictApplicative) {
            return coproduct(function ($17) {
                return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($18) {
                    return Coproduct(Data_Either.Left.create($18));
                })(Data_Traversable.sequence(dictTraversable)(dictApplicative)($17));
            })(function ($19) {
                return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($20) {
                    return Coproduct(Data_Either.Right.create($20));
                })(Data_Traversable.sequence(dictTraversable1)(dictApplicative)($19));
            });
        }, function (dictApplicative) {
            return function (f) {
                return coproduct(function ($21) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($22) {
                        return Coproduct(Data_Either.Left.create($22));
                    })(Data_Traversable.traverse(dictTraversable)(dictApplicative)(f)($21));
                })(function ($23) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(function ($24) {
                        return Coproduct(Data_Either.Right.create($24));
                    })(Data_Traversable.traverse(dictTraversable1)(dictApplicative)(f)($23));
                });
            };
        });
    };
};
module.exports = {
    Coproduct: Coproduct, 
    coproduct: coproduct, 
    right: right, 
    left: left, 
    runCoproduct: runCoproduct, 
    functorCoproduct: functorCoproduct, 
    foldableCoproduct: foldableCoproduct, 
    traversableCoproduct: traversableCoproduct
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Invariant = function (imap) {
    this.imap = imap;
};
var imapF = function (dictFunctor) {
    return function ($1) {
        return Prelude["const"](Prelude.map(dictFunctor)($1));
    };
};
var invariantArray = new Invariant(imapF(Prelude.functorArray));
var invariantFn = new Invariant(imapF(Prelude.functorFn));
var imap = function (dict) {
    return dict.imap;
};
module.exports = {
    Invariant: Invariant, 
    imapF: imapF, 
    imap: imap, 
    invariantFn: invariantFn, 
    invariantArray: invariantArray
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Functor/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var $less$dollar = function (dictFunctor) {
    return function (x) {
        return function (f) {
            return Prelude["<$>"](dictFunctor)(Prelude["const"](x))(f);
        };
    };
};
var $dollar$greater = function (dictFunctor) {
    return function (f) {
        return function (x) {
            return Prelude["<$>"](dictFunctor)(Prelude["const"](x))(f);
        };
    };
};
module.exports = {
    "$>": $dollar$greater, 
    "<$": $less$dollar
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Foldable = require("Data.Foldable");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Monoid = require("Data.Monoid");
var Data_Traversable = require("Data.Traversable");
var Identity = function (x) {
    return x;
};
var showIdentity = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Identity (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semiringIdentity = function (dictSemiring) {
    return new Prelude.Semiring(function (v) {
        return function (v1) {
            return Prelude["+"](dictSemiring)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude["*"](dictSemiring)(v)(v1);
        };
    }, Prelude.one(dictSemiring), Prelude.zero(dictSemiring));
};
var semigroupIdenity = function (dictSemigroup) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["<>"](dictSemigroup)(v)(v1);
        };
    });
};
var runIdentity = function (v) {
    return v;
};
var ringIdentity = function (dictRing) {
    return new Prelude.Ring(function () {
        return semiringIdentity(dictRing["__superclass_Prelude.Semiring_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude["-"](dictRing)(v)(v1);
        };
    });
};
var monoidIdentity = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupIdenity(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var moduloSemiringIdentity = function (dictModuloSemiring) {
    return new Prelude.ModuloSemiring(function () {
        return semiringIdentity(dictModuloSemiring["__superclass_Prelude.Semiring_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude["/"](dictModuloSemiring)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude.mod(dictModuloSemiring)(v)(v1);
        };
    });
};
var functorIdentity = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var invariantIdentity = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorIdentity));
var foldableIdentity = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var traversableIdentity = new Data_Traversable.Traversable(function () {
    return foldableIdentity;
}, function () {
    return functorIdentity;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Identity)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Identity)(f(v));
        };
    };
});
var extendIdentity = new Control_Extend.Extend(function () {
    return functorIdentity;
}, function (f) {
    return function (m) {
        return f(m);
    };
});
var eqIdentity = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordIdentity = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqIdentity(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var divisionRingIdentity = function (dictDivisionRing) {
    return new Prelude.DivisionRing(function () {
        return moduloSemiringIdentity(dictDivisionRing["__superclass_Prelude.ModuloSemiring_1"]());
    }, function () {
        return ringIdentity(dictDivisionRing["__superclass_Prelude.Ring_0"]());
    });
};
var numIdentity = function (dictNum) {
    return new Prelude.Num(function () {
        return divisionRingIdentity(dictNum["__superclass_Prelude.DivisionRing_0"]());
    });
};
var comonadIdentity = new Control_Comonad.Comonad(function () {
    return extendIdentity;
}, function (v) {
    return v;
});
var boundedIdentity = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(dictBounded), Prelude.top(dictBounded));
};
var boundedOrdIdentity = function (dictBoundedOrd) {
    return new Prelude.BoundedOrd(function () {
        return boundedIdentity(dictBoundedOrd["__superclass_Prelude.Bounded_0"]());
    }, function () {
        return ordIdentity(dictBoundedOrd["__superclass_Prelude.Ord_1"]());
    });
};
var booleanAlgebraIdentity = function (dictBooleanAlgebra) {
    return new Prelude.BooleanAlgebra(function () {
        return boundedIdentity(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.conj(dictBooleanAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude.disj(dictBooleanAlgebra)(v)(v1);
        };
    }, function (v) {
        return Prelude.not(dictBooleanAlgebra)(v);
    });
};
var applyIdentity = new Prelude.Apply(function () {
    return functorIdentity;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindIdentity = new Prelude.Bind(function () {
    return applyIdentity;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeIdentity = new Prelude.Applicative(function () {
    return applyIdentity;
}, Identity);
var monadIdentity = new Prelude.Monad(function () {
    return applicativeIdentity;
}, function () {
    return bindIdentity;
});
module.exports = {
    Identity: Identity, 
    runIdentity: runIdentity, 
    eqIdentity: eqIdentity, 
    ordIdentity: ordIdentity, 
    boundedIdentity: boundedIdentity, 
    boundedOrdIdentity: boundedOrdIdentity, 
    booleanAlgebraIdentity: booleanAlgebraIdentity, 
    semigroupIdenity: semigroupIdenity, 
    monoidIdentity: monoidIdentity, 
    semiringIdentity: semiringIdentity, 
    moduloSemiringIdentity: moduloSemiringIdentity, 
    ringIdentity: ringIdentity, 
    divisionRingIdentity: divisionRingIdentity, 
    numIdentity: numIdentity, 
    showIdentity: showIdentity, 
    functorIdentity: functorIdentity, 
    invariantIdentity: invariantIdentity, 
    applyIdentity: applyIdentity, 
    applicativeIdentity: applicativeIdentity, 
    bindIdentity: bindIdentity, 
    monadIdentity: monadIdentity, 
    extendIdentity: extendIdentity, 
    comonadIdentity: comonadIdentity, 
    foldableIdentity: foldableIdentity, 
    traversableIdentity: traversableIdentity
};

},{"Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Inject/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Functor_Coproduct = require("Data.Functor.Coproduct");
var Data_Maybe = require("Data.Maybe");
var Inject = function (inj, prj) {
    this.inj = inj;
    this.prj = prj;
};
var prj = function (dict) {
    return dict.prj;
};
var injectReflexive = new Inject(Prelude.id(Prelude.categoryFn), Data_Maybe.Just.create);
var injectLeft = new Inject(function ($1) {
    return Data_Functor_Coproduct.Coproduct(Data_Either.Left.create($1));
}, Data_Functor_Coproduct.coproduct(Data_Maybe.Just.create)(Prelude["const"](Data_Maybe.Nothing.value)));
var inj = function (dict) {
    return dict.inj;
};
var injectRight = function (dictInject) {
    return new Inject(function ($2) {
        return Data_Functor_Coproduct.Coproduct(Data_Either.Right.create(inj(dictInject)($2)));
    }, Data_Functor_Coproduct.coproduct(Prelude["const"](Data_Maybe.Nothing.value))(prj(dictInject)));
};
module.exports = {
    Inject: Inject, 
    prj: prj, 
    inj: inj, 
    injectReflexive: injectReflexive, 
    injectLeft: injectLeft, 
    injectRight: injectRight
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Functor.Coproduct":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Coproduct/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Injector/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Const = require("Data.Const");
var Data_Either = require("Data.Either");
var Data_Functor_Coproduct = require("Data.Functor.Coproduct");
var Data_Identity = require("Data.Identity");
var Data_Maybe = require("Data.Maybe");
var Data_Maybe_First = require("Data.Maybe.First");
var Data_Profunctor = require("Data.Profunctor");
var Data_Profunctor_Choice = require("Data.Profunctor.Choice");
var Tagged = function (x) {
    return x;
};
var unTagged = function (v) {
    return v;
};
var profunctorTagged = new Data_Profunctor.Profunctor(function (v) {
    return function (f) {
        return function (v1) {
            return f(v1);
        };
    };
});
var prj = function (p) {
    return function ($25) {
        return Data_Maybe_First.runFirst(Data_Const.getConst(p(Data_Profunctor_Choice.choiceFn)(Data_Const.applicativeConst(Data_Maybe_First.monoidFirst))(function ($26) {
            return Data_Const.Const(Data_Maybe_First.First(Data_Maybe.Just.create($26)));
        })($25)));
    };
};
var prism = function (f) {
    return function (g) {
        return function (dictChoice) {
            return function (dictApplicative) {
                return function ($27) {
                    return Data_Profunctor.dimap(dictChoice["__superclass_Data.Profunctor.Profunctor_0"]())(g)(Data_Either.either(Prelude.pure(dictApplicative))(Prelude.map((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(f)))(Data_Profunctor_Choice.right(dictChoice)($27));
                };
            };
        };
    };
};
var prism$prime = function (f) {
    return function (g) {
        return function (dictChoice) {
            return function (dictApplicative) {
                return prism(f)(function (s) {
                    return Data_Maybe.maybe(new Data_Either.Left(s))(Data_Either.Right.create)(g(s));
                })(dictChoice)(dictApplicative);
            };
        };
    };
};
var injRE = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Data_Either.Right.create)(Data_Either.either(Prelude["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create))(dictChoice)(dictApplicative);
    };
};
var injRC = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Data_Functor_Coproduct.right)(Data_Functor_Coproduct.coproduct(Prelude["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create))(dictChoice)(dictApplicative);
    };
};
var injLE = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Data_Either.Left.create)(Data_Either.either(Data_Maybe.Just.create)(Prelude["const"](Data_Maybe.Nothing.value)))(dictChoice)(dictApplicative);
    };
};
var injLC = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Data_Functor_Coproduct.left)(Data_Functor_Coproduct.coproduct(Data_Maybe.Just.create)(Prelude["const"](Data_Maybe.Nothing.value)))(dictChoice)(dictApplicative);
    };
};
var injI = function (dictChoice) {
    return function (dictApplicative) {
        return prism$prime(Prelude.id(Prelude.categoryFn))(Data_Maybe.Just.create)(dictChoice)(dictApplicative);
    };
};
var choiceTagged = new Data_Profunctor_Choice.Choice(function () {
    return profunctorTagged;
}, function (v) {
    return new Data_Either.Left(v);
}, function (v) {
    return new Data_Either.Right(v);
});
var inj = function (p) {
    return function ($28) {
        return Data_Identity.runIdentity(unTagged(p(choiceTagged)(Data_Identity.applicativeIdentity)(Tagged(Data_Identity.Identity($28)))));
    };
};
module.exports = {
    injRC: injRC, 
    injRE: injRE, 
    injLC: injLC, 
    injLE: injLE, 
    injI: injI, 
    prj: prj, 
    inj: inj
};

},{"Data.Const":"/Users/anders/dev/ps/halo/ajax/output/Data.Const/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Functor.Coproduct":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Coproduct/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.First":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.First/index.js","Data.Profunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Profunctor/index.js","Data.Profunctor.Choice":"/Users/anders/dev/ps/halo/ajax/output/Data.Profunctor.Choice/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Int.Bits/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Int.Bits

exports.andImpl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 & n2;
  };
};

exports.orImpl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 | n2;
  };
};

exports.xorImpl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 ^ n2;
  };
};

exports.shl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 << n2;
  };
};

exports.shr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >> n2;
  };
};

exports.zshr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return n1 >>> n2;
  };
};

exports.complement = function (n) {
  /* jshint bitwise: false */
  return ~n;
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Int.Bits/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var $dot$bar$dot = $foreign.orImpl;
var $dot$up$dot = $foreign.xorImpl;
var $dot$amp$dot = $foreign.andImpl;
module.exports = {
    ".^.": $dot$up$dot, 
    ".|.": $dot$bar$dot, 
    ".&.": $dot$amp$dot, 
    complement: $foreign.complement, 
    zshr: $foreign.zshr, 
    shr: $foreign.shr, 
    shl: $foreign.shl
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Int.Bits/foreign.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Int/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Int

exports.fromNumberImpl = function (just) {
  return function (nothing) {
    return function (n) {
      /* jshint bitwise: false */
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};

exports.toNumber = function (n) {
  return n;
};

exports.fromStringImpl = function (just) {
  return function (nothing) {
    return function (s) {
      /* jshint bitwise: false */
      var i = parseFloat(s);
      return (i | 0) === i ? just(i) : nothing;
    };
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Int/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Int_Bits = require("Data.Int.Bits");
var Data_Maybe = require("Data.Maybe");
var Data_Maybe_Unsafe = require("Data.Maybe.Unsafe");
var $$Math = require("Math");
var odd = function (x) {
    return (x & 1) !== 0;
};
var fromString = $foreign.fromStringImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unsafeClamp = function (x) {
    if (x >= $foreign.toNumber(Prelude.top(Prelude.boundedInt))) {
        return Prelude.top(Prelude.boundedInt);
    };
    if (x <= $foreign.toNumber(Prelude.bottom(Prelude.boundedInt))) {
        return Prelude.bottom(Prelude.boundedInt);
    };
    if (Prelude.otherwise) {
        return Data_Maybe_Unsafe.fromJust(fromNumber(x));
    };
    throw new Error("Failed pattern match at Data.Int line 48, column 1 - line 49, column 1: " + [ x.constructor.name ]);
};
var round = function ($1) {
    return unsafeClamp($$Math.round($1));
};
var floor = function ($2) {
    return unsafeClamp($$Math.floor($2));
};
var even = function (x) {
    return (x & 1) === 0;
};
var ceil = function ($3) {
    return unsafeClamp($$Math.ceil($3));
};
module.exports = {
    odd: odd, 
    even: even, 
    fromString: fromString, 
    round: round, 
    floor: floor, 
    ceil: ceil, 
    fromNumber: fromNumber, 
    toNumber: $foreign.toNumber
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Int/foreign.js","Data.Int.Bits":"/Users/anders/dev/ps/halo/ajax/output/Data.Int.Bits/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/index.js","Math":"/Users/anders/dev/ps/halo/ajax/output/Math/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.JSON/foreign.js":[function(require,module,exports){
  /* global exports */
"use strict";

// module Data.JSON

exports.jsonParseImpl = function jsonParseImpl(left, right, string) {
    try       { return right(JSON.parse(string)); }
    catch (e) { return left(e.toString()); }
};

exports.jsonToValueImpl = function jsonToValueImpl(auxes, ctors) {
    var left   = auxes.left;
    var right  = auxes.right;
    var either = auxes.either;
    var insert = auxes.insert;
    var empty  = auxes.empty;
    var Null   = ctors.null;
    var Number = ctors.number;
    var Int    = ctors.int;
    var Bool   = ctors.bool;
    var String = ctors.string;
    var Array  = ctors.array;
    var Object = ctors.object;
    var parse  = function(json) {
        var typ = Object.prototype.toString.call(json).slice(8,-1);
        if (typ === 'Number') {
            return right((json | 0) === json ? Int(json) : Number(json));
        } else if (typ === 'Boolean') {
            return right(Bool(json));
        } else if (typ === 'String') {
            return right(String(json));
        } else if (typ === 'Null') {
            return right(Null);
        } else if (typ === 'Array') {
            var ary = [];
            for(var i = 0; i < json.length; i++) {
                either
                    (function(l){return left(l)})
                    (function(r){ary.push(r)})
                    (parse(json[i]))
            }
            return right(Array(ary));
        } else if (typ === 'Object') {
            var obj = empty;
            for(var k in json) {
                either
                    (function(l){return left(l)})
                    (function(r){obj = insert(k)(r)(obj)})
                    (parse(json[k]));
            }
            return right(Object(obj));
        } else {
            return left('unknown type: ' + typ);
        }
   };
   return parse;
};

exports.jsNull = null;

exports.unsafeCoerce = function unsafeCoerce(a) {
  return a;
};

exports.objToHash = function objToHash(valueToJSONImpl, fst, snd, obj) {
    var hash = {};
    for(var i = 0; i < obj.length; i++) {
        hash[fst(obj[i])] = valueToJSONImpl(snd(obj[i]));
    }
    return hash;
};

exports.jsonStringify = function jsonStringify(json) {
  return JSON.stringify(json);
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.JSON/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Map = require("Data.Map");
var Data_Set = require("Data.Set");
var Data_Either = require("Data.Either");
var Data_Int = require("Data.Int");
var Data_Maybe = require("Data.Maybe");
var Data_Function = require("Data.Function");
var Data_Tuple = require("Data.Tuple");
var Data_Traversable = require("Data.Traversable");
var Data_List = require("Data.List");
var Data_Unfoldable = require("Data.Unfoldable");
var Data_Foldable = require("Data.Foldable");
var JObject = (function () {
    function JObject(value0) {
        this.value0 = value0;
    };
    JObject.create = function (value0) {
        return new JObject(value0);
    };
    return JObject;
})();
var JArray = (function () {
    function JArray(value0) {
        this.value0 = value0;
    };
    JArray.create = function (value0) {
        return new JArray(value0);
    };
    return JArray;
})();
var JString = (function () {
    function JString(value0) {
        this.value0 = value0;
    };
    JString.create = function (value0) {
        return new JString(value0);
    };
    return JString;
})();
var JNumber = (function () {
    function JNumber(value0) {
        this.value0 = value0;
    };
    JNumber.create = function (value0) {
        return new JNumber(value0);
    };
    return JNumber;
})();
var JInt = (function () {
    function JInt(value0) {
        this.value0 = value0;
    };
    JInt.create = function (value0) {
        return new JInt(value0);
    };
    return JInt;
})();
var JBool = (function () {
    function JBool(value0) {
        this.value0 = value0;
    };
    JBool.create = function (value0) {
        return new JBool(value0);
    };
    return JBool;
})();
var JNull = (function () {
    function JNull() {

    };
    JNull.value = new JNull();
    return JNull;
})();
var FromJSON = function (parseJSON) {
    this.parseJSON = parseJSON;
};
var ToJSON = function (toJSON) {
    this.toJSON = toJSON;
};
var $dot$bang$eq = function (pmval) {
    return function (val) {
        return Prelude["<$>"](Data_Either.functorEither)(Data_Maybe.fromMaybe(val))(pmval);
    };
};
var valueToJSONImpl = function (v) {
    if (v instanceof JObject) {
        return $foreign.objToHash(valueToJSONImpl, Data_Tuple.fst, Data_Tuple.snd, Data_List.fromList(Data_Unfoldable.unfoldableArray)(Data_Map.toList(v.value0)));
    };
    if (v instanceof JArray) {
        return $foreign.unsafeCoerce(Prelude["<$>"](Prelude.functorArray)(valueToJSONImpl)(v.value0));
    };
    if (v instanceof JString) {
        return $foreign.unsafeCoerce(v.value0);
    };
    if (v instanceof JNumber) {
        return $foreign.unsafeCoerce(v.value0);
    };
    if (v instanceof JInt) {
        return $foreign.unsafeCoerce(v.value0);
    };
    if (v instanceof JBool) {
        return $foreign.unsafeCoerce(v.value0);
    };
    if (v instanceof JNull) {
        return $foreign.jsNull;
    };
    throw new Error("Failed pattern match: " + [ v.constructor.name ]);
};
var valueToString = function (v) {
    return $foreign.jsonStringify(valueToJSONImpl(v));
};
var valueToJSON = new ToJSON(Prelude.id(Prelude.categoryFn));
var valueFromJSON = new FromJSON(Data_Either.Right.create);
var unitToJSON = new ToJSON(function (v) {
    return JNull.value;
});
var toJSON = function (dict) {
    return dict.toJSON;
};
var tupleToJSON = function (dictToJSON) {
    return function (dictToJSON1) {
        return new ToJSON(function (v) {
            return new JArray([ toJSON(dictToJSON)(v.value0), toJSON(dictToJSON1)(v.value1) ]);
        });
    };
};
var $dot$eq = function (dictToJSON) {
    return function (name) {
        return function (value) {
            return new Data_Tuple.Tuple(name, toJSON(dictToJSON)(value));
        };
    };
};
var stringToJSON = new ToJSON(JString.create);
var showValue = new Prelude.Show(function (v) {
    if (v instanceof JObject) {
        return "JObject " + Prelude.show(Data_Map.showMap(Prelude.showString)(showValue))(v.value0);
    };
    if (v instanceof JArray) {
        return "JArray " + Prelude.show(Prelude.showArray(showValue))(v.value0);
    };
    if (v instanceof JString) {
        return "JString " + Prelude.show(Prelude.showString)(v.value0);
    };
    if (v instanceof JNumber) {
        return "JNumber " + Prelude.show(Prelude.showNumber)(v.value0);
    };
    if (v instanceof JInt) {
        return "JInt " + Prelude.show(Prelude.showInt)(v.value0);
    };
    if (v instanceof JBool) {
        return "JBool " + Prelude.show(Prelude.showBoolean)(v.value0);
    };
    if (v instanceof JNull) {
        return "JNull";
    };
    throw new Error("Failed pattern match: " + [ v.constructor.name ]);
});
var setToJSON = function (dictToJSON) {
    return new ToJSON(function (s) {
        return JArray.create(Data_List.fromList(Data_Unfoldable.unfoldableArray)(Prelude["<$>"](Data_List.functorList)(toJSON(dictToJSON))(Data_Set.toList(s))));
    });
};
var parseJSON = function (dict) {
    return dict.parseJSON;
};
var $dot$colon = function (dictFromJSON) {
    return function (obj) {
        return function (key) {
            var $59 = Data_Map.lookup(Prelude.ordString)(key)(obj);
            if ($59 instanceof Data_Maybe.Nothing) {
                return Data_Either.Left.create("key " + (Prelude.show(Prelude.showString)(key) + " not present"));
            };
            if ($59 instanceof Data_Maybe.Just) {
                return parseJSON(dictFromJSON)($59.value0);
            };
            throw new Error("Failed pattern match at Data.JSON line 128, column 1 - line 129, column 1: " + [ $59.constructor.name ]);
        };
    };
};
var object = function (ps) {
    return JObject.create(Data_Map.fromList(Prelude.ordString)(Data_List.toList(Data_Foldable.foldableArray)(ps)));
};
var numberToJSON = new ToJSON(JNumber.create);
var maybeToJSON = function (dictToJSON) {
    return new ToJSON(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return JNull.value;
        };
        if (v instanceof Data_Maybe.Just) {
            return toJSON(dictToJSON)(v.value0);
        };
        throw new Error("Failed pattern match at Data.JSON line 215, column 1 - line 219, column 1: " + [ v.constructor.name ]);
    });
};
var maybeFromJSON = function (dictFromJSON) {
    return new FromJSON(function (a) {
        return Prelude["return"](Data_Either.applicativeEither)((function () {
            var $63 = parseJSON(dictFromJSON)(a);
            if ($63 instanceof Data_Either.Left) {
                return Data_Maybe.Nothing.value;
            };
            if ($63 instanceof Data_Either.Right) {
                return new Data_Maybe.Just($63.value0);
            };
            throw new Error("Failed pattern match at Data.JSON line 112, column 1 - line 117, column 1: " + [ $63.constructor.name ]);
        })());
    });
};
var $dot$colon$qmark = function (dictFromJSON) {
    return function (obj) {
        return function (key) {
            var $66 = Data_Map.lookup(Prelude.ordString)(key)(obj);
            if ($66 instanceof Data_Maybe.Nothing) {
                return Prelude["return"](Data_Either.applicativeEither)(Data_Maybe.Nothing.value);
            };
            if ($66 instanceof Data_Maybe.Just) {
                return parseJSON(maybeFromJSON(dictFromJSON))($66.value0);
            };
            throw new Error("Failed pattern match at Data.JSON line 133, column 1 - line 134, column 1: " + [ $66.constructor.name ]);
        };
    };
};
var mapToJSON = function (dictToJSON) {
    return new ToJSON(function (m) {
        return JObject.create(Prelude.map(Data_Map.functorMap)(toJSON(dictToJSON))(m));
    });
};
var jsonToValue = function (s) {
    var insertʹ = Data_Map.insert(Prelude.ordString);
    var ctors = {
        "null": JNull.value, 
        number: JNumber.create, 
        "int": JInt.create, 
        bool: JBool.create, 
        string: JString.create, 
        array: JArray.create, 
        object: JObject.create
    };
    var auxes = {
        left: Data_Either.Left.create, 
        right: Data_Either.Right.create, 
        either: Data_Either.either, 
        insert: insertʹ, 
        empty: Data_Map.empty
    };
    return $foreign.jsonParseImpl(Data_Either.Left.create, $foreign.jsonToValueImpl(auxes, ctors), s);
};
var jsonParse = function (s) {
    return $foreign.jsonParseImpl(Data_Either.Left.create, Data_Either.Right.create, s);
};
var intToJSON = new ToJSON(JInt.create);
var fail = Data_Either.Left.create;
var intFromJSON = new FromJSON(function (v) {
    if (v instanceof JInt) {
        return Prelude["return"](Data_Either.applicativeEither)(v.value0);
    };
    if (v instanceof JNumber) {
        return Data_Maybe.maybe(fail(Prelude.show(Prelude.showNumber)(v.value0) + " is not Int."))(Prelude["return"](Data_Either.applicativeEither))(Data_Int.fromNumber(v.value0));
    };
    return fail(Prelude.show(showValue)(v) + " is not Int.");
});
var mapFromJSON = function (dictFromJSON) {
    return new FromJSON(function (v) {
        if (v instanceof JObject) {
            var fn = function (v1) {
                var $73 = parseJSON(dictFromJSON)(v1.value1);
                if ($73 instanceof Data_Either.Right) {
                    return Prelude["return"](Data_Either.applicativeEither)(new Data_Tuple.Tuple(v1.value0, $73.value0));
                };
                if ($73 instanceof Data_Either.Left) {
                    return fail($73.value0);
                };
                throw new Error("Failed pattern match at Data.JSON line 123, column 9 - line 126, column 5: " + [ $73.constructor.name ]);
            };
            return Prelude["<$>"](Data_Either.functorEither)(Data_Map.fromList(Prelude.ordString))(Data_Traversable.sequence(Data_List.traversableList)(Data_Either.applicativeEither)(Prelude["<$>"](Data_List.functorList)(fn)(Data_Map.toList(v.value0))));
        };
        return fail(Prelude.show(showValue)(v) + " is not (Map String a).");
    });
};
var numberFromJSON = new FromJSON(function (v) {
    if (v instanceof JNumber) {
        return Prelude["return"](Data_Either.applicativeEither)(v.value0);
    };
    if (v instanceof JInt) {
        return Prelude["return"](Data_Either.applicativeEither)(Data_Int.toNumber(v.value0));
    };
    return fail(Prelude.show(showValue)(v) + " is not Number.");
});
var stringFromJSON = new FromJSON(function (v) {
    if (v instanceof JString) {
        return Prelude["return"](Data_Either.applicativeEither)(v.value0);
    };
    return fail(Prelude.show(showValue)(v) + " is not String.");
});
var tupleFromJSON = function (dictFromJSON) {
    return function (dictFromJSON1) {
        return new FromJSON(function (v) {
            if (v instanceof JArray && v.value0.length === 2) {
                return Prelude["<*>"](Data_Either.applyEither)(Prelude["<$>"](Data_Either.functorEither)(Data_Tuple.Tuple.create)(parseJSON(dictFromJSON)(v.value0[0])))(parseJSON(dictFromJSON1)(v.value0[1]));
            };
            return fail(Prelude.show(showValue)(v) + " is not (a,b).");
        });
    };
};
var unitFromJSON = new FromJSON(function (v) {
    if (v instanceof JNull) {
        return Prelude["return"](Data_Either.applicativeEither)(Prelude.unit);
    };
    return fail(Prelude.show(showValue)(v) + " is not Null.");
});
var eqValue = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof JObject && v1 instanceof JObject) {
            return Prelude["=="](Data_Map.eqMap(Prelude.eqString)(eqValue))(v.value0)(v1.value0);
        };
        if (v instanceof JArray && v1 instanceof JArray) {
            return Prelude["=="](Prelude.eqArray(eqValue))(v.value0)(v1.value0);
        };
        if (v instanceof JString && v1 instanceof JString) {
            return Prelude["=="](Prelude.eqString)(v.value0)(v1.value0);
        };
        if (v instanceof JNumber && v1 instanceof JNumber) {
            return v.value0 === v1.value0;
        };
        if (v instanceof JInt && v1 instanceof JInt) {
            return v.value0 === v1.value0;
        };
        if (v instanceof JBool && v1 instanceof JBool) {
            return Prelude["=="](Prelude.eqBoolean)(v.value0)(v1.value0);
        };
        if (v instanceof JNull && v1 instanceof JNull) {
            return true;
        };
        return false;
    };
});
var encode = function (dictToJSON) {
    return function (a) {
        return valueToString(toJSON(dictToJSON)(a));
    };
};
var eitherToJSON = function (dictToJSON) {
    return function (dictToJSON1) {
        return new ToJSON(function (v) {
            if (v instanceof Data_Either.Right) {
                return object([ $dot$eq(dictToJSON1)("Right")(v.value0) ]);
            };
            if (v instanceof Data_Either.Left) {
                return object([ $dot$eq(dictToJSON)("Left")(v.value0) ]);
            };
            throw new Error("Failed pattern match at Data.JSON line 208, column 1 - line 212, column 1: " + [ v.constructor.name ]);
        });
    };
};
var eitherFromJSON = function (dictFromJSON) {
    return function (dictFromJSON1) {
        return new FromJSON(function (v) {
            if (v instanceof JObject) {
                var $107 = Data_List.fromList(Data_Unfoldable.unfoldableArray)(Data_Map.toList(v.value0));
                if ($107.length === 1 && ($107[0]).value0 === "Right") {
                    return Prelude["<$>"](Data_Either.functorEither)(Data_Either.Right.create)(parseJSON(dictFromJSON1)(($107[0]).value1));
                };
                if ($107.length === 1 && ($107[0]).value0 === "Left") {
                    return Prelude["<$>"](Data_Either.functorEither)(Data_Either.Left.create)(parseJSON(dictFromJSON)(($107[0]).value1));
                };
                return fail(Prelude.show(Data_Map.showMap(Prelude.showString)(showValue))(v.value0) + " is not (Either a b).");
            };
            return fail(Prelude.show(showValue)(v) + " is not (Either a b).");
        });
    };
};
var eitherDecode = function (dictFromJSON) {
    return function (s) {
        return Prelude.bind(Data_Either.bindEither)(jsonToValue(s))(function (v) {
            return parseJSON(dictFromJSON)(v);
        });
    };
};
var decode = function (dictFromJSON) {
    return function (s) {
        var $116 = eitherDecode(dictFromJSON)(s);
        if ($116 instanceof Data_Either.Right) {
            return new Data_Maybe.Just($116.value0);
        };
        if ($116 instanceof Data_Either.Left) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Data.JSON line 64, column 1 - line 65, column 1: " + [ $116.constructor.name ]);
    };
};
var boolToJSON = new ToJSON(JBool.create);
var boolFromJSON = new FromJSON(function (v) {
    if (v instanceof JBool) {
        return new Data_Either.Right(v.value0);
    };
    return fail(Prelude.show(showValue)(v) + " is not Boolean.");
});
var arrayToJSON = function (dictToJSON) {
    return new ToJSON(function (a) {
        return JArray.create(Prelude["<$>"](Prelude.functorArray)(toJSON(dictToJSON))(a));
    });
};
var arrayFromJSON = function (dictFromJSON) {
    return new FromJSON(function (v) {
        if (v instanceof JArray) {
            return Data_Traversable.sequence(Data_Traversable.traversableArray)(Data_Either.applicativeEither)(Prelude["<$>"](Prelude.functorArray)(parseJSON(dictFromJSON))(v.value0));
        };
        return fail(Prelude.show(showValue)(v) + " is not [a].");
    });
};
var setFromJSON = function (dictOrd) {
    return function (dictFromJSON) {
        return new FromJSON(function (x) {
            return Prelude["<$>"](Data_Either.functorEither)(Prelude["<$>"](Prelude.functorFn)(Data_Set.fromList(dictOrd))(Data_List.toList(Data_Foldable.foldableArray)))(parseJSON(arrayFromJSON(dictFromJSON))(x));
        });
    };
};
module.exports = {
    JObject: JObject, 
    JArray: JArray, 
    JString: JString, 
    JNumber: JNumber, 
    JInt: JInt, 
    JBool: JBool, 
    JNull: JNull, 
    ToJSON: ToJSON, 
    FromJSON: FromJSON, 
    object: object, 
    ".=": $dot$eq, 
    encode: encode, 
    toJSON: toJSON, 
    ".!=": $dot$bang$eq, 
    ".:?": $dot$colon$qmark, 
    ".:": $dot$colon, 
    eitherDecode: eitherDecode, 
    decode: decode, 
    fail: fail, 
    parseJSON: parseJSON, 
    showValue: showValue, 
    eqValue: eqValue, 
    valueFromJSON: valueFromJSON, 
    boolFromJSON: boolFromJSON, 
    numberFromJSON: numberFromJSON, 
    intFromJSON: intFromJSON, 
    unitFromJSON: unitFromJSON, 
    stringFromJSON: stringFromJSON, 
    arrayFromJSON: arrayFromJSON, 
    tupleFromJSON: tupleFromJSON, 
    eitherFromJSON: eitherFromJSON, 
    maybeFromJSON: maybeFromJSON, 
    setFromJSON: setFromJSON, 
    mapFromJSON: mapFromJSON, 
    boolToJSON: boolToJSON, 
    numberToJSON: numberToJSON, 
    intToJSON: intToJSON, 
    stringToJSON: stringToJSON, 
    unitToJSON: unitToJSON, 
    arrayToJSON: arrayToJSON, 
    eitherToJSON: eitherToJSON, 
    mapToJSON: mapToJSON, 
    maybeToJSON: maybeToJSON, 
    setToJSON: setToJSON, 
    tupleToJSON: tupleToJSON, 
    valueToJSON: valueToJSON
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.JSON/foreign.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Data.Int":"/Users/anders/dev/ps/halo/ajax/output/Data.Int/index.js","Data.List":"/Users/anders/dev/ps/halo/ajax/output/Data.List/index.js","Data.Map":"/Users/anders/dev/ps/halo/ajax/output/Data.Map/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Set":"/Users/anders/dev/ps/halo/ajax/output/Data.Set/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Data.Unfoldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Unfoldable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Lazy/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Lazy

exports.defer = function () {

  function Defer (thunk) {
    if (this instanceof Defer) {
      this.thunk = thunk;
      return this;
    } else {
      return new Defer(thunk);
    }
  }

  Defer.prototype.force = function () {
    var value = this.thunk();
    delete this.thunk;
    this.force = function () {
      return value;
    };
    return value;
  };

  return Defer;

}();

exports.force = function (l) {
  return l.force();
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Lazy/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Monoid = require("Data.Monoid");
var Control_Lazy = require("Control.Lazy");
var showLazy = function (dictShow) {
    return new Prelude.Show(function (x) {
        return "Lazy " + Prelude.show(dictShow)($foreign.force(x));
    });
};
var semiringLazy = function (dictSemiring) {
    return new Prelude.Semiring(function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["+"](dictSemiring)($foreign.force(a))($foreign.force(b));
            });
        };
    }, function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["*"](dictSemiring)($foreign.force(a))($foreign.force(b));
            });
        };
    }, $foreign.defer(function (v) {
        return Prelude.one(dictSemiring);
    }), $foreign.defer(function (v) {
        return Prelude.zero(dictSemiring);
    }));
};
var semigroupLazy = function (dictSemigroup) {
    return new Prelude.Semigroup(function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["<>"](dictSemigroup)($foreign.force(a))($foreign.force(b));
            });
        };
    });
};
var ringLazy = function (dictRing) {
    return new Prelude.Ring(function () {
        return semiringLazy(dictRing["__superclass_Prelude.Semiring_0"]());
    }, function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["-"](dictRing)($foreign.force(a))($foreign.force(b));
            });
        };
    });
};
var monoidLazy = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupLazy(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, $foreign.defer(function (v) {
        return Data_Monoid.mempty(dictMonoid);
    }));
};
var moduloSemiringLazy = function (dictModuloSemiring) {
    return new Prelude.ModuloSemiring(function () {
        return semiringLazy(dictModuloSemiring["__superclass_Prelude.Semiring_0"]());
    }, function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude["/"](dictModuloSemiring)($foreign.force(a))($foreign.force(b));
            });
        };
    }, function (a) {
        return function (b) {
            return $foreign.defer(function (v) {
                return Prelude.mod(dictModuloSemiring)($foreign.force(a))($foreign.force(b));
            });
        };
    });
};
var lazyLazy = new Control_Lazy.Lazy(function (f) {
    return $foreign.defer(function (v) {
        return $foreign.force(f(Prelude.unit));
    });
});
var functorLazy = new Prelude.Functor(function (f) {
    return function (l) {
        return $foreign.defer(function (v) {
            return f($foreign.force(l));
        });
    };
});
var extendLazy = new Control_Extend.Extend(function () {
    return functorLazy;
}, function (f) {
    return function (x) {
        return $foreign.defer(function (v) {
            return f(x);
        });
    };
});
var eqLazy = function (dictEq) {
    return new Prelude.Eq(function (x) {
        return function (y) {
            return Prelude["=="](dictEq)($foreign.force(x))($foreign.force(y));
        };
    });
};
var ordLazy = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqLazy(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (x) {
        return function (y) {
            return Prelude.compare(dictOrd)($foreign.force(x))($foreign.force(y));
        };
    });
};
var divisionRingLazy = function (dictDivisionRing) {
    return new Prelude.DivisionRing(function () {
        return moduloSemiringLazy(dictDivisionRing["__superclass_Prelude.ModuloSemiring_1"]());
    }, function () {
        return ringLazy(dictDivisionRing["__superclass_Prelude.Ring_0"]());
    });
};
var numLazy = function (dictNum) {
    return new Prelude.Num(function () {
        return divisionRingLazy(dictNum["__superclass_Prelude.DivisionRing_0"]());
    });
};
var comonadLazy = new Control_Comonad.Comonad(function () {
    return extendLazy;
}, $foreign.force);
var boundedLazy = function (dictBounded) {
    return new Prelude.Bounded($foreign.defer(function (v) {
        return Prelude.bottom(dictBounded);
    }), $foreign.defer(function (v) {
        return Prelude.top(dictBounded);
    }));
};
var boundedOrdLazy = function (dictBoundedOrd) {
    return new Prelude.BoundedOrd(function () {
        return boundedLazy(dictBoundedOrd["__superclass_Prelude.Bounded_0"]());
    }, function () {
        return ordLazy(dictBoundedOrd["__superclass_Prelude.Ord_1"]());
    });
};
var applyLazy = new Prelude.Apply(function () {
    return functorLazy;
}, function (f) {
    return function (x) {
        return $foreign.defer(function (v) {
            return $foreign.force(f)($foreign.force(x));
        });
    };
});
var bindLazy = new Prelude.Bind(function () {
    return applyLazy;
}, function (l) {
    return function (f) {
        return $foreign.defer(function (v) {
            return $foreign.force(f($foreign.force(l)));
        });
    };
});
var booleanAlgebraLazy = function (dictBooleanAlgebra) {
    return new Prelude.BooleanAlgebra(function () {
        return boundedLazy(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]());
    }, function (a) {
        return function (b) {
            return Prelude["<*>"](applyLazy)(Prelude["<$>"](functorLazy)(Prelude.conj(dictBooleanAlgebra))(a))(b);
        };
    }, function (a) {
        return function (b) {
            return Prelude["<*>"](applyLazy)(Prelude["<$>"](functorLazy)(Prelude.disj(dictBooleanAlgebra))(a))(b);
        };
    }, function (a) {
        return Prelude["<$>"](functorLazy)(Prelude.not(dictBooleanAlgebra))(a);
    });
};
var applicativeLazy = new Prelude.Applicative(function () {
    return applyLazy;
}, function (a) {
    return $foreign.defer(function (v) {
        return a;
    });
});
var monadLazy = new Prelude.Monad(function () {
    return applicativeLazy;
}, function () {
    return bindLazy;
});
module.exports = {
    semiringLazy: semiringLazy, 
    ringLazy: ringLazy, 
    moduloSemiringLazy: moduloSemiringLazy, 
    divisionRingLazy: divisionRingLazy, 
    numLazy: numLazy, 
    eqLazy: eqLazy, 
    ordLazy: ordLazy, 
    boundedLazy: boundedLazy, 
    boundedOrdLazy: boundedOrdLazy, 
    semigroupLazy: semigroupLazy, 
    monoidLazy: monoidLazy, 
    booleanAlgebraLazy: booleanAlgebraLazy, 
    functorLazy: functorLazy, 
    applyLazy: applyLazy, 
    applicativeLazy: applicativeLazy, 
    bindLazy: bindLazy, 
    monadLazy: monadLazy, 
    extendLazy: extendLazy, 
    comonadLazy: comonadLazy, 
    showLazy: showLazy, 
    lazyLazy: lazyLazy, 
    force: $foreign.force, 
    defer: $foreign.defer
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Lazy/foreign.js","Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Control.Lazy":"/Users/anders/dev/ps/halo/ajax/output/Control.Lazy/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.List/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Lazy = require("Control.Lazy");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var Data_Foldable = require("Data.Foldable");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var Data_Traversable = require("Data.Traversable");
var Data_Tuple = require("Data.Tuple");
var Data_Unfoldable = require("Data.Unfoldable");
var Nil = (function () {
    function Nil() {

    };
    Nil.value = new Nil();
    return Nil;
})();
var Cons = (function () {
    function Cons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Cons.create = function (value0) {
        return function (value1) {
            return new Cons(value0, value1);
        };
    };
    return Cons;
})();
var $colon = Cons.create;
var updateAt = function (v) {
    return function (x) {
        return function (v1) {
            if (v === 0 && v1 instanceof Cons) {
                return new Data_Maybe.Just(new Cons(x, v1.value1));
            };
            if (v1 instanceof Cons) {
                return Prelude["<$>"](Data_Maybe.functorMaybe)(Cons.create(v1.value0))(updateAt(v - 1)(x)(v1.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var unfoldableList = new Data_Unfoldable.Unfoldable(function (f) {
    return function (b) {
        var go = function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Nil.value;
            };
            if (v instanceof Data_Maybe.Just) {
                return new Cons(v.value0.value0, go(f(v.value0.value1)));
            };
            throw new Error("Failed pattern match at Data.List line 735, column 1 - line 741, column 1: " + [ v.constructor.name ]);
        };
        return go(f(b));
    };
});
var uncons = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just({
            head: v.value0, 
            tail: v.value1
        });
    };
    throw new Error("Failed pattern match at Data.List line 267, column 1 - line 268, column 1: " + [ v.constructor.name ]);
};
var toList = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(Cons.create)(Nil.value);
};
var tail = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just(v.value1);
    };
    throw new Error("Failed pattern match at Data.List line 248, column 1 - line 249, column 1: " + [ v.constructor.name ]);
};
var span = function (p) {
    return function (v) {
        if (v instanceof Cons && p(v.value0)) {
            var $117 = span(p)(v.value1);
            return {
                init: new Cons(v.value0, $117.init), 
                rest: $117.rest
            };
        };
        return {
            init: Nil.value, 
            rest: v
        };
    };
};
var singleton = function (a) {
    return new Cons(a, Nil.value);
};
var sortBy = function (cmp) {
    var merge = function (v) {
        return function (v1) {
            if (v instanceof Cons && v1 instanceof Cons) {
                if (Prelude["=="](Prelude.eqOrdering)(cmp(v.value0)(v1.value0))(Prelude.GT.value)) {
                    return new Cons(v1.value0, merge(v)(v1.value1));
                };
                if (Prelude.otherwise) {
                    return new Cons(v.value0, merge(v.value1)(v1));
                };
            };
            if (v instanceof Nil) {
                return v1;
            };
            if (v1 instanceof Nil) {
                return v;
            };
            throw new Error("Failed pattern match at Data.List line 441, column 1 - line 442, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
    var mergePairs = function (v) {
        if (v instanceof Cons && v.value1 instanceof Cons) {
            return new Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
        };
        return v;
    };
    var mergeAll = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            if (v instanceof Cons && v.value1 instanceof Nil) {
                return v.value0;
            };
            var __tco_v = mergePairs(v);
            v = __tco_v;
            continue tco;
        };
    };
    var sequences = function (v) {
        if (v instanceof Cons && v.value1 instanceof Cons) {
            if (Prelude["=="](Prelude.eqOrdering)(cmp(v.value0)(v.value1.value0))(Prelude.GT.value)) {
                return descending(v.value1.value0)(singleton(v.value0))(v.value1.value1);
            };
            if (Prelude.otherwise) {
                return ascending(v.value1.value0)(Cons.create(v.value0))(v.value1.value1);
            };
        };
        return singleton(v);
    };
    var descending = function (__copy_a) {
        return function (__copy_as) {
            return function (__copy_v) {
                var a = __copy_a;
                var as = __copy_as;
                var v = __copy_v;
                tco: while (true) {
                    var a1 = a;
                    var as1 = as;
                    if (v instanceof Cons && Prelude["=="](Prelude.eqOrdering)(cmp(a1)(v.value0))(Prelude.GT.value)) {
                        var __tco_a = v.value0;
                        var __tco_as = new Cons(a1, as1);
                        var __tco_v = v.value1;
                        a = __tco_a;
                        as = __tco_as;
                        v = __tco_v;
                        continue tco;
                    };
                    return new Cons(new Cons(a, as), sequences(v));
                };
            };
        };
    };
    var ascending = function (a) {
        return function (as) {
            return function (v) {
                if (v instanceof Cons && Prelude["/="](Prelude.eqOrdering)(cmp(a)(v.value0))(Prelude.GT.value)) {
                    return ascending(v.value0)(function (ys) {
                        return as(new Cons(a, ys));
                    })(v.value1);
                };
                return new Cons(as(singleton(a)), sequences(v));
            };
        };
    };
    return function ($344) {
        return mergeAll(sequences($344));
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Prelude.compare(dictOrd))(xs);
    };
};
var showList = function (dictShow) {
    return new Prelude.Show(function (v) {
        if (v instanceof Nil) {
            return "Nil";
        };
        if (v instanceof Cons) {
            return "Cons (" + (Prelude.show(dictShow)(v.value0) + (") (" + (Prelude.show(showList(dictShow))(v.value1) + ")")));
        };
        throw new Error("Failed pattern match: " + [ v.constructor.name ]);
    });
};
var semigroupList = new Prelude.Semigroup(function (v) {
    return function (ys) {
        if (v instanceof Nil) {
            return ys;
        };
        if (v instanceof Cons) {
            return new Cons(v.value0, Prelude["<>"](semigroupList)(v.value1)(ys));
        };
        throw new Error("Failed pattern match: " + [ v.constructor.name, ys.constructor.name ]);
    };
});
var reverse = (function () {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                var acc1 = acc;
                if (v instanceof Nil) {
                    return acc1;
                };
                if (v instanceof Cons) {
                    var __tco_acc = new Cons(v.value0, acc);
                    var __tco_v = v.value1;
                    acc = __tco_acc;
                    v = __tco_v;
                    continue tco;
                };
                throw new Error("Failed pattern match at Data.List line 365, column 1 - line 366, column 1: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
})();
var snoc = function (xs) {
    return function (x) {
        return reverse(new Cons(x, reverse(xs)));
    };
};
var take = (function () {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            return function (__copy_v1) {
                var acc = __copy_acc;
                var v = __copy_v;
                var v1 = __copy_v1;
                tco: while (true) {
                    var acc1 = acc;
                    if (v === 0) {
                        return reverse(acc1);
                    };
                    var acc1 = acc;
                    if (v1 instanceof Nil) {
                        return reverse(acc1);
                    };
                    if (v1 instanceof Cons) {
                        var __tco_acc = new Cons(v1.value0, acc);
                        var __tco_v = v - 1;
                        var __tco_v1 = v1.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 487, column 1 - line 488, column 1: " + [ acc.constructor.name, v.constructor.name, v1.constructor.name ]);
                };
            };
        };
    };
    return go(Nil.value);
})();
var takeWhile = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                var acc1 = acc;
                if (v instanceof Cons && p(v.value0)) {
                    var __tco_acc = new Cons(v.value0, acc1);
                    var __tco_v = v.value1;
                    acc = __tco_acc;
                    v = __tco_v;
                    continue tco;
                };
                return reverse(acc);
            };
        };
    };
    return go(Nil.value);
};
var zipWith = function (f) {
    return function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    return function (__copy_acc) {
                        var v = __copy_v;
                        var v1 = __copy_v1;
                        var acc = __copy_acc;
                        tco: while (true) {
                            if (v instanceof Nil) {
                                return acc;
                            };
                            if (v1 instanceof Nil) {
                                return acc;
                            };
                            if (v instanceof Cons && v1 instanceof Cons) {
                                var __tco_v = v.value1;
                                var __tco_v1 = v1.value1;
                                var __tco_acc = new Cons(f(v.value0)(v1.value0), acc);
                                v = __tco_v;
                                v1 = __tco_v1;
                                acc = __tco_acc;
                                continue tco;
                            };
                            throw new Error("Failed pattern match at Data.List line 651, column 1 - line 652, column 1: " + [ v.constructor.name, v1.constructor.name, acc.constructor.name ]);
                        };
                    };
                };
            };
            return reverse(go(xs)(ys)(Nil.value));
        };
    };
};
var zip = zipWith(Data_Tuple.Tuple.create);
var replicateM = function (dictMonad) {
    return function (n) {
        return function (m) {
            if (n < 1) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Nil.value);
            };
            if (Prelude.otherwise) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(m)(function (v) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(replicateM(dictMonad)(n - 1)(m))(function (v1) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(new Cons(v, v1));
                    });
                });
            };
            throw new Error("Failed pattern match: " + [ n.constructor.name, m.constructor.name ]);
        };
    };
};
var replicate = function (n) {
    return function (value) {
        var go = function (__copy_n1) {
            return function (__copy_rest) {
                var n1 = __copy_n1;
                var rest = __copy_rest;
                tco: while (true) {
                    if (n1 <= 0) {
                        return rest;
                    };
                    if (Prelude.otherwise) {
                        var __tco_n1 = n1 - 1;
                        var __tco_rest = new Cons(value, rest);
                        n1 = __tco_n1;
                        rest = __tco_rest;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 145, column 1 - line 146, column 1: " + [ n1.constructor.name, rest.constructor.name ]);
                };
            };
        };
        return go(n)(Nil.value);
    };
};
var range = function (start) {
    return function (end) {
        if (start === end) {
            return singleton(start);
        };
        if (Prelude.otherwise) {
            var go = function (__copy_s) {
                return function (__copy_e) {
                    return function (__copy_step) {
                        return function (__copy_rest) {
                            var s = __copy_s;
                            var e = __copy_e;
                            var step = __copy_step;
                            var rest = __copy_rest;
                            tco: while (true) {
                                if (s === e) {
                                    return new Cons(s, rest);
                                };
                                if (Prelude.otherwise) {
                                    var __tco_s = s + step | 0;
                                    var __tco_e = e;
                                    var __tco_step = step;
                                    var __tco_rest = new Cons(s, rest);
                                    s = __tco_s;
                                    e = __tco_e;
                                    step = __tco_step;
                                    rest = __tco_rest;
                                    continue tco;
                                };
                                throw new Error("Failed pattern match at Data.List line 137, column 1 - line 138, column 1: " + [ s.constructor.name, e.constructor.name, step.constructor.name, rest.constructor.name ]);
                            };
                        };
                    };
                };
            };
            return go(end)(start)((function () {
                var $190 = start > end;
                if ($190) {
                    return 1;
                };
                if (!$190) {
                    return -1;
                };
                throw new Error("Failed pattern match at Data.List line 137, column 1 - line 138, column 1: " + [ $190.constructor.name ]);
            })())(Nil.value);
        };
        throw new Error("Failed pattern match at Data.List line 137, column 1 - line 138, column 1: " + [ start.constructor.name, end.constructor.name ]);
    };
};
var $dot$dot = range;
var $$null = function (v) {
    if (v instanceof Nil) {
        return true;
    };
    return false;
};
var monoidList = new Data_Monoid.Monoid(function () {
    return semigroupList;
}, Nil.value);
var mapMaybe = function (f) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                var acc1 = acc;
                if (v instanceof Nil) {
                    return reverse(acc1);
                };
                if (v instanceof Cons) {
                    var $194 = f(v.value0);
                    if ($194 instanceof Data_Maybe.Nothing) {
                        var __tco_acc = acc;
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    if ($194 instanceof Data_Maybe.Just) {
                        var __tco_acc = new Cons($194.value0, acc);
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 417, column 1 - line 418, column 1: " + [ $194.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.List line 417, column 1 - line 418, column 1: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Prelude["<*>"]((dictAlternative["__superclass_Prelude.Applicative_0"]())["__superclass_Prelude.Apply_0"]())(Prelude["<$>"](((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())["__superclass_Prelude.Functor_0"]())(Cons.create)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt["<|>"]((dictAlternative["__superclass_Control.Plus.Plus_1"]())["__superclass_Control.Alt.Alt_0"]())(some(dictAlternative)(dictLazy)(v))(Prelude.pure(dictAlternative["__superclass_Prelude.Applicative_0"]())(Nil.value));
        };
    };
};
var last = function (__copy_v) {
    var v = __copy_v;
    tco: while (true) {
        if (v instanceof Cons && v.value1 instanceof Nil) {
            return new Data_Maybe.Just(v.value0);
        };
        if (v instanceof Cons) {
            var __tco_v = v.value1;
            v = __tco_v;
            continue tco;
        };
        return Data_Maybe.Nothing.value;
    };
};
var insertBy = function (cmp) {
    return function (x) {
        return function (v) {
            if (v instanceof Nil) {
                return new Cons(x, Nil.value);
            };
            if (v instanceof Cons) {
                var $207 = cmp(x)(v.value0);
                if ($207 instanceof Prelude.GT) {
                    return new Cons(v.value0, insertBy(cmp)(x)(v.value1));
                };
                return new Cons(x, v);
            };
            throw new Error("Failed pattern match: " + [ cmp.constructor.name, x.constructor.name, v.constructor.name ]);
        };
    };
};
var insertAt = function (v) {
    return function (x) {
        return function (v1) {
            if (v === 0) {
                return new Data_Maybe.Just(new Cons(x, v1));
            };
            if (v1 instanceof Cons) {
                return Prelude["<$>"](Data_Maybe.functorMaybe)(Cons.create(v1.value0))(insertAt(v - 1)(x)(v1.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Prelude.compare(dictOrd));
};
var init = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    var go = function (__copy_v1) {
        return function (__copy_acc) {
            var v1 = __copy_v1;
            var acc = __copy_acc;
            tco: while (true) {
                if (v1 instanceof Cons && v1.value1 instanceof Nil) {
                    return acc;
                };
                if (v1 instanceof Cons) {
                    var __tco_v1 = v1.value1;
                    var __tco_acc = new Cons(v1.value0, acc);
                    v1 = __tco_v1;
                    acc = __tco_acc;
                    continue tco;
                };
                return acc;
            };
        };
    };
    return Data_Maybe.Just.create(reverse(go(v)(Nil.value)));
};
var index = function (__copy_v) {
    return function (__copy_v1) {
        var v = __copy_v;
        var v1 = __copy_v1;
        tco: while (true) {
            if (v instanceof Nil) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Cons && v1 === 0) {
                return new Data_Maybe.Just(v.value0);
            };
            if (v instanceof Cons) {
                var __tco_v = v.value1;
                var __tco_v1 = v1 - 1;
                v = __tco_v;
                v1 = __tco_v1;
                continue tco;
            };
            throw new Error("Failed pattern match: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var $bang$bang = index;
var head = function (v) {
    if (v instanceof Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Cons) {
        return new Data_Maybe.Just(v.value0);
    };
    throw new Error("Failed pattern match at Data.List line 233, column 1 - line 234, column 1: " + [ v.constructor.name ]);
};
var groupBy = function (eq) {
    return function (v) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            var $233 = span(eq(v.value0))(v.value1);
            return new Cons(new Cons(v.value0, $233.init), groupBy(eq)($233.rest));
        };
        throw new Error("Failed pattern match: " + [ eq.constructor.name, v.constructor.name ]);
    };
};
var group = function (dictEq) {
    return groupBy(Prelude["=="](dictEq));
};
var group$prime = function (dictOrd) {
    return function ($345) {
        return group(dictOrd["__superclass_Prelude.Eq_0"]())(sort(dictOrd)($345));
    };
};
var functorList = new Prelude.Functor(function (f) {
    return function (lst) {
        var go = function (__copy_v) {
            return function (__copy_acc) {
                var v = __copy_v;
                var acc = __copy_acc;
                tco: while (true) {
                    if (v instanceof Nil) {
                        return acc;
                    };
                    if (v instanceof Cons) {
                        var __tco_v = v.value1;
                        var __tco_acc = new Cons(f(v.value0), acc);
                        v = __tco_v;
                        acc = __tco_acc;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.List line 718, column 1 - line 725, column 1: " + [ v.constructor.name, acc.constructor.name ]);
                };
            };
        };
        return reverse(go(lst)(Nil.value));
    };
});
var fromList = function (dictUnfoldable) {
    return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
        return Prelude["<$>"](Data_Maybe.functorMaybe)(function (rec) {
            return new Data_Tuple.Tuple(rec.head, rec.tail);
        })(uncons(xs));
    });
};
var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return Data_Foldable.foldl(foldableList)(function (acc) {
            return function ($346) {
                return Prelude.append(dictMonoid["__superclass_Prelude.Semigroup_0"]())(acc)(f($346));
            };
        })(Data_Monoid.mempty(dictMonoid));
    };
}, (function () {
    var go = function (__copy_o) {
        return function (__copy_b) {
            return function (__copy_v) {
                var o = __copy_o;
                var b = __copy_b;
                var v = __copy_v;
                tco: while (true) {
                    var b1 = b;
                    if (v instanceof Nil) {
                        return b1;
                    };
                    if (v instanceof Cons) {
                        var __tco_o = o;
                        var __tco_b = o(b)(v.value0);
                        var __tco_v = v.value1;
                        o = __tco_o;
                        b = __tco_b;
                        v = __tco_v;
                        continue tco;
                    };
                    throw new Error("Failed pattern match: " + [ o.constructor.name, b.constructor.name, v.constructor.name ]);
                };
            };
        };
    };
    return go;
})(), function (o) {
    return function (b) {
        return function (v) {
            if (v instanceof Nil) {
                return b;
            };
            if (v instanceof Cons) {
                return o(v.value0)(Data_Foldable.foldr(foldableList)(o)(b)(v.value1));
            };
            throw new Error("Failed pattern match: " + [ o.constructor.name, b.constructor.name, v.constructor.name ]);
        };
    };
});
var length = Data_Foldable.foldl(foldableList)(function (acc) {
    return function (v) {
        return acc + 1 | 0;
    };
})(0);
var traversableList = new Data_Traversable.Traversable(function () {
    return foldableList;
}, function () {
    return functorList;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Nil) {
            return Prelude.pure(dictApplicative)(Nil.value);
        };
        if (v instanceof Cons) {
            return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Cons.create)(v.value0))(Data_Traversable.sequence(traversableList)(dictApplicative)(v.value1));
        };
        throw new Error("Failed pattern match: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            if (v instanceof Nil) {
                return Prelude.pure(dictApplicative)(Nil.value);
            };
            if (v instanceof Cons) {
                return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Cons.create)(f(v.value0)))(Data_Traversable.traverse(traversableList)(dictApplicative)(f)(v.value1));
            };
            throw new Error("Failed pattern match: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
});
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(traversableList)(dictApplicative)(zipWith(f)(xs)(ys));
            };
        };
    };
};
var unzip = Data_Foldable.foldr(foldableList)(function (v) {
    return function (v1) {
        return new Data_Tuple.Tuple(new Cons(v.value0, v1.value0), new Cons(v.value1, v1.value1));
    };
})(new Data_Tuple.Tuple(Nil.value, Nil.value));
var foldM = function (dictMonad) {
    return function (f) {
        return function (a) {
            return function (v) {
                if (v instanceof Nil) {
                    return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(a);
                };
                if (v instanceof Cons) {
                    return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(f(a)(v.value0))(function (aʹ) {
                        return foldM(dictMonad)(f)(aʹ)(v.value1);
                    });
                };
                throw new Error("Failed pattern match: " + [ f.constructor.name, a.constructor.name, v.constructor.name ]);
            };
        };
    };
};
var findIndex = function (fn) {
    var go = function (__copy_n) {
        return function (__copy_v) {
            var n = __copy_n;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Cons) {
                    if (fn(v.value0)) {
                        return new Data_Maybe.Just(n);
                    };
                    if (Prelude.otherwise) {
                        var __tco_n = n + 1 | 0;
                        var __tco_v = v.value1;
                        n = __tco_n;
                        v = __tco_v;
                        continue tco;
                    };
                };
                if (v instanceof Nil) {
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.List line 298, column 1 - line 299, column 1: " + [ n.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(0);
};
var findLastIndex = function (fn) {
    return function (xs) {
        return Prelude["<$>"](Data_Maybe.functorMaybe)(Prelude["-"](Prelude.ringInt)(length(xs) - 1))(findIndex(fn)(reverse(xs)));
    };
};
var filterM = function (dictMonad) {
    return function (p) {
        return function (v) {
            if (v instanceof Nil) {
                return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())(Nil.value);
            };
            if (v instanceof Cons) {
                return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(p(v.value0))(function (v1) {
                    return Prelude.bind(dictMonad["__superclass_Prelude.Bind_1"]())(filterM(dictMonad)(p)(v.value1))(function (v2) {
                        return Prelude["return"](dictMonad["__superclass_Prelude.Applicative_0"]())((function () {
                            if (v1) {
                                return new Cons(v.value0, v2);
                            };
                            if (!v1) {
                                return v2;
                            };
                            throw new Error("Failed pattern match: " + [ v1.constructor.name ]);
                        })());
                    });
                });
            };
            throw new Error("Failed pattern match: " + [ p.constructor.name, v.constructor.name ]);
        };
    };
};
var filter = function (p) {
    var go = function (__copy_acc) {
        return function (__copy_v) {
            var acc = __copy_acc;
            var v = __copy_v;
            tco: while (true) {
                var acc1 = acc;
                if (v instanceof Nil) {
                    return reverse(acc1);
                };
                if (v instanceof Cons) {
                    if (p(v.value0)) {
                        var __tco_acc = new Cons(v.value0, acc);
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                    if (Prelude.otherwise) {
                        var __tco_acc = acc;
                        var __tco_v = v.value1;
                        acc = __tco_acc;
                        v = __tco_v;
                        continue tco;
                    };
                };
                throw new Error("Failed pattern match at Data.List line 388, column 1 - line 389, column 1: " + [ acc.constructor.name, v.constructor.name ]);
            };
        };
    };
    return go(Nil.value);
};
var intersectBy = function (eq) {
    return function (v) {
        return function (v1) {
            if (v instanceof Nil) {
                return Nil.value;
            };
            if (v1 instanceof Nil) {
                return Nil.value;
            };
            return filter(function (x) {
                return Data_Foldable.any(foldableList)(Prelude.booleanAlgebraBoolean)(eq(x))(v1);
            })(v);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Prelude["=="](dictEq));
};
var nubBy = function ($eq$eq) {
    return function (v) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return new Cons(v.value0, nubBy($eq$eq)(filter(function (y) {
                return !$eq$eq(v.value0)(y);
            })(v.value1)));
        };
        throw new Error("Failed pattern match: " + [ $eq$eq.constructor.name, v.constructor.name ]);
    };
};
var nub = function (dictEq) {
    return nubBy(Prelude.eq(dictEq));
};
var eqList = function (dictEq) {
    return new Prelude.Eq(function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    return function (__copy_v2) {
                        var v = __copy_v;
                        var v1 = __copy_v1;
                        var v2 = __copy_v2;
                        tco: while (true) {
                            if (!v2) {
                                return false;
                            };
                            if (v instanceof Nil && v1 instanceof Nil) {
                                return v2;
                            };
                            if (v instanceof Cons && v1 instanceof Cons) {
                                var __tco_v = v.value1;
                                var __tco_v1 = v1.value1;
                                var __tco_v2 = v2 && Prelude["=="](dictEq)(v1.value0)(v.value0);
                                v = __tco_v;
                                v1 = __tco_v1;
                                v2 = __tco_v2;
                                continue tco;
                            };
                            return false;
                        };
                    };
                };
            };
            return go(xs)(ys)(true);
        };
    });
};
var ordList = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqList(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (xs) {
        return function (ys) {
            var go = function (__copy_v) {
                return function (__copy_v1) {
                    var v = __copy_v;
                    var v1 = __copy_v1;
                    tco: while (true) {
                        if (v instanceof Nil && v1 instanceof Nil) {
                            return Prelude.EQ.value;
                        };
                        if (v instanceof Nil) {
                            return Prelude.LT.value;
                        };
                        if (v1 instanceof Nil) {
                            return Prelude.GT.value;
                        };
                        if (v instanceof Cons && v1 instanceof Cons) {
                            var $302 = Prelude.compare(dictOrd)(v.value0)(v1.value0);
                            if ($302 instanceof Prelude.EQ) {
                                var __tco_v = v.value1;
                                var __tco_v1 = v1.value1;
                                v = __tco_v;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            return $302;
                        };
                        throw new Error("Failed pattern match at Data.List line 700, column 1 - line 711, column 1: " + [ v.constructor.name, v1.constructor.name ]);
                    };
                };
            };
            return go(xs)(ys);
        };
    });
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Prelude["=="](dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Prelude["=="](dictEq)(v)(x);
        });
    };
};
var dropWhile = function (p) {
    var go = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            if (v instanceof Cons && p(v.value0)) {
                var __tco_v = v.value1;
                v = __tco_v;
                continue tco;
            };
            return v;
        };
    };
    return go;
};
var drop = function (__copy_v) {
    return function (__copy_v1) {
        var v = __copy_v;
        var v1 = __copy_v1;
        tco: while (true) {
            if (v === 0) {
                return v1;
            };
            if (v1 instanceof Nil) {
                return Nil.value;
            };
            if (v1 instanceof Cons) {
                var __tco_v = v - 1;
                var __tco_v1 = v1.value1;
                v = __tco_v;
                v1 = __tco_v1;
                continue tco;
            };
            throw new Error("Failed pattern match: " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var slice = function (start) {
    return function (end) {
        return function (xs) {
            return take(end - start)(drop(start)(xs));
        };
    };
};
var deleteBy = function ($eq$eq) {
    return function (x) {
        return function (v) {
            if (v instanceof Nil) {
                return Nil.value;
            };
            if (v instanceof Cons && $eq$eq(x)(v.value0)) {
                return v.value1;
            };
            if (v instanceof Cons) {
                return new Cons(v.value0, deleteBy($eq$eq)(x)(v.value1));
            };
            throw new Error("Failed pattern match: " + [ $eq$eq.constructor.name, x.constructor.name, v.constructor.name ]);
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Prelude["<>"](semigroupList)(xs)(Data_Foldable.foldl(foldableList)(Prelude.flip(deleteBy(eq)))(nubBy(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Prelude["=="](dictEq));
};
var deleteAt = function (v) {
    return function (v1) {
        if (v === 0 && v1 instanceof Cons) {
            return new Data_Maybe.Just(v1.value1);
        };
        if (v1 instanceof Cons) {
            return Prelude["<$>"](Data_Maybe.functorMaybe)(Cons.create(v1.value0))(deleteAt(v - 1)(v1.value1));
        };
        return Data_Maybe.Nothing.value;
    };
};
var $$delete = function (dictEq) {
    return deleteBy(Prelude["=="](dictEq));
};
var $bslash$bslash = function (dictEq) {
    return Data_Foldable.foldl(foldableList)(Prelude.flip($$delete(dictEq)));
};
var concatMap = function (f) {
    return function (v) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return Prelude["<>"](semigroupList)(f(v.value0))(concatMap(f)(v.value1));
        };
        throw new Error("Failed pattern match: " + [ f.constructor.name, v.constructor.name ]);
    };
};
var catMaybes = mapMaybe(Prelude.id(Prelude.categoryFn));
var applyList = new Prelude.Apply(function () {
    return functorList;
}, function (v) {
    return function (xs) {
        if (v instanceof Nil) {
            return Nil.value;
        };
        if (v instanceof Cons) {
            return Prelude["<>"](semigroupList)(Prelude["<$>"](functorList)(v.value0)(xs))(Prelude["<*>"](applyList)(v.value1)(xs));
        };
        throw new Error("Failed pattern match: " + [ v.constructor.name, xs.constructor.name ]);
    };
});
var bindList = new Prelude.Bind(function () {
    return applyList;
}, Prelude.flip(concatMap));
var concat = function (v) {
    return Prelude[">>="](bindList)(v)(Prelude.id(Prelude.categoryFn));
};
var applicativeList = new Prelude.Applicative(function () {
    return applyList;
}, function (a) {
    return new Cons(a, Nil.value);
});
var monadList = new Prelude.Monad(function () {
    return applicativeList;
}, function () {
    return bindList;
});
var alterAt = function (v) {
    return function (f) {
        return function (v1) {
            if (v === 0 && v1 instanceof Cons) {
                return Data_Maybe.Just.create((function () {
                    var $338 = f(v1.value0);
                    if ($338 instanceof Data_Maybe.Nothing) {
                        return v1.value1;
                    };
                    if ($338 instanceof Data_Maybe.Just) {
                        return new Cons($338.value0, v1.value1);
                    };
                    throw new Error("Failed pattern match: " + [ $338.constructor.name ]);
                })());
            };
            if (v1 instanceof Cons) {
                return Prelude["<$>"](Data_Maybe.functorMaybe)(Cons.create(v1.value0))(alterAt(v - 1)(f)(v1.value1));
            };
            return Data_Maybe.Nothing.value;
        };
    };
};
var modifyAt = function (n) {
    return function (f) {
        return alterAt(n)(function ($347) {
            return Data_Maybe.Just.create(f($347));
        });
    };
};
var altList = new Control_Alt.Alt(function () {
    return functorList;
}, Prelude.append(semigroupList));
var plusList = new Control_Plus.Plus(function () {
    return altList;
}, Nil.value);
var alternativeList = new Control_Alternative.Alternative(function () {
    return plusList;
}, function () {
    return applicativeList;
});
var monadPlusList = new Control_MonadPlus.MonadPlus(function () {
    return alternativeList;
}, function () {
    return monadList;
});
module.exports = {
    Nil: Nil, 
    Cons: Cons, 
    foldM: foldM, 
    unzip: unzip, 
    zip: zip, 
    zipWithA: zipWithA, 
    zipWith: zipWith, 
    intersectBy: intersectBy, 
    intersect: intersect, 
    "\\\\": $bslash$bslash, 
    deleteBy: deleteBy, 
    "delete": $$delete, 
    unionBy: unionBy, 
    union: union, 
    nubBy: nubBy, 
    nub: nub, 
    groupBy: groupBy, 
    "group'": group$prime, 
    group: group, 
    span: span, 
    dropWhile: dropWhile, 
    drop: drop, 
    takeWhile: takeWhile, 
    take: take, 
    slice: slice, 
    sortBy: sortBy, 
    sort: sort, 
    catMaybes: catMaybes, 
    mapMaybe: mapMaybe, 
    filterM: filterM, 
    filter: filter, 
    concatMap: concatMap, 
    concat: concat, 
    reverse: reverse, 
    alterAt: alterAt, 
    modifyAt: modifyAt, 
    updateAt: updateAt, 
    deleteAt: deleteAt, 
    insertAt: insertAt, 
    findLastIndex: findLastIndex, 
    findIndex: findIndex, 
    elemLastIndex: elemLastIndex, 
    elemIndex: elemIndex, 
    index: index, 
    "!!": $bang$bang, 
    uncons: uncons, 
    init: init, 
    tail: tail, 
    last: last, 
    head: head, 
    insertBy: insertBy, 
    insert: insert, 
    snoc: snoc, 
    ":": $colon, 
    length: length, 
    "null": $$null, 
    many: many, 
    some: some, 
    replicateM: replicateM, 
    replicate: replicate, 
    range: range, 
    "..": $dot$dot, 
    singleton: singleton, 
    toList: toList, 
    fromList: fromList, 
    showList: showList, 
    eqList: eqList, 
    ordList: ordList, 
    semigroupList: semigroupList, 
    monoidList: monoidList, 
    functorList: functorList, 
    foldableList: foldableList, 
    unfoldableList: unfoldableList, 
    traversableList: traversableList, 
    applyList: applyList, 
    applicativeList: applicativeList, 
    bindList: bindList, 
    monadList: monadList, 
    altList: altList, 
    plusList: plusList, 
    alternativeList: alternativeList, 
    monadPlusList: monadPlusList
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Lazy":"/Users/anders/dev/ps/halo/ajax/output/Control.Lazy/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Data.Unfoldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Unfoldable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Map/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Foldable = require("Data.Foldable");
var Data_List = require("Data.List");
var Data_Maybe = require("Data.Maybe");
var Data_Maybe_Unsafe = require("Data.Maybe.Unsafe");
var Data_Monoid = require("Data.Monoid");
var Data_Traversable = require("Data.Traversable");
var Data_Tuple = require("Data.Tuple");
var Leaf = (function () {
    function Leaf() {

    };
    Leaf.value = new Leaf();
    return Leaf;
})();
var Two = (function () {
    function Two(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Two.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Two(value0, value1, value2, value3);
                };
            };
        };
    };
    return Two;
})();
var Three = (function () {
    function Three(value0, value1, value2, value3, value4, value5, value6) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
        this.value6 = value6;
    };
    Three.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return function (value6) {
                                return new Three(value0, value1, value2, value3, value4, value5, value6);
                            };
                        };
                    };
                };
            };
        };
    };
    return Three;
})();
var TwoLeft = (function () {
    function TwoLeft(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoLeft(value0, value1, value2);
            };
        };
    };
    return TwoLeft;
})();
var TwoRight = (function () {
    function TwoRight(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoRight(value0, value1, value2);
            };
        };
    };
    return TwoRight;
})();
var ThreeLeft = (function () {
    function ThreeLeft(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeLeft(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeLeft;
})();
var ThreeMiddle = (function () {
    function ThreeMiddle(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeMiddle.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeMiddle(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeMiddle;
})();
var ThreeRight = (function () {
    function ThreeRight(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeRight(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeRight;
})();
var KickUp = (function () {
    function KickUp(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    KickUp.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new KickUp(value0, value1, value2, value3);
                };
            };
        };
    };
    return KickUp;
})();
var values = function (v) {
    if (v instanceof Leaf) {
        return Data_List.Nil.value;
    };
    if (v instanceof Two) {
        return Prelude["++"](Data_List.semigroupList)(values(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value2))(values(v.value3)));
    };
    if (v instanceof Three) {
        return Prelude["++"](Data_List.semigroupList)(values(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value2))(Prelude["++"](Data_List.semigroupList)(values(v.value3))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value5))(values(v.value6)))));
    };
    throw new Error("Failed pattern match: " + [ v.constructor.name ]);
};
var toList = function (v) {
    if (v instanceof Leaf) {
        return Data_List.Nil.value;
    };
    if (v instanceof Two) {
        return Prelude["++"](Data_List.semigroupList)(toList(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(new Data_Tuple.Tuple(v.value1, v.value2)))(toList(v.value3)));
    };
    if (v instanceof Three) {
        return Prelude["++"](Data_List.semigroupList)(toList(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(new Data_Tuple.Tuple(v.value1, v.value2)))(Prelude["++"](Data_List.semigroupList)(toList(v.value3))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(new Data_Tuple.Tuple(v.value4, v.value5)))(toList(v.value6)))));
    };
    throw new Error("Failed pattern match: " + [ v.constructor.name ]);
};
var size = function ($598) {
    return Data_List.length(values($598));
};
var singleton = function (k) {
    return function (v) {
        return new Two(Leaf.value, k, v, Leaf.value);
    };
};
var showTree = function (dictShow) {
    return function (dictShow1) {
        return function (v) {
            if (v instanceof Leaf) {
                return "Leaf";
            };
            if (v instanceof Two) {
                return "Two (" + (showTree(dictShow)(dictShow1)(v.value0) + (") (" + (Prelude.show(dictShow)(v.value1) + (") (" + (Prelude.show(dictShow1)(v.value2) + (") (" + (showTree(dictShow)(dictShow1)(v.value3) + ")")))))));
            };
            if (v instanceof Three) {
                return "Three (" + (showTree(dictShow)(dictShow1)(v.value0) + (") (" + (Prelude.show(dictShow)(v.value1) + (") (" + (Prelude.show(dictShow1)(v.value2) + (") (" + (showTree(dictShow)(dictShow1)(v.value3) + (") (" + (Prelude.show(dictShow)(v.value4) + (") (" + (Prelude.show(dictShow1)(v.value5) + (") (" + (showTree(dictShow)(dictShow1)(v.value6) + ")")))))))))))));
            };
            throw new Error("Failed pattern match: " + [ v.constructor.name ]);
        };
    };
};
var showMap = function (dictShow) {
    return function (dictShow1) {
        return new Prelude.Show(function (m) {
            return "fromList " + Prelude.show(Data_List.showList(Data_Tuple.showTuple(dictShow)(dictShow1)))(toList(m));
        });
    };
};
var lookup = function (__copy_dictOrd) {
    return function (__copy_k) {
        return function (__copy_v) {
            var dictOrd = __copy_dictOrd;
            var k = __copy_k;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Leaf) {
                    return Data_Maybe.Nothing.value;
                };
                var k1 = k;
                if (v instanceof Two && Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(k1)(v.value1)) {
                    return new Data_Maybe.Just(v.value2);
                };
                var k1 = k;
                if (v instanceof Two && Prelude["<"](dictOrd)(k1)(v.value1)) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value0;
                    dictOrd = __tco_dictOrd;
                    k = k1;
                    v = __tco_v;
                    continue tco;
                };
                var k1 = k;
                if (v instanceof Two) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value3;
                    dictOrd = __tco_dictOrd;
                    k = k1;
                    v = __tco_v;
                    continue tco;
                };
                var k1 = k;
                if (v instanceof Three && Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(k1)(v.value1)) {
                    return new Data_Maybe.Just(v.value2);
                };
                var k1 = k;
                if (v instanceof Three && Prelude["=="](dictOrd["__superclass_Prelude.Eq_0"]())(k1)(v.value4)) {
                    return new Data_Maybe.Just(v.value5);
                };
                var k1 = k;
                if (v instanceof Three && Prelude["<"](dictOrd)(k1)(v.value1)) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value0;
                    dictOrd = __tco_dictOrd;
                    k = k1;
                    v = __tco_v;
                    continue tco;
                };
                var k1 = k;
                if (v instanceof Three && (Prelude["<"](dictOrd)(v.value1)(k1) && Prelude["<="](dictOrd)(k1)(v.value4))) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value3;
                    dictOrd = __tco_dictOrd;
                    k = k1;
                    v = __tco_v;
                    continue tco;
                };
                if (v instanceof Three) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_k = k;
                    var __tco_v = v.value6;
                    dictOrd = __tco_dictOrd;
                    k = __tco_k;
                    v = __tco_v;
                    continue tco;
                };
                throw new Error("Failed pattern match: " + [ k.constructor.name, v.constructor.name ]);
            };
        };
    };
};
var member = function (dictOrd) {
    return function (k) {
        return function (m) {
            return Data_Maybe.isJust(lookup(dictOrd)(k)(m));
        };
    };
};
var keys = function (v) {
    if (v instanceof Leaf) {
        return Data_List.Nil.value;
    };
    if (v instanceof Two) {
        return Prelude["++"](Data_List.semigroupList)(keys(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value1))(keys(v.value3)));
    };
    if (v instanceof Three) {
        return Prelude["++"](Data_List.semigroupList)(keys(v.value0))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value1))(Prelude["++"](Data_List.semigroupList)(keys(v.value3))(Prelude["++"](Data_List.semigroupList)(Prelude.pure(Data_List.applicativeList)(v.value4))(keys(v.value6)))));
    };
    throw new Error("Failed pattern match: " + [ v.constructor.name ]);
};
var isEmpty = function (v) {
    if (v instanceof Leaf) {
        return true;
    };
    return false;
};
var functorMap = new Prelude.Functor(function (f) {
    return function (v) {
        if (v instanceof Leaf) {
            return Leaf.value;
        };
        if (v instanceof Two) {
            return new Two(Prelude.map(functorMap)(f)(v.value0), v.value1, f(v.value2), Prelude.map(functorMap)(f)(v.value3));
        };
        if (v instanceof Three) {
            return new Three(Prelude.map(functorMap)(f)(v.value0), v.value1, f(v.value2), Prelude.map(functorMap)(f)(v.value3), v.value4, f(v.value5), Prelude.map(functorMap)(f)(v.value6));
        };
        throw new Error("Failed pattern match: " + [ f.constructor.name, v.constructor.name ]);
    };
});
var fromZipper = function (__copy_dictOrd) {
    return function (__copy_v) {
        return function (__copy_v1) {
            var dictOrd = __copy_dictOrd;
            var v = __copy_v;
            var v1 = __copy_v1;
            tco: while (true) {
                if (v instanceof Data_List.Nil) {
                    return v1;
                };
                if (v instanceof Data_List.Cons && v.value0 instanceof TwoLeft) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value1;
                    var __tco_v1 = new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2);
                    dictOrd = __tco_dictOrd;
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                if (v instanceof Data_List.Cons && v.value0 instanceof TwoRight) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value1;
                    var __tco_v1 = new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1);
                    dictOrd = __tco_dictOrd;
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                if (v instanceof Data_List.Cons && v.value0 instanceof ThreeLeft) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value1;
                    var __tco_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
                    dictOrd = __tco_dictOrd;
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                if (v instanceof Data_List.Cons && v.value0 instanceof ThreeMiddle) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value1;
                    var __tco_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1, v.value0.value3, v.value0.value4, v.value0.value5);
                    dictOrd = __tco_dictOrd;
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                if (v instanceof Data_List.Cons && v.value0 instanceof ThreeRight) {
                    var __tco_dictOrd = dictOrd;
                    var __tco_v = v.value1;
                    var __tco_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, v1);
                    dictOrd = __tco_dictOrd;
                    v = __tco_v;
                    v1 = __tco_v1;
                    continue tco;
                };
                throw new Error("Failed pattern match: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    };
};
var insert = function (dictOrd) {
    var up = function (__copy_dictOrd1) {
        return function (__copy_v) {
            return function (__copy_v1) {
                var dictOrd1 = __copy_dictOrd1;
                var v = __copy_v;
                var v1 = __copy_v1;
                tco: while (true) {
                    if (v instanceof Data_List.Nil) {
                        return new Two(v1.value0, v1.value1, v1.value2, v1.value3);
                    };
                    if (v instanceof Data_List.Cons && v.value0 instanceof TwoLeft) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, v.value0.value0, v.value0.value1, v.value0.value2));
                    };
                    if (v instanceof Data_List.Cons && v.value0 instanceof TwoRight) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1.value0, v1.value1, v1.value2, v1.value3));
                    };
                    if (v instanceof Data_List.Cons && v.value0 instanceof ThreeLeft) {
                        var __tco_dictOrd1 = dictOrd1;
                        var __tco_v = v.value1;
                        var __tco_v1 = new KickUp(new Two(v1.value0, v1.value1, v1.value2, v1.value3), v.value0.value0, v.value0.value1, new Two(v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5));
                        dictOrd1 = __tco_dictOrd1;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    if (v instanceof Data_List.Cons && v.value0 instanceof ThreeMiddle) {
                        var __tco_dictOrd1 = dictOrd1;
                        var __tco_v = v.value1;
                        var __tco_v1 = new KickUp(new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1.value0), v1.value1, v1.value2, new Two(v1.value3, v.value0.value3, v.value0.value4, v.value0.value5));
                        dictOrd1 = __tco_dictOrd1;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    if (v instanceof Data_List.Cons && v.value0 instanceof ThreeRight) {
                        var __tco_dictOrd1 = dictOrd1;
                        var __tco_v = v.value1;
                        var __tco_v1 = new KickUp(new Two(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3), v.value0.value4, v.value0.value5, new Two(v1.value0, v1.value1, v1.value2, v1.value3));
                        dictOrd1 = __tco_dictOrd1;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    throw new Error("Failed pattern match at Data.Map line 148, column 1 - line 149, column 1: " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
        };
    };
    var down = function (__copy_dictOrd1) {
        return function (__copy_ctx) {
            return function (__copy_k) {
                return function (__copy_v) {
                    return function (__copy_v1) {
                        var dictOrd1 = __copy_dictOrd1;
                        var ctx = __copy_ctx;
                        var k = __copy_k;
                        var v = __copy_v;
                        var v1 = __copy_v1;
                        tco: while (true) {
                            var ctx1 = ctx;
                            var k1 = k;
                            var v2 = v;
                            if (v1 instanceof Leaf) {
                                return up(dictOrd1)(ctx1)(new KickUp(Leaf.value, k1, v2, Leaf.value));
                            };
                            var ctx1 = ctx;
                            var k1 = k;
                            var v2 = v;
                            if (v1 instanceof Two && Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k1)(v1.value1)) {
                                return fromZipper(dictOrd1)(ctx1)(new Two(v1.value0, k1, v2, v1.value3));
                            };
                            var ctx1 = ctx;
                            var k1 = k;
                            var v2 = v;
                            if (v1 instanceof Two && Prelude["<"](dictOrd1)(k1)(v1.value1)) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx1);
                                var __tco_v1 = v1.value0;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = k1;
                                v = v2;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            var ctx1 = ctx;
                            var k1 = k;
                            var v2 = v;
                            if (v1 instanceof Two) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx1);
                                var __tco_v1 = v1.value3;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = k1;
                                v = v2;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            var ctx1 = ctx;
                            var k1 = k;
                            var v2 = v;
                            if (v1 instanceof Three && Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k1)(v1.value1)) {
                                return fromZipper(dictOrd1)(ctx1)(new Three(v1.value0, k1, v2, v1.value3, v1.value4, v1.value5, v1.value6));
                            };
                            var ctx1 = ctx;
                            var k1 = k;
                            var v2 = v;
                            if (v1 instanceof Three && Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k1)(v1.value4)) {
                                return fromZipper(dictOrd1)(ctx1)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k1, v2, v1.value6));
                            };
                            var ctx1 = ctx;
                            var k1 = k;
                            var v2 = v;
                            if (v1 instanceof Three && Prelude["<"](dictOrd1)(k1)(v1.value1)) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx1);
                                var __tco_v1 = v1.value0;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = k1;
                                v = v2;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            var ctx1 = ctx;
                            var k1 = k;
                            var v2 = v;
                            if (v1 instanceof Three && (Prelude["<"](dictOrd1)(v1.value1)(k1) && Prelude["<="](dictOrd1)(k1)(v1.value4))) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx1);
                                var __tco_v1 = v1.value3;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = k1;
                                v = v2;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            if (v1 instanceof Three) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                                var __tco_k = k;
                                var __tco_v = v;
                                var __tco_v1 = v1.value6;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = __tco_k;
                                v = __tco_v;
                                v1 = __tco_v1;
                                continue tco;
                            };
                            throw new Error("Failed pattern match at Data.Map line 148, column 1 - line 149, column 1: " + [ ctx.constructor.name, k.constructor.name, v.constructor.name, v1.constructor.name ]);
                        };
                    };
                };
            };
        };
    };
    return down(dictOrd)(Data_List.Nil.value);
};
var foldableMap = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (m) {
            return Data_Foldable.foldMap(Data_List.foldableList)(dictMonoid)(f)(values(m));
        };
    };
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldl(Data_List.foldableList)(f)(z)(values(m));
        };
    };
}, function (f) {
    return function (z) {
        return function (m) {
            return Data_Foldable.foldr(Data_List.foldableList)(f)(z)(values(m));
        };
    };
});
var eqMap = function (dictEq) {
    return function (dictEq1) {
        return new Prelude.Eq(function (m1) {
            return function (m2) {
                return Prelude["=="](Data_List.eqList(Data_Tuple.eqTuple(dictEq)(dictEq1)))(toList(m1))(toList(m2));
            };
        });
    };
};
var ordMap = function (dictOrd) {
    return function (dictOrd1) {
        return new Prelude.Ord(function () {
            return eqMap(dictOrd["__superclass_Prelude.Eq_0"]())(dictOrd1["__superclass_Prelude.Eq_0"]());
        }, function (m1) {
            return function (m2) {
                return Prelude.compare(Data_List.ordList(Data_Tuple.ordTuple(dictOrd)(dictOrd1)))(toList(m1))(toList(m2));
            };
        });
    };
};
var empty = Leaf.value;
var fromList = function (dictOrd) {
    return Data_Foldable.foldl(Data_List.foldableList)(function (m) {
        return function (v) {
            return insert(dictOrd)(v.value0)(v.value1)(m);
        };
    })(empty);
};
var $$delete = function (dictOrd) {
    var up = function (__copy_dictOrd1) {
        return function (__copy_v) {
            return function (__copy_v1) {
                var dictOrd1 = __copy_dictOrd1;
                var v = __copy_v;
                var v1 = __copy_v1;
                tco: while (true) {
                    if (v instanceof Data_List.Nil) {
                        return v1;
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof TwoLeft && (v.value0.value2 instanceof Leaf && v1 instanceof Leaf))) {
                        return fromZipper(dictOrd1)(v.value1)(new Two(Leaf.value, v.value0.value0, v.value0.value1, Leaf.value));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof TwoRight && (v.value0.value0 instanceof Leaf && v1 instanceof Leaf))) {
                        return fromZipper(dictOrd1)(v.value1)(new Two(Leaf.value, v.value0.value1, v.value0.value2, Leaf.value));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof TwoLeft && v.value0.value2 instanceof Two)) {
                        var __tco_dictOrd1 = dictOrd1;
                        var __tco_v = v.value1;
                        var __tco_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2.value0, v.value0.value2.value1, v.value0.value2.value2, v.value0.value2.value3);
                        dictOrd1 = __tco_dictOrd1;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof TwoRight && v.value0.value0 instanceof Two)) {
                        var __tco_dictOrd1 = dictOrd1;
                        var __tco_v = v.value1;
                        var __tco_v1 = new Three(v.value0.value0.value0, v.value0.value0.value1, v.value0.value0.value2, v.value0.value0.value3, v.value0.value1, v.value0.value2, v1);
                        dictOrd1 = __tco_dictOrd1;
                        v = __tco_v;
                        v1 = __tco_v1;
                        continue tco;
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof TwoLeft && v.value0.value2 instanceof Three)) {
                        return fromZipper(dictOrd1)(v.value1)(new Two(new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2.value0), v.value0.value2.value1, v.value0.value2.value2, new Two(v.value0.value2.value3, v.value0.value2.value4, v.value0.value2.value5, v.value0.value2.value6)));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof TwoRight && v.value0.value0 instanceof Three)) {
                        return fromZipper(dictOrd1)(v.value1)(new Two(new Two(v.value0.value0.value0, v.value0.value0.value1, v.value0.value0.value2, v.value0.value0.value3), v.value0.value0.value4, v.value0.value0.value5, new Two(v.value0.value0.value6, v.value0.value1, v.value0.value2, v1)));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeLeft && (v.value0.value2 instanceof Leaf && (v.value0.value5 instanceof Leaf && v1 instanceof Leaf)))) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(Leaf.value, v.value0.value0, v.value0.value1, Leaf.value, v.value0.value3, v.value0.value4, Leaf.value));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && (v.value0.value0 instanceof Leaf && (v.value0.value5 instanceof Leaf && v1 instanceof Leaf)))) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(Leaf.value, v.value0.value1, v.value0.value2, Leaf.value, v.value0.value3, v.value0.value4, Leaf.value));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeRight && (v.value0.value0 instanceof Leaf && (v.value0.value3 instanceof Leaf && v1 instanceof Leaf)))) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(Leaf.value, v.value0.value1, v.value0.value2, Leaf.value, v.value0.value4, v.value0.value5, Leaf.value));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeLeft && v.value0.value2 instanceof Two)) {
                        return fromZipper(dictOrd1)(v.value1)(new Two(new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2.value0, v.value0.value2.value1, v.value0.value2.value2, v.value0.value2.value3), v.value0.value3, v.value0.value4, v.value0.value5));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && v.value0.value0 instanceof Two)) {
                        return fromZipper(dictOrd1)(v.value1)(new Two(new Three(v.value0.value0.value0, v.value0.value0.value1, v.value0.value0.value2, v.value0.value0.value3, v.value0.value1, v.value0.value2, v1), v.value0.value3, v.value0.value4, v.value0.value5));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && v.value0.value5 instanceof Two)) {
                        return fromZipper(dictOrd1)(v.value1)(new Two(v.value0.value0, v.value0.value1, v.value0.value2, new Three(v1, v.value0.value3, v.value0.value4, v.value0.value5.value0, v.value0.value5.value1, v.value0.value5.value2, v.value0.value5.value3)));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeRight && v.value0.value3 instanceof Two)) {
                        return fromZipper(dictOrd1)(v.value1)(new Two(v.value0.value0, v.value0.value1, v.value0.value2, new Three(v.value0.value3.value0, v.value0.value3.value1, v.value0.value3.value2, v.value0.value3.value3, v.value0.value4, v.value0.value5, v1)));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeLeft && v.value0.value2 instanceof Three)) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2.value0), v.value0.value2.value1, v.value0.value2.value2, new Two(v.value0.value2.value3, v.value0.value2.value4, v.value0.value2.value5, v.value0.value2.value6), v.value0.value3, v.value0.value4, v.value0.value5));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && v.value0.value0 instanceof Three)) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(new Two(v.value0.value0.value0, v.value0.value0.value1, v.value0.value0.value2, v.value0.value0.value3), v.value0.value0.value4, v.value0.value0.value5, new Two(v.value0.value0.value6, v.value0.value1, v.value0.value2, v1), v.value0.value3, v.value0.value4, v.value0.value5));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeMiddle && v.value0.value5 instanceof Three)) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(v.value0.value0, v.value0.value1, v.value0.value2, new Two(v1, v.value0.value3, v.value0.value4, v.value0.value5.value0), v.value0.value5.value1, v.value0.value5.value2, new Two(v.value0.value5.value3, v.value0.value5.value4, v.value0.value5.value5, v.value0.value5.value6)));
                    };
                    if (v instanceof Data_List.Cons && (v.value0 instanceof ThreeRight && v.value0.value3 instanceof Three)) {
                        return fromZipper(dictOrd1)(v.value1)(new Three(v.value0.value0, v.value0.value1, v.value0.value2, new Two(v.value0.value3.value0, v.value0.value3.value1, v.value0.value3.value2, v.value0.value3.value3), v.value0.value3.value4, v.value0.value3.value5, new Two(v.value0.value3.value6, v.value0.value4, v.value0.value5, v1)));
                    };
                    return Data_Maybe_Unsafe.unsafeThrow("Impossible case in 'up'");
                };
            };
        };
    };
    var removeMaxNode = function (__copy_dictOrd1) {
        return function (__copy_ctx) {
            return function (__copy_v) {
                var dictOrd1 = __copy_dictOrd1;
                var ctx = __copy_ctx;
                var v = __copy_v;
                tco: while (true) {
                    var ctx1 = ctx;
                    if (v instanceof Two && (v.value0 instanceof Leaf && v.value3 instanceof Leaf)) {
                        return up(dictOrd1)(ctx1)(Leaf.value);
                    };
                    var ctx1 = ctx;
                    if (v instanceof Two) {
                        var __tco_dictOrd1 = dictOrd1;
                        var __tco_ctx = new Data_List.Cons(new TwoRight(v.value0, v.value1, v.value2), ctx1);
                        var __tco_v = v.value3;
                        dictOrd1 = __tco_dictOrd1;
                        ctx = __tco_ctx;
                        v = __tco_v;
                        continue tco;
                    };
                    var ctx1 = ctx;
                    if (v instanceof Three && (v.value0 instanceof Leaf && (v.value3 instanceof Leaf && v.value6 instanceof Leaf))) {
                        return up(dictOrd1)(new Data_List.Cons(new TwoRight(Leaf.value, v.value1, v.value2), ctx1))(Leaf.value);
                    };
                    if (v instanceof Three) {
                        var __tco_dictOrd1 = dictOrd1;
                        var __tco_ctx = new Data_List.Cons(new ThreeRight(v.value0, v.value1, v.value2, v.value3, v.value4, v.value5), ctx);
                        var __tco_v = v.value6;
                        dictOrd1 = __tco_dictOrd1;
                        ctx = __tco_ctx;
                        v = __tco_v;
                        continue tco;
                    };
                    if (v instanceof Leaf) {
                        return Data_Maybe_Unsafe.unsafeThrow("Impossible case in 'removeMaxNode'");
                    };
                    throw new Error("Failed pattern match at Data.Map line 171, column 1 - line 172, column 1: " + [ ctx.constructor.name, v.constructor.name ]);
                };
            };
        };
    };
    var maxNode = function (__copy_dictOrd1) {
        return function (__copy_v) {
            var dictOrd1 = __copy_dictOrd1;
            var v = __copy_v;
            tco: while (true) {
                if (v instanceof Two && v.value3 instanceof Leaf) {
                    return {
                        key: v.value1, 
                        value: v.value2
                    };
                };
                if (v instanceof Two) {
                    var __tco_dictOrd1 = dictOrd1;
                    var __tco_v = v.value3;
                    dictOrd1 = __tco_dictOrd1;
                    v = __tco_v;
                    continue tco;
                };
                if (v instanceof Three && v.value6 instanceof Leaf) {
                    return {
                        key: v.value4, 
                        value: v.value5
                    };
                };
                if (v instanceof Three) {
                    var __tco_dictOrd1 = dictOrd1;
                    var __tco_v = v.value6;
                    dictOrd1 = __tco_dictOrd1;
                    v = __tco_v;
                    continue tco;
                };
                if (v instanceof Leaf) {
                    return Data_Maybe_Unsafe.unsafeThrow("Impossible case in 'maxNode'");
                };
                throw new Error("Failed pattern match at Data.Map line 171, column 1 - line 172, column 1: " + [ v.constructor.name ]);
            };
        };
    };
    var down = function (__copy_dictOrd1) {
        return function (__copy_ctx) {
            return function (__copy_k) {
                return function (__copy_v) {
                    var dictOrd1 = __copy_dictOrd1;
                    var ctx = __copy_ctx;
                    var k = __copy_k;
                    var v = __copy_v;
                    tco: while (true) {
                        var ctx1 = ctx;
                        if (v instanceof Leaf) {
                            return fromZipper(dictOrd1)(ctx1)(Leaf.value);
                        };
                        var ctx1 = ctx;
                        var k1 = k;
                        if (v instanceof Two && (v.value0 instanceof Leaf && (v.value3 instanceof Leaf && Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k1)(v.value1)))) {
                            return up(dictOrd1)(ctx1)(Leaf.value);
                        };
                        var ctx1 = ctx;
                        var k1 = k;
                        if (v instanceof Two) {
                            if (Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k1)(v.value1)) {
                                var max = maxNode(dictOrd1)(v.value0);
                                return removeMaxNode(dictOrd1)(new Data_List.Cons(new TwoLeft(max.key, max.value, v.value3), ctx1))(v.value0);
                            };
                            if (Prelude["<"](dictOrd1)(k1)(v.value1)) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new TwoLeft(v.value1, v.value2, v.value3), ctx1);
                                var __tco_v = v.value0;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = k1;
                                v = __tco_v;
                                continue tco;
                            };
                            if (Prelude.otherwise) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new TwoRight(v.value0, v.value1, v.value2), ctx1);
                                var __tco_v = v.value3;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = k1;
                                v = __tco_v;
                                continue tco;
                            };
                        };
                        var ctx1 = ctx;
                        var k1 = k;
                        if (v instanceof Three && (v.value0 instanceof Leaf && (v.value3 instanceof Leaf && v.value6 instanceof Leaf))) {
                            if (Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k1)(v.value1)) {
                                return fromZipper(dictOrd1)(ctx1)(new Two(Leaf.value, v.value4, v.value5, Leaf.value));
                            };
                            if (Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k1)(v.value4)) {
                                return fromZipper(dictOrd1)(ctx1)(new Two(Leaf.value, v.value1, v.value2, Leaf.value));
                            };
                        };
                        if (v instanceof Three) {
                            if (Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k)(v.value1)) {
                                var max = maxNode(dictOrd1)(v.value0);
                                return removeMaxNode(dictOrd1)(new Data_List.Cons(new ThreeLeft(max.key, max.value, v.value3, v.value4, v.value5, v.value6), ctx))(v.value0);
                            };
                            if (Prelude["=="](dictOrd1["__superclass_Prelude.Eq_0"]())(k)(v.value4)) {
                                var max = maxNode(dictOrd1)(v.value3);
                                return removeMaxNode(dictOrd1)(new Data_List.Cons(new ThreeMiddle(v.value0, v.value1, v.value2, max.key, max.value, v.value6), ctx))(v.value3);
                            };
                            if (Prelude["<"](dictOrd1)(k)(v.value1)) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new ThreeLeft(v.value1, v.value2, v.value3, v.value4, v.value5, v.value6), ctx);
                                var __tco_k = k;
                                var __tco_v = v.value0;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = __tco_k;
                                v = __tco_v;
                                continue tco;
                            };
                            if (Prelude["<"](dictOrd1)(v.value1)(k) && Prelude["<"](dictOrd1)(k)(v.value4)) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new ThreeMiddle(v.value0, v.value1, v.value2, v.value4, v.value5, v.value6), ctx);
                                var __tco_k = k;
                                var __tco_v = v.value3;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = __tco_k;
                                v = __tco_v;
                                continue tco;
                            };
                            if (Prelude.otherwise) {
                                var __tco_dictOrd1 = dictOrd1;
                                var __tco_ctx = new Data_List.Cons(new ThreeRight(v.value0, v.value1, v.value2, v.value3, v.value4, v.value5), ctx);
                                var __tco_k = k;
                                var __tco_v = v.value6;
                                dictOrd1 = __tco_dictOrd1;
                                ctx = __tco_ctx;
                                k = __tco_k;
                                v = __tco_v;
                                continue tco;
                            };
                        };
                        throw new Error("Failed pattern match at Data.Map line 171, column 1 - line 172, column 1: " + [ ctx.constructor.name, k.constructor.name, v.constructor.name ]);
                    };
                };
            };
        };
    };
    return down(dictOrd)(Data_List.Nil.value);
};
var checkValid = function (tree) {
    var allHeights = function (v) {
        if (v instanceof Leaf) {
            return Prelude.pure(Data_List.applicativeList)(0);
        };
        if (v instanceof Two) {
            return Prelude.map(Data_List.functorList)(function (n) {
                return n + 1 | 0;
            })(Prelude["++"](Data_List.semigroupList)(allHeights(v.value0))(allHeights(v.value3)));
        };
        if (v instanceof Three) {
            return Prelude.map(Data_List.functorList)(function (n) {
                return n + 1 | 0;
            })(Prelude["++"](Data_List.semigroupList)(allHeights(v.value0))(Prelude["++"](Data_List.semigroupList)(allHeights(v.value3))(allHeights(v.value6))));
        };
        throw new Error("Failed pattern match at Data.Map line 106, column 1 - line 107, column 1: " + [ v.constructor.name ]);
    };
    return Data_List.length(Data_List.nub(Prelude.eqInt)(allHeights(tree))) === 1;
};
var alter = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (m) {
                var $586 = f(lookup(dictOrd)(k)(m));
                if ($586 instanceof Data_Maybe.Nothing) {
                    return $$delete(dictOrd)(k)(m);
                };
                if ($586 instanceof Data_Maybe.Just) {
                    return insert(dictOrd)(k)($586.value0)(m);
                };
                throw new Error("Failed pattern match at Data.Map line 233, column 1 - line 234, column 1: " + [ $586.constructor.name ]);
            };
        };
    };
};
var fromListWith = function (dictOrd) {
    return function (f) {
        var combine = function (v) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Just) {
                    return Data_Maybe.Just.create(f(v)(v1.value0));
                };
                if (v1 instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v);
                };
                throw new Error("Failed pattern match at Data.Map line 256, column 3 - line 257, column 3: " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return Data_Foldable.foldl(Data_List.foldableList)(function (m) {
            return function (v) {
                return alter(dictOrd)(combine(v.value1))(v.value0)(m);
            };
        })(empty);
    };
};
var unionWith = function (dictOrd) {
    return function (f) {
        return function (m1) {
            return function (m2) {
                var go = function (m) {
                    return function (v) {
                        return alter(dictOrd)(function ($599) {
                            return Data_Maybe.Just.create(Data_Maybe.maybe(v.value1)(f(v.value1))($599));
                        })(v.value0)(m);
                    };
                };
                return Data_Foldable.foldl(Data_List.foldableList)(go)(m2)(toList(m1));
            };
        };
    };
};
var union = function (dictOrd) {
    return unionWith(dictOrd)(Prelude["const"]);
};
var semigroupMap = function (dictOrd) {
    return new Prelude.Semigroup(union(dictOrd));
};
var monoidMap = function (dictOrd) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMap(dictOrd);
    }, empty);
};
var traversableMap = function (dictOrd) {
    return new Data_Traversable.Traversable(function () {
        return foldableMap;
    }, function () {
        return functorMap;
    }, function (dictApplicative) {
        return Data_Traversable.traverse(traversableMap(dictOrd))(dictApplicative)(Prelude.id(Prelude.categoryFn));
    }, function (dictApplicative) {
        return function (f) {
            return function (ms) {
                return Data_Foldable.foldr(Data_List.foldableList)(function (x) {
                    return function (acc) {
                        return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(union(dictOrd))(x))(acc);
                    };
                })(Prelude.pure(dictApplicative)(empty))(Prelude["<$>"](Data_List.functorList)(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Tuple.uncurry(singleton)))(Prelude["<$>"](Data_List.functorList)(Data_Traversable.traverse(Data_Tuple.traversableTuple)(dictApplicative)(f))(toList(ms))));
            };
        };
    });
};
var unions = function (dictOrd) {
    return function (dictFoldable) {
        return Data_Foldable.foldl(dictFoldable)(union(dictOrd))(empty);
    };
};
var update = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (m) {
                return alter(dictOrd)(Data_Maybe.maybe(Data_Maybe.Nothing.value)(f))(k)(m);
            };
        };
    };
};
module.exports = {
    size: size, 
    unions: unions, 
    unionWith: unionWith, 
    union: union, 
    values: values, 
    keys: keys, 
    update: update, 
    alter: alter, 
    member: member, 
    "delete": $$delete, 
    fromListWith: fromListWith, 
    fromList: fromList, 
    toList: toList, 
    lookup: lookup, 
    insert: insert, 
    checkValid: checkValid, 
    singleton: singleton, 
    isEmpty: isEmpty, 
    empty: empty, 
    showTree: showTree, 
    eqMap: eqMap, 
    showMap: showMap, 
    ordMap: ordMap, 
    semigroupMap: semigroupMap, 
    monoidMap: monoidMap, 
    functorMap: functorMap, 
    foldableMap: foldableMap, 
    traversableMap: traversableMap
};

},{"Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.List":"/Users/anders/dev/ps/halo/ajax/output/Data.List/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.First/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var First = function (x) {
    return x;
};
var showFirst = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "First (" + (Prelude.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupFirst = new Prelude.Semigroup(function (v) {
    return function (second) {
        if (v instanceof Data_Maybe.Just) {
            return v;
        };
        return second;
    };
});
var runFirst = function (v) {
    return v;
};
var monoidFirst = new Data_Monoid.Monoid(function () {
    return semigroupFirst;
}, Data_Maybe.Nothing.value);
var functorFirst = new Prelude.Functor(function (f) {
    return function (v) {
        return Prelude["<$>"](Data_Maybe.functorMaybe)(f)(v);
    };
});
var invariantFirst = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorFirst));
var extendFirst = new Control_Extend.Extend(function () {
    return functorFirst;
}, function (f) {
    return function (v) {
        return Control_Extend.extend(Data_Maybe.extendMaybe)(function ($33) {
            return f(First($33));
        })(v);
    };
});
var eqFirst = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](Data_Maybe.eqMaybe(dictEq))(v)(v1);
        };
    });
};
var ordFirst = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqFirst(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(Data_Maybe.ordMaybe(dictOrd))(v)(v1);
        };
    });
};
var boundedFirst = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(Data_Maybe.boundedMaybe(dictBounded)), Prelude.top(Data_Maybe.boundedMaybe(dictBounded)));
};
var applyFirst = new Prelude.Apply(function () {
    return functorFirst;
}, function (v) {
    return function (v1) {
        return Prelude["<*>"](Data_Maybe.applyMaybe)(v)(v1);
    };
});
var bindFirst = new Prelude.Bind(function () {
    return applyFirst;
}, function (v) {
    return function (f) {
        return Prelude.bind(Data_Maybe.bindMaybe)(v)(function ($34) {
            return runFirst(f($34));
        });
    };
});
var applicativeFirst = new Prelude.Applicative(function () {
    return applyFirst;
}, function ($35) {
    return First(Prelude.pure(Data_Maybe.applicativeMaybe)($35));
});
var monadFirst = new Prelude.Monad(function () {
    return applicativeFirst;
}, function () {
    return bindFirst;
});
module.exports = {
    First: First, 
    runFirst: runFirst, 
    eqFirst: eqFirst, 
    ordFirst: ordFirst, 
    boundedFirst: boundedFirst, 
    functorFirst: functorFirst, 
    applyFirst: applyFirst, 
    applicativeFirst: applicativeFirst, 
    bindFirst: bindFirst, 
    monadFirst: monadFirst, 
    extendFirst: extendFirst, 
    invariantFirst: invariantFirst, 
    showFirst: showFirst, 
    semigroupFirst: semigroupFirst, 
    monoidFirst: monoidFirst
};

},{"Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Last/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var Last = function (x) {
    return x;
};
var showLast = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Last (" + (Prelude.show(Data_Maybe.showMaybe(dictShow))(v) + ")");
    });
};
var semigroupLast = new Prelude.Semigroup(function (last) {
    return function (v) {
        if (v instanceof Data_Maybe.Just) {
            return v;
        };
        if (v instanceof Data_Maybe.Nothing) {
            return last;
        };
        throw new Error("Failed pattern match at Data.Maybe.Last line 57, column 1 - line 61, column 1: " + [ last.constructor.name, v.constructor.name ]);
    };
});
var runLast = function (v) {
    return v;
};
var monoidLast = new Data_Monoid.Monoid(function () {
    return semigroupLast;
}, Data_Maybe.Nothing.value);
var functorLast = new Prelude.Functor(function (f) {
    return function (v) {
        return Prelude["<$>"](Data_Maybe.functorMaybe)(f)(v);
    };
});
var invariantLast = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorLast));
var extendLast = new Control_Extend.Extend(function () {
    return functorLast;
}, function (f) {
    return function (v) {
        return Control_Extend.extend(Data_Maybe.extendMaybe)(function ($33) {
            return f(Last($33));
        })(v);
    };
});
var eqLast = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](Data_Maybe.eqMaybe(dictEq))(v)(v1);
        };
    });
};
var ordLast = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqLast(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(Data_Maybe.ordMaybe(dictOrd))(v)(v1);
        };
    });
};
var boundedLast = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(Data_Maybe.boundedMaybe(dictBounded)), Prelude.top(Data_Maybe.boundedMaybe(dictBounded)));
};
var applyLast = new Prelude.Apply(function () {
    return functorLast;
}, function (v) {
    return function (v1) {
        return Prelude["<*>"](Data_Maybe.applyMaybe)(v)(v1);
    };
});
var bindLast = new Prelude.Bind(function () {
    return applyLast;
}, function (v) {
    return function (f) {
        return Prelude.bind(Data_Maybe.bindMaybe)(v)(function ($34) {
            return runLast(f($34));
        });
    };
});
var applicativeLast = new Prelude.Applicative(function () {
    return applyLast;
}, function ($35) {
    return Last(Prelude.pure(Data_Maybe.applicativeMaybe)($35));
});
var monadLast = new Prelude.Monad(function () {
    return applicativeLast;
}, function () {
    return bindLast;
});
module.exports = {
    Last: Last, 
    runLast: runLast, 
    eqLast: eqLast, 
    ordLast: ordLast, 
    boundedLast: boundedLast, 
    functorLast: functorLast, 
    applyLast: applyLast, 
    applicativeLast: applicativeLast, 
    bindLast: bindLast, 
    monadLast: monadLast, 
    extendLast: extendLast, 
    invariantLast: invariantLast, 
    showLast: showLast, 
    semigroupLast: semigroupLast, 
    monoidLast: monoidLast
};

},{"Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Maybe.Unsafe

exports.unsafeThrow = function (msg) {
  throw new Error(msg);
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var fromJust = function (v) {
    if (v instanceof Data_Maybe.Just) {
        return v.value0;
    };
    if (v instanceof Data_Maybe.Nothing) {
        return $foreign.unsafeThrow("Data.Maybe.Unsafe.fromJust called on Nothing");
    };
    throw new Error("Failed pattern match at Data.Maybe.Unsafe line 10, column 1 - line 11, column 1: " + [ v.constructor.name ]);
};
module.exports = {
    fromJust: fromJust, 
    unsafeThrow: $foreign.unsafeThrow
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/foreign.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Extend = require("Control.Extend");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Monoid = require("Data.Monoid");
var Nothing = (function () {
    function Nothing() {

    };
    Nothing.value = new Nothing();
    return Nothing;
})();
var Just = (function () {
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return Just;
})();
var showMaybe = function (dictShow) {
    return new Prelude.Show(function (v) {
        if (v instanceof Just) {
            return "Just (" + (Prelude.show(dictShow)(v.value0) + ")");
        };
        if (v instanceof Nothing) {
            return "Nothing";
        };
        throw new Error("Failed pattern match at Data.Maybe line 289, column 1 - line 291, column 19: " + [ v.constructor.name ]);
    });
};
var semigroupMaybe = function (dictSemigroup) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            if (v instanceof Nothing) {
                return v1;
            };
            if (v1 instanceof Nothing) {
                return v;
            };
            if (v instanceof Just && v1 instanceof Just) {
                return new Just(Prelude["<>"](dictSemigroup)(v.value0)(v1.value0));
            };
            throw new Error("Failed pattern match at Data.Maybe line 231, column 1 - line 236, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var monoidMaybe = function (dictSemigroup) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMaybe(dictSemigroup);
    }, Nothing.value);
};
var maybe$prime = function (g) {
    return function (f) {
        return function (v) {
            if (v instanceof Nothing) {
                return g(Prelude.unit);
            };
            if (v instanceof Just) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 39, column 1 - line 40, column 1: " + [ g.constructor.name, f.constructor.name, v.constructor.name ]);
        };
    };
};
var maybe = function (b) {
    return function (f) {
        return function (v) {
            if (v instanceof Nothing) {
                return b;
            };
            if (v instanceof Just) {
                return f(v.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe line 26, column 1 - line 27, column 1: " + [ b.constructor.name, f.constructor.name, v.constructor.name ]);
        };
    };
};
var isNothing = maybe(true)(Prelude["const"](false));
var isJust = maybe(false)(Prelude["const"](true));
var functorMaybe = new Prelude.Functor(function (fn) {
    return function (v) {
        if (v instanceof Just) {
            return new Just(fn(v.value0));
        };
        return Nothing.value;
    };
});
var invariantMaybe = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorMaybe));
var fromMaybe$prime = function (a) {
    return maybe$prime(a)(Prelude.id(Prelude.categoryFn));
};
var fromMaybe = function (a) {
    return maybe(a)(Prelude.id(Prelude.categoryFn));
};
var extendMaybe = new Control_Extend.Extend(function () {
    return functorMaybe;
}, function (f) {
    return function (v) {
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        return new Just(f(v));
    };
});
var eqMaybe = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            if (v instanceof Nothing && v1 instanceof Nothing) {
                return true;
            };
            if (v instanceof Just && v1 instanceof Just) {
                return Prelude["=="](dictEq)(v.value0)(v1.value0);
            };
            return false;
        };
    });
};
var ordMaybe = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqMaybe(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            if (v instanceof Just && v1 instanceof Just) {
                return Prelude.compare(dictOrd)(v.value0)(v1.value0);
            };
            if (v instanceof Nothing && v1 instanceof Nothing) {
                return Prelude.EQ.value;
            };
            if (v instanceof Nothing) {
                return Prelude.LT.value;
            };
            if (v1 instanceof Nothing) {
                return Prelude.GT.value;
            };
            throw new Error("Failed pattern match at Data.Maybe line 269, column 1 - line 275, column 1: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var boundedMaybe = function (dictBounded) {
    return new Prelude.Bounded(Nothing.value, new Just(Prelude.top(dictBounded)));
};
var boundedOrdMaybe = function (dictBoundedOrd) {
    return new Prelude.BoundedOrd(function () {
        return boundedMaybe(dictBoundedOrd["__superclass_Prelude.Bounded_0"]());
    }, function () {
        return ordMaybe(dictBoundedOrd["__superclass_Prelude.Ord_1"]());
    });
};
var applyMaybe = new Prelude.Apply(function () {
    return functorMaybe;
}, function (v) {
    return function (x) {
        if (v instanceof Just) {
            return Prelude["<$>"](functorMaybe)(v.value0)(x);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe line 121, column 1 - line 145, column 1: " + [ v.constructor.name, x.constructor.name ]);
    };
});
var bindMaybe = new Prelude.Bind(function () {
    return applyMaybe;
}, function (v) {
    return function (k) {
        if (v instanceof Just) {
            return k(v.value0);
        };
        if (v instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Maybe line 180, column 1 - line 199, column 1: " + [ v.constructor.name, k.constructor.name ]);
    };
});
var booleanAlgebraMaybe = function (dictBooleanAlgebra) {
    return new Prelude.BooleanAlgebra(function () {
        return boundedMaybe(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]());
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.conj(dictBooleanAlgebra))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.disj(dictBooleanAlgebra))(x))(y);
        };
    }, Prelude.map(functorMaybe)(Prelude.not(dictBooleanAlgebra)));
};
var semiringMaybe = function (dictSemiring) {
    return new Prelude.Semiring(function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.add(dictSemiring))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.mul(dictSemiring))(x))(y);
        };
    }, new Just(Prelude.one(dictSemiring)), new Just(Prelude.zero(dictSemiring)));
};
var moduloSemiringMaybe = function (dictModuloSemiring) {
    return new Prelude.ModuloSemiring(function () {
        return semiringMaybe(dictModuloSemiring["__superclass_Prelude.Semiring_0"]());
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.div(dictModuloSemiring))(x))(y);
        };
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.mod(dictModuloSemiring))(x))(y);
        };
    });
};
var ringMaybe = function (dictRing) {
    return new Prelude.Ring(function () {
        return semiringMaybe(dictRing["__superclass_Prelude.Semiring_0"]());
    }, function (x) {
        return function (y) {
            return Prelude["<*>"](applyMaybe)(Prelude["<$>"](functorMaybe)(Prelude.sub(dictRing))(x))(y);
        };
    });
};
var divisionRingMaybe = function (dictDivisionRing) {
    return new Prelude.DivisionRing(function () {
        return moduloSemiringMaybe(dictDivisionRing["__superclass_Prelude.ModuloSemiring_1"]());
    }, function () {
        return ringMaybe(dictDivisionRing["__superclass_Prelude.Ring_0"]());
    });
};
var numMaybe = function (dictNum) {
    return new Prelude.Num(function () {
        return divisionRingMaybe(dictNum["__superclass_Prelude.DivisionRing_0"]());
    });
};
var applicativeMaybe = new Prelude.Applicative(function () {
    return applyMaybe;
}, Just.create);
var monadMaybe = new Prelude.Monad(function () {
    return applicativeMaybe;
}, function () {
    return bindMaybe;
});
var altMaybe = new Control_Alt.Alt(function () {
    return functorMaybe;
}, function (v) {
    return function (r) {
        if (v instanceof Nothing) {
            return r;
        };
        return v;
    };
});
var plusMaybe = new Control_Plus.Plus(function () {
    return altMaybe;
}, Nothing.value);
var alternativeMaybe = new Control_Alternative.Alternative(function () {
    return plusMaybe;
}, function () {
    return applicativeMaybe;
});
var monadPlusMaybe = new Control_MonadPlus.MonadPlus(function () {
    return alternativeMaybe;
}, function () {
    return monadMaybe;
});
module.exports = {
    Nothing: Nothing, 
    Just: Just, 
    isNothing: isNothing, 
    isJust: isJust, 
    "fromMaybe'": fromMaybe$prime, 
    fromMaybe: fromMaybe, 
    "maybe'": maybe$prime, 
    maybe: maybe, 
    functorMaybe: functorMaybe, 
    applyMaybe: applyMaybe, 
    applicativeMaybe: applicativeMaybe, 
    altMaybe: altMaybe, 
    plusMaybe: plusMaybe, 
    alternativeMaybe: alternativeMaybe, 
    bindMaybe: bindMaybe, 
    monadMaybe: monadMaybe, 
    monadPlusMaybe: monadPlusMaybe, 
    extendMaybe: extendMaybe, 
    invariantMaybe: invariantMaybe, 
    semigroupMaybe: semigroupMaybe, 
    monoidMaybe: monoidMaybe, 
    semiringMaybe: semiringMaybe, 
    moduloSemiringMaybe: moduloSemiringMaybe, 
    ringMaybe: ringMaybe, 
    divisionRingMaybe: divisionRingMaybe, 
    numMaybe: numMaybe, 
    eqMaybe: eqMaybe, 
    ordMaybe: ordMaybe, 
    boundedMaybe: boundedMaybe, 
    boundedOrdMaybe: boundedOrdMaybe, 
    booleanAlgebraMaybe: booleanAlgebraMaybe, 
    showMaybe: showMaybe
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Alternative":"/Users/anders/dev/ps/halo/ajax/output/Control.Alternative/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Control.MonadPlus":"/Users/anders/dev/ps/halo/ajax/output/Control.MonadPlus/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Additive/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Monoid = require("Data.Monoid");
var Additive = function (x) {
    return x;
};
var showAdditive = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Additive (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semigroupAdditive = function (dictSemiring) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["+"](dictSemiring)(v)(v1);
        };
    });
};
var runAdditive = function (v) {
    return v;
};
var monoidAdditive = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupAdditive(dictSemiring);
    }, Prelude.zero(dictSemiring));
};
var invariantAdditive = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorAdditive = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendAdditive = new Control_Extend.Extend(function () {
    return functorAdditive;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqAdditive = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordAdditive = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqAdditive(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadAdditive = new Control_Comonad.Comonad(function () {
    return extendAdditive;
}, runAdditive);
var applyAdditive = new Prelude.Apply(function () {
    return functorAdditive;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindAdditive = new Prelude.Bind(function () {
    return applyAdditive;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeAdditive = new Prelude.Applicative(function () {
    return applyAdditive;
}, Additive);
var monadAdditive = new Prelude.Monad(function () {
    return applicativeAdditive;
}, function () {
    return bindAdditive;
});
module.exports = {
    Additive: Additive, 
    runAdditive: runAdditive, 
    eqAdditive: eqAdditive, 
    ordAdditive: ordAdditive, 
    functorAdditive: functorAdditive, 
    applyAdditive: applyAdditive, 
    applicativeAdditive: applicativeAdditive, 
    bindAdditive: bindAdditive, 
    monadAdditive: monadAdditive, 
    extendAdditive: extendAdditive, 
    comonadAdditive: comonadAdditive, 
    invariantAdditive: invariantAdditive, 
    showAdditive: showAdditive, 
    semigroupAdditive: semigroupAdditive, 
    monoidAdditive: monoidAdditive
};

},{"Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Conj/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Monoid = require("Data.Monoid");
var Conj = function (x) {
    return x;
};
var showConj = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Conj (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semiringConj = function (dictBooleanAlgebra) {
    return new Prelude.Semiring(function (v) {
        return function (v1) {
            return Prelude.conj(dictBooleanAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude.disj(dictBooleanAlgebra)(v)(v1);
        };
    }, Prelude.bottom(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()), Prelude.top(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
};
var semigroupConj = function (dictBooleanAlgebra) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude.conj(dictBooleanAlgebra)(v)(v1);
        };
    });
};
var runConj = function (v) {
    return v;
};
var monoidConj = function (dictBooleanAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupConj(dictBooleanAlgebra);
    }, Prelude.top(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
};
var functorConj = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendConj = new Control_Extend.Extend(function () {
    return functorConj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqConj = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordConj = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqConj(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadConj = new Control_Comonad.Comonad(function () {
    return extendConj;
}, runConj);
var boundedConj = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(dictBounded), Prelude.top(dictBounded));
};
var applyConj = new Prelude.Apply(function () {
    return functorConj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindConj = new Prelude.Bind(function () {
    return applyConj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeConj = new Prelude.Applicative(function () {
    return applyConj;
}, Conj);
var monadConj = new Prelude.Monad(function () {
    return applicativeConj;
}, function () {
    return bindConj;
});
module.exports = {
    Conj: Conj, 
    runConj: runConj, 
    eqConj: eqConj, 
    ordConj: ordConj, 
    boundedConj: boundedConj, 
    functorConj: functorConj, 
    applyConj: applyConj, 
    applicativeConj: applicativeConj, 
    bindConj: bindConj, 
    monadConj: monadConj, 
    extendConj: extendConj, 
    comonadConj: comonadConj, 
    showConj: showConj, 
    semigroupConj: semigroupConj, 
    monoidConj: monoidConj, 
    semiringConj: semiringConj
};

},{"Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Disj/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Monoid = require("Data.Monoid");
var Disj = function (x) {
    return x;
};
var showDisj = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Disj (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semiringDisj = function (dictBooleanAlgebra) {
    return new Prelude.Semiring(function (v) {
        return function (v1) {
            return Prelude.disj(dictBooleanAlgebra)(v)(v1);
        };
    }, function (v) {
        return function (v1) {
            return Prelude.conj(dictBooleanAlgebra)(v)(v1);
        };
    }, Prelude.top(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()), Prelude.bottom(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
};
var semigroupDisj = function (dictBooleanAlgebra) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude.disj(dictBooleanAlgebra)(v)(v1);
        };
    });
};
var runDisj = function (v) {
    return v;
};
var monoidDisj = function (dictBooleanAlgebra) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDisj(dictBooleanAlgebra);
    }, Prelude.bottom(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]()));
};
var functorDisj = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendDisj = new Control_Extend.Extend(function () {
    return functorDisj;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqDisj = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordDisj = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqDisj(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadDisj = new Control_Comonad.Comonad(function () {
    return extendDisj;
}, runDisj);
var boundedDisj = function (dictBounded) {
    return new Prelude.Bounded(Prelude.bottom(dictBounded), Prelude.top(dictBounded));
};
var applyDisj = new Prelude.Apply(function () {
    return functorDisj;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDisj = new Prelude.Bind(function () {
    return applyDisj;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDisj = new Prelude.Applicative(function () {
    return applyDisj;
}, Disj);
var monadDisj = new Prelude.Monad(function () {
    return applicativeDisj;
}, function () {
    return bindDisj;
});
module.exports = {
    Disj: Disj, 
    runDisj: runDisj, 
    eqDisj: eqDisj, 
    ordDisj: ordDisj, 
    boundedDisj: boundedDisj, 
    functorDisj: functorDisj, 
    applyDisj: applyDisj, 
    applicativeDisj: applicativeDisj, 
    bindDisj: bindDisj, 
    monadDisj: monadDisj, 
    extendDisj: extendDisj, 
    comonadDisj: comonadDisj, 
    showDisj: showDisj, 
    semigroupDisj: semigroupDisj, 
    monoidDisj: monoidDisj, 
    semiringDisj: semiringDisj
};

},{"Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Dual/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Monoid = require("Data.Monoid");
var Dual = function (x) {
    return x;
};
var showDual = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Dual (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semigroupDual = function (dictSemigroup) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["<>"](dictSemigroup)(v1)(v);
        };
    });
};
var runDual = function (v) {
    return v;
};
var monoidDual = function (dictMonoid) {
    return new Data_Monoid.Monoid(function () {
        return semigroupDual(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Data_Monoid.mempty(dictMonoid));
};
var invariantDual = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorDual = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendDual = new Control_Extend.Extend(function () {
    return functorDual;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqDual = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordDual = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqDual(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadDual = new Control_Comonad.Comonad(function () {
    return extendDual;
}, runDual);
var applyDual = new Prelude.Apply(function () {
    return functorDual;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindDual = new Prelude.Bind(function () {
    return applyDual;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeDual = new Prelude.Applicative(function () {
    return applyDual;
}, Dual);
var monadDual = new Prelude.Monad(function () {
    return applicativeDual;
}, function () {
    return bindDual;
});
module.exports = {
    Dual: Dual, 
    runDual: runDual, 
    eqDual: eqDual, 
    ordDual: ordDual, 
    functorDual: functorDual, 
    applyDual: applyDual, 
    applicativeDual: applicativeDual, 
    bindDual: bindDual, 
    monadDual: monadDual, 
    extendDual: extendDual, 
    comonadDual: comonadDual, 
    invariantDual: invariantDual, 
    showDual: showDual, 
    semigroupDual: semigroupDual, 
    monoidDual: monoidDual
};

},{"Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Endo/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Monoid = require("Data.Monoid");
var Endo = function (x) {
    return x;
};
var semigroupEndo = new Prelude.Semigroup(function (v) {
    return function (v1) {
        return function ($10) {
            return v(v1($10));
        };
    };
});
var runEndo = function (v) {
    return v;
};
var monoidEndo = new Data_Monoid.Monoid(function () {
    return semigroupEndo;
}, Prelude.id(Prelude.categoryFn));
var invariantEndo = new Data_Functor_Invariant.Invariant(function (ab) {
    return function (ba) {
        return function (v) {
            return function ($11) {
                return ab(v(ba($11)));
            };
        };
    };
});
module.exports = {
    Endo: Endo, 
    runEndo: runEndo, 
    invariantEndo: invariantEndo, 
    semigroupEndo: semigroupEndo, 
    monoidEndo: monoidEndo
};

},{"Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Multiplicative/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Monoid = require("Data.Monoid");
var Multiplicative = function (x) {
    return x;
};
var showMultiplicative = function (dictShow) {
    return new Prelude.Show(function (v) {
        return "Multiplicative (" + (Prelude.show(dictShow)(v) + ")");
    });
};
var semigroupMultiplicative = function (dictSemiring) {
    return new Prelude.Semigroup(function (v) {
        return function (v1) {
            return Prelude["*"](dictSemiring)(v)(v1);
        };
    });
};
var runMultiplicative = function (v) {
    return v;
};
var monoidMultiplicative = function (dictSemiring) {
    return new Data_Monoid.Monoid(function () {
        return semigroupMultiplicative(dictSemiring);
    }, Prelude.one(dictSemiring));
};
var invariantMultiplicative = new Data_Functor_Invariant.Invariant(function (f) {
    return function (v) {
        return function (v1) {
            return f(v1);
        };
    };
});
var functorMultiplicative = new Prelude.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var extendMultiplicative = new Control_Extend.Extend(function () {
    return functorMultiplicative;
}, function (f) {
    return function (x) {
        return f(x);
    };
});
var eqMultiplicative = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](dictEq)(v)(v1);
        };
    });
};
var ordMultiplicative = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqMultiplicative(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (v) {
        return function (v1) {
            return Prelude.compare(dictOrd)(v)(v1);
        };
    });
};
var comonadMultiplicative = new Control_Comonad.Comonad(function () {
    return extendMultiplicative;
}, runMultiplicative);
var applyMultiplicative = new Prelude.Apply(function () {
    return functorMultiplicative;
}, function (v) {
    return function (v1) {
        return v(v1);
    };
});
var bindMultiplicative = new Prelude.Bind(function () {
    return applyMultiplicative;
}, function (v) {
    return function (f) {
        return f(v);
    };
});
var applicativeMultiplicative = new Prelude.Applicative(function () {
    return applyMultiplicative;
}, Multiplicative);
var monadMultiplicative = new Prelude.Monad(function () {
    return applicativeMultiplicative;
}, function () {
    return bindMultiplicative;
});
module.exports = {
    Multiplicative: Multiplicative, 
    runMultiplicative: runMultiplicative, 
    eqMultiplicative: eqMultiplicative, 
    ordMultiplicative: ordMultiplicative, 
    functorMultiplicative: functorMultiplicative, 
    applyMultiplicative: applyMultiplicative, 
    applicativeMultiplicative: applicativeMultiplicative, 
    bindMultiplicative: bindMultiplicative, 
    monadMultiplicative: monadMultiplicative, 
    extendMultiplicative: extendMultiplicative, 
    comonadMultiplicative: comonadMultiplicative, 
    invariantMultiplicative: invariantMultiplicative, 
    showMultiplicative: showMultiplicative, 
    semigroupMultiplicative: semigroupMultiplicative, 
    monoidMultiplicative: monoidMultiplicative
};

},{"Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Monoid = function (__superclass_Prelude$dotSemigroup_0, mempty) {
    this["__superclass_Prelude.Semigroup_0"] = __superclass_Prelude$dotSemigroup_0;
    this.mempty = mempty;
};
var monoidUnit = new Monoid(function () {
    return Prelude.semigroupUnit;
}, Prelude.unit);
var monoidString = new Monoid(function () {
    return Prelude.semigroupString;
}, "");
var monoidArray = new Monoid(function () {
    return Prelude.semigroupArray;
}, [  ]);
var mempty = function (dict) {
    return dict.mempty;
};
var monoidFn = function (dictMonoid) {
    return new Monoid(function () {
        return Prelude.semigroupFn(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Prelude["const"](mempty(dictMonoid)));
};
module.exports = {
    Monoid: Monoid, 
    mempty: mempty, 
    monoidUnit: monoidUnit, 
    monoidFn: monoidFn, 
    monoidString: monoidString, 
    monoidArray: monoidArray
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js":[function(require,module,exports){
arguments[4]["/Users/anders/dev/ps/halo/ajax/output/DOM.File.Types/index.js"][0].apply(exports,arguments)
},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Nullable

exports["null"] = null;

exports.nullable = function(a, r, f) {
    return a == null ? r : f(a);
};

exports.notNull = function(x) {
    return x;
}; 

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Function = require("Data.Function");
var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);
var toMaybe = function (n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
};
var showNullable = function (dictShow) {
    return new Prelude.Show(function (n) {
        var $3 = toMaybe(n);
        if ($3 instanceof Data_Maybe.Nothing) {
            return "null";
        };
        if ($3 instanceof Data_Maybe.Just) {
            return Prelude.show(dictShow)($3.value0);
        };
        throw new Error("Failed pattern match at Data.Nullable line 37, column 1 - line 42, column 1: " + [ $3.constructor.name ]);
    });
};
var eqNullable = function (dictEq) {
    return new Prelude.Eq(Data_Function.on(Prelude.eq(Data_Maybe.eqMaybe(dictEq)))(toMaybe));
};
var ordNullable = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqNullable(dictOrd["__superclass_Prelude.Eq_0"]());
    }, Data_Function.on(Prelude.compare(Data_Maybe.ordMaybe(dictOrd)))(toMaybe));
};
module.exports = {
    toNullable: toNullable, 
    toMaybe: toMaybe, 
    showNullable: showNullable, 
    eqNullable: eqNullable, 
    ordNullable: ordNullable
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/foreign.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Profunctor.Choice/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Profunctor = require("Data.Profunctor");
var Choice = function (__superclass_Data$dotProfunctor$dotProfunctor_0, left, right) {
    this["__superclass_Data.Profunctor.Profunctor_0"] = __superclass_Data$dotProfunctor$dotProfunctor_0;
    this.left = left;
    this.right = right;
};
var right = function (dict) {
    return dict.right;
};
var left = function (dict) {
    return dict.left;
};
var $plus$plus$plus = function (dictCategory) {
    return function (dictChoice) {
        return function (l) {
            return function (r) {
                return Prelude[">>>"](dictCategory["__superclass_Prelude.Semigroupoid_0"]())(left(dictChoice)(l))(right(dictChoice)(r));
            };
        };
    };
};
var $bar$bar$bar = function (dictCategory) {
    return function (dictChoice) {
        return function (l) {
            return function (r) {
                var join = Data_Profunctor.dimap(dictChoice["__superclass_Data.Profunctor.Profunctor_0"]())(Data_Either.either(Prelude.id(Prelude.categoryFn))(Prelude.id(Prelude.categoryFn)))(Prelude.id(Prelude.categoryFn))(Prelude.id(dictCategory));
                return Prelude[">>>"](dictCategory["__superclass_Prelude.Semigroupoid_0"]())($plus$plus$plus(dictCategory)(dictChoice)(l)(r))(join);
            };
        };
    };
};
var choiceFn = new Choice(function () {
    return Data_Profunctor.profunctorFn;
}, function (a2b) {
    return function (v) {
        if (v instanceof Data_Either.Left) {
            return Data_Either.Left.create(a2b(v.value0));
        };
        if (v instanceof Data_Either.Right) {
            return new Data_Either.Right(v.value0);
        };
        throw new Error("Failed pattern match at Data.Profunctor.Choice line 17, column 1 - line 22, column 1: " + [ a2b.constructor.name, v.constructor.name ]);
    };
}, Prelude["<$>"](Data_Either.functorEither));
module.exports = {
    Choice: Choice, 
    "|||": $bar$bar$bar, 
    "+++": $plus$plus$plus, 
    right: right, 
    left: left, 
    choiceFn: choiceFn
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Profunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Profunctor/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Profunctor/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Profunctor = function (dimap) {
    this.dimap = dimap;
};
var profunctorFn = new Profunctor(function (a2b) {
    return function (c2d) {
        return function (b2c) {
            return Prelude[">>>"](Prelude.semigroupoidFn)(a2b)(Prelude[">>>"](Prelude.semigroupoidFn)(b2c)(c2d));
        };
    };
});
var dimap = function (dict) {
    return dict.dimap;
};
var lmap = function (dictProfunctor) {
    return function (a2b) {
        return dimap(dictProfunctor)(a2b)(Prelude.id(Prelude.categoryFn));
    };
};
var rmap = function (dictProfunctor) {
    return function (b2c) {
        return dimap(dictProfunctor)(Prelude.id(Prelude.categoryFn))(b2c);
    };
};
var arr = function (dictCategory) {
    return function (dictProfunctor) {
        return function (f) {
            return rmap(dictProfunctor)(f)(Prelude.id(dictCategory));
        };
    };
};
module.exports = {
    Profunctor: Profunctor, 
    arr: arr, 
    rmap: rmap, 
    lmap: lmap, 
    dimap: dimap, 
    profunctorFn: profunctorFn
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Set/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Map = require("Data.Map");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var Data_Tuple = require("Data.Tuple");
var Data_Foldable = require("Data.Foldable");
var Data_List = require("Data.List");
var Set = (function () {
    function Set(value0) {
        this.value0 = value0;
    };
    Set.create = function (value0) {
        return new Set(value0);
    };
    return Set;
})();
var union = function (dictOrd) {
    return function (v) {
        return function (v1) {
            return new Set(Data_Map.union(dictOrd)(v.value0)(v1.value0));
        };
    };
};
var toList = function (v) {
    return Prelude.map(Data_List.functorList)(Data_Tuple.fst)(Data_Map.toList(v.value0));
};
var size = function (v) {
    return Data_Map.size(v.value0);
};
var singleton = function (a) {
    return new Set(Data_Map.singleton(a)(Prelude.unit));
};
var showSet = function (dictShow) {
    return new Prelude.Show(function (s) {
        return "fromList " + Prelude.show(Data_List.showList(dictShow))(toList(s));
    });
};
var monoidSemigroup = function (dictOrd) {
    return new Prelude.Semigroup(union(dictOrd));
};
var member = function (dictOrd) {
    return function (a) {
        return function (v) {
            return Data_Map.member(dictOrd)(a)(v.value0);
        };
    };
};
var isEmpty = function (v) {
    return Data_Map.isEmpty(v.value0);
};
var insert = function (dictOrd) {
    return function (a) {
        return function (v) {
            return new Set(Data_Map.insert(dictOrd)(a)(Prelude.unit)(v.value0));
        };
    };
};
var foldableSet = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function ($49) {
            return Data_Foldable.foldMap(Data_List.foldableList)(dictMonoid)(f)(toList($49));
        };
    };
}, function (f) {
    return function (x) {
        return function ($50) {
            return Data_Foldable.foldl(Data_List.foldableList)(f)(x)(toList($50));
        };
    };
}, function (f) {
    return function (x) {
        return function ($51) {
            return Data_Foldable.foldr(Data_List.foldableList)(f)(x)(toList($51));
        };
    };
});
var eqSet = function (dictEq) {
    return new Prelude.Eq(function (v) {
        return function (v1) {
            return Prelude["=="](Data_Map.eqMap(dictEq)(Prelude.eqUnit))(v.value0)(v1.value0);
        };
    });
};
var ordSet = function (dictOrd) {
    return new Prelude.Ord(function () {
        return eqSet(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (s1) {
        return function (s2) {
            return Prelude.compare(Data_List.ordList(dictOrd))(toList(s1))(toList(s2));
        };
    });
};
var empty = new Set(Data_Map.empty);
var fromList = function (dictOrd) {
    return Data_Foldable.foldl(Data_List.foldableList)(function (m) {
        return function (a) {
            return insert(dictOrd)(a)(m);
        };
    })(empty);
};
var monoidSet = function (dictOrd) {
    return new Data_Monoid.Monoid(function () {
        return monoidSemigroup(dictOrd);
    }, empty);
};
var unions = function (dictOrd) {
    return Data_Foldable.foldl(Data_List.foldableList)(union(dictOrd))(empty);
};
var $$delete = function (dictOrd) {
    return function (a) {
        return function (v) {
            return new Set(Data_Map["delete"](dictOrd)(a)(v.value0));
        };
    };
};
var difference = function (dictOrd) {
    return function (s1) {
        return function (s2) {
            return Data_Foldable.foldl(Data_List.foldableList)(Prelude.flip($$delete(dictOrd)))(s1)(toList(s2));
        };
    };
};
var checkValid = function (v) {
    return Data_Map.checkValid(v.value0);
};
module.exports = {
    difference: difference, 
    unions: unions, 
    union: union, 
    size: size, 
    fromList: fromList, 
    toList: toList, 
    "delete": $$delete, 
    member: member, 
    insert: insert, 
    checkValid: checkValid, 
    singleton: singleton, 
    isEmpty: isEmpty, 
    empty: empty, 
    eqSet: eqSet, 
    showSet: showSet, 
    ordSet: ordSet, 
    monoidSet: monoidSet, 
    monoidSemigroup: monoidSemigroup, 
    foldableSet: foldableSet
};

},{"Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.List":"/Users/anders/dev/ps/halo/ajax/output/Data.List/index.js","Data.Map":"/Users/anders/dev/ps/halo/ajax/output/Data.Map/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.String.Unsafe/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.String.Unsafe

exports.charCodeAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charCodeAt(i);
    throw new Error("Data.String.Unsafe.charCodeAt: Invalid index.");
  };
};

exports.charAt = function (i) {
  return function (s) {
    if (i >= 0 && i < s.length) return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

exports.char = function (s) {
  if (s.length === 1) return s.charAt(0);
  throw new Error("Data.String.Unsafe.char: Expected string of length 1.");
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.String.Unsafe/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Data_Char = require("Data.Char");
module.exports = {
    charCodeAt: $foreign.charCodeAt, 
    charAt: $foreign.charAt, 
    "char": $foreign["char"]
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.String.Unsafe/foreign.js","Data.Char":"/Users/anders/dev/ps/halo/ajax/output/Data.Char/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.String/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.String

exports._charAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
      };
    };
  };
};

exports._charCodeAt = function (just) {
  return function (nothing) {
    return function (i) {
      return function (s) {
        return i >= 0 && i < s.length ? just(s.charCodeAt(i)) : nothing;
      };
    };
  };
};

exports._toChar = function (just) {
  return function (nothing) {
    return function (s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};

exports.fromCharArray = function (a) {
  return a.join("");
};

exports._indexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.indexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_indexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.indexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports._lastIndexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.lastIndexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};

exports["_lastIndexOf'"] = function (just) {
  return function (nothing) {
    return function (x) {
      return function (startAt) {
        return function (s) {
          if (startAt < 0 || startAt > s.length) return nothing;
          var i = s.lastIndexOf(x, startAt);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
};

exports.length = function (s) {
  return s.length;
};

exports._localeCompare = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (s1) {
        return function (s2) {
          var result = s1.localeCompare(s2);
          return result < 0 ? lt : result > 0 ? gt : eq;
        };
      };
    };
  };
};

exports.replace = function (s1) {
  return function (s2) {
    return function (s3) {
      return s3.replace(s1, s2);
    };
  };
};

exports.take = function (n) {
  return function (s) {
    return s.substr(0, n);
  };
};

exports.drop = function (n) {
  return function (s) {
    return s.substr(n);
  };
};

exports.count = function (p) {
  return function (s) {
    for (var i = 0; i < s.length && p(s.charAt(i)); i++); {}
    return i;
  };
};

exports.split = function (sep) {
  return function (s) {
    return s.split(sep);
  };
};

exports.toCharArray = function (s) {
  return s.split("");
};

exports.toLower = function (s) {
  return s.toLowerCase();
};

exports.toUpper = function (s) {
  return s.toUpperCase();
};

exports.trim = function (s) {
  return s.trim();
};

exports.joinWith = function (s) {
  return function (xs) {
    return xs.join(s);
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.String/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Char = require("Data.Char");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var Data_String_Unsafe = require("Data.String.Unsafe");
var uncons = function (v) {
    if (v === "") {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just({
        head: Data_String_Unsafe.charAt(0)(v), 
        tail: $foreign.drop(1)(v)
    });
};
var toChar = $foreign._toChar(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var takeWhile = function (p) {
    return function (s) {
        return $foreign.take($foreign.count(p)(s))(s);
    };
};
var $$null = function (s) {
    return $foreign.length(s) === 0;
};
var localeCompare = $foreign._localeCompare(Prelude.LT.value)(Prelude.EQ.value)(Prelude.GT.value);
var lastIndexOf$prime = $foreign["_lastIndexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var lastIndexOf = $foreign._lastIndexOf(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripSuffix = function (suffix) {
    return function (str) {
        var $2 = lastIndexOf(suffix)(str);
        if ($2 instanceof Data_Maybe.Just && $2.value0 === $foreign.length(str) - $foreign.length(suffix)) {
            return Data_Maybe.Just.create($foreign.take($2.value0)(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var indexOf$prime = $foreign["_indexOf'"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var indexOf = $foreign._indexOf(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var stripPrefix = function (prefix) {
    return function (str) {
        var $4 = indexOf(prefix)(str);
        if ($4 instanceof Data_Maybe.Just && $4.value0 === 0) {
            return Data_Maybe.Just.create($foreign.drop($foreign.length(prefix))(str));
        };
        return Data_Maybe.Nothing.value;
    };
};
var fromChar = Data_Char.toString;
var singleton = fromChar;
var dropWhile = function (p) {
    return function (s) {
        return $foreign.drop($foreign.count(p)(s))(s);
    };
};
var contains = function (x) {
    return function (s) {
        return Data_Maybe.isJust(indexOf(x)(s));
    };
};
var charCodeAt = $foreign._charCodeAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var charAt = $foreign._charAt(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
module.exports = {
    stripSuffix: stripSuffix, 
    stripPrefix: stripPrefix, 
    dropWhile: dropWhile, 
    takeWhile: takeWhile, 
    localeCompare: localeCompare, 
    singleton: singleton, 
    uncons: uncons, 
    "null": $$null, 
    "lastIndexOf'": lastIndexOf$prime, 
    lastIndexOf: lastIndexOf, 
    "indexOf'": indexOf$prime, 
    indexOf: indexOf, 
    contains: contains, 
    toChar: toChar, 
    fromChar: fromChar, 
    charCodeAt: charCodeAt, 
    charAt: charAt, 
    joinWith: $foreign.joinWith, 
    trim: $foreign.trim, 
    toUpper: $foreign.toUpper, 
    toLower: $foreign.toLower, 
    toCharArray: $foreign.toCharArray, 
    split: $foreign.split, 
    drop: $foreign.drop, 
    take: $foreign.take, 
    count: $foreign.count, 
    replace: $foreign.replace, 
    length: $foreign.length, 
    fromCharArray: $foreign.fromCharArray
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.String/foreign.js","Data.Char":"/Users/anders/dev/ps/halo/ajax/output/Data.Char/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.String.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.String.Unsafe/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Data.Traversable

// jshint maxparams: 3

exports.traverseArrayImpl = function () {
  function Cont (fn) {
    this.fn = fn;
  }

  var emptyList = {};

  var ConsCell = function (head, tail) {
    this.head = head;
    this.tail = tail;
  };

  function consList (x) {
    return function (xs) {
      return new ConsCell(x, xs);
    };
  }

  function listToArray (list) {
    var arr = [];
    while (list !== emptyList) {
      arr.push(list.head);
      list = list.tail;
    }
    return arr;
  }

  return function (apply) {
    return function (map) {
      return function (pure) {
        return function (f) {
          var buildFrom = function (x, ys) {
            return apply(map(consList)(f(x)))(ys);
          };

          var go = function (acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last = xs[currentLen - 1];
              return new Cont(function () {
                return go(buildFrom(last, acc), currentLen - 1, xs);
              });
            }
          };

          return function (array) {
            var result = go(pure(emptyList), array.length, array);
            while (result instanceof Cont) {
              result = result.fn();
            }

            return map(listToArray)(result);
          };
        };
      };
    };
  };
}();

},{}],"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Data_Foldable = require("Data.Foldable");
var Data_Maybe = require("Data.Maybe");
var Data_Maybe_First = require("Data.Maybe.First");
var Data_Maybe_Last = require("Data.Maybe.Last");
var Data_Monoid_Additive = require("Data.Monoid.Additive");
var Data_Monoid_Conj = require("Data.Monoid.Conj");
var Data_Monoid_Disj = require("Data.Monoid.Disj");
var Data_Monoid_Dual = require("Data.Monoid.Dual");
var Data_Monoid_Multiplicative = require("Data.Monoid.Multiplicative");
var StateL = function (x) {
    return x;
};
var StateR = function (x) {
    return x;
};
var Traversable = function (__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Prelude$dotFunctor_0, sequence, traverse) {
    this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.sequence = sequence;
    this.traverse = traverse;
};
var traverse = function (dict) {
    return dict.traverse;
};
var traversableMultiplicative = new Traversable(function () {
    return Data_Foldable.foldableMultiplicative;
}, function () {
    return Data_Monoid_Multiplicative.functorMultiplicative;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Multiplicative.Multiplicative)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Multiplicative.Multiplicative)(f(v));
        };
    };
});
var traversableMaybe = new Traversable(function () {
    return Data_Foldable.foldableMaybe;
}, function () {
    return Data_Maybe.functorMaybe;
}, function (dictApplicative) {
    return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Prelude.pure(dictApplicative)(Data_Maybe.Nothing.value);
        };
        if (v instanceof Data_Maybe.Just) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe.Just.create)(v.value0);
        };
        throw new Error("Failed pattern match at Data.Traversable line 76, column 1 - line 82, column 1: " + [ v.constructor.name ]);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Prelude.pure(dictApplicative)(Data_Maybe.Nothing.value);
            };
            if (v instanceof Data_Maybe.Just) {
                return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe.Just.create)(f(v.value0));
            };
            throw new Error("Failed pattern match at Data.Traversable line 76, column 1 - line 82, column 1: " + [ f.constructor.name, v.constructor.name ]);
        };
    };
});
var traversableDual = new Traversable(function () {
    return Data_Foldable.foldableDual;
}, function () {
    return Data_Monoid_Dual.functorDual;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Dual.Dual)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Dual.Dual)(f(v));
        };
    };
});
var traversableDisj = new Traversable(function () {
    return Data_Foldable.foldableDisj;
}, function () {
    return Data_Monoid_Disj.functorDisj;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Disj.Disj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Disj.Disj)(f(v));
        };
    };
});
var traversableConj = new Traversable(function () {
    return Data_Foldable.foldableConj;
}, function () {
    return Data_Monoid_Conj.functorConj;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Conj.Conj)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Conj.Conj)(f(v));
        };
    };
});
var traversableAdditive = new Traversable(function () {
    return Data_Foldable.foldableAdditive;
}, function () {
    return Data_Monoid_Additive.functorAdditive;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Additive.Additive)(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Monoid_Additive.Additive)(f(v));
        };
    };
});
var stateR = function (v) {
    return v;
};
var stateL = function (v) {
    return v;
};
var sequenceDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (tma) {
            return traverse(dictTraversable)(dictApplicative)(Prelude.id(Prelude.categoryFn))(tma);
        };
    };
};
var traversableArray = new Traversable(function () {
    return Data_Foldable.foldableArray;
}, function () {
    return Prelude.functorArray;
}, function (dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
}, function (dictApplicative) {
    return $foreign.traverseArrayImpl(Prelude.apply(dictApplicative["__superclass_Prelude.Apply_0"]()))(Prelude.map((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]()))(Prelude.pure(dictApplicative));
});
var sequence = function (dict) {
    return dict.sequence;
};
var traversableFirst = new Traversable(function () {
    return Data_Foldable.foldableFirst;
}, function () {
    return Data_Maybe_First.functorFirst;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe_First.First)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe_First.First)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traversableLast = new Traversable(function () {
    return Data_Foldable.foldableLast;
}, function () {
    return Data_Maybe_Last.functorLast;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe_Last.Last)(sequence(traversableMaybe)(dictApplicative)(v));
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Maybe_Last.Last)(traverse(traversableMaybe)(dictApplicative)(f)(v));
        };
    };
});
var traverseDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (ta) {
                return sequence(dictTraversable)(dictApplicative)(Prelude.map(dictTraversable["__superclass_Prelude.Functor_0"]())(f)(ta));
            };
        };
    };
};
var functorStateR = new Prelude.Functor(function (f) {
    return function (k) {
        return function (s) {
            var $74 = stateR(k)(s);
            return {
                accum: $74.accum, 
                value: f($74.value)
            };
        };
    };
});
var functorStateL = new Prelude.Functor(function (f) {
    return function (k) {
        return function (s) {
            var $77 = stateL(k)(s);
            return {
                accum: $77.accum, 
                value: f($77.value)
            };
        };
    };
});
var $$for = function (dictApplicative) {
    return function (dictTraversable) {
        return function (x) {
            return function (f) {
                return traverse(dictTraversable)(dictApplicative)(f)(x);
            };
        };
    };
};
var applyStateR = new Prelude.Apply(function () {
    return functorStateR;
}, function (f) {
    return function (x) {
        return function (s) {
            var $80 = stateR(x)(s);
            var $81 = stateR(f)($80.accum);
            return {
                accum: $81.accum, 
                value: $81.value($80.value)
            };
        };
    };
});
var applyStateL = new Prelude.Apply(function () {
    return functorStateL;
}, function (f) {
    return function (x) {
        return function (s) {
            var $86 = stateL(f)(s);
            var $87 = stateL(x)($86.accum);
            return {
                accum: $87.accum, 
                value: $86.value($87.value)
            };
        };
    };
});
var applicativeStateR = new Prelude.Applicative(function () {
    return applyStateR;
}, function (a) {
    return function (s) {
        return {
            accum: s, 
            value: a
        };
    };
});
var mapAccumR = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return stateR(traverse(dictTraversable)(applicativeStateR)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanr = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumR(dictTraversable)(function (b) {
                    return function (a) {
                        var bʹ = f(a)(b);
                        return {
                            accum: bʹ, 
                            value: bʹ
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var applicativeStateL = new Prelude.Applicative(function () {
    return applyStateL;
}, function (a) {
    return function (s) {
        return {
            accum: s, 
            value: a
        };
    };
});
var mapAccumL = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return stateL(traverse(dictTraversable)(applicativeStateL)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanl = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumL(dictTraversable)(function (b) {
                    return function (a) {
                        var bʹ = f(b)(a);
                        return {
                            accum: bʹ, 
                            value: bʹ
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
module.exports = {
    Traversable: Traversable, 
    mapAccumR: mapAccumR, 
    mapAccumL: mapAccumL, 
    scanr: scanr, 
    scanl: scanl, 
    "for": $$for, 
    sequenceDefault: sequenceDefault, 
    traverseDefault: traverseDefault, 
    sequence: sequence, 
    traverse: traverse, 
    traversableArray: traversableArray, 
    traversableMaybe: traversableMaybe, 
    traversableFirst: traversableFirst, 
    traversableLast: traversableLast, 
    traversableAdditive: traversableAdditive, 
    traversableDual: traversableDual, 
    traversableConj: traversableConj, 
    traversableDisj: traversableDisj, 
    traversableMultiplicative: traversableMultiplicative
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/foreign.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.First":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.First/index.js","Data.Maybe.Last":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Last/index.js","Data.Monoid.Additive":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Additive/index.js","Data.Monoid.Conj":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Conj/index.js","Data.Monoid.Disj":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Disj/index.js","Data.Monoid.Dual":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Dual/index.js","Data.Monoid.Multiplicative":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid.Multiplicative/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Biapplicative = require("Control.Biapplicative");
var Control_Biapply = require("Control.Biapply");
var Control_Comonad = require("Control.Comonad");
var Control_Extend = require("Control.Extend");
var Control_Lazy = require("Control.Lazy");
var Data_Bifoldable = require("Data.Bifoldable");
var Data_Bifunctor = require("Data.Bifunctor");
var Data_Bitraversable = require("Data.Bitraversable");
var Data_Foldable = require("Data.Foldable");
var Data_Functor_Invariant = require("Data.Functor.Invariant");
var Data_Maybe = require("Data.Maybe");
var Data_Maybe_First = require("Data.Maybe.First");
var Data_Monoid = require("Data.Monoid");
var Data_Traversable = require("Data.Traversable");
var Tuple = (function () {
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return Tuple;
})();
var uncurry = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var swap = function (v) {
    return new Tuple(v.value1, v.value0);
};
var snd = function (v) {
    return v.value1;
};
var showTuple = function (dictShow) {
    return function (dictShow1) {
        return new Prelude.Show(function (v) {
            return "Tuple (" + (Prelude.show(dictShow)(v.value0) + (") (" + (Prelude.show(dictShow1)(v.value1) + ")")));
        });
    };
};
var semiringTuple = function (dictSemiring) {
    return function (dictSemiring1) {
        return new Prelude.Semiring(function (v) {
            return function (v1) {
                return new Tuple(Prelude.add(dictSemiring)(v.value0)(v1.value0), Prelude.add(dictSemiring1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.mul(dictSemiring)(v.value0)(v1.value0), Prelude.mul(dictSemiring1)(v.value1)(v1.value1));
            };
        }, new Tuple(Prelude.one(dictSemiring), Prelude.one(dictSemiring1)), new Tuple(Prelude.zero(dictSemiring), Prelude.zero(dictSemiring1)));
    };
};
var semigroupoidTuple = new Prelude.Semigroupoid(function (v) {
    return function (v1) {
        return new Tuple(v1.value0, v.value1);
    };
});
var semigroupTuple = function (dictSemigroup) {
    return function (dictSemigroup1) {
        return new Prelude.Semigroup(function (v) {
            return function (v1) {
                return new Tuple(Prelude["<>"](dictSemigroup)(v.value0)(v1.value0), Prelude["<>"](dictSemigroup1)(v.value1)(v1.value1));
            };
        });
    };
};
var ringTuple = function (dictRing) {
    return function (dictRing1) {
        return new Prelude.Ring(function () {
            return semiringTuple(dictRing["__superclass_Prelude.Semiring_0"]())(dictRing1["__superclass_Prelude.Semiring_0"]());
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.sub(dictRing)(v.value0)(v1.value0), Prelude.sub(dictRing1)(v.value1)(v1.value1));
            };
        });
    };
};
var monoidTuple = function (dictMonoid) {
    return function (dictMonoid1) {
        return new Data_Monoid.Monoid(function () {
            return semigroupTuple(dictMonoid["__superclass_Prelude.Semigroup_0"]())(dictMonoid1["__superclass_Prelude.Semigroup_0"]());
        }, new Tuple(Data_Monoid.mempty(dictMonoid), Data_Monoid.mempty(dictMonoid1)));
    };
};
var moduloSemiringTuple = function (dictModuloSemiring) {
    return function (dictModuloSemiring1) {
        return new Prelude.ModuloSemiring(function () {
            return semiringTuple(dictModuloSemiring["__superclass_Prelude.Semiring_0"]())(dictModuloSemiring1["__superclass_Prelude.Semiring_0"]());
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.div(dictModuloSemiring)(v.value0)(v1.value0), Prelude.div(dictModuloSemiring1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.mod(dictModuloSemiring)(v.value0)(v1.value0), Prelude.mod(dictModuloSemiring1)(v.value1)(v1.value1));
            };
        });
    };
};
var lookup = function (dictFoldable) {
    return function (dictEq) {
        return function (a) {
            return function (f) {
                return Data_Maybe_First.runFirst(Data_Foldable.foldMap(dictFoldable)(Data_Maybe_First.monoidFirst)(function (v) {
                    var $145 = Prelude["=="](dictEq)(a)(v.value0);
                    if ($145) {
                        return new Data_Maybe.Just(v.value1);
                    };
                    if (!$145) {
                        return Data_Maybe.Nothing.value;
                    };
                    throw new Error("Failed pattern match at Data.Tuple line 173, column 1 - line 174, column 1: " + [ $145.constructor.name ]);
                })(f));
            };
        };
    };
};
var functorTuple = new Prelude.Functor(function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v.value1));
    };
});
var invariantTuple = new Data_Functor_Invariant.Invariant(Data_Functor_Invariant.imapF(functorTuple));
var fst = function (v) {
    return v.value0;
};
var lazyTuple = function (dictLazy) {
    return function (dictLazy1) {
        return new Control_Lazy.Lazy(function (f) {
            return new Tuple(Control_Lazy.defer(dictLazy)(function (v) {
                return fst(f(Prelude.unit));
            }), Control_Lazy.defer(dictLazy1)(function (v) {
                return snd(f(Prelude.unit));
            }));
        });
    };
};
var foldableTuple = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v.value1);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v.value1)(z);
        };
    };
});
var traversableTuple = new Data_Traversable.Traversable(function () {
    return foldableTuple;
}, function () {
    return functorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Tuple.create(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Tuple.create(v.value0))(f(v.value1));
        };
    };
});
var extendTuple = new Control_Extend.Extend(function () {
    return functorTuple;
}, function (f) {
    return function (v) {
        return new Tuple(v.value0, f(v));
    };
});
var eqTuple = function (dictEq) {
    return function (dictEq1) {
        return new Prelude.Eq(function (v) {
            return function (v1) {
                return Prelude["=="](dictEq)(v.value0)(v1.value0) && Prelude["=="](dictEq1)(v.value1)(v1.value1);
            };
        });
    };
};
var ordTuple = function (dictOrd) {
    return function (dictOrd1) {
        return new Prelude.Ord(function () {
            return eqTuple(dictOrd["__superclass_Prelude.Eq_0"]())(dictOrd1["__superclass_Prelude.Eq_0"]());
        }, function (v) {
            return function (v1) {
                var $190 = Prelude.compare(dictOrd)(v.value0)(v1.value0);
                if ($190 instanceof Prelude.EQ) {
                    return Prelude.compare(dictOrd1)(v.value1)(v1.value1);
                };
                return $190;
            };
        });
    };
};
var divisionRingTuple = function (dictDivisionRing) {
    return function (dictDivisionRing1) {
        return new Prelude.DivisionRing(function () {
            return moduloSemiringTuple(dictDivisionRing["__superclass_Prelude.ModuloSemiring_1"]())(dictDivisionRing1["__superclass_Prelude.ModuloSemiring_1"]());
        }, function () {
            return ringTuple(dictDivisionRing["__superclass_Prelude.Ring_0"]())(dictDivisionRing1["__superclass_Prelude.Ring_0"]());
        });
    };
};
var numTuple = function (dictNum) {
    return function (dictNum1) {
        return new Prelude.Num(function () {
            return divisionRingTuple(dictNum["__superclass_Prelude.DivisionRing_0"]())(dictNum1["__superclass_Prelude.DivisionRing_0"]());
        });
    };
};
var curry = function (f) {
    return function (a) {
        return function (b) {
            return f(new Tuple(a, b));
        };
    };
};
var comonadTuple = new Control_Comonad.Comonad(function () {
    return extendTuple;
}, snd);
var boundedTuple = function (dictBounded) {
    return function (dictBounded1) {
        return new Prelude.Bounded(new Tuple(Prelude.bottom(dictBounded), Prelude.bottom(dictBounded1)), new Tuple(Prelude.top(dictBounded), Prelude.top(dictBounded1)));
    };
};
var boundedOrdTuple = function (dictBoundedOrd) {
    return function (dictBoundedOrd1) {
        return new Prelude.BoundedOrd(function () {
            return boundedTuple(dictBoundedOrd["__superclass_Prelude.Bounded_0"]())(dictBoundedOrd1["__superclass_Prelude.Bounded_0"]());
        }, function () {
            return ordTuple(dictBoundedOrd["__superclass_Prelude.Ord_1"]())(dictBoundedOrd1["__superclass_Prelude.Ord_1"]());
        });
    };
};
var booleanAlgebraTuple = function (dictBooleanAlgebra) {
    return function (dictBooleanAlgebra1) {
        return new Prelude.BooleanAlgebra(function () {
            return boundedTuple(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]())(dictBooleanAlgebra1["__superclass_Prelude.Bounded_0"]());
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.conj(dictBooleanAlgebra)(v.value0)(v1.value0), Prelude.conj(dictBooleanAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return function (v1) {
                return new Tuple(Prelude.disj(dictBooleanAlgebra)(v.value0)(v1.value0), Prelude.disj(dictBooleanAlgebra1)(v.value1)(v1.value1));
            };
        }, function (v) {
            return new Tuple(Prelude.not(dictBooleanAlgebra)(v.value0), Prelude.not(dictBooleanAlgebra1)(v.value1));
        });
    };
};
var bifunctorTuple = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        return function (v) {
            return new Tuple(f(v.value0), g(v.value1));
        };
    };
});
var bifoldableTuple = new Data_Bifoldable.Bifoldable(function (dictMonoid) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Prelude["<>"](dictMonoid["__superclass_Prelude.Semigroup_0"]())(f(v.value0))(g(v.value1));
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return g(f(z)(v.value0))(v.value1);
            };
        };
    };
}, function (f) {
    return function (g) {
        return function (z) {
            return function (v) {
                return f(v.value0)(g(v.value1)(z));
            };
        };
    };
});
var bitraversableTuple = new Data_Bitraversable.Bitraversable(function () {
    return bifoldableTuple;
}, function () {
    return bifunctorTuple;
}, function (dictApplicative) {
    return function (v) {
        return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Tuple.create)(v.value0))(v.value1);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (v) {
                return Prelude["<*>"](dictApplicative["__superclass_Prelude.Apply_0"]())(Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Tuple.create)(f(v.value0)))(g(v.value1));
            };
        };
    };
});
var biapplyTuple = new Control_Biapply.Biapply(function () {
    return bifunctorTuple;
}, function (v) {
    return function (v1) {
        return new Tuple(v.value0(v1.value0), v.value1(v1.value1));
    };
});
var biapplicativeTuple = new Control_Biapplicative.Biapplicative(function () {
    return biapplyTuple;
}, Tuple.create);
var applyTuple = function (dictSemigroup) {
    return new Prelude.Apply(function () {
        return functorTuple;
    }, function (v) {
        return function (v1) {
            return new Tuple(Prelude["<>"](dictSemigroup)(v.value0)(v1.value0), v.value1(v1.value1));
        };
    });
};
var bindTuple = function (dictSemigroup) {
    return new Prelude.Bind(function () {
        return applyTuple(dictSemigroup);
    }, function (v) {
        return function (f) {
            var $254 = f(v.value1);
            return new Tuple(Prelude["<>"](dictSemigroup)(v.value0)($254.value0), $254.value1);
        };
    });
};
var applicativeTuple = function (dictMonoid) {
    return new Prelude.Applicative(function () {
        return applyTuple(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    }, Tuple.create(Data_Monoid.mempty(dictMonoid)));
};
var monadTuple = function (dictMonoid) {
    return new Prelude.Monad(function () {
        return applicativeTuple(dictMonoid);
    }, function () {
        return bindTuple(dictMonoid["__superclass_Prelude.Semigroup_0"]());
    });
};
module.exports = {
    Tuple: Tuple, 
    lookup: lookup, 
    swap: swap, 
    uncurry: uncurry, 
    curry: curry, 
    snd: snd, 
    fst: fst, 
    showTuple: showTuple, 
    eqTuple: eqTuple, 
    ordTuple: ordTuple, 
    boundedTuple: boundedTuple, 
    boundedOrdTuple: boundedOrdTuple, 
    semigroupoidTuple: semigroupoidTuple, 
    semigroupTuple: semigroupTuple, 
    monoidTuple: monoidTuple, 
    semiringTuple: semiringTuple, 
    moduloSemiringTuple: moduloSemiringTuple, 
    ringTuple: ringTuple, 
    divisionRingTuple: divisionRingTuple, 
    numTuple: numTuple, 
    booleanAlgebraTuple: booleanAlgebraTuple, 
    functorTuple: functorTuple, 
    invariantTuple: invariantTuple, 
    bifunctorTuple: bifunctorTuple, 
    applyTuple: applyTuple, 
    biapplyTuple: biapplyTuple, 
    applicativeTuple: applicativeTuple, 
    biapplicativeTuple: biapplicativeTuple, 
    bindTuple: bindTuple, 
    monadTuple: monadTuple, 
    extendTuple: extendTuple, 
    comonadTuple: comonadTuple, 
    lazyTuple: lazyTuple, 
    foldableTuple: foldableTuple, 
    bifoldableTuple: bifoldableTuple, 
    traversableTuple: traversableTuple, 
    bitraversableTuple: bitraversableTuple
};

},{"Control.Biapplicative":"/Users/anders/dev/ps/halo/ajax/output/Control.Biapplicative/index.js","Control.Biapply":"/Users/anders/dev/ps/halo/ajax/output/Control.Biapply/index.js","Control.Comonad":"/Users/anders/dev/ps/halo/ajax/output/Control.Comonad/index.js","Control.Extend":"/Users/anders/dev/ps/halo/ajax/output/Control.Extend/index.js","Control.Lazy":"/Users/anders/dev/ps/halo/ajax/output/Control.Lazy/index.js","Data.Bifoldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifoldable/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Data.Bitraversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Bitraversable/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Functor.Invariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Invariant/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.First":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.First/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Unfoldable/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_Tuple = require("Data.Tuple");
var Data_Array_ST = require("Data.Array.ST");
var Data_Traversable = require("Data.Traversable");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_ST = require("Control.Monad.ST");
var Unfoldable = function (unfoldr) {
    this.unfoldr = unfoldr;
};
var unfoldr = function (dict) {
    return dict.unfoldr;
};
var unfoldableArray = new Unfoldable(function (f) {
    return function (b) {
        return Control_Monad_Eff.runPure(Data_Array_ST.runSTArray(function __do() {
            var v = Data_Array_ST.emptySTArray();
            var v1 = Control_Monad_ST.newSTRef(b)();
            (function () {
                while (!(function __do() {
                    var v2 = Control_Monad_ST.readSTRef(v1)();
                    return (function () {
                        var $12 = f(v2);
                        if ($12 instanceof Data_Maybe.Nothing) {
                            return Prelude["return"](Control_Monad_Eff.applicativeEff)(true);
                        };
                        if ($12 instanceof Data_Maybe.Just) {
                            return function __do() {
                                Data_Array_ST.pushSTArray(v)($12.value0.value0)();
                                Control_Monad_ST.writeSTRef(v1)($12.value0.value1)();
                                return Prelude["return"](Control_Monad_Eff.applicativeEff)(false)();
                            };
                        };
                        throw new Error("Failed pattern match at Data.Unfoldable line 29, column 1 - line 49, column 1: " + [ $12.constructor.name ]);
                    })()();
                })()) {

                };
                return {};
            })();
            return Prelude["return"](Control_Monad_Eff.applicativeEff)(v)();
        }));
    };
});
var replicate = function (dictUnfoldable) {
    return function (n) {
        return function (v) {
            var step = function (i) {
                var $16 = i <= 0;
                if ($16) {
                    return Data_Maybe.Nothing.value;
                };
                if (!$16) {
                    return new Data_Maybe.Just(new Data_Tuple.Tuple(v, i - 1));
                };
                throw new Error("Failed pattern match at Data.Unfoldable line 52, column 5 - line 53, column 5: " + [ $16.constructor.name ]);
            };
            return unfoldr(dictUnfoldable)(step)(n);
        };
    };
};
var replicateA = function (dictApplicative) {
    return function (dictUnfoldable) {
        return function (dictTraversable) {
            return function (n) {
                return function (m) {
                    return Data_Traversable.sequence(dictTraversable)(dictApplicative)(replicate(dictUnfoldable)(n)(m));
                };
            };
        };
    };
};
var singleton = function (dictUnfoldable) {
    return replicate(dictUnfoldable)(1);
};
var none = function (dictUnfoldable) {
    return unfoldr(dictUnfoldable)(Prelude["const"](Data_Maybe.Nothing.value))(Prelude.unit);
};
module.exports = {
    Unfoldable: Unfoldable, 
    singleton: singleton, 
    none: none, 
    replicateA: replicateA, 
    replicate: replicate, 
    unfoldr: unfoldr, 
    unfoldableArray: unfoldableArray
};

},{"Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.ST":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.ST/index.js","Data.Array.ST":"/Users/anders/dev/ps/halo/ajax/output/Data.Array.ST/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Data.Void/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Functor_Contravariant = require("Data.Functor.Contravariant");
var Void = function (x) {
    return x;
};
var showVoid = new Prelude.Show(function (v) {
    return "Void";
});
var eqVoid = new Prelude.Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var absurd = function (a) {
    var spin = function (__copy_v) {
        var v = __copy_v;
        tco: while (true) {
            var __tco_v = v;
            v = __tco_v;
            continue tco;
        };
    };
    return spin(a);
};
var coerce = function (dictContravariant) {
    return function (dictFunctor) {
        return function (a) {
            return Prelude["<$>"](dictFunctor)(absurd)(Data_Functor_Contravariant[">$<"](dictContravariant)(absurd)(a));
        };
    };
};
module.exports = {
    Void: Void, 
    absurd: absurd, 
    coerce: coerce, 
    eqVoid: eqVoid, 
    showVoid: showVoid
};

},{"Data.Functor.Contravariant":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Contravariant/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Component.ChildPath/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Functor_Coproduct = require("Data.Functor.Coproduct");
var Data_Injector = require("Data.Injector");
var Data_Maybe = require("Data.Maybe");
var Data_NaturalTransformation = require("Data.NaturalTransformation");
var ChildPath = (function () {
    function ChildPath(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    ChildPath.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new ChildPath(value0, value1, value2);
            };
        };
    };
    return ChildPath;
})();
var prjState = function (v) {
    return Data_Injector.prj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value0(dictChoice)(dictApplicative);
        };
    });
};
var prjSlot = function (v) {
    return Data_Injector.prj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value2(dictChoice)(dictApplicative);
        };
    });
};
var prjQuery = function (v) {
    return Data_Injector.prj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value1(dictChoice)(dictApplicative);
        };
    });
};
var injState = function (v) {
    return Data_Injector.inj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value0(dictChoice)(dictApplicative);
        };
    });
};
var injSlot = function (v) {
    return Data_Injector.inj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value2(dictChoice)(dictApplicative);
        };
    });
};
var injQuery = function (v) {
    return Data_Injector.inj(function (dictChoice) {
        return function (dictApplicative) {
            return v.value1(dictChoice)(dictApplicative);
        };
    });
};
var cpR = new ChildPath(function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injRE(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injRC(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injRE(dictChoice)(dictApplicative);
    };
});
var cpL = new ChildPath(function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injLE(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injLC(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injLE(dictChoice)(dictApplicative);
    };
});
var cpI = new ChildPath(function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injI(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injI(dictChoice)(dictApplicative);
    };
}, function (dictChoice) {
    return function (dictApplicative) {
        return Data_Injector.injI(dictChoice)(dictApplicative);
    };
});
var compose = function (v) {
    return function (v1) {
        return new ChildPath(function (dictChoice) {
            return function (dictApplicative) {
                return function ($76) {
                    return v.value0(dictChoice)(dictApplicative)(v1.value0(dictChoice)(dictApplicative)($76));
                };
            };
        }, function (dictChoice) {
            return function (dictApplicative) {
                return function ($77) {
                    return v.value1(dictChoice)(dictApplicative)(v1.value1(dictChoice)(dictApplicative)($77));
                };
            };
        }, function (dictChoice) {
            return function (dictApplicative) {
                return function ($78) {
                    return v.value2(dictChoice)(dictApplicative)(v1.value2(dictChoice)(dictApplicative)($78));
                };
            };
        });
    };
};
var $colon$greater = compose;
module.exports = {
    ChildPath: ChildPath, 
    prjSlot: prjSlot, 
    injSlot: injSlot, 
    prjQuery: prjQuery, 
    injQuery: injQuery, 
    prjState: prjState, 
    injState: injState, 
    cpI: cpI, 
    cpR: cpR, 
    cpL: cpL, 
    ":>": $colon$greater, 
    compose: compose
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Functor.Coproduct":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Coproduct/index.js","Data.Injector":"/Users/anders/dev/ps/halo/ajax/output/Data.Injector/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.NaturalTransformation":"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Component/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Apply = require("Control.Apply");
var Control_Bind = require("Control.Bind");
var Control_Coroutine = require("Control.Coroutine");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Free = require("Control.Monad.Free");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_State = require("Control.Monad.State");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Plus = require("Control.Plus");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Control_Monad_State_Trans = require("Control.Monad.State.Trans");
var Data_Bifunctor = require("Data.Bifunctor");
var Data_Functor_Coproduct = require("Data.Functor.Coproduct");
var Data_Maybe = require("Data.Maybe");
var Data_NaturalTransformation = require("Data.NaturalTransformation");
var Data_Profunctor_Choice = require("Data.Profunctor.Choice");
var Data_Traversable = require("Data.Traversable");
var Data_Tuple = require("Data.Tuple");
var Data_Void = require("Data.Void");
var Data_Map = require("Data.Map");
var Data_Maybe_Unsafe = require("Data.Maybe.Unsafe");
var Halogen_Component_ChildPath = require("Halogen.Component.ChildPath");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var Halogen_Query = require("Halogen.Query");
var Halogen_Query_StateF = require("Halogen.Query.StateF");
var Halogen_Query_SubscribeF = require("Halogen.Query.SubscribeF");
var Data_Identity = require("Data.Identity");
var Control_Monad_Free_Trans = require("Control.Monad.Free.Trans");
var Component = function (x) {
    return x;
};
var SlotConstructor = (function () {
    function SlotConstructor(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SlotConstructor.create = function (value0) {
        return function (value1) {
            return new SlotConstructor(value0, value1);
        };
    };
    return SlotConstructor;
})();
var ChildF = (function () {
    function ChildF(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ChildF.create = function (value0) {
        return function (value1) {
            return new ChildF(value0, value1);
        };
    };
    return ChildF;
})();
var InstalledState = function (x) {
    return x;
};
var transform = function (dictFunctor) {
    return function (reviewS) {
        return function (previewS) {
            return function (reviewQ) {
                return function (previewQ) {
                    return function (v) {
                        var renderʹ = function (st) {
                            return function (v1) {
                                return Prelude["<$>"](Data_Identity.functorIdentity)(Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(Prelude.map(Halogen_HTML_Core.functorHTML)(reviewQ))(reviewS))(Control_Monad_State_Trans.runStateT(v.render)(st));
                            };
                        };
                        var modifyState = function (f) {
                            return function (sʹ) {
                                return Data_Maybe.maybe(sʹ)(function ($103) {
                                    return reviewS(f($103));
                                })(previewS(sʹ));
                            };
                        };
                        var go = function (v1) {
                            if (v1 instanceof Halogen_Query.StateHF && v1.value0 instanceof Halogen_Query_StateF.Get) {
                                return Control_Bind["=<<"](Control_Monad_Free.freeBind)(function ($104) {
                                    return Control_Monad_Free.liftF(Data_Maybe.maybe(Halogen_Query.HaltHF.value)(function (stʹ) {
                                        return new Halogen_Query.StateHF(new Halogen_Query_StateF.Get(function ($105) {
                                            return v1.value0.value0(Prelude["const"](stʹ)($105));
                                        }));
                                    })(previewS($104)));
                                })(Halogen_Query.get);
                            };
                            if (v1 instanceof Halogen_Query.StateHF && v1.value0 instanceof Halogen_Query_StateF.Modify) {
                                return Control_Monad_Free.liftF(new Halogen_Query.StateHF(new Halogen_Query_StateF.Modify(modifyState(v1.value0.value0), v1.value0.value1)));
                            };
                            if (v1 instanceof Halogen_Query.SubscribeHF) {
                                return Control_Monad_Free.liftF(new Halogen_Query.SubscribeHF(Halogen_Query_SubscribeF.transformSubscribe(dictFunctor)(dictFunctor)(reviewQ)(Prelude.id(Prelude.categoryFn))(v1.value0)));
                            };
                            if (v1 instanceof Halogen_Query.QueryHF) {
                                return Control_Monad_Free.liftF(new Halogen_Query.QueryHF(v1.value0));
                            };
                            if (v1 instanceof Halogen_Query.HaltHF) {
                                return Control_Monad_Free.liftF(Halogen_Query.HaltHF.value);
                            };
                            throw new Error("Failed pattern match at Halogen.Component line 362, column 1 - line 371, column 1: " + [ v1.constructor.name ]);
                        };
                        return {
                            render: Control_Bind["=<<"](Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(function ($106) {
                                return Data_Maybe.maybe(Prelude.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(new Halogen_HTML_Core.Text("")))(renderʹ)(previewS($106));
                            })(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))), 
                            "eval": function ($107) {
                                return Data_Maybe.maybe(Control_Monad_Free.liftF(Halogen_Query.HaltHF.value))(function ($108) {
                                    return Control_Monad_Free.foldFree(Control_Monad_Free.freeMonadRec)(go)(v["eval"]($108));
                                })(previewQ($107));
                            }
                        };
                    };
                };
            };
        };
    };
};
var transformChild = function (dictFunctor) {
    return function (i) {
        return transform(dictFunctor)(Halogen_Component_ChildPath.injState(i))(Halogen_Component_ChildPath.prjState(i))(Halogen_Component_ChildPath.injQuery(i))(Halogen_Component_ChildPath.prjQuery(i));
    };
};
var renderComponent = function (v) {
    return Control_Monad_State.runState(v.render);
};
var render = function (dictOrd) {
    return function (rc) {
        var renderChildʹ = function (p) {
            return function (c) {
                return function (s) {
                    var $60 = renderComponent(c)(s);
                    return Prelude.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Control_Monad_State_Class.modify(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (v) {
                        return {
                            parent: v.parent, 
                            children: Data_Map.insert(dictOrd)(p)(new Data_Tuple.Tuple(c, $60.value1))(v.children), 
                            memo: v.memo
                        };
                    }))(function () {
                        return Prelude.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(Prelude["<$>"](Halogen_HTML_Core.functorHTML)(function ($109) {
                            return Data_Functor_Coproduct.right(ChildF.create(p)($109));
                        })($60.value0));
                    });
                };
            };
        };
        var renderChild = function (v) {
            return function (v1) {
                var childState = Data_Map.lookup(dictOrd)(v1.value0)(v.children);
                var $66 = Data_Map.lookup(dictOrd)(v1.value0)(v.memo);
                if ($66 instanceof Data_Maybe.Just) {
                    return Prelude.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Control_Monad_State_Class.modify(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (v2) {
                        return {
                            parent: v2.parent, 
                            children: Data_Map.alter(dictOrd)(Prelude["const"](childState))(v1.value0)(v2.children), 
                            memo: Data_Map.insert(dictOrd)(v1.value0)($66.value0)(v2.memo)
                        };
                    }))(function () {
                        return Prelude.pure(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))($66.value0);
                    });
                };
                if ($66 instanceof Data_Maybe.Nothing) {
                    if (childState instanceof Data_Maybe.Just) {
                        return renderChildʹ(v1.value0)(childState.value0.value0)(childState.value0.value1);
                    };
                    if (childState instanceof Data_Maybe.Nothing) {
                        var defʹ = v1.value1(Prelude.unit);
                        return renderChildʹ(v1.value0)(defʹ.component)(defʹ.initialState);
                    };
                    throw new Error("Failed pattern match at Halogen.Component line 255, column 1 - line 260, column 1: " + [ childState.constructor.name ]);
                };
                throw new Error("Failed pattern match at Halogen.Component line 255, column 1 - line 260, column 1: " + [ $66.constructor.name ]);
            };
        };
        return Prelude.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity)))(function (v) {
            var html = rc(v.parent);
            return Prelude.bind(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))({
                parent: v.parent, 
                children: Data_Map.empty, 
                memo: Data_Map.empty
            }))(function () {
                return Halogen_HTML_Core.fillSlot(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(renderChild(v))(Data_Functor_Coproduct.left)(html);
            });
        });
    };
};
var queryComponent = function (v) {
    return v["eval"];
};
var mapStateFParent = Halogen_Query_StateF.mapState(function (v) {
    return v.parent;
})(function (f) {
    return function (v) {
        return {
            parent: f(v.parent), 
            children: v.children, 
            memo: v.memo
        };
    };
});
var mergeParentStateF = function ($110) {
    return Control_Monad_Free.liftF(Halogen_Query.StateHF.create(mapStateFParent($110)));
};
var mapStateFChild = function (dictOrd) {
    return function (p) {
        return Halogen_Query_StateF.mapState(function (v) {
            return Data_Maybe_Unsafe.fromJust(Prelude["<$>"](Data_Maybe.functorMaybe)(Data_Tuple.snd)(Data_Map.lookup(dictOrd)(p)(v.children)));
        })(function (f) {
            return function (v) {
                return {
                    parent: v.parent, 
                    children: Data_Map.update(dictOrd)(function ($111) {
                        return Data_Maybe.Just.create(Data_Bifunctor.rmap(Data_Tuple.bifunctorTuple)(f)($111));
                    })(p)(v.children), 
                    memo: v.memo
                };
            };
        });
    };
};
var mkQuery = function (dictFunctor) {
    return function (dictOrd) {
        return function (p) {
            return function (q) {
                return Prelude.bind(Control_Monad_Free.freeBind)(Halogen_Query.get)(function (v) {
                    return Data_Traversable["for"](Control_Monad_Free.freeApplicative)(Data_Traversable.traversableMaybe)(Data_Map.lookup(dictOrd)(p)(v.children))(function (v1) {
                        return Control_Monad_Free.mapF(Halogen_Query.transformHF(dictFunctor)(dictFunctor)(mapStateFChild(dictOrd)(p))(ChildF.create(p))(Prelude.id(Prelude.categoryFn)))(queryComponent(v1.value0)(q));
                    });
                });
            };
        };
    };
};
var mkQuery$prime = function (dictFunctor) {
    return function (dictOrd) {
        return function (i) {
            return function (p) {
                return function (q) {
                    return mkQuery(dictFunctor)(dictOrd)(Halogen_Component_ChildPath.injSlot(i)(p))(Halogen_Component_ChildPath.injQuery(i)(q));
                };
            };
        };
    };
};
var queryChild = function (dictFunctor) {
    return function (dictOrd) {
        return function (v) {
            return Prelude.bind(Control_Monad_Free.freeBind)(Halogen_Query.modify(function (v1) {
                return {
                    parent: v1.parent, 
                    children: v1.children, 
                    memo: Data_Map["delete"](dictOrd)(v.value0)(v1.memo)
                };
            }))(function () {
                return Prelude[">>="](Control_Monad_Free.freeBind)(Control_Monad_Free.mapF(Halogen_Query.transformHF(dictFunctor)(dictFunctor)(Prelude.id(Prelude.categoryFn))(Data_Functor_Coproduct.right)(Prelude.id(Prelude.categoryFn)))(mkQuery(dictFunctor)(dictOrd)(v.value0)(v.value1)))(Data_Maybe.maybe(Control_Monad_Free.liftF(Control_Plus.empty(Halogen_Query.plusHalogenF(dictFunctor))))(Prelude.pure(Control_Monad_Free.freeApplicative)));
            });
        };
    };
};
var liftQuery = function (dictFunctor) {
    return Halogen_Query.liftH;
};
var query = function (dictFunctor) {
    return function (dictOrd) {
        return function (p) {
            return function (q) {
                return liftQuery(dictFunctor)(mkQuery(dictFunctor)(dictOrd)(p)(q));
            };
        };
    };
};
var query$prime = function (dictFunctor) {
    return function (dictOrd) {
        return function (i) {
            return function (p) {
                return function (q) {
                    return liftQuery(dictFunctor)(mkQuery$prime(dictFunctor)(dictOrd)(i)(p)(q));
                };
            };
        };
    };
};
var liftChildF = function (dictFunctor) {
    return Control_Monad_Free.mapF(Halogen_Query.transformHF(dictFunctor)(dictFunctor)(Prelude.id(Prelude.categoryFn))(Data_Functor_Coproduct.right)(Prelude.id(Prelude.categoryFn)));
};
var runSubscribeF = function (dictFunctor) {
    return function (queryParentʹ) {
        return function ($112) {
            return Halogen_Query_SubscribeF.subscribeN(Control_Monad_Free.freeMonadRec)(Control_Monad_Rec_Class.forever(Control_Monad_Free_Trans.monadRecFreeT(Control_Coroutine.functorAwait)(Control_Monad_Free.freeMonad))(Control_Bind["=<<"](Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Control_Monad_Free.freeMonad))(function ($113) {
                return Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Control_Monad_Free.freeMonad)(queryParentʹ($113));
            })(Control_Coroutine.await(Control_Monad_Free.freeMonad))))(Halogen_Query_SubscribeF.hoistSubscribe(Control_Monad_Free.freeFunctor)(liftChildF(dictFunctor))($112));
        };
    };
};
var queryParent = function (dictFunctor) {
    return function (f) {
        return Prelude[">>>"](Prelude.semigroupoidFn)(f)(Control_Monad_Free.foldFree(Control_Monad_Free.freeMonadRec)(function (h) {
            if (h instanceof Halogen_Query.StateHF) {
                return mergeParentStateF(h.value0);
            };
            if (h instanceof Halogen_Query.SubscribeHF) {
                return runSubscribeF(dictFunctor)(queryParent(dictFunctor)(f))(h.value0);
            };
            if (h instanceof Halogen_Query.QueryHF) {
                return liftChildF(dictFunctor)(h.value0);
            };
            if (h instanceof Halogen_Query.HaltHF) {
                return Control_Monad_Free.liftF(Control_Plus.empty(Halogen_Query.plusHalogenF(dictFunctor)));
            };
            throw new Error("Failed pattern match: " + [ h.constructor.name ]);
        }));
    };
};
var parentComponent = function (dictFunctor) {
    return function (dictOrd) {
        return function (r) {
            return function (e) {
                var $$eval = Data_Functor_Coproduct.coproduct(queryParent(dictFunctor)(e))(queryChild(dictFunctor)(dictOrd));
                return {
                    render: render(dictOrd)(r), 
                    "eval": $$eval
                };
            };
        };
    };
};
var parentComponent$prime = function (dictFunctor) {
    return function (dictOrd) {
        return function (r) {
            return function (e) {
                return function (p) {
                    var peek = Prelude[">>>"](Prelude.semigroupoidFn)(p)(Control_Monad_Free.foldFree(Control_Monad_Free.freeMonadRec)(function (h) {
                        if (h instanceof Halogen_Query.StateHF) {
                            return mergeParentStateF(h.value0);
                        };
                        if (h instanceof Halogen_Query.SubscribeHF) {
                            return runSubscribeF(dictFunctor)(queryParent(dictFunctor)(e))(h.value0);
                        };
                        if (h instanceof Halogen_Query.QueryHF) {
                            return liftChildF(dictFunctor)(h.value0);
                        };
                        if (h instanceof Halogen_Query.HaltHF) {
                            return Control_Monad_Free.liftF(Control_Plus.empty(Halogen_Query.plusHalogenF(dictFunctor)));
                        };
                        throw new Error("Failed pattern match at Halogen.Component line 140, column 3 - line 141, column 3: " + [ h.constructor.name ]);
                    }));
                    var $$eval = Data_Functor_Coproduct.coproduct(queryParent(dictFunctor)(e))(function (q) {
                        return Control_Apply["<*"](Control_Monad_Free.freeApply)(queryChild(dictFunctor)(dictOrd)(q))(peek(q));
                    });
                    return {
                        render: render(dictOrd)(r), 
                        "eval": $$eval
                    };
                };
            };
        };
    };
};
var interpret = function (dictFunctor) {
    return function (nat) {
        return function (v) {
            return {
                render: v.render, 
                "eval": function ($114) {
                    return Control_Monad_Free.mapF(Halogen_Query.hoistHalogenF(dictFunctor)(nat))(v["eval"]($114));
                }
            };
        };
    };
};
var installedState = function (dictOrd) {
    return function (st) {
        return {
            parent: st, 
            children: Data_Map.empty, 
            memo: Data_Map.empty
        };
    };
};
var functorChildF = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return function (v) {
            return new ChildF(v.value0, Prelude["<$>"](dictFunctor)(f)(v.value1));
        };
    });
};
var component = function (r) {
    return function (e) {
        return {
            render: Control_Monad_State_Class.gets(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(r), 
            "eval": e
        };
    };
};
module.exports = {
    ChildF: ChildF, 
    SlotConstructor: SlotConstructor, 
    queryComponent: queryComponent, 
    renderComponent: renderComponent, 
    interpret: interpret, 
    transformChild: transformChild, 
    transform: transform, 
    "query'": query$prime, 
    query: query, 
    liftQuery: liftQuery, 
    "mkQuery'": mkQuery$prime, 
    mkQuery: mkQuery, 
    installedState: installedState, 
    "parentComponent'": parentComponent$prime, 
    parentComponent: parentComponent, 
    component: component, 
    functorChildF: functorChildF
};

},{"Control.Apply":"/Users/anders/dev/ps/halo/ajax/output/Control.Apply/index.js","Control.Bind":"/Users/anders/dev/ps/halo/ajax/output/Control.Bind/index.js","Control.Coroutine":"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Free":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free/index.js","Control.Monad.Free.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free.Trans/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.State":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Control.Monad.State.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Trans/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Data.Functor.Coproduct":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Coproduct/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Map":"/Users/anders/dev/ps/halo/ajax/output/Data.Map/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Maybe.Unsafe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe.Unsafe/index.js","Data.NaturalTransformation":"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js","Data.Profunctor.Choice":"/Users/anders/dev/ps/halo/ajax/output/Data.Profunctor.Choice/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Data.Void":"/Users/anders/dev/ps/halo/ajax/output/Data.Void/index.js","Halogen.Component.ChildPath":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Component.ChildPath/index.js","Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.Query":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query/index.js","Halogen.Query.StateF":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query.StateF/index.js","Halogen.Query.SubscribeF":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query.SubscribeF/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Driver/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Bind = require("Control.Bind");
var Control_Coroutine = require("Control.Coroutine");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("Control.Monad.Aff.AVar");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Free = require("Control.Monad.Free");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Monad_State = require("Control.Monad.State");
var Control_Monad_Trans = require("Control.Monad.Trans");
var Control_Plus = require("Control.Plus");
var Data_NaturalTransformation = require("Data.NaturalTransformation");
var Data_Tuple = require("Data.Tuple");
var DOM_HTML_Types = require("DOM.HTML.Types");
var Halogen_Component = require("Halogen.Component");
var Halogen_Effects = require("Halogen.Effects");
var Halogen_HTML_Renderer_VirtualDOM = require("Halogen.HTML.Renderer.VirtualDOM");
var Halogen_Internal_VirtualDOM = require("Halogen.Internal.VirtualDOM");
var Halogen_Query = require("Halogen.Query");
var Halogen_Query_StateF = require("Halogen.Query.StateF");
var Halogen_Query_SubscribeF = require("Halogen.Query.SubscribeF");
var Control_Monad_Free_Trans = require("Control.Monad.Free.Trans");
var Control_Monad_State_Trans = require("Control.Monad.State.Trans");
var Data_Identity = require("Data.Identity");
var runUI = function (c) {
    return function (s) {
        var render = function (ref) {
            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(ref))(function (v) {
                var $7 = !v.renderPending;
                if ($7) {
                    return Control_Monad_Aff_AVar.putVar(ref)(v);
                };
                if (!$7) {
                    var $8 = Halogen_Component.renderComponent(c)(v.state);
                    var vtreeʹ = Halogen_HTML_Renderer_VirtualDOM.renderHTML(driver(ref))($8.value0);
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Halogen_Internal_VirtualDOM.patch(Halogen_Internal_VirtualDOM.diff(v.vtree)(vtreeʹ))(v.node)))(function (v1) {
                        return Control_Monad_Aff_AVar.putVar(ref)({
                            node: v1, 
                            vtree: vtreeʹ, 
                            state: $8.value1, 
                            renderPending: false
                        });
                    });
                };
                throw new Error("Failed pattern match at Halogen.Driver line 54, column 1 - line 59, column 1: " + [ $7.constructor.name ]);
            });
        };
        var $$eval = function (ref) {
            return function (h) {
                var runSubscribe = function (v) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Halogen_Query_SubscribeF.subscribeN(Control_Monad_Aff.monadRecAff)(Control_Monad_Rec_Class.forever(Control_Monad_Free_Trans.monadRecFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(Control_Bind["=<<"](Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(function ($28) {
                        return Control_Monad_Trans.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Control_Monad_Aff.monadAff)(driver(ref)($28));
                    })(Control_Coroutine.await(Control_Monad_Aff.monadAff))))(v)))(function () {
                        return Prelude.pure(Control_Monad_Aff.applicativeAff)(v.value1);
                    });
                };
                var runAff = function (g) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(render(ref))(function () {
                        return g;
                    });
                };
                var evalState = function (i) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(ref))(function (v) {
                        var $16 = Control_Monad_State.runState(Halogen_Query_StateF.stateN(Control_Monad_State_Trans.monadStateT(Data_Identity.monadIdentity))(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(i))(v.state);
                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(ref)({
                            node: v.node, 
                            vtree: v.vtree, 
                            state: $16.value1, 
                            renderPending: true
                        }))(function () {
                            return Prelude.pure(Control_Monad_Aff.applicativeAff)($16.value0);
                        });
                    });
                };
                if (h instanceof Halogen_Query.StateHF) {
                    return evalState(h.value0);
                };
                if (h instanceof Halogen_Query.SubscribeHF) {
                    return runSubscribe(h.value0);
                };
                if (h instanceof Halogen_Query.QueryHF) {
                    return runAff(h.value0);
                };
                if (h instanceof Halogen_Query.HaltHF) {
                    return Control_Plus.empty(Control_Monad_Aff.plusAff);
                };
                throw new Error("Failed pattern match at Halogen.Driver line 54, column 1 - line 59, column 1: " + [ h.constructor.name ]);
            };
        };
        var driver = function (ref) {
            return function (q) {
                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Free.runFreeM(Halogen_Query.functorHalogenF(Control_Monad_Aff.functorAff))(Control_Monad_Aff.monadRecAff)($$eval(ref))(Halogen_Component.queryComponent(c)(q)))(function (v) {
                    return Prelude.bind(Control_Monad_Aff.bindAff)(render(ref))(function () {
                        return Prelude.pure(Control_Monad_Aff.applicativeAff)(v);
                    });
                });
            };
        };
        var $24 = Halogen_Component.renderComponent(c)(s);
        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.makeVar)(function (v) {
            var vtree = Halogen_HTML_Renderer_VirtualDOM.renderHTML(driver(v))($24.value0);
            var node = Halogen_Internal_VirtualDOM.createElement(vtree);
            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(v)({
                node: node, 
                vtree: vtree, 
                state: $24.value1, 
                renderPending: false
            }))(function () {
                return Prelude.pure(Control_Monad_Aff.applicativeAff)({
                    node: node, 
                    driver: driver(v)
                });
            });
        });
    };
};
module.exports = {
    runUI: runUI
};

},{"Control.Bind":"/Users/anders/dev/ps/halo/ajax/output/Control.Bind/index.js","Control.Coroutine":"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine/index.js","Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Aff.AVar":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Free":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free/index.js","Control.Monad.Free.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free.Trans/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Monad.State":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State/index.js","Control.Monad.State.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Trans/index.js","Control.Monad.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Trans/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.NaturalTransformation":"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Halogen.Component":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Component/index.js","Halogen.Effects":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Effects/index.js","Halogen.HTML.Renderer.VirtualDOM":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Renderer.VirtualDOM/index.js","Halogen.Internal.VirtualDOM":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Internal.VirtualDOM/index.js","Halogen.Query":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query/index.js","Halogen.Query.StateF":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query.StateF/index.js","Halogen.Query.SubscribeF":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query.SubscribeF/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Effects/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Control_Monad_Aff_AVar = require("Control.Monad.Aff.AVar");
var Control_Monad_Eff_Exception = require("Control.Monad.Eff.Exception");
var DOM = require("DOM");
module.exports = {};

},{"Control.Monad.Aff.AVar":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/index.js","Control.Monad.Eff.Exception":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Bifunctor = require("Data.Bifunctor");
var Data_Exists = require("Data.Exists");
var Data_ExistsR = require("Data.ExistsR");
var Data_Maybe = require("Data.Maybe");
var Data_Traversable = require("Data.Traversable");
var Data_Tuple = require("Data.Tuple");
var DOM_HTML_Types = require("DOM.HTML.Types");
var Halogen_HTML_Events_Handler = require("Halogen.HTML.Events.Handler");
var Halogen_HTML_Events_Types = require("Halogen.HTML.Events.Types");
var TagName = function (x) {
    return x;
};
var PropName = function (x) {
    return x;
};
var Namespace = function (x) {
    return x;
};
var EventName = function (x) {
    return x;
};
var HandlerF = (function () {
    function HandlerF(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    HandlerF.create = function (value0) {
        return function (value1) {
            return new HandlerF(value0, value1);
        };
    };
    return HandlerF;
})();
var ClassName = function (x) {
    return x;
};
var AttrName = function (x) {
    return x;
};
var PropF = (function () {
    function PropF(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    PropF.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new PropF(value0, value1, value2);
            };
        };
    };
    return PropF;
})();
var Prop = (function () {
    function Prop(value0) {
        this.value0 = value0;
    };
    Prop.create = function (value0) {
        return new Prop(value0);
    };
    return Prop;
})();
var Attr = (function () {
    function Attr(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Attr.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Attr(value0, value1, value2);
            };
        };
    };
    return Attr;
})();
var Key = (function () {
    function Key(value0) {
        this.value0 = value0;
    };
    Key.create = function (value0) {
        return new Key(value0);
    };
    return Key;
})();
var Handler = (function () {
    function Handler(value0) {
        this.value0 = value0;
    };
    Handler.create = function (value0) {
        return new Handler(value0);
    };
    return Handler;
})();
var Initializer = (function () {
    function Initializer(value0) {
        this.value0 = value0;
    };
    Initializer.create = function (value0) {
        return new Initializer(value0);
    };
    return Initializer;
})();
var Finalizer = (function () {
    function Finalizer(value0) {
        this.value0 = value0;
    };
    Finalizer.create = function (value0) {
        return new Finalizer(value0);
    };
    return Finalizer;
})();
var Text = (function () {
    function Text(value0) {
        this.value0 = value0;
    };
    Text.create = function (value0) {
        return new Text(value0);
    };
    return Text;
})();
var Element = (function () {
    function Element(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Element.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Element(value0, value1, value2, value3);
                };
            };
        };
    };
    return Element;
})();
var Slot = (function () {
    function Slot(value0) {
        this.value0 = value0;
    };
    Slot.create = function (value0) {
        return new Slot(value0);
    };
    return Slot;
})();
var IsProp = function (toPropString) {
    this.toPropString = toPropString;
};
var toPropString = function (dict) {
    return dict.toPropString;
};
var tagName = TagName;
var stringIsProp = new IsProp(function (v) {
    return function (v1) {
        return function (s) {
            return s;
        };
    };
});
var runTagName = function (v) {
    return v;
};
var runPropName = function (v) {
    return v;
};
var runNamespace = function (v) {
    return v;
};
var runEventName = function (v) {
    return v;
};
var runClassName = function (v) {
    return v;
};
var runAttrName = function (v) {
    return v;
};
var propName = PropName;
var prop = function (dictIsProp) {
    return function (name) {
        return function (attr) {
            return function (v) {
                return new Prop(Data_Exists.mkExists(new PropF(name, v, Prelude["<$>"](Data_Maybe.functorMaybe)(Prelude.flip(Data_Tuple.Tuple.create)(toPropString(dictIsProp)))(attr))));
            };
        };
    };
};
var numberIsProp = new IsProp(function (v) {
    return function (v1) {
        return function (n) {
            return Prelude.show(Prelude.showNumber)(n);
        };
    };
});
var namespace = Namespace;
var intIsProp = new IsProp(function (v) {
    return function (v1) {
        return function (i) {
            return Prelude.show(Prelude.showInt)(i);
        };
    };
});
var handler$prime = function (name) {
    return function (k) {
        return new Handler(Data_ExistsR.mkExistsR(new HandlerF(name, k)));
    };
};
var handler = function (name) {
    return function (k) {
        return new Handler(Data_ExistsR.mkExistsR(new HandlerF(name, function ($58) {
            return Prelude.map(Halogen_HTML_Events_Handler.functorEventHandler)(Data_Maybe.Just.create)(k($58));
        })));
    };
};
var functorProp = new Prelude.Functor(function (f) {
    return function (v) {
        if (v instanceof Prop) {
            return new Prop(v.value0);
        };
        if (v instanceof Key) {
            return new Key(v.value0);
        };
        if (v instanceof Attr) {
            return new Attr(v.value0, v.value1, v.value2);
        };
        if (v instanceof Handler) {
            return Data_ExistsR.runExistsR(function (v1) {
                return new Handler(Data_ExistsR.mkExistsR(new HandlerF(v1.value0, function ($59) {
                    return Prelude.map(Halogen_HTML_Events_Handler.functorEventHandler)(Prelude.map(Data_Maybe.functorMaybe)(f))(v1.value1($59));
                })));
            })(v.value0);
        };
        if (v instanceof Initializer) {
            return new Initializer(function ($60) {
                return f(v.value0($60));
            });
        };
        if (v instanceof Finalizer) {
            return new Finalizer(function ($61) {
                return f(v.value0($61));
            });
        };
        throw new Error("Failed pattern match at Halogen.HTML.Core line 101, column 1 - line 111, column 1: " + [ f.constructor.name, v.constructor.name ]);
    };
});
var fillSlot = function (dictApplicative) {
    return function (f) {
        return function (g) {
            return function (v) {
                if (v instanceof Text) {
                    return Prelude.pure(dictApplicative)(new Text(v.value0));
                };
                if (v instanceof Element) {
                    return Prelude["<$>"]((dictApplicative["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Element.create(v.value0)(v.value1)(Prelude["<$>"](Prelude.functorArray)(Prelude["<$>"](functorProp)(g))(v.value2)))(Data_Traversable.traverse(Data_Traversable.traversableArray)(dictApplicative)(fillSlot(dictApplicative)(f)(g))(v.value3));
                };
                if (v instanceof Slot) {
                    return f(v.value0);
                };
                throw new Error("Failed pattern match: " + [ f.constructor.name, g.constructor.name, v.constructor.name ]);
            };
        };
    };
};
var eventName = EventName;
var element = Element.create(Data_Maybe.Nothing.value);
var className = ClassName;
var booleanIsProp = new IsProp(function (name) {
    return function (v) {
        return function (v1) {
            if (v1) {
                return runAttrName(name);
            };
            if (!v1) {
                return "";
            };
            throw new Error("Failed pattern match at Halogen.HTML.Core line 146, column 1 - line 151, column 1: " + [ name.constructor.name, v.constructor.name, v1.constructor.name ]);
        };
    };
});
var bifunctorHTML = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
        var go = function (v) {
            if (v instanceof Text) {
                return new Text(v.value0);
            };
            if (v instanceof Element) {
                return new Element(v.value0, v.value1, Prelude["<$>"](Prelude.functorArray)(Prelude["<$>"](functorProp)(g))(v.value2), Prelude["<$>"](Prelude.functorArray)(go)(v.value3));
            };
            if (v instanceof Slot) {
                return new Slot(f(v.value0));
            };
            throw new Error("Failed pattern match at Halogen.HTML.Core line 62, column 1 - line 69, column 1: " + [ v.constructor.name ]);
        };
        return go;
    };
});
var functorHTML = new Prelude.Functor(Data_Bifunctor.rmap(bifunctorHTML));
var attrName = AttrName;
module.exports = {
    HandlerF: HandlerF, 
    PropF: PropF, 
    Prop: Prop, 
    Attr: Attr, 
    Key: Key, 
    Handler: Handler, 
    Initializer: Initializer, 
    Finalizer: Finalizer, 
    Text: Text, 
    Element: Element, 
    Slot: Slot, 
    IsProp: IsProp, 
    runClassName: runClassName, 
    className: className, 
    runEventName: runEventName, 
    eventName: eventName, 
    runAttrName: runAttrName, 
    attrName: attrName, 
    runPropName: runPropName, 
    propName: propName, 
    runTagName: runTagName, 
    tagName: tagName, 
    runNamespace: runNamespace, 
    namespace: namespace, 
    toPropString: toPropString, 
    "handler'": handler$prime, 
    handler: handler, 
    prop: prop, 
    fillSlot: fillSlot, 
    element: element, 
    bifunctorHTML: bifunctorHTML, 
    functorHTML: functorHTML, 
    functorProp: functorProp, 
    stringIsProp: stringIsProp, 
    intIsProp: intIsProp, 
    numberIsProp: numberIsProp, 
    booleanIsProp: booleanIsProp
};

},{"DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Data.Exists":"/Users/anders/dev/ps/halo/ajax/output/Data.Exists/index.js","Data.ExistsR":"/Users/anders/dev/ps/halo/ajax/output/Data.ExistsR/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Traversable":"/Users/anders/dev/ps/halo/ajax/output/Data.Traversable/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Halogen.HTML.Events.Handler":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Handler/index.js","Halogen.HTML.Events.Types":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Types/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Elements.Indexed/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var Halogen_HTML_Properties_Indexed = require("Halogen.HTML.Properties.Indexed");
var Halogen_HTML_Elements = require("Halogen.HTML.Elements");
var Unsafe_Coerce = require("Unsafe.Coerce");
var wbr = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.wbr);
var video = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.video);
var $$var = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements["var"]);
var ul = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.ul);
var u = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.u);
var tt = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.tt);
var track = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.track);
var tr = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.tr);
var title = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.title);
var time = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.time);
var thead = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.thead);
var th = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.th);
var tfoot = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.tfoot);
var textarea = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.textarea);
var td = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.td);
var tbody = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.tbody);
var table = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.table);
var sup = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.sup);
var summary = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.summary);
var sub = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.sub);
var style = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.style);
var strong = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.strong);
var span = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.span);
var source = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.source);
var small = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.small);
var select = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.select);
var section = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.section);
var script = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.script);
var samp = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.samp);
var s = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.s);
var ruby = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.ruby);
var rt = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.rt);
var rp = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.rp);
var q = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.q);
var progress = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.progress);
var pre = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.pre);
var param = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.param);
var p = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.p);
var output = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.output);
var option = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.option);
var optgroup = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.optgroup);
var ol = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.ol);
var object = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.object);
var noscript = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.noscript);
var noframes = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.noframes);
var nav = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.nav);
var meter = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.meter);
var meta = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.meta);
var menuitem = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.menuitem);
var menu = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.menu);
var mark = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.mark);
var map = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.map);
var main = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.main);
var link = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.link);
var li = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.li);
var legend = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.legend);
var label = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.label);
var keygen = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.keygen);
var kbd = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.kbd);
var ins = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.ins);
var input = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.input);
var img = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.img);
var iframe = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.iframe);
var i = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.i);
var html = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.html);
var hr = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.hr);
var header = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.header);
var head = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.head);
var h6 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.h6);
var h5 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.h5);
var h4 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.h4);
var h3 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.h3);
var h2 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.h2);
var h1 = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.h1);
var form = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.form);
var footer = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.footer);
var figure = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.figure);
var figcaption = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.figcaption);
var fieldset = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.fieldset);
var embed = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.embed);
var em = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.em);
var dt = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.dt);
var dl = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.dl);
var div = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.div);
var dir = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.dir);
var dialog = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.dialog);
var dfn = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.dfn);
var details = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.details);
var del = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.del);
var dd = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.dd);
var datalist = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.datalist);
var command = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.command);
var colgroup = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.colgroup);
var col = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.col);
var code = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.code);
var cite = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.cite);
var center = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.center);
var caption = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.caption);
var canvas = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.canvas);
var button = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.button);
var br = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.br);
var body = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.body);
var blockquote = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.blockquote);
var big = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.big);
var bdo = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.bdo);
var bdi = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.bdi);
var basefont = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.basefont);
var base = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.base);
var b = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.b);
var audio = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.audio);
var aside = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.aside);
var article = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.article);
var area = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.area);
var applet = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.applet);
var address = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.address);
var acronym = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.acronym);
var abbr = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.a);
var a = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Elements.a);
module.exports = {
    wbr: wbr, 
    video: video, 
    "var": $$var, 
    ul: ul, 
    u: u, 
    tt: tt, 
    track: track, 
    tr: tr, 
    title: title, 
    time: time, 
    thead: thead, 
    th: th, 
    tfoot: tfoot, 
    textarea: textarea, 
    td: td, 
    tbody: tbody, 
    table: table, 
    sup: sup, 
    summary: summary, 
    sub: sub, 
    style: style, 
    strong: strong, 
    span: span, 
    source: source, 
    small: small, 
    select: select, 
    section: section, 
    script: script, 
    samp: samp, 
    s: s, 
    ruby: ruby, 
    rt: rt, 
    rp: rp, 
    q: q, 
    progress: progress, 
    pre: pre, 
    param: param, 
    p: p, 
    output: output, 
    option: option, 
    optgroup: optgroup, 
    ol: ol, 
    object: object, 
    noscript: noscript, 
    noframes: noframes, 
    nav: nav, 
    meter: meter, 
    meta: meta, 
    menuitem: menuitem, 
    menu: menu, 
    mark: mark, 
    map: map, 
    main: main, 
    link: link, 
    li: li, 
    legend: legend, 
    label: label, 
    keygen: keygen, 
    kbd: kbd, 
    ins: ins, 
    input: input, 
    img: img, 
    iframe: iframe, 
    i: i, 
    html: html, 
    hr: hr, 
    header: header, 
    head: head, 
    h6: h6, 
    h5: h5, 
    h4: h4, 
    h3: h3, 
    h2: h2, 
    h1: h1, 
    form: form, 
    footer: footer, 
    figure: figure, 
    figcaption: figcaption, 
    fieldset: fieldset, 
    embed: embed, 
    em: em, 
    dt: dt, 
    dl: dl, 
    div: div, 
    dir: dir, 
    dialog: dialog, 
    dfn: dfn, 
    details: details, 
    del: del, 
    dd: dd, 
    datalist: datalist, 
    command: command, 
    colgroup: colgroup, 
    col: col, 
    code: code, 
    cite: cite, 
    center: center, 
    caption: caption, 
    canvas: canvas, 
    button: button, 
    br: br, 
    body: body, 
    blockquote: blockquote, 
    big: big, 
    bdo: bdo, 
    bdi: bdi, 
    basefont: basefont, 
    base: base, 
    b: b, 
    audio: audio, 
    aside: aside, 
    article: article, 
    area: area, 
    applet: applet, 
    address: address, 
    acronym: acronym, 
    abbr: abbr, 
    a: a
};

},{"Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.HTML.Elements":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Elements/index.js","Halogen.HTML.Properties.Indexed":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Properties.Indexed/index.js","Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Elements/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var wbr = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("wbr"))(props)([  ]);
};
var video = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("video"))(xs);
};
var video_ = video([  ]);
var $$var = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("var"))(xs);
};
var var_ = $$var([  ]);
var ul = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("ul"))(xs);
};
var ul_ = ul([  ]);
var u = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("u"))(xs);
};
var u_ = u([  ]);
var tt = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("tt"))(xs);
};
var tt_ = tt([  ]);
var track = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("track"))(props)([  ]);
};
var tr = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("tr"))(xs);
};
var tr_ = tr([  ]);
var title = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("title"))(xs);
};
var title_ = title([  ]);
var time = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("time"))(xs);
};
var time_ = time([  ]);
var thead = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("thead"))(xs);
};
var thead_ = thead([  ]);
var th = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("th"))(xs);
};
var th_ = th([  ]);
var tfoot = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("tfoot"))(xs);
};
var tfoot_ = tfoot([  ]);
var textarea = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("textarea"))(xs)([  ]);
};
var td = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("td"))(xs);
};
var td_ = td([  ]);
var tbody = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("tbody"))(xs);
};
var tbody_ = tbody([  ]);
var table = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("table"))(xs);
};
var table_ = table([  ]);
var sup = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("sup"))(xs);
};
var sup_ = sup([  ]);
var summary = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("summary"))(xs);
};
var summary_ = summary([  ]);
var sub = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("sub"))(xs);
};
var sub_ = sub([  ]);
var style = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("style"))(xs);
};
var style_ = style([  ]);
var strong = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("strong"))(xs);
};
var strong_ = strong([  ]);
var strike = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("strike"))(xs);
};
var strike_ = strike([  ]);
var span = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("span"))(xs);
};
var span_ = span([  ]);
var source = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("source"))(props)([  ]);
};
var small = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("small"))(xs);
};
var small_ = small([  ]);
var select = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("select"))(xs);
};
var select_ = select([  ]);
var section = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("section"))(xs);
};
var section_ = section([  ]);
var script = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("script"))(xs);
};
var script_ = script([  ]);
var samp = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("samp"))(xs);
};
var samp_ = samp([  ]);
var s = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("s"))(xs);
};
var s_ = s([  ]);
var ruby = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("ruby"))(xs);
};
var ruby_ = ruby([  ]);
var rt = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("rt"))(xs);
};
var rt_ = rt([  ]);
var rp = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("rp"))(xs);
};
var rp_ = rp([  ]);
var q = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("q"))(xs);
};
var q_ = q([  ]);
var progress = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("progress"))(xs);
};
var progress_ = progress([  ]);
var pre = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("pre"))(xs);
};
var pre_ = pre([  ]);
var param = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("param"))(props)([  ]);
};
var p = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("p"))(xs);
};
var p_ = p([  ]);
var output = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("output"))(xs);
};
var output_ = output([  ]);
var option = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("option"))(xs);
};
var option_ = option([  ]);
var optgroup = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("optgroup"))(xs);
};
var optgroup_ = optgroup([  ]);
var ol = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("ol"))(xs);
};
var ol_ = ol([  ]);
var object = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("object"))(xs);
};
var object_ = object([  ]);
var noscript = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("noscript"))(xs);
};
var noscript_ = noscript([  ]);
var noframes = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("noframes"))(xs);
};
var noframes_ = noframes([  ]);
var nav = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("nav"))(xs);
};
var nav_ = nav([  ]);
var meter = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("meter"))(xs);
};
var meter_ = meter([  ]);
var meta = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("meta"))(props)([  ]);
};
var menuitem = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("menuitem"))(xs);
};
var menuitem_ = menuitem([  ]);
var menu = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("menu"))(xs);
};
var menu_ = menu([  ]);
var mark = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("mark"))(xs);
};
var mark_ = mark([  ]);
var map = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("map"))(xs);
};
var map_ = map([  ]);
var main = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("main"))(xs);
};
var main_ = main([  ]);
var link = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("link"))(props)([  ]);
};
var li = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("li"))(xs);
};
var li_ = li([  ]);
var legend = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("legend"))(xs);
};
var legend_ = legend([  ]);
var label = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("label"))(xs);
};
var label_ = label([  ]);
var keygen = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("keygen"))(props)([  ]);
};
var kbd = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("kbd"))(xs);
};
var kbd_ = kbd([  ]);
var ins = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("ins"))(xs);
};
var ins_ = ins([  ]);
var input = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("input"))(props)([  ]);
};
var img = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("img"))(props)([  ]);
};
var iframe = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("iframe"))(props)([  ]);
};
var i = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("i"))(xs);
};
var i_ = i([  ]);
var html = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("html"))(xs);
};
var html_ = html([  ]);
var hr = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("hr"))(props)([  ]);
};
var hr_ = hr([  ]);
var header = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("header"))(xs);
};
var header_ = header([  ]);
var head = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("head"))(xs);
};
var head_ = head([  ]);
var h6 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h6"))(xs);
};
var h6_ = h6([  ]);
var h5 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h5"))(xs);
};
var h5_ = h5([  ]);
var h4 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h4"))(xs);
};
var h4_ = h4([  ]);
var h3 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h3"))(xs);
};
var h3_ = h3([  ]);
var h2 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h2"))(xs);
};
var h2_ = h2([  ]);
var h1 = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("h1"))(xs);
};
var h1_ = h1([  ]);
var frameset = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("frameset"))(xs);
};
var frameset_ = frameset([  ]);
var frame = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("frame"))(xs);
};
var frame_ = frame([  ]);
var form = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("form"))(xs);
};
var form_ = form([  ]);
var footer = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("footer"))(xs);
};
var footer_ = footer([  ]);
var font = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("font"))(xs);
};
var font_ = font([  ]);
var figure = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("figure"))(xs);
};
var figure_ = figure([  ]);
var figcaption = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("figcaption"))(xs);
};
var figcaption_ = figcaption([  ]);
var fieldset = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("fieldset"))(xs);
};
var fieldset_ = fieldset([  ]);
var embed = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("embed"))(xs);
};
var embed_ = embed([  ]);
var em = Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("em"));
var em_ = em([  ]);
var dt = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dt"))(xs);
};
var dt_ = dt([  ]);
var dl = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dl"))(xs);
};
var dl_ = dl([  ]);
var div = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("div"))(xs);
};
var div_ = div([  ]);
var dir = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dir"))(xs);
};
var dir_ = dir([  ]);
var dialog = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dialog"))(xs);
};
var dialog_ = dialog([  ]);
var dfn = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dfn"))(xs);
};
var dfn_ = dfn([  ]);
var details = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("details"))(xs);
};
var details_ = details([  ]);
var del = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("del"))(xs);
};
var del_ = del([  ]);
var dd = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("dd"))(xs);
};
var dd_ = dd([  ]);
var datalist = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("datalist"))(xs);
};
var datalist_ = datalist([  ]);
var command = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("command"))(props)([  ]);
};
var colgroup = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("colgroup"))(xs);
};
var colgroup_ = colgroup([  ]);
var col = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("col"))(props)([  ]);
};
var code = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("code"))(xs);
};
var code_ = code([  ]);
var cite = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("cite"))(xs);
};
var cite_ = cite([  ]);
var center = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("center"))(xs);
};
var center_ = center([  ]);
var caption = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("caption"))(xs);
};
var caption_ = caption([  ]);
var canvas = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("canvas"))(props)([  ]);
};
var button = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("button"))(xs);
};
var button_ = button([  ]);
var br = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("br"))(props)([  ]);
};
var br_ = br([  ]);
var body = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("body"))(xs);
};
var body_ = body([  ]);
var blockquote = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("blockquote"))(xs);
};
var blockquote_ = blockquote([  ]);
var big = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("big"))(xs);
};
var big_ = big([  ]);
var bdo = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("bdo"))(xs);
};
var bdo_ = bdo([  ]);
var bdi = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("bdi"))(xs);
};
var bdi_ = bdi([  ]);
var basefont = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("basefont"))(xs);
};
var basefont_ = basefont([  ]);
var base = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("base"))(props)([  ]);
};
var b = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("b"))(xs);
};
var b_ = b([  ]);
var audio = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("audio"))(xs);
};
var audio_ = audio([  ]);
var aside = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("aside"))(xs);
};
var aside_ = aside([  ]);
var article = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("article"))(xs);
};
var article_ = article([  ]);
var area = function (props) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("area"))(props)([  ]);
};
var applet = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("applet"))(xs);
};
var applet_ = applet([  ]);
var address = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("address"))(xs);
};
var address_ = address([  ]);
var acronym = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("acronym"))(xs);
};
var acronym_ = acronym([  ]);
var abbr = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("abbr"))(xs);
};
var abbr_ = abbr([  ]);
var a = function (xs) {
    return Halogen_HTML_Core.element(Halogen_HTML_Core.tagName("a"))(xs);
};
var a_ = a([  ]);
module.exports = {
    wbr: wbr, 
    video_: video_, 
    video: video, 
    var_: var_, 
    "var": $$var, 
    ul_: ul_, 
    ul: ul, 
    u_: u_, 
    u: u, 
    tt_: tt_, 
    tt: tt, 
    track: track, 
    tr_: tr_, 
    tr: tr, 
    title_: title_, 
    title: title, 
    time_: time_, 
    time: time, 
    thead_: thead_, 
    thead: thead, 
    th_: th_, 
    th: th, 
    tfoot_: tfoot_, 
    tfoot: tfoot, 
    textarea: textarea, 
    td_: td_, 
    td: td, 
    tbody_: tbody_, 
    tbody: tbody, 
    table_: table_, 
    table: table, 
    sup_: sup_, 
    sup: sup, 
    summary_: summary_, 
    summary: summary, 
    sub_: sub_, 
    sub: sub, 
    style_: style_, 
    style: style, 
    strong_: strong_, 
    strong: strong, 
    strike_: strike_, 
    strike: strike, 
    span_: span_, 
    span: span, 
    source: source, 
    small_: small_, 
    small: small, 
    select_: select_, 
    select: select, 
    section_: section_, 
    section: section, 
    script_: script_, 
    script: script, 
    samp_: samp_, 
    samp: samp, 
    s_: s_, 
    s: s, 
    ruby_: ruby_, 
    ruby: ruby, 
    rt_: rt_, 
    rt: rt, 
    rp_: rp_, 
    rp: rp, 
    q_: q_, 
    q: q, 
    progress_: progress_, 
    progress: progress, 
    pre_: pre_, 
    pre: pre, 
    param: param, 
    p_: p_, 
    p: p, 
    output_: output_, 
    output: output, 
    option_: option_, 
    option: option, 
    optgroup_: optgroup_, 
    optgroup: optgroup, 
    ol_: ol_, 
    ol: ol, 
    object_: object_, 
    object: object, 
    noscript_: noscript_, 
    noscript: noscript, 
    noframes_: noframes_, 
    noframes: noframes, 
    nav_: nav_, 
    nav: nav, 
    meter_: meter_, 
    meter: meter, 
    meta: meta, 
    menuitem_: menuitem_, 
    menuitem: menuitem, 
    menu_: menu_, 
    menu: menu, 
    mark_: mark_, 
    mark: mark, 
    map_: map_, 
    map: map, 
    main_: main_, 
    main: main, 
    link: link, 
    li_: li_, 
    li: li, 
    legend_: legend_, 
    legend: legend, 
    label_: label_, 
    label: label, 
    keygen: keygen, 
    kbd_: kbd_, 
    kbd: kbd, 
    ins_: ins_, 
    ins: ins, 
    input: input, 
    img: img, 
    iframe: iframe, 
    i_: i_, 
    i: i, 
    html_: html_, 
    html: html, 
    hr_: hr_, 
    hr: hr, 
    header_: header_, 
    header: header, 
    head_: head_, 
    head: head, 
    h6_: h6_, 
    h6: h6, 
    h5_: h5_, 
    h5: h5, 
    h4_: h4_, 
    h4: h4, 
    h3_: h3_, 
    h3: h3, 
    h2_: h2_, 
    h2: h2, 
    h1_: h1_, 
    h1: h1, 
    frameset_: frameset_, 
    frameset: frameset, 
    frame_: frame_, 
    frame: frame, 
    form_: form_, 
    form: form, 
    footer_: footer_, 
    footer: footer, 
    font_: font_, 
    font: font, 
    figure_: figure_, 
    figure: figure, 
    figcaption_: figcaption_, 
    figcaption: figcaption, 
    fieldset_: fieldset_, 
    fieldset: fieldset, 
    embed_: embed_, 
    embed: embed, 
    em_: em_, 
    em: em, 
    dt_: dt_, 
    dt: dt, 
    dl_: dl_, 
    dl: dl, 
    div_: div_, 
    div: div, 
    dir_: dir_, 
    dir: dir, 
    dialog_: dialog_, 
    dialog: dialog, 
    dfn_: dfn_, 
    dfn: dfn, 
    details_: details_, 
    details: details, 
    del_: del_, 
    del: del, 
    dd_: dd_, 
    dd: dd, 
    datalist_: datalist_, 
    datalist: datalist, 
    command: command, 
    colgroup_: colgroup_, 
    colgroup: colgroup, 
    col: col, 
    code_: code_, 
    code: code, 
    cite_: cite_, 
    cite: cite, 
    center_: center_, 
    center: center, 
    caption_: caption_, 
    caption: caption, 
    canvas: canvas, 
    button_: button_, 
    button: button, 
    br_: br_, 
    br: br, 
    body_: body_, 
    body: body, 
    blockquote_: blockquote_, 
    blockquote: blockquote, 
    big_: big_, 
    big: big, 
    bdo_: bdo_, 
    bdo: bdo, 
    bdi_: bdi_, 
    bdi: bdi, 
    basefont_: basefont_, 
    basefont: basefont, 
    base: base, 
    b_: b_, 
    b: b, 
    audio_: audio_, 
    audio: audio, 
    aside_: aside_, 
    aside: aside, 
    article_: article_, 
    article: article, 
    area: area, 
    applet_: applet_, 
    applet: applet, 
    address_: address_, 
    address: address, 
    acronym_: acronym_, 
    acronym: acronym, 
    abbr_: abbr_, 
    abbr: abbr, 
    a_: a_, 
    a: a
};

},{"Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Forms/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Foreign = require("Data.Foreign");
var Data_Foreign_Class = require("Data.Foreign.Class");
var Data_Maybe = require("Data.Maybe");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var Halogen_HTML_Events_Handler = require("Halogen.HTML.Events.Handler");
var Data_Foreign_Index = require("Data.Foreign.Index");
var addForeignPropHandler = function (dictIsForeign) {
    return function (key) {
        return function (prop) {
            return function (f) {
                return Halogen_HTML_Core["handler'"](Halogen_HTML_Core.eventName(key))(function ($2) {
                    return Data_Either.either(Prelude["const"](Prelude.pure(Halogen_HTML_Events_Handler.applicativeEventHandler)(Data_Maybe.Nothing.value)))(function ($3) {
                        return Prelude.map(Halogen_HTML_Events_Handler.functorEventHandler)(Data_Maybe.Just.create)(f($3));
                    })(Data_Foreign_Class.readProp(dictIsForeign)(Data_Foreign_Index.indexString)(prop)(Data_Foreign.toForeign((function (v) {
                        return v.target;
                    })($2))));
                });
            };
        };
    };
};
var onChecked = addForeignPropHandler(Data_Foreign_Class.booleanIsForeign)("change")("checked");
var onValueChange = addForeignPropHandler(Data_Foreign_Class.stringIsForeign)("change")("value");
var onValueInput = addForeignPropHandler(Data_Foreign_Class.stringIsForeign)("input")("value");
module.exports = {
    onChecked: onChecked, 
    onValueInput: onValueInput, 
    onValueChange: onValueChange
};

},{"Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Foreign.Class":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Class/index.js","Data.Foreign.Index":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Index/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.HTML.Events.Handler":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Handler/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Handler/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Halogen.HTML.Events.Handler

exports.preventDefaultImpl = function (e) {
  return function () {
    e.preventDefault();
  };
};

exports.stopPropagationImpl = function (e) {
  return function () {
    e.stopPropagation();
  };
};

exports.stopImmediatePropagationImpl = function (e) {
  return function () {
    e.stopImmediatePropagation();
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Handler/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Apply = require("Control.Apply");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Writer = require("Control.Monad.Writer");
var Control_Monad_Writer_Class = require("Control.Monad.Writer.Class");
var Data_Foldable = require("Data.Foldable");
var Data_Tuple = require("Data.Tuple");
var DOM = require("DOM");
var Halogen_HTML_Events_Types = require("Halogen.HTML.Events.Types");
var Data_Monoid = require("Data.Monoid");
var Control_Monad_Writer_Trans = require("Control.Monad.Writer.Trans");
var Data_Identity = require("Data.Identity");
var PreventDefault = (function () {
    function PreventDefault() {

    };
    PreventDefault.value = new PreventDefault();
    return PreventDefault;
})();
var StopPropagation = (function () {
    function StopPropagation() {

    };
    StopPropagation.value = new StopPropagation();
    return StopPropagation;
})();
var StopImmediatePropagation = (function () {
    function StopImmediatePropagation() {

    };
    StopImmediatePropagation.value = new StopImmediatePropagation();
    return StopImmediatePropagation;
})();
var EventHandler = function (x) {
    return x;
};
var unEventHandler = function (v) {
    return v;
};
var stopPropagation = Control_Monad_Writer_Class.tell(Data_Monoid.monoidArray)(Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))([ StopPropagation.value ]);
var stopImmediatePropagation = Control_Monad_Writer_Class.tell(Data_Monoid.monoidArray)(Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))([ StopImmediatePropagation.value ]);
var runEventHandler = function (dictMonad) {
    return function (dictMonadEff) {
        return function (e) {
            return function (v) {
                var applyUpdate = function (v1) {
                    if (v1 instanceof PreventDefault) {
                        return $foreign.preventDefaultImpl(e);
                    };
                    if (v1 instanceof StopPropagation) {
                        return $foreign.stopPropagationImpl(e);
                    };
                    if (v1 instanceof StopImmediatePropagation) {
                        return $foreign.stopImmediatePropagationImpl(e);
                    };
                    throw new Error("Failed pattern match at Halogen.HTML.Events.Handler line 88, column 3 - line 89, column 3: " + [ v1.constructor.name ]);
                };
                var $13 = Control_Monad_Writer.runWriter(v);
                return Control_Monad_Eff_Class.liftEff(dictMonadEff)(Control_Apply["*>"](Control_Monad_Eff.applyEff)(Data_Foldable.for_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)($13.value1)(applyUpdate))(Prelude["return"](Control_Monad_Eff.applicativeEff)($13.value0)));
            };
        };
    };
};
var preventDefault = Control_Monad_Writer_Class.tell(Data_Monoid.monoidArray)(Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(Data_Identity.monadIdentity))([ PreventDefault.value ]);
var functorEventHandler = new Prelude.Functor(function (f) {
    return function (v) {
        return Prelude["<$>"](Control_Monad_Writer_Trans.functorWriterT(Data_Identity.functorIdentity))(f)(v);
    };
});
var applyEventHandler = new Prelude.Apply(function () {
    return functorEventHandler;
}, function (v) {
    return function (v1) {
        return Prelude["<*>"](Control_Monad_Writer_Trans.applyWriterT(Prelude.semigroupArray)(Data_Identity.applyIdentity))(v)(v1);
    };
});
var bindEventHandler = new Prelude.Bind(function () {
    return applyEventHandler;
}, function (v) {
    return function (f) {
        return Prelude[">>="](Control_Monad_Writer_Trans.bindWriterT(Prelude.semigroupArray)(Data_Identity.monadIdentity))(v)(function ($22) {
            return unEventHandler(f($22));
        });
    };
});
var applicativeEventHandler = new Prelude.Applicative(function () {
    return applyEventHandler;
}, function ($23) {
    return EventHandler(Prelude.pure(Control_Monad_Writer_Trans.applicativeWriterT(Data_Monoid.monoidArray)(Data_Identity.applicativeIdentity))($23));
});
var monadEventHandler = new Prelude.Monad(function () {
    return applicativeEventHandler;
}, function () {
    return bindEventHandler;
});
module.exports = {
    runEventHandler: runEventHandler, 
    stopImmediatePropagation: stopImmediatePropagation, 
    stopPropagation: stopPropagation, 
    preventDefault: preventDefault, 
    functorEventHandler: functorEventHandler, 
    applyEventHandler: applyEventHandler, 
    applicativeEventHandler: applicativeEventHandler, 
    bindEventHandler: bindEventHandler, 
    monadEventHandler: monadEventHandler
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Handler/foreign.js","Control.Apply":"/Users/anders/dev/ps/halo/ajax/output/Control.Apply/index.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Writer":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer/index.js","Control.Monad.Writer.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Class/index.js","Control.Monad.Writer.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Writer.Trans/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Halogen.HTML.Events.Types":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Types/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Indexed/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Unsafe_Coerce = require("Unsafe.Coerce");
var Halogen_HTML_Events_Handler = require("Halogen.HTML.Events.Handler");
var Halogen_HTML_Events_Types = require("Halogen.HTML.Events.Types");
var Halogen_HTML_Properties_Indexed = require("Halogen.HTML.Properties.Indexed");
var Halogen_HTML_Events = require("Halogen.HTML.Events");
var Halogen_HTML_Events_Forms = require("Halogen.HTML.Events.Forms");
var onValueInput = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events_Forms.onValueInput);
var onValueChange = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events_Forms.onValueChange);
var onUnload = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onUnload);
var onSubmit = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onSubmit);
var onSelect = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onSelect);
var onSearch = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onSearch);
var onScroll = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onScroll);
var onResize = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onResize);
var onReset = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onReset);
var onPageShow = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onPageShow);
var onPageHide = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onPageHide);
var onMouseUp = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onMouseUp);
var onMouseOver = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onMouseOver);
var onMouseOut = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onMouseOut);
var onMouseMove = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onMouseMove);
var onMouseLeave = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onMouseLeave);
var onMouseDown = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onMouseDown);
var onLoad = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onLoad);
var onKeyUp = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onKeyUp);
var onKeyPress = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onKeyPress);
var onKeyDown = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onKeyDown);
var onInvalid = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onInvalid);
var onInput = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onInput);
var onHashChange = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onHashChange);
var onFocusOut = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onFocusOut);
var onFocusIn = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onFocusIn);
var onFocus = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onFocus);
var onError = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onError);
var onDoubleClick = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onDoubleClick);
var onContextMenu = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onContextMenu);
var onClick = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onClick);
var onChecked = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events_Forms.onChecked);
var onChange = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onChange);
var onBlur = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onBlur);
var onBeforeUnload = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onBeforeUnload);
var onAbort = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Events.onAbort);
module.exports = {
    onChecked: onChecked, 
    onValueInput: onValueInput, 
    onValueChange: onValueChange, 
    onFocusOut: onFocusOut, 
    onFocusIn: onFocusIn, 
    onFocus: onFocus, 
    onBlur: onBlur, 
    onKeyUp: onKeyUp, 
    onKeyPress: onKeyPress, 
    onKeyDown: onKeyDown, 
    onMouseUp: onMouseUp, 
    onMouseOut: onMouseOut, 
    onMouseOver: onMouseOver, 
    onMouseMove: onMouseMove, 
    onMouseLeave: onMouseLeave, 
    onMouseDown: onMouseDown, 
    onDoubleClick: onDoubleClick, 
    onContextMenu: onContextMenu, 
    onClick: onClick, 
    onSubmit: onSubmit, 
    onSelect: onSelect, 
    onSearch: onSearch, 
    onReset: onReset, 
    onInvalid: onInvalid, 
    onInput: onInput, 
    onChange: onChange, 
    onUnload: onUnload, 
    onScroll: onScroll, 
    onResize: onResize, 
    onPageHide: onPageHide, 
    onPageShow: onPageShow, 
    onLoad: onLoad, 
    onHashChange: onHashChange, 
    onError: onError, 
    onBeforeUnload: onBeforeUnload, 
    onAbort: onAbort
};

},{"Halogen.HTML.Events":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events/index.js","Halogen.HTML.Events.Forms":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Forms/index.js","Halogen.HTML.Events.Handler":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Handler/index.js","Halogen.HTML.Events.Types":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Types/index.js","Halogen.HTML.Properties.Indexed":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Properties.Indexed/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js","Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Types/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var DOM_HTML_Types = require("DOM.HTML.Types");
module.exports = {};

},{"DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Halogen_Query = require("Halogen.Query");
var Halogen_HTML_Events_Handler = require("Halogen.HTML.Events.Handler");
var Halogen_HTML_Events_Types = require("Halogen.HTML.Events.Types");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var onUnload = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("unload"));
var onSubmit = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("submit"));
var onSelect = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("select"));
var onSearch = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("search"));
var onScroll = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("scroll"));
var onResize = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("resize"));
var onReset = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("reset"));
var onPageShow = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("pageshow"));
var onPageHide = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("pagehide"));
var onMouseUp = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("mouseup"));
var onMouseOver = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("mouseover"));
var onMouseOut = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("mouseout"));
var onMouseMove = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("mousemove"));
var onMouseLeave = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("mouseleave"));
var onMouseEnter = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("mouseenter"));
var onMouseDown = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("mousedown"));
var onLoad = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("load"));
var onKeyUp = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("keyup"));
var onKeyPress = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("keypress"));
var onKeyDown = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("keydown"));
var onInvalid = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("invalid"));
var onInput = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("input"));
var onHashChange = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("hashchange"));
var onFocusOut = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("focusout"));
var onFocusIn = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("focusin"));
var onFocus = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("focus"));
var onError = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("error"));
var onDoubleClick = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("dblclick"));
var onContextMenu = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("contextmenu"));
var onClick = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("click"));
var onChange = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("change"));
var onBlur = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("blur"));
var onBeforeUnload = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("beforeunload"));
var onAbort = Halogen_HTML_Core.handler(Halogen_HTML_Core.eventName("abort"));
var input_ = function (f) {
    return function (v) {
        return Prelude.pure(Halogen_HTML_Events_Handler.applicativeEventHandler)(Halogen_Query.action(f));
    };
};
var input = function (f) {
    return function (x) {
        return Prelude.pure(Halogen_HTML_Events_Handler.applicativeEventHandler)(Halogen_Query.action(f(x)));
    };
};
module.exports = {
    onFocusOut: onFocusOut, 
    onFocusIn: onFocusIn, 
    onFocus: onFocus, 
    onBlur: onBlur, 
    onKeyUp: onKeyUp, 
    onKeyPress: onKeyPress, 
    onKeyDown: onKeyDown, 
    onMouseUp: onMouseUp, 
    onMouseOut: onMouseOut, 
    onMouseOver: onMouseOver, 
    onMouseMove: onMouseMove, 
    onMouseLeave: onMouseLeave, 
    onMouseEnter: onMouseEnter, 
    onMouseDown: onMouseDown, 
    onDoubleClick: onDoubleClick, 
    onContextMenu: onContextMenu, 
    onClick: onClick, 
    onSubmit: onSubmit, 
    onSelect: onSelect, 
    onSearch: onSearch, 
    onReset: onReset, 
    onInvalid: onInvalid, 
    onInput: onInput, 
    onChange: onChange, 
    onUnload: onUnload, 
    onScroll: onScroll, 
    onResize: onResize, 
    onPageHide: onPageHide, 
    onPageShow: onPageShow, 
    onLoad: onLoad, 
    onHashChange: onHashChange, 
    onError: onError, 
    onBeforeUnload: onBeforeUnload, 
    onAbort: onAbort, 
    input_: input_, 
    input: input
};

},{"Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.HTML.Events.Handler":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Handler/index.js","Halogen.HTML.Events.Types":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Types/index.js","Halogen.Query":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Indexed/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Halogen_HTML = require("Halogen.HTML");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var Halogen_HTML_Elements_Indexed = require("Halogen.HTML.Elements.Indexed");
module.exports = {};

},{"Halogen.HTML":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML/index.js","Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.HTML.Elements.Indexed":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Elements.Indexed/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Properties.Indexed/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Foldable = require("Data.Foldable");
var Data_Tuple = require("Data.Tuple");
var Data_Array = require("Data.Array");
var Unsafe_Coerce = require("Unsafe.Coerce");
var DOM_HTML_Types = require("DOM.HTML.Types");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var Halogen_HTML_Properties = require("Halogen.HTML.Properties");
var Data_Monoid = require("Data.Monoid");
var MenuitemCommand = (function () {
    function MenuitemCommand() {

    };
    MenuitemCommand.value = new MenuitemCommand();
    return MenuitemCommand;
})();
var MenuitemCheckbox = (function () {
    function MenuitemCheckbox() {

    };
    MenuitemCheckbox.value = new MenuitemCheckbox();
    return MenuitemCheckbox;
})();
var MenuitemRadio = (function () {
    function MenuitemRadio() {

    };
    MenuitemRadio.value = new MenuitemRadio();
    return MenuitemRadio;
})();
var MenuList = (function () {
    function MenuList() {

    };
    MenuList.value = new MenuList();
    return MenuList;
})();
var MenuContext = (function () {
    function MenuContext() {

    };
    MenuContext.value = new MenuContext();
    return MenuContext;
})();
var MenuToolbar = (function () {
    function MenuToolbar() {

    };
    MenuToolbar.value = new MenuToolbar();
    return MenuToolbar;
})();
var InputButton = (function () {
    function InputButton() {

    };
    InputButton.value = new InputButton();
    return InputButton;
})();
var InputCheckbox = (function () {
    function InputCheckbox() {

    };
    InputCheckbox.value = new InputCheckbox();
    return InputCheckbox;
})();
var InputColor = (function () {
    function InputColor() {

    };
    InputColor.value = new InputColor();
    return InputColor;
})();
var InputDate = (function () {
    function InputDate() {

    };
    InputDate.value = new InputDate();
    return InputDate;
})();
var InputDatetime = (function () {
    function InputDatetime() {

    };
    InputDatetime.value = new InputDatetime();
    return InputDatetime;
})();
var InputDatetimeLocal = (function () {
    function InputDatetimeLocal() {

    };
    InputDatetimeLocal.value = new InputDatetimeLocal();
    return InputDatetimeLocal;
})();
var InputEmail = (function () {
    function InputEmail() {

    };
    InputEmail.value = new InputEmail();
    return InputEmail;
})();
var InputFile = (function () {
    function InputFile() {

    };
    InputFile.value = new InputFile();
    return InputFile;
})();
var InputHidden = (function () {
    function InputHidden() {

    };
    InputHidden.value = new InputHidden();
    return InputHidden;
})();
var InputImage = (function () {
    function InputImage() {

    };
    InputImage.value = new InputImage();
    return InputImage;
})();
var InputMonth = (function () {
    function InputMonth() {

    };
    InputMonth.value = new InputMonth();
    return InputMonth;
})();
var InputNumber = (function () {
    function InputNumber() {

    };
    InputNumber.value = new InputNumber();
    return InputNumber;
})();
var InputPassword = (function () {
    function InputPassword() {

    };
    InputPassword.value = new InputPassword();
    return InputPassword;
})();
var InputRadio = (function () {
    function InputRadio() {

    };
    InputRadio.value = new InputRadio();
    return InputRadio;
})();
var InputRange = (function () {
    function InputRange() {

    };
    InputRange.value = new InputRange();
    return InputRange;
})();
var InputReset = (function () {
    function InputReset() {

    };
    InputReset.value = new InputReset();
    return InputReset;
})();
var InputSearch = (function () {
    function InputSearch() {

    };
    InputSearch.value = new InputSearch();
    return InputSearch;
})();
var InputSubmit = (function () {
    function InputSubmit() {

    };
    InputSubmit.value = new InputSubmit();
    return InputSubmit;
})();
var InputTel = (function () {
    function InputTel() {

    };
    InputTel.value = new InputTel();
    return InputTel;
})();
var InputText = (function () {
    function InputText() {

    };
    InputText.value = new InputText();
    return InputText;
})();
var InputTime = (function () {
    function InputTime() {

    };
    InputTime.value = new InputTime();
    return InputTime;
})();
var InputUrl = (function () {
    function InputUrl() {

    };
    InputUrl.value = new InputUrl();
    return InputUrl;
})();
var InputWeek = (function () {
    function InputWeek() {

    };
    InputWeek.value = new InputWeek();
    return InputWeek;
})();
var IProp = function (x) {
    return x;
};
var Uppercase = (function () {
    function Uppercase() {

    };
    Uppercase.value = new Uppercase();
    return Uppercase;
})();
var Lowercase = (function () {
    function Lowercase() {

    };
    Lowercase.value = new Lowercase();
    return Lowercase;
})();
var NumeralDecimal = (function () {
    function NumeralDecimal() {

    };
    NumeralDecimal.value = new NumeralDecimal();
    return NumeralDecimal;
})();
var NumeralRoman = (function () {
    function NumeralRoman(value0) {
        this.value0 = value0;
    };
    NumeralRoman.create = function (value0) {
        return new NumeralRoman(value0);
    };
    return NumeralRoman;
})();
var OrderedListNumeric = (function () {
    function OrderedListNumeric(value0) {
        this.value0 = value0;
    };
    OrderedListNumeric.create = function (value0) {
        return new OrderedListNumeric(value0);
    };
    return OrderedListNumeric;
})();
var OrderedListAlphabetic = (function () {
    function OrderedListAlphabetic(value0) {
        this.value0 = value0;
    };
    OrderedListAlphabetic.create = function (value0) {
        return new OrderedListAlphabetic(value0);
    };
    return OrderedListAlphabetic;
})();
var ButtonButton = (function () {
    function ButtonButton() {

    };
    ButtonButton.value = new ButtonButton();
    return ButtonButton;
})();
var ButtonSubmit = (function () {
    function ButtonSubmit() {

    };
    ButtonSubmit.value = new ButtonSubmit();
    return ButtonSubmit;
})();
var ButtonReset = (function () {
    function ButtonReset() {

    };
    ButtonReset.value = new ButtonReset();
    return ButtonReset;
})();
var width = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.width);
var value = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.value);
var title = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.title);
var target = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.target);
var src = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.src);
var spellcheck = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.spellcheck);
var selected = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.selected);
var rowSpan = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.rowSpan);
var required = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.required);
var renderOrderedListType = function (ty) {
    if (ty instanceof OrderedListNumeric) {
        if (ty.value0 instanceof NumeralDecimal) {
            return "1";
        };
        if (ty.value0 instanceof NumeralRoman) {
            if (ty.value0.value0 instanceof Lowercase) {
                return "i";
            };
            if (ty.value0.value0 instanceof Uppercase) {
                return "I";
            };
            throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 270, column 1 - line 271, column 1: " + [ ty.value0.value0.constructor.name ]);
        };
        throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 270, column 1 - line 271, column 1: " + [ ty.value0.constructor.name ]);
    };
    if (ty instanceof OrderedListAlphabetic) {
        if (ty.value0 instanceof Lowercase) {
            return "a";
        };
        if (ty.value0 instanceof Uppercase) {
            return "A";
        };
        throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 270, column 1 - line 271, column 1: " + [ ty.value0.constructor.name ]);
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 270, column 1 - line 271, column 1: " + [ ty.constructor.name ]);
};
var renderMenuitemType = function (ty) {
    if (ty instanceof MenuitemCommand) {
        return "command";
    };
    if (ty instanceof MenuitemCheckbox) {
        return "checkbox";
    };
    if (ty instanceof MenuitemRadio) {
        return "radio";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 213, column 1 - line 214, column 1: " + [ ty.constructor.name ]);
};
var renderMenuType = function (ty) {
    if (ty instanceof MenuList) {
        return "list";
    };
    if (ty instanceof MenuContext) {
        return "context";
    };
    if (ty instanceof MenuToolbar) {
        return "toolbar";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 198, column 1 - line 199, column 1: " + [ ty.constructor.name ]);
};
var renderMediaType = function (ty) {
    var renderParameter = function (v) {
        return v.value0 + ("=" + v.value1);
    };
    var renderParameters = function (ps) {
        if (Data_Array.length(ps) === 0) {
            return "";
        };
        if (Prelude.otherwise) {
            return ";" + Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(";")(Prelude["<$>"](Prelude.functorArray)(renderParameter)(ps));
        };
        throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 229, column 1 - line 230, column 1: " + [ ps.constructor.name ]);
    };
    return ty.type + ("/" + (ty.subtype + renderParameters(ty.parameters)));
};
var renderInputType = function (ty) {
    if (ty instanceof InputButton) {
        return "button";
    };
    if (ty instanceof InputCheckbox) {
        return "checkbox";
    };
    if (ty instanceof InputColor) {
        return "color";
    };
    if (ty instanceof InputDate) {
        return "date";
    };
    if (ty instanceof InputDatetime) {
        return "datetime";
    };
    if (ty instanceof InputDatetimeLocal) {
        return "datetime-local";
    };
    if (ty instanceof InputEmail) {
        return "email";
    };
    if (ty instanceof InputFile) {
        return "file";
    };
    if (ty instanceof InputHidden) {
        return "hidden";
    };
    if (ty instanceof InputImage) {
        return "image";
    };
    if (ty instanceof InputMonth) {
        return "month";
    };
    if (ty instanceof InputNumber) {
        return "number";
    };
    if (ty instanceof InputPassword) {
        return "password";
    };
    if (ty instanceof InputRadio) {
        return "radio";
    };
    if (ty instanceof InputRange) {
        return "range";
    };
    if (ty instanceof InputReset) {
        return "reset";
    };
    if (ty instanceof InputSearch) {
        return "search";
    };
    if (ty instanceof InputSubmit) {
        return "submit";
    };
    if (ty instanceof InputTel) {
        return "tel";
    };
    if (ty instanceof InputText) {
        return "text";
    };
    if (ty instanceof InputTime) {
        return "time";
    };
    if (ty instanceof InputUrl) {
        return "url";
    };
    if (ty instanceof InputWeek) {
        return "week";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 163, column 1 - line 164, column 1: " + [ ty.constructor.name ]);
};
var renderButtonType = function (ty) {
    if (ty instanceof ButtonButton) {
        return "button";
    };
    if (ty instanceof ButtonSubmit) {
        return "submit";
    };
    if (ty instanceof ButtonReset) {
        return "reset";
    };
    throw new Error("Failed pattern match at Halogen.HTML.Properties.Indexed line 248, column 1 - line 249, column 1: " + [ ty.constructor.name ]);
};
var rel = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.rel);
var readonly = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.readonly);
var placeholder = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.placeholder);
var olType = function ($16) {
    return Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.type_)(renderOrderedListType($16));
};
var name = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.name);
var menuitemType = function ($17) {
    return Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.type_)(renderMenuitemType($17));
};
var menuType = function ($18) {
    return Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.type_)(renderMenuType($18));
};
var mediaType = function ($19) {
    return Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.type_)(renderMediaType($19));
};
var key = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.key);
var inputType = function ($20) {
    return Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.type_)(renderInputType($20));
};
var initializer = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.initializer);
var id_ = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.id_);
var href = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.href);
var height = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.height);
var $$for = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties["for"]);
var finalizer = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.finalizer);
var disabled = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.disabled);
var enabled = function ($21) {
    return disabled(!$21);
};
var colSpan = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.colSpan);
var classes = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.classes);
var class_ = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.class_);
var checked = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.checked);
var charset = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.charset);
var buttonType = function ($22) {
    return Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.type_)(renderButtonType($22));
};
var autocomplete = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.autocomplete);
var alt = Unsafe_Coerce.unsafeCoerce(Halogen_HTML_Properties.alt);
module.exports = {
    Uppercase: Uppercase, 
    Lowercase: Lowercase, 
    NumeralDecimal: NumeralDecimal, 
    NumeralRoman: NumeralRoman, 
    OrderedListNumeric: OrderedListNumeric, 
    OrderedListAlphabetic: OrderedListAlphabetic, 
    MenuitemCommand: MenuitemCommand, 
    MenuitemCheckbox: MenuitemCheckbox, 
    MenuitemRadio: MenuitemRadio, 
    MenuList: MenuList, 
    MenuContext: MenuContext, 
    MenuToolbar: MenuToolbar, 
    InputButton: InputButton, 
    InputCheckbox: InputCheckbox, 
    InputColor: InputColor, 
    InputDate: InputDate, 
    InputDatetime: InputDatetime, 
    InputDatetimeLocal: InputDatetimeLocal, 
    InputEmail: InputEmail, 
    InputFile: InputFile, 
    InputHidden: InputHidden, 
    InputImage: InputImage, 
    InputMonth: InputMonth, 
    InputNumber: InputNumber, 
    InputPassword: InputPassword, 
    InputRadio: InputRadio, 
    InputRange: InputRange, 
    InputReset: InputReset, 
    InputSearch: InputSearch, 
    InputSubmit: InputSubmit, 
    InputTel: InputTel, 
    InputText: InputText, 
    InputTime: InputTime, 
    InputUrl: InputUrl, 
    InputWeek: InputWeek, 
    ButtonButton: ButtonButton, 
    ButtonSubmit: ButtonSubmit, 
    ButtonReset: ButtonReset, 
    IProp: IProp, 
    finalizer: finalizer, 
    initializer: initializer, 
    placeholder: placeholder, 
    selected: selected, 
    checked: checked, 
    spellcheck: spellcheck, 
    readonly: readonly, 
    required: required, 
    enabled: enabled, 
    disabled: disabled, 
    value: value, 
    olType: olType, 
    menuitemType: menuitemType, 
    menuType: menuType, 
    mediaType: mediaType, 
    inputType: inputType, 
    buttonType: buttonType, 
    title: title, 
    target: target, 
    src: src, 
    rel: rel, 
    name: name, 
    id_: id_, 
    href: href, 
    width: width, 
    height: height, 
    "for": $$for, 
    rowSpan: rowSpan, 
    colSpan: colSpan, 
    classes: classes, 
    class_: class_, 
    charset: charset, 
    alt: alt, 
    key: key
};

},{"DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js","Data.Array":"/Users/anders/dev/ps/halo/ajax/output/Data.Array/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.HTML.Properties":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Properties/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js","Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Properties/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Data_String = require("Data.String");
var DOM_HTML_Types = require("DOM.HTML.Types");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var Pixels = (function () {
    function Pixels(value0) {
        this.value0 = value0;
    };
    Pixels.create = function (value0) {
        return new Pixels(value0);
    };
    return Pixels;
})();
var Percent = (function () {
    function Percent(value0) {
        this.value0 = value0;
    };
    Percent.create = function (value0) {
        return new Percent(value0);
    };
    return Percent;
})();
var value = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("value"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("value")));
var type_ = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("type"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("type")));
var title = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("title"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("title")));
var target = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("target"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("target")));
var src = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("src"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("src")));
var spellcheck = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("spellcheck"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("spellcheck")));
var selected = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("selected"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("selected")));
var rowSpan = Halogen_HTML_Core.prop(Halogen_HTML_Core.intIsProp)(Halogen_HTML_Core.propName("rowSpan"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("rowspan")));
var required = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("required"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("required")));
var rel = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("rel"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("rel")));
var readonly = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("readonly"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("readonly")));
var placeholder = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("placeholder"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("placeholder")));
var name = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("name"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("name")));
var lengthLiteralIsProp = new Halogen_HTML_Core.IsProp(function (v) {
    return function (v1) {
        return function (lit) {
            if (lit instanceof Pixels) {
                return Prelude.show(Prelude.showNumber)(lit.value0);
            };
            if (lit instanceof Percent) {
                return Prelude.show(Prelude.showNumber)(lit.value0) + "%";
            };
            throw new Error("Failed pattern match at Halogen.HTML.Properties line 49, column 1 - line 57, column 1: " + [ lit.constructor.name ]);
        };
    };
});
var width = Halogen_HTML_Core.prop(lengthLiteralIsProp)(Halogen_HTML_Core.propName("width"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("width")));
var key = Halogen_HTML_Core.Key.create;
var initializer = Halogen_HTML_Core.Initializer.create;
var id_ = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("id"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("id")));
var href = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("href"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("href")));
var height = Halogen_HTML_Core.prop(lengthLiteralIsProp)(Halogen_HTML_Core.propName("height"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("height")));
var $$for = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("htmlFor"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("for")));
var finalizer = Halogen_HTML_Core.Finalizer.create;
var disabled = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("disabled"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("disabled")));
var enabled = function ($6) {
    return disabled(!$6);
};
var colSpan = Halogen_HTML_Core.prop(Halogen_HTML_Core.intIsProp)(Halogen_HTML_Core.propName("colSpan"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("colspan")));
var classes = function ($7) {
    return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("className"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("class")))(Data_String.joinWith(" ")(Prelude.map(Prelude.functorArray)(Halogen_HTML_Core.runClassName)($7)));
};
var class_ = function ($8) {
    return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("className"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("class")))(Halogen_HTML_Core.runClassName($8));
};
var checked = Halogen_HTML_Core.prop(Halogen_HTML_Core.booleanIsProp)(Halogen_HTML_Core.propName("checked"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("checked")));
var charset = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("charset"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("charset")));
var autocomplete = function ($9) {
    return Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("autocomplete"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("autocomplete")))((function (b) {
        if (b) {
            return "on";
        };
        if (!b) {
            return "off";
        };
        throw new Error("Failed pattern match at Halogen.HTML.Properties line 139, column 1 - line 140, column 1: " + [ b.constructor.name ]);
    })($9));
};
var alt = Halogen_HTML_Core.prop(Halogen_HTML_Core.stringIsProp)(Halogen_HTML_Core.propName("alt"))(Data_Maybe.Just.create(Halogen_HTML_Core.attrName("alt")));
module.exports = {
    Pixels: Pixels, 
    Percent: Percent, 
    finalizer: finalizer, 
    initializer: initializer, 
    autocomplete: autocomplete, 
    placeholder: placeholder, 
    selected: selected, 
    checked: checked, 
    enabled: enabled, 
    spellcheck: spellcheck, 
    readonly: readonly, 
    required: required, 
    disabled: disabled, 
    width: width, 
    value: value, 
    type_: type_, 
    title: title, 
    target: target, 
    src: src, 
    rel: rel, 
    name: name, 
    id_: id_, 
    href: href, 
    height: height, 
    "for": $$for, 
    rowSpan: rowSpan, 
    colSpan: colSpan, 
    classes: classes, 
    class_: class_, 
    charset: charset, 
    alt: alt, 
    key: key, 
    lengthLiteralIsProp: lengthLiteralIsProp
};

},{"DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.String":"/Users/anders/dev/ps/halo/ajax/output/Data.String/index.js","Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Renderer.VirtualDOM/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Exception = require("Control.Monad.Eff.Exception");
var Data_Exists = require("Data.Exists");
var Data_ExistsR = require("Data.ExistsR");
var Data_Foldable = require("Data.Foldable");
var Data_Function = require("Data.Function");
var Data_Maybe = require("Data.Maybe");
var Data_Monoid = require("Data.Monoid");
var Data_Nullable = require("Data.Nullable");
var Halogen_Effects = require("Halogen.Effects");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var Halogen_HTML_Events_Handler = require("Halogen.HTML.Events.Handler");
var Halogen_Internal_VirtualDOM = require("Halogen.Internal.VirtualDOM");
var handleAff = Control_Monad_Aff.runAff(Control_Monad_Eff_Exception.throwException)(Prelude["const"](Prelude.pure(Control_Monad_Eff.applicativeEff)(Prelude.unit)));
var renderProp = function (dr) {
    return function (v) {
        if (v instanceof Halogen_HTML_Core.Prop) {
            return Data_Exists.runExists(function (v1) {
                return Halogen_Internal_VirtualDOM.prop(Halogen_HTML_Core.runPropName(v1.value0), v1.value1);
            })(v.value0);
        };
        if (v instanceof Halogen_HTML_Core.Attr) {
            var attrName = Data_Maybe.maybe("")(function (nsʹ) {
                return Halogen_HTML_Core.runNamespace(nsʹ) + ":";
            })(v.value0) + Halogen_HTML_Core.runAttrName(v.value1);
            return Halogen_Internal_VirtualDOM.attr(attrName, v.value2);
        };
        if (v instanceof Halogen_HTML_Core.Handler) {
            return Data_ExistsR.runExistsR(function (v1) {
                return Halogen_Internal_VirtualDOM.handlerProp(Halogen_HTML_Core.runEventName(v1.value0), function (ev) {
                    return handleAff(Prelude[">>="](Control_Monad_Aff.bindAff)(Halogen_HTML_Events_Handler.runEventHandler(Control_Monad_Aff.monadAff)(Control_Monad_Aff.monadEffAff)(ev)(v1.value1(ev)))(Data_Maybe.maybe(Prelude.pure(Control_Monad_Aff.applicativeAff)(Prelude.unit))(dr)));
                });
            })(v.value0);
        };
        if (v instanceof Halogen_HTML_Core.Initializer) {
            return Halogen_Internal_VirtualDOM.initProp(function ($31) {
                return handleAff(dr(v.value0($31)));
            });
        };
        if (v instanceof Halogen_HTML_Core.Finalizer) {
            return Halogen_Internal_VirtualDOM.finalizerProp(function ($32) {
                return handleAff(dr(v.value0($32)));
            });
        };
        return Data_Monoid.mempty(Halogen_Internal_VirtualDOM.monoidProps);
    };
};
var findKey = function (r) {
    return function (v) {
        if (v instanceof Halogen_HTML_Core.Key) {
            return new Data_Maybe.Just(v.value0);
        };
        return r;
    };
};
var renderHTML = function (f) {
    var go = function (v) {
        if (v instanceof Halogen_HTML_Core.Text) {
            return Halogen_Internal_VirtualDOM.vtext(v.value0);
        };
        if (v instanceof Halogen_HTML_Core.Element) {
            var tag = Halogen_HTML_Core.runTagName(v.value1);
            var nsʹ = Data_Nullable.toNullable(Prelude["<$>"](Data_Maybe.functorMaybe)(Halogen_HTML_Core.runNamespace)(v.value0));
            var key = Data_Nullable.toNullable(Data_Foldable.foldl(Data_Foldable.foldableArray)(findKey)(Data_Maybe.Nothing.value)(v.value2));
            return Halogen_Internal_VirtualDOM.vnode(nsʹ)(tag)(key)(Data_Foldable.foldMap(Data_Foldable.foldableArray)(Halogen_Internal_VirtualDOM.monoidProps)(renderProp(f))(v.value2))(Prelude.map(Prelude.functorArray)(go)(v.value3));
        };
        if (v instanceof Halogen_HTML_Core.Slot) {
            return Halogen_Internal_VirtualDOM.vtext("");
        };
        throw new Error("Failed pattern match at Halogen.HTML.Renderer.VirtualDOM line 27, column 1 - line 28, column 1: " + [ v.constructor.name ]);
    };
    return go;
};
module.exports = {
    renderHTML: renderHTML
};

},{"Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Exception":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/index.js","Data.Exists":"/Users/anders/dev/ps/halo/ajax/output/Data.Exists/index.js","Data.ExistsR":"/Users/anders/dev/ps/halo/ajax/output/Data.ExistsR/index.js","Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Nullable":"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/index.js","Halogen.Effects":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Effects/index.js","Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.HTML.Events.Handler":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Handler/index.js","Halogen.Internal.VirtualDOM":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Internal.VirtualDOM/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Halogen_Component = require("Halogen.Component");
var Halogen_Component_ChildPath = require("Halogen.Component.ChildPath");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
var Halogen_HTML_Elements = require("Halogen.HTML.Elements");
var text = Halogen_HTML_Core.Text.create;
var slot$prime = function (dictFunctor) {
    return function (i) {
        return function (p) {
            return function (l) {
                var transform = function (def) {
                    return {
                        component: Halogen_Component.transformChild(dictFunctor)(i)(def.component), 
                        initialState: Halogen_Component_ChildPath.injState(i)(def.initialState)
                    };
                };
                return new Halogen_HTML_Core.Slot(new Halogen_Component.SlotConstructor(Halogen_Component_ChildPath.injSlot(i)(p), Prelude["<$>"](Prelude.functorFn)(transform)(l)));
            };
        };
    };
};
var slot = function (p) {
    return function (l) {
        return new Halogen_HTML_Core.Slot(new Halogen_Component.SlotConstructor(p, l));
    };
};
module.exports = {
    "slot'": slot$prime, 
    slot: slot, 
    text: text
};

},{"Halogen.Component":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Component/index.js","Halogen.Component.ChildPath":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Component.ChildPath/index.js","Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.HTML.Elements":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Elements/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Internal.VirtualDOM/foreign.js":[function(require,module,exports){
/* global exports, require */
"use strict";

// module Halogen.Internal.VirtualDOM

// jshint maxparams: 2
exports.prop = function (key, value) {
  var props = {};
  props[key] = value;
  return props;
};

// jshint maxparams: 2
exports.attr = function (key, value) {
  var props = { attributes: {} };
  props.attributes[key] = value;
  return props;
};

function HandlerHook (key, f) {
  this.key = key;
  this.callback = function (e) {
    f(e)();
  };
}

HandlerHook.prototype = {
  hook: function (node) {
    node.addEventListener(this.key, this.callback);
  },
  unhook: function (node) {
    node.removeEventListener(this.key, this.callback);
  }
};

// jshint maxparams: 2
exports.handlerProp = function (key, f) {
  var props = {};
  props["halogen-hook-" + key] = new HandlerHook(key, f);
  return props;
};

// jshint maxparams: 3
function ifHookFn (node, prop, diff) {
  // jshint validthis: true
  if (typeof diff === "undefined") {
    this.f(node)();
  }
}

// jshint maxparams: 1
function InitHook (f) {
  this.f = f;
}

InitHook.prototype = {
  hook: ifHookFn
};

exports.initProp = function (f) {
  return { "halogen-init": new InitHook(f) };
};

function FinalHook (f) {
  this.f = f;
}

FinalHook.prototype = {
  unhook: ifHookFn
};

exports.finalizerProp = function (f) {
  return { "halogen-final": new FinalHook(f) };
};

exports.concatProps = function () {
  // jshint maxparams: 2
  var hOP = Object.prototype.hasOwnProperty;
  var copy = function (props, result) {
    for (var key in props) {
      if (hOP.call(props, key)) {
        if (key === "attributes") {
          var attrs = props[key];
          var resultAttrs = result[key] || (result[key] = {});
          for (var attr in attrs) {
            if (hOP.call(attrs, attr)) {
              resultAttrs[attr] = attrs[attr];
            }
          }
        } else {
          result[key] = props[key];
        }
      }
    }
    return result;
  };
  return function (p1, p2) {
    return copy(p2, copy(p1, {}));
  };
}();

exports.emptyProps = {};

exports.createElement = function () {
  var vcreateElement = require("virtual-dom/create-element");
  return function (vtree) {
    return vcreateElement(vtree);
  };
}();

exports.diff = function () {
  var vdiff = require("virtual-dom/diff");
  return function (vtree1) {
    return function (vtree2) {
      return vdiff(vtree1, vtree2);
    };
  };
}();

exports.patch = function () {
  var vpatch = require("virtual-dom/patch");
  return function (p) {
    return function (node) {
      return function () {
        return vpatch(node, p);
      };
    };
  };
}();

exports.vtext = function () {
  var VText = require("virtual-dom/vnode/vtext");
  return function (s) {
    return new VText(s);
  };
}();

exports.vnode = function () {
  var VirtualNode = require("virtual-dom/vnode/vnode");
  var SoftSetHook = require("virtual-dom/virtual-hyperscript/hooks/soft-set-hook");
  return function (namespace) {
    return function (name) {
      return function (key) {
        return function (props) {
          return function (children) {
            if (name === "input" && props.value !== undefined) {
              props.value = new SoftSetHook(props.value);
            }
            return new VirtualNode(name, props, children, key, namespace);
          };
        };
      };
    };
  };
}();

},{"virtual-dom/create-element":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/create-element.js","virtual-dom/diff":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/diff.js","virtual-dom/patch":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/patch.js","virtual-dom/virtual-hyperscript/hooks/soft-set-hook":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/virtual-hyperscript/hooks/soft-set-hook.js","virtual-dom/vnode/vnode":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/vnode.js","virtual-dom/vnode/vtext":"/Users/anders/dev/ps/halo/ajax/node_modules/virtual-dom/vnode/vtext.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Internal.VirtualDOM/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Data_Monoid = require("Data.Monoid");
var Data_Nullable = require("Data.Nullable");
var Data_Function = require("Data.Function");
var DOM = require("DOM");
var DOM_HTML_Types = require("DOM.HTML.Types");
var semigroupProps = new Prelude.Semigroup(Data_Function.runFn2($foreign.concatProps));
var monoidProps = new Data_Monoid.Monoid(function () {
    return semigroupProps;
}, $foreign.emptyProps);
module.exports = {
    semigroupProps: semigroupProps, 
    monoidProps: monoidProps, 
    vnode: $foreign.vnode, 
    vtext: $foreign.vtext, 
    patch: $foreign.patch, 
    diff: $foreign.diff, 
    createElement: $foreign.createElement, 
    finalizerProp: $foreign.finalizerProp, 
    initProp: $foreign.initProp, 
    handlerProp: $foreign.handlerProp, 
    attr: $foreign.attr, 
    prop: $foreign.prop
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Internal.VirtualDOM/foreign.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","Data.Nullable":"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query.StateF/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Monad_State = require("Control.Monad.State");
var Data_Functor = require("Data.Functor");
var Data_NaturalTransformation = require("Data.NaturalTransformation");
var Control_Monad_State_Class = require("Control.Monad.State.Class");
var Get = (function () {
    function Get(value0) {
        this.value0 = value0;
    };
    Get.create = function (value0) {
        return new Get(value0);
    };
    return Get;
})();
var Modify = (function () {
    function Modify(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Modify.create = function (value0) {
        return function (value1) {
            return new Modify(value0, value1);
        };
    };
    return Modify;
})();
var stateN = function (dictMonad) {
    return function (dictMonadState) {
        return function (v) {
            if (v instanceof Get) {
                return Prelude[">>="](dictMonad["__superclass_Prelude.Bind_1"]())(Control_Monad_State_Class.get(dictMonadState))(function ($22) {
                    return Prelude.pure(dictMonad["__superclass_Prelude.Applicative_0"]())(v.value0($22));
                });
            };
            if (v instanceof Modify) {
                return Data_Functor["$>"](((dictMonad["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Control_Monad_State_Class.modify(dictMonadState)(v.value0))(v.value1);
            };
            throw new Error("Failed pattern match at Halogen.Query.StateF line 33, column 1 - line 34, column 1: " + [ v.constructor.name ]);
        };
    };
};
var mapState = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Get) {
                return new Get(function ($23) {
                    return v2.value0(v($23));
                });
            };
            if (v2 instanceof Modify) {
                return new Modify(v1(v2.value0), v2.value1);
            };
            throw new Error("Failed pattern match at Halogen.Query.StateF line 27, column 1 - line 28, column 1: " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var functorStateF = new Prelude.Functor(function (f) {
    return function (v) {
        if (v instanceof Get) {
            return new Get(function ($24) {
                return f(v.value0($24));
            });
        };
        if (v instanceof Modify) {
            return new Modify(v.value0, f(v.value1));
        };
        throw new Error("Failed pattern match at Halogen.Query.StateF line 21, column 1 - line 27, column 1: " + [ f.constructor.name, v.constructor.name ]);
    };
});
module.exports = {
    Get: Get, 
    Modify: Modify, 
    stateN: stateN, 
    mapState: mapState, 
    functorStateF: functorStateF
};

},{"Control.Monad.State":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State/index.js","Control.Monad.State.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.State.Class/index.js","Data.Functor":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor/index.js","Data.NaturalTransformation":"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query.SubscribeF/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Bind = require("Control.Bind");
var Control_Coroutine = require("Control.Coroutine");
var Control_Coroutine_Stalling = require("Control.Coroutine.Stalling");
var Control_Coroutine_Aff = require("Control.Coroutine.Aff");
var Control_Monad_Free_Trans = require("Control.Monad.Free.Trans");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("Control.Monad.Aff.AVar");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Rec_Class = require("Control.Monad.Rec.Class");
var Control_Plus = require("Control.Plus");
var Data_Bifunctor = require("Data.Bifunctor");
var Data_Const = require("Data.Const");
var Data_Either = require("Data.Either");
var Data_Functor = require("Data.Functor");
var Data_Functor_Coproduct = require("Data.Functor.Coproduct");
var Data_Identity = require("Data.Identity");
var Data_Maybe = require("Data.Maybe");
var Data_NaturalTransformation = require("Data.NaturalTransformation");
var Subscribe = (function () {
    function Subscribe(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Subscribe.create = function (value0) {
        return function (value1) {
            return new Subscribe(value0, value1);
        };
    };
    return Subscribe;
})();
var transformSubscribe = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (eta) {
            return function (theta) {
                return function (v) {
                    return new Subscribe(Control_Monad_Free_Trans.hoistFreeT(Control_Coroutine_Stalling.functorStallF)(dictFunctor1)(theta)(Control_Monad_Free_Trans.interpret(Control_Coroutine_Stalling.functorStallF)(dictFunctor)(Data_Bifunctor.lmap(Control_Coroutine_Stalling.bifunctorStallF)(eta))(v.value0)), v.value1);
                };
            };
        };
    };
};
var subscribeN = function (dictMonadRec) {
    return function (c) {
        return function (v) {
            return Data_Functor["$>"]((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Control_Coroutine_Stalling.runStallingProcess(dictMonadRec)(Control_Coroutine_Stalling["$$?"](dictMonadRec)(v.value0)(c)))(v.value1);
        };
    };
};
var remapSubscribe = function (dictFunctor) {
    return function (eta) {
        return function (v) {
            return new Subscribe(Control_Monad_Free_Trans.interpret(Control_Coroutine_Stalling.functorStallF)(dictFunctor)(Data_Bifunctor.lmap(Control_Coroutine_Stalling.bifunctorStallF)(eta))(v.value0), v.value1);
        };
    };
};
var hoistSubscribe = function (dictFunctor) {
    return function (eta) {
        return function (v) {
            return new Subscribe(Control_Monad_Free_Trans.hoistFreeT(Control_Coroutine_Stalling.functorStallF)(dictFunctor)(eta)(v.value0), v.value1);
        };
    };
};
var functorSubscribeF = new Prelude.Functor(function (f) {
    return function (v) {
        return new Subscribe(v.value0, f(v.value1));
    };
});
var eventSource_ = function (attach) {
    return function (handle) {
        return Control_Coroutine_Stalling.producerToStallingProducer(Control_Monad_Aff.functorAff)(Control_Coroutine_Aff.produce(function (emit) {
            return attach(Control_Bind["=<<"](Control_Monad_Eff.bindEff)(function ($34) {
                return emit(Data_Either.Left.create($34));
            })(handle));
        }));
    };
};
var eventSource = function (attach) {
    return function (handle) {
        return Control_Coroutine_Stalling.producerToStallingProducer(Control_Monad_Aff.functorAff)(Control_Coroutine_Aff.produce(function (emit) {
            return attach(Control_Bind["<=<"](Control_Monad_Eff.bindEff)(function ($35) {
                return emit(Data_Either.Left.create($35));
            })(handle));
        }));
    };
};
var catEventSource = function (dictMonadRec) {
    return function ($36) {
        return Control_Coroutine_Stalling.catMaybes(dictMonadRec)(Control_Coroutine_Stalling.mapStallingProducer((((dictMonadRec["__superclass_Prelude.Monad_0"]())["__superclass_Prelude.Bind_1"]())["__superclass_Prelude.Apply_0"]())["__superclass_Prelude.Functor_0"]())(Data_Functor_Coproduct.coproduct(function (v) {
            return Data_Maybe.Nothing.value;
        })(Data_Maybe.Just.create))($36));
    };
};
module.exports = {
    Subscribe: Subscribe, 
    subscribeN: subscribeN, 
    transformSubscribe: transformSubscribe, 
    hoistSubscribe: hoistSubscribe, 
    remapSubscribe: remapSubscribe, 
    catEventSource: catEventSource, 
    eventSource_: eventSource_, 
    eventSource: eventSource, 
    functorSubscribeF: functorSubscribeF
};

},{"Control.Bind":"/Users/anders/dev/ps/halo/ajax/output/Control.Bind/index.js","Control.Coroutine":"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine/index.js","Control.Coroutine.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine.Aff/index.js","Control.Coroutine.Stalling":"/Users/anders/dev/ps/halo/ajax/output/Control.Coroutine.Stalling/index.js","Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Aff.AVar":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/index.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Free.Trans":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free.Trans/index.js","Control.Monad.Rec.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Rec.Class/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Bifunctor":"/Users/anders/dev/ps/halo/ajax/output/Data.Bifunctor/index.js","Data.Const":"/Users/anders/dev/ps/halo/ajax/output/Data.Const/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Functor":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor/index.js","Data.Functor.Coproduct":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor.Coproduct/index.js","Data.Identity":"/Users/anders/dev/ps/halo/ajax/output/Data.Identity/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.NaturalTransformation":"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Plus = require("Control.Plus");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Aff_Class = require("Control.Monad.Aff.Class");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Free = require("Control.Monad.Free");
var Data_Inject = require("Data.Inject");
var Data_Maybe = require("Data.Maybe");
var Data_NaturalTransformation = require("Data.NaturalTransformation");
var Halogen_Query_StateF = require("Halogen.Query.StateF");
var Halogen_Query_SubscribeF = require("Halogen.Query.SubscribeF");
var StateHF = (function () {
    function StateHF(value0) {
        this.value0 = value0;
    };
    StateHF.create = function (value0) {
        return new StateHF(value0);
    };
    return StateHF;
})();
var SubscribeHF = (function () {
    function SubscribeHF(value0) {
        this.value0 = value0;
    };
    SubscribeHF.create = function (value0) {
        return new SubscribeHF(value0);
    };
    return SubscribeHF;
})();
var QueryHF = (function () {
    function QueryHF(value0) {
        this.value0 = value0;
    };
    QueryHF.create = function (value0) {
        return new QueryHF(value0);
    };
    return QueryHF;
})();
var HaltHF = (function () {
    function HaltHF() {

    };
    HaltHF.value = new HaltHF();
    return HaltHF;
})();
var subscribe = function (p) {
    return Control_Monad_Free.liftF(new SubscribeHF(new Halogen_Query_SubscribeF.Subscribe(p, Prelude.unit)));
};
var request = function (req) {
    return req(Prelude.id(Prelude.categoryFn));
};
var modify = function (f) {
    return Control_Monad_Free.liftF(new StateHF(new Halogen_Query_StateF.Modify(f, Prelude.unit)));
};
var liftH = function ($34) {
    return Control_Monad_Free.liftF(QueryHF.create($34));
};
var liftEff$prime = function (dictMonadEff) {
    return function (dictFunctor) {
        return function ($35) {
            return liftH(Control_Monad_Eff_Class.liftEff(dictMonadEff)($35));
        };
    };
};
var liftAff$prime = function (dictMonadAff) {
    return function (dictFunctor) {
        return function ($36) {
            return liftH(Control_Monad_Aff_Class.liftAff(dictMonadAff)($36));
        };
    };
};
var injectSubscribeHF = new Data_Inject.Inject(SubscribeHF.create, function (v) {
    if (v instanceof SubscribeHF) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
});
var injectStateHF = new Data_Inject.Inject(StateHF.create, function (v) {
    if (v instanceof StateHF) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
});
var injectQueryHF = new Data_Inject.Inject(QueryHF.create, function (v) {
    if (v instanceof QueryHF) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
});
var gets = function ($37) {
    return Control_Monad_Free.liftF(StateHF.create(Halogen_Query_StateF.Get.create($37)));
};
var get = gets(Prelude.id(Prelude.categoryFn));
var functorHalogenF = function (dictFunctor) {
    return new Prelude.Functor(function (f) {
        return function (h) {
            if (h instanceof StateHF) {
                return new StateHF(Prelude.map(Halogen_Query_StateF.functorStateF)(f)(h.value0));
            };
            if (h instanceof SubscribeHF) {
                return new SubscribeHF(Prelude.map(Halogen_Query_SubscribeF.functorSubscribeF)(f)(h.value0));
            };
            if (h instanceof QueryHF) {
                return new QueryHF(Prelude.map(dictFunctor)(f)(h.value0));
            };
            if (h instanceof HaltHF) {
                return HaltHF.value;
            };
            throw new Error("Failed pattern match at Halogen.Query line 101, column 1 - line 109, column 1: " + [ h.constructor.name ]);
        };
    });
};
var altHalogenF = function (dictFunctor) {
    return new Control_Alt.Alt(function () {
        return functorHalogenF(dictFunctor);
    }, function (v) {
        return function (h) {
            if (v instanceof HaltHF) {
                return h;
            };
            return v;
        };
    });
};
var plusHalogenF = function (dictFunctor) {
    return new Control_Plus.Plus(function () {
        return altHalogenF(dictFunctor);
    }, HaltHF.value);
};
var hoistHalogenF = function (dictFunctor) {
    return function (eta) {
        return function (h) {
            if (h instanceof StateHF) {
                return new StateHF(h.value0);
            };
            if (h instanceof SubscribeHF) {
                return new SubscribeHF(Halogen_Query_SubscribeF.hoistSubscribe(dictFunctor)(eta)(h.value0));
            };
            if (h instanceof QueryHF) {
                return new QueryHF(eta(h.value0));
            };
            if (h instanceof HaltHF) {
                return Control_Plus.empty(plusHalogenF(dictFunctor));
            };
            throw new Error("Failed pattern match at Halogen.Query line 218, column 1 - line 219, column 1: " + [ h.constructor.name ]);
        };
    };
};
var transformHF = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (sigma) {
            return function (phi) {
                return function (gamma) {
                    return function (h) {
                        if (h instanceof StateHF) {
                            return new StateHF(sigma(h.value0));
                        };
                        if (h instanceof SubscribeHF) {
                            return new SubscribeHF(Halogen_Query_SubscribeF.transformSubscribe(dictFunctor)(dictFunctor1)(phi)(gamma)(h.value0));
                        };
                        if (h instanceof QueryHF) {
                            return new QueryHF(gamma(h.value0));
                        };
                        if (h instanceof HaltHF) {
                            return Control_Plus.empty(plusHalogenF(dictFunctor1));
                        };
                        throw new Error("Failed pattern match at Halogen.Query line 203, column 1 - line 210, column 1: " + [ h.constructor.name ]);
                    };
                };
            };
        };
    };
};
var action = function (act) {
    return act(Prelude.unit);
};
module.exports = {
    StateHF: StateHF, 
    SubscribeHF: SubscribeHF, 
    QueryHF: QueryHF, 
    HaltHF: HaltHF, 
    hoistHalogenF: hoistHalogenF, 
    transformHF: transformHF, 
    "liftEff'": liftEff$prime, 
    "liftAff'": liftAff$prime, 
    liftH: liftH, 
    subscribe: subscribe, 
    modify: modify, 
    gets: gets, 
    get: get, 
    request: request, 
    action: action, 
    functorHalogenF: functorHalogenF, 
    injectStateHF: injectStateHF, 
    injectSubscribeHF: injectSubscribeHF, 
    injectQueryHF: injectQueryHF, 
    altHalogenF: altHalogenF, 
    plusHalogenF: plusHalogenF
};

},{"Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Aff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.Class/index.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Free":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free/index.js","Control.Plus":"/Users/anders/dev/ps/halo/ajax/output/Control.Plus/index.js","Data.Inject":"/Users/anders/dev/ps/halo/ajax/output/Data.Inject/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.NaturalTransformation":"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js","Halogen.Query.StateF":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query.StateF/index.js","Halogen.Query.SubscribeF":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query.SubscribeF/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen.Util/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Control_Bind = require("Control.Bind");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Data_Maybe = require("Data.Maybe");
var Data_Nullable = require("Data.Nullable");
var DOM = require("DOM");
var DOM_Event_EventTarget = require("DOM.Event.EventTarget");
var DOM_Event_EventTypes = require("DOM.Event.EventTypes");
var DOM_Event_Types = require("DOM.Event.Types");
var DOM_HTML = require("DOM.HTML");
var DOM_HTML_Types = require("DOM.HTML.Types");
var DOM_HTML_Window = require("DOM.HTML.Window");
var DOM_Node_Node = require("DOM.Node.Node");
var DOM_Node_ParentNode = require("DOM.Node.ParentNode");
var DOM_Node_Types = require("DOM.Node.Types");
var onLoad = function (dictMonadEff) {
    return function (callback) {
        return Control_Monad_Eff_Class.liftEff(dictMonadEff)(Control_Bind["=<<"](Control_Monad_Eff.bindEff)(function ($9) {
            return DOM_Event_EventTarget.addEventListener(DOM_Event_EventTypes.load)(DOM_Event_EventTarget.eventListener(function (v) {
                return callback;
            }))(false)(DOM_HTML_Types.windowToEventTarget($9));
        })(DOM_HTML.window));
    };
};
var appendTo = function (dictMonadEff) {
    return function (query) {
        return function (elem) {
            return Control_Monad_Eff_Class.liftEff(dictMonadEff)(function __do() {
                var v = Prelude["<$>"](Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)(Control_Bind["=<<"](Control_Monad_Eff.bindEff)(Control_Bind["<=<"](Control_Monad_Eff.bindEff)(function ($10) {
                    return DOM_Node_ParentNode.querySelector(query)(DOM_HTML_Types.htmlDocumentToParentNode($10));
                })(DOM_HTML_Window.document))(DOM_HTML.window))();
                return (function () {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Prelude.pure(Control_Monad_Eff.applicativeEff)(Prelude.unit);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return Prelude["void"](Control_Monad_Eff.functorEff)(DOM_Node_Node.appendChild(DOM_HTML_Types.htmlElementToNode(elem))(DOM_Node_Types.elementToNode(v.value0)));
                    };
                    throw new Error("Failed pattern match at Halogen.Util line 29, column 1 - line 31, column 1: " + [ v.constructor.name ]);
                })()();
            });
        };
    };
};
var appendToBody = function (dictMonadEff) {
    return appendTo(dictMonadEff)("body");
};
module.exports = {
    onLoad: onLoad, 
    appendToBody: appendToBody, 
    appendTo: appendTo
};

},{"Control.Bind":"/Users/anders/dev/ps/halo/ajax/output/Control.Bind/index.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","DOM":"/Users/anders/dev/ps/halo/ajax/output/DOM/index.js","DOM.Event.EventTarget":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.EventTarget/index.js","DOM.Event.EventTypes":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.EventTypes/index.js","DOM.Event.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Event.Types/index.js","DOM.HTML":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML/index.js","DOM.HTML.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Types/index.js","DOM.HTML.Window":"/Users/anders/dev/ps/halo/ajax/output/DOM.HTML.Window/index.js","DOM.Node.Node":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Node/index.js","DOM.Node.ParentNode":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.ParentNode/index.js","DOM.Node.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Types/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Nullable":"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Halogen/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_NaturalTransformation = require("Data.NaturalTransformation");
var Halogen_Component = require("Halogen.Component");
var Halogen_Driver = require("Halogen.Driver");
var Halogen_Effects = require("Halogen.Effects");
var Halogen_Query = require("Halogen.Query");
var Halogen_HTML_Core = require("Halogen.HTML.Core");
module.exports = {};

},{"Data.NaturalTransformation":"/Users/anders/dev/ps/halo/ajax/output/Data.NaturalTransformation/index.js","Halogen.Component":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Component/index.js","Halogen.Driver":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Driver/index.js","Halogen.Effects":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Effects/index.js","Halogen.HTML.Core":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Core/index.js","Halogen.Query":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Main/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("../Prelude");
var Control_Alt = require("../Control.Alt");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Free = require("../Control.Monad.Free");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Foreign_Class = require("../Data.Foreign.Class");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Halogen = require("../Halogen");
var Halogen_Util = require("../Halogen.Util");
var Halogen_HTML_Indexed = require("../Halogen.HTML.Indexed");
var Halogen_HTML_Events_Indexed = require("../Halogen.HTML.Events.Indexed");
var Halogen_HTML_Properties_Indexed = require("../Halogen.HTML.Properties.Indexed");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var Data_JSON = require("../Data.JSON");
var Data_Tuple = require("../Data.Tuple");
var Data_Map = require("../Data.Map");
var Network_HTTP_Affjax_Request = require("../Network.HTTP.Affjax.Request");
var Network_HTTP_Affjax_Response = require("../Network.HTTP.Affjax.Response");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements");
var Halogen_HTML = require("../Halogen.HTML");
var Halogen_HTML_Elements_Indexed = require("../Halogen.HTML.Elements.Indexed");
var Halogen_HTML_Events = require("../Halogen.HTML.Events");
var Data_Monoid = require("../Data.Monoid");
var Halogen_Query = require("../Halogen.Query");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Halogen_Component = require("../Halogen.Component");
var Halogen_Driver = require("../Halogen.Driver");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var SetPayload = (function () {
    function SetPayload(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SetPayload.create = function (value0) {
        return function (value1) {
            return new SetPayload(value0, value1);
        };
    };
    return SetPayload;
})();
var SetUser = (function () {
    function SetUser(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SetUser.create = function (value0) {
        return function (value1) {
            return new SetUser(value0, value1);
        };
    };
    return SetUser;
})();
var MakeRequest = (function () {
    function MakeRequest(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    MakeRequest.create = function (value0) {
        return function (value1) {
            return new MakeRequest(value0, value1);
        };
    };
    return MakeRequest;
})();
var userRequest = function (name) {
    var jsonData = new Data_Tuple.Tuple("useridkey", name);
    return Data_JSON.encode(Data_JSON.tupleToJSON(Data_JSON.stringToJSON)(Data_JSON.stringToJSON))(jsonData);
};
var initialState = {
    busy: false, 
    payload: userRequest("Jack"), 
    user: "Jack", 
    result: Data_Maybe.Nothing.value
};
var firebaseUrl = "https://samplechat.firebaseio-demo.com/message_list.json";
var fetchJS = function (payload) {
    return Prelude.bind(Control_Monad_Aff.bindAff)(Network_HTTP_Affjax.post(Network_HTTP_Affjax_Request.requestableString)(Network_HTTP_Affjax_Response.responsableString)(firebaseUrl)(payload))(function (v) {
        return Prelude["return"](Control_Monad_Aff.applicativeAff)(v.response);
    });
};
var ui = (function () {
    var render = function (st) {
        return Halogen_HTML_Elements.div_(Prelude["++"](Prelude.semigroupArray)([ Halogen_HTML_Elements.h1_([ Halogen_HTML.text("Halogen Ajax") ]), Halogen_HTML_Elements.h2_([ Halogen_HTML.text("Meddelande:") ]), Halogen_HTML_Elements_Indexed.input([ Halogen_HTML_Properties_Indexed.id_("msgdata"), Halogen_HTML_Properties_Indexed.value(st.user), Halogen_HTML_Events_Indexed.onValueInput(Halogen_HTML_Events.input(SetUser.create)) ]), Halogen_HTML_Elements.p_([ Halogen_HTML.text(st.user) ]), Halogen_HTML_Elements.p_([ Halogen_HTML_Elements_Indexed.button([ Halogen_HTML_Properties_Indexed.disabled(st.busy), Halogen_HTML_Events_Indexed.onClick(Halogen_HTML_Events.input_(MakeRequest.create(st.payload))) ])([ Halogen_HTML.text("Skicka") ]) ]), Halogen_HTML_Elements.p_([ Halogen_HTML.text((function () {
            if (st.busy) {
                return "Working...";
            };
            if (!st.busy) {
                return "";
            };
            throw new Error("Failed pattern match at Main line 100, column 3 - line 101, column 3: " + [ st.busy.constructor.name ]);
        })()) ]) ])(Prelude.flip(Data_Foldable.foldMap(Data_Foldable.foldableMaybe)(Data_Monoid.monoidArray))(st.result)(function (js) {
            return [ Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.h2_([ Halogen_HTML.text("Svar:") ]), Halogen_HTML_Elements.pre_([ Halogen_HTML_Elements.code_([ Halogen_HTML.text(js) ]) ]) ]) ];
        })));
    };
    var $$eval = function (v) {
        if (v instanceof SetPayload) {
            return Data_Functor["$>"](Control_Monad_Free.freeFunctor)(Halogen_Query.modify(function (v1) {
                var $11 = {};
                for (var $12 in v1) {
                    if (v1.hasOwnProperty($12)) {
                        $11[$12] = v1[$12];
                    };
                };
                $11.payload = v.value0;
                $11.result = Data_Maybe.Nothing.value;
                return $11;
            }))(v.value1);
        };
        if (v instanceof SetUser) {
            return Data_Functor["$>"](Control_Monad_Free.freeFunctor)(Halogen_Query.modify(function (v1) {
                var $15 = {};
                for (var $16 in v1) {
                    if (v1.hasOwnProperty($16)) {
                        $15[$16] = v1[$16];
                    };
                };
                $15.user = v.value0;
                return $15;
            }))(v.value1);
        };
        if (v instanceof MakeRequest) {
            return Prelude.bind(Control_Monad_Free.freeBind)(Halogen_Query.modify(function (v1) {
                var $19 = {};
                for (var $20 in v1) {
                    if (v1.hasOwnProperty($20)) {
                        $19[$20] = v1[$20];
                    };
                };
                $19.busy = true;
                return $19;
            }))(function () {
                return Prelude.bind(Control_Monad_Free.freeBind)(Halogen_Query["liftAff'"](Control_Monad_Aff_Class.monadAffAff)(Control_Monad_Aff.functorAff)(fetchJS(v.value0)))(function (v1) {
                    return Prelude.bind(Control_Monad_Free.freeBind)(Halogen_Query.modify(function (v2) {
                        var $22 = {};
                        for (var $23 in v2) {
                            if (v2.hasOwnProperty($23)) {
                                $22[$23] = v2[$23];
                            };
                        };
                        $22.busy = false;
                        $22.result = new Data_Maybe.Just(v1);
                        return $22;
                    }))(function () {
                        return Prelude.pure(Control_Monad_Free.freeApplicative)(v.value1);
                    });
                });
            });
        };
        throw new Error("Failed pattern match at Main line 59, column 1 - line 60, column 1: " + [ v.constructor.name ]);
    };
    return Halogen_Component.component(render)($$eval);
})();
var main = Control_Monad_Aff.runAff(Control_Monad_Eff_Exception.throwException)(Prelude["const"](Prelude.pure(Control_Monad_Eff.applicativeEff)(Prelude.unit)))(Prelude.bind(Control_Monad_Aff.bindAff)(Halogen_Driver.runUI(ui)(initialState))(function (v) {
    return Halogen_Util.onLoad(Control_Monad_Aff.monadEffAff)(Halogen_Util.appendToBody(Control_Monad_Eff_Class.monadEffEff)(v.node));
}));
module.exports = {
    SetPayload: SetPayload, 
    SetUser: SetUser, 
    MakeRequest: MakeRequest, 
    main: main, 
    fetchJS: fetchJS, 
    ui: ui, 
    firebaseUrl: firebaseUrl, 
    userRequest: userRequest, 
    initialState: initialState
};

},{"../Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","../Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","../Control.Monad.Aff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.Class/index.js","../Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","../Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","../Control.Monad.Eff.Exception":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/index.js","../Control.Monad.Free":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Free/index.js","../Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","../Data.Foldable":"/Users/anders/dev/ps/halo/ajax/output/Data.Foldable/index.js","../Data.Foreign.Class":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign.Class/index.js","../Data.Functor":"/Users/anders/dev/ps/halo/ajax/output/Data.Functor/index.js","../Data.JSON":"/Users/anders/dev/ps/halo/ajax/output/Data.JSON/index.js","../Data.Map":"/Users/anders/dev/ps/halo/ajax/output/Data.Map/index.js","../Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","../Data.Monoid":"/Users/anders/dev/ps/halo/ajax/output/Data.Monoid/index.js","../Data.Tuple":"/Users/anders/dev/ps/halo/ajax/output/Data.Tuple/index.js","../Halogen":"/Users/anders/dev/ps/halo/ajax/output/Halogen/index.js","../Halogen.Component":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Component/index.js","../Halogen.Driver":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Driver/index.js","../Halogen.HTML":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML/index.js","../Halogen.HTML.Elements":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Elements/index.js","../Halogen.HTML.Elements.Indexed":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Elements.Indexed/index.js","../Halogen.HTML.Events":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events/index.js","../Halogen.HTML.Events.Indexed":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Events.Indexed/index.js","../Halogen.HTML.Indexed":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Indexed/index.js","../Halogen.HTML.Properties.Indexed":"/Users/anders/dev/ps/halo/ajax/output/Halogen.HTML.Properties.Indexed/index.js","../Halogen.Query":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Query/index.js","../Halogen.Util":"/Users/anders/dev/ps/halo/ajax/output/Halogen.Util/index.js","../Network.HTTP.Affjax":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax/index.js","../Network.HTTP.Affjax.Request":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax.Request/index.js","../Network.HTTP.Affjax.Response":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax.Response/index.js","../Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Math/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Math

exports.abs = Math.abs;

exports.acos = Math.acos;

exports.asin = Math.asin;

exports.atan = Math.atan;

exports.atan2 = function (y) {
  return function (x) {
    return Math.atan2(y, x);
  };
};

exports.ceil = Math.ceil;

exports.cos = Math.cos;

exports.exp = Math.exp;

exports.floor = Math.floor;

exports.log = Math.log;

exports.max = function (n1) {
  return function (n2) {
    return Math.max(n1, n2);
  };
};

exports.min = function (n1) {
  return function (n2) {
    return Math.min(n1, n2);
  };
};

exports.pow = function (n) {
  return function (p) {
    return Math.pow(n, p);
  };
};

exports["%"] = function(n) {
  return function(m) {
    return n % m;
  };
};

exports.round = Math.round;

exports.sin = Math.sin;

exports.sqrt = Math.sqrt;

exports.tan = Math.tan;

exports.e = Math.E;

exports.ln2 = Math.LN2;

exports.ln10 = Math.LN10;

exports.log2e = Math.LOG2E;

exports.log10e = Math.LOG10E;

exports.pi = Math.PI;

exports.sqrt1_2 = Math.SQRT1_2;

exports.sqrt2 = Math.SQRT2;

},{}],"/Users/anders/dev/ps/halo/ajax/output/Math/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
module.exports = {
    sqrt2: $foreign.sqrt2, 
    sqrt1_2: $foreign.sqrt1_2, 
    pi: $foreign.pi, 
    log10e: $foreign.log10e, 
    log2e: $foreign.log2e, 
    ln10: $foreign.ln10, 
    ln2: $foreign.ln2, 
    e: $foreign.e, 
    "%": $foreign["%"], 
    tan: $foreign.tan, 
    sqrt: $foreign.sqrt, 
    sin: $foreign.sin, 
    round: $foreign.round, 
    pow: $foreign.pow, 
    min: $foreign.min, 
    max: $foreign.max, 
    log: $foreign.log, 
    floor: $foreign.floor, 
    exp: $foreign.exp, 
    cos: $foreign.cos, 
    ceil: $foreign.ceil, 
    atan2: $foreign.atan2, 
    atan: $foreign.atan, 
    asin: $foreign.asin, 
    acos: $foreign.acos, 
    abs: $foreign.abs
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Math/foreign.js"}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax.Request/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var DOM_File_Types = require("DOM.File.Types");
var DOM_Node_Types = require("DOM.Node.Types");
var DOM_XHR_Types = require("DOM.XHR.Types");
var Unsafe_Coerce = require("Unsafe.Coerce");
var Data_ArrayBuffer_Types = require("Data.ArrayBuffer.Types");
var Requestable = function (toRequest) {
    this.toRequest = toRequest;
};
var toRequest = function (dict) {
    return dict.toRequest;
};
var requestableUnit = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableUint8ClampedArray = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableUint8Array = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableUint32Array = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableUint16Array = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableString = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableRequestContent = new Requestable(Prelude.id(Prelude.categoryFn));
var requestableInt8Array = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableInt32Array = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableInt16Array = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableFormData = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableFloat64Array = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableFloat32Array = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableDocument = new Requestable(Unsafe_Coerce.unsafeCoerce);
var requestableBlob = new Requestable(Unsafe_Coerce.unsafeCoerce);
module.exports = {
    Requestable: Requestable, 
    toRequest: toRequest, 
    requestableRequestContent: requestableRequestContent, 
    requestableInt8Array: requestableInt8Array, 
    requestableInt16Array: requestableInt16Array, 
    requestableInt32Array: requestableInt32Array, 
    requestableUint8Array: requestableUint8Array, 
    requestableUint16Array: requestableUint16Array, 
    requestableUint32Array: requestableUint32Array, 
    requestableUint8ClampedArray: requestableUint8ClampedArray, 
    requestableFloat32Array: requestableFloat32Array, 
    requestableFloat64Array: requestableFloat64Array, 
    requestableBlob: requestableBlob, 
    requestableDocument: requestableDocument, 
    requestableString: requestableString, 
    requestableFormData: requestableFormData, 
    requestableUnit: requestableUnit
};

},{"DOM.File.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.File.Types/index.js","DOM.Node.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Types/index.js","DOM.XHR.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.XHR.Types/index.js","Data.ArrayBuffer.Types":"/Users/anders/dev/ps/halo/ajax/output/Data.ArrayBuffer.Types/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js","Unsafe.Coerce":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax.Response/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Either = require("Data.Either");
var Data_Foreign = require("Data.Foreign");
var DOM_File_Types = require("DOM.File.Types");
var DOM_Node_Types = require("DOM.Node.Types");
var DOM_XHR_Types = require("DOM.XHR.Types");
var Data_ArrayBuffer_Types = require("Data.ArrayBuffer.Types");
var ArrayBufferResponse = (function () {
    function ArrayBufferResponse() {

    };
    ArrayBufferResponse.value = new ArrayBufferResponse();
    return ArrayBufferResponse;
})();
var BlobResponse = (function () {
    function BlobResponse() {

    };
    BlobResponse.value = new BlobResponse();
    return BlobResponse;
})();
var DocumentResponse = (function () {
    function DocumentResponse() {

    };
    DocumentResponse.value = new DocumentResponse();
    return DocumentResponse;
})();
var JSONResponse = (function () {
    function JSONResponse() {

    };
    JSONResponse.value = new JSONResponse();
    return JSONResponse;
})();
var StringResponse = (function () {
    function StringResponse() {

    };
    StringResponse.value = new StringResponse();
    return StringResponse;
})();
var Respondable = function (fromResponse, responseType) {
    this.fromResponse = fromResponse;
    this.responseType = responseType;
};
var showResponseType = new Prelude.Show(function (v) {
    if (v instanceof ArrayBufferResponse) {
        return "ArrayBufferResponse";
    };
    if (v instanceof BlobResponse) {
        return "BlobResponse";
    };
    if (v instanceof DocumentResponse) {
        return "DocumentResponse";
    };
    if (v instanceof JSONResponse) {
        return "JSONResponse";
    };
    if (v instanceof StringResponse) {
        return "StringResponse";
    };
    throw new Error("Failed pattern match at Network.HTTP.Affjax.Response line 34, column 1 - line 41, column 1: " + [ v.constructor.name ]);
});
var responseTypeToString = function (v) {
    if (v instanceof ArrayBufferResponse) {
        return "arraybuffer";
    };
    if (v instanceof BlobResponse) {
        return "blob";
    };
    if (v instanceof DocumentResponse) {
        return "document";
    };
    if (v instanceof JSONResponse) {
        return "text";
    };
    if (v instanceof StringResponse) {
        return "text";
    };
    throw new Error("Failed pattern match at Network.HTTP.Affjax.Response line 41, column 1 - line 42, column 1: " + [ v.constructor.name ]);
};
var responseType = function (dict) {
    return dict.responseType;
};
var responsableUnit = new Respondable(Prelude["const"](new Data_Either.Right(Prelude.unit)), StringResponse.value);
var responsableString = new Respondable(Data_Foreign.readString, StringResponse.value);
var responsableJSON = new Respondable(Data_Either.Right.create, JSONResponse.value);
var responsableDocument = new Respondable(Data_Foreign.unsafeReadTagged("Document"), DocumentResponse.value);
var responsableBlob = new Respondable(Data_Foreign.unsafeReadTagged("Blob"), BlobResponse.value);
var responsableArrayBuffer = new Respondable(Data_Foreign.unsafeReadTagged("ArrayBuffer"), ArrayBufferResponse.value);
var fromResponse = function (dict) {
    return dict.fromResponse;
};
var eqResponseType = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof ArrayBufferResponse && v1 instanceof ArrayBufferResponse) {
            return true;
        };
        if (v instanceof BlobResponse && v1 instanceof BlobResponse) {
            return true;
        };
        if (v instanceof DocumentResponse && v1 instanceof DocumentResponse) {
            return true;
        };
        if (v instanceof JSONResponse && v1 instanceof JSONResponse) {
            return true;
        };
        if (v instanceof StringResponse && v1 instanceof StringResponse) {
            return true;
        };
        return false;
    };
});
module.exports = {
    ArrayBufferResponse: ArrayBufferResponse, 
    BlobResponse: BlobResponse, 
    DocumentResponse: DocumentResponse, 
    JSONResponse: JSONResponse, 
    StringResponse: StringResponse, 
    Respondable: Respondable, 
    fromResponse: fromResponse, 
    responseType: responseType, 
    responseTypeToString: responseTypeToString, 
    eqResponseType: eqResponseType, 
    showResponseType: showResponseType, 
    responsableBlob: responsableBlob, 
    responsableDocument: responsableDocument, 
    responsableJSON: responsableJSON, 
    responsableString: responsableString, 
    responsableUnit: responsableUnit, 
    responsableArrayBuffer: responsableArrayBuffer
};

},{"DOM.File.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.File.Types/index.js","DOM.Node.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.Node.Types/index.js","DOM.XHR.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.XHR.Types/index.js","Data.ArrayBuffer.Types":"/Users/anders/dev/ps/halo/ajax/output/Data.ArrayBuffer.Types/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax/foreign.js":[function(require,module,exports){
/* global exports */
/* global XMLHttpRequest */
/* global module */
"use strict";

// module Network.HTTP.Affjax

// jshint maxparams: 5
exports._ajax = function (mkHeader, options, canceler, errback, callback) {
  var platformSpecific = { };
  if (typeof module !== "undefined" && module.require) {
    // We are on node.js
    platformSpecific.newXHR = function () {
      var XHR = module.require("xmlhttprequest").XMLHttpRequest;
      return new XHR();
    };

    platformSpecific.fixupUrl = function (url) {
      var urllib = module.require("url");
      var u = urllib.parse(url);
      u.protocol = u.protocol || "http:";
      u.hostname = u.hostname || "localhost";
      return urllib.format(u);
    };

    platformSpecific.getResponse = function (xhr) {
      // the node package 'xmlhttprequest' does not support xhr.response.
      return xhr.responseText;
    };
  } else {
    // We are in the browser
    platformSpecific.newXHR = function () {
      return new XMLHttpRequest();
    };

    platformSpecific.fixupUrl = function (url) {
      return url || "/";
    };

    platformSpecific.getResponse = function (xhr) {
      return xhr.response;
    };
  }

  return function () {
    var xhr = platformSpecific.newXHR();
    var fixedUrl = platformSpecific.fixupUrl(options.url);
    xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
    if (options.headers) {
      for (var i = 0, header; (header = options.headers[i]) != null; i++) {
        xhr.setRequestHeader(header.field, header.value);
      }
    }
    xhr.onerror = function () {
      errback(new Error("AJAX request failed: " + options.method + " " + options.url))();
    };
    xhr.onload = function () {
      callback({
        status: xhr.status,
        headers: xhr.getAllResponseHeaders().split("\n")
          .filter(function (header) {
            return header.length > 0;
          })
          .map(function (header) {
            var i = header.indexOf(":");
            return mkHeader(header.substring(0, i))(header.substring(i + 2));
          }),
        response: platformSpecific.getResponse(xhr)
      })();
    };
    xhr.responseType = options.responseType;
    xhr.send(options.content);
    return canceler(xhr);
  };
};

// jshint maxparams: 4
exports._cancelAjax = function (xhr, cancelError, errback, callback) {
  return function () {
    try { xhr.abort(); } catch (e) { return callback(false)(); }
    return callback(true)();
  };
};


},{}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Bind = require("Control.Bind");
var Control_Monad_Aff = require("Control.Monad.Aff");
var Control_Monad_Aff_Par = require("Control.Monad.Aff.Par");
var Control_Monad_Aff_AVar = require("Control.Monad.Aff.AVar");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_Eff_Class = require("Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("Control.Monad.Eff.Exception");
var Control_Monad_Eff_Ref = require("Control.Monad.Eff.Ref");
var Control_Monad_Error_Class = require("Control.Monad.Error.Class");
var Data_Either = require("Data.Either");
var Data_Foreign = require("Data.Foreign");
var Data_Function = require("Data.Function");
var Data_Int = require("Data.Int");
var Data_Maybe = require("Data.Maybe");
var Data_Nullable = require("Data.Nullable");
var DOM_XHR_Types = require("DOM.XHR.Types");
var $$Math = require("Math");
var Network_HTTP_Affjax_Request = require("Network.HTTP.Affjax.Request");
var Network_HTTP_Affjax_Response = require("Network.HTTP.Affjax.Response");
var Network_HTTP_Method = require("Network.HTTP.Method");
var Network_HTTP_RequestHeader = require("Network.HTTP.RequestHeader");
var Network_HTTP_ResponseHeader = require("Network.HTTP.ResponseHeader");
var Network_HTTP_StatusCode = require("Network.HTTP.StatusCode");
var retry = function (dictRequestable) {
    return function (policy) {
        return function (run) {
            return function (req) {
                var retryState = function (v) {
                    if (v instanceof Data_Either.Left) {
                        return Data_Either.Left.create(new Data_Either.Left(v.value0));
                    };
                    if (v instanceof Data_Either.Right) {
                        if (v.value0.status === 200) {
                            return new Data_Either.Right(v.value0);
                        };
                        var $32 = policy.shouldRetryWithStatusCode(v.value0.status);
                        if ($32) {
                            return Data_Either.Left.create(new Data_Either.Right(v.value0));
                        };
                        if (!$32) {
                            return new Data_Either.Right(v.value0);
                        };
                        throw new Error("Failed pattern match at Network.HTTP.Affjax line 177, column 5 - line 178, column 5: " + [ $32.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Network.HTTP.Affjax line 177, column 5 - line 178, column 5: " + [ v.constructor.name ]);
                };
                var go = function (failureRef) {
                    return function (n) {
                        return Prelude.bind(Control_Monad_Aff.bindAff)(Prelude["<$>"](Control_Monad_Aff.functorAff)(retryState)(Control_Monad_Aff.attempt(run(req))))(function (v) {
                            if (v instanceof Data_Either.Left) {
                                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.writeRef(failureRef)(new Data_Maybe.Just(v.value0))))(function () {
                                    return Control_Monad_Aff["later'"](policy.delayCurve(n))(go(failureRef)(n + 1 | 0));
                                });
                            };
                            if (v instanceof Data_Either.Right) {
                                return Prelude.pure(Control_Monad_Aff.applicativeAff)(v.value0);
                            };
                            throw new Error("Failed pattern match at Network.HTTP.Affjax line 154, column 1 - line 155, column 1: " + [ v.constructor.name ]);
                        });
                    };
                };
                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.newRef(Data_Maybe.Nothing.value)))(function (v) {
                    var loop = go(v);
                    if (policy.timeout instanceof Data_Maybe.Nothing) {
                        return loop(1);
                    };
                    if (policy.timeout instanceof Data_Maybe.Just) {
                        return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.makeVar)(function (v1) {
                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Prelude[">>="](Control_Monad_Aff.bindAff)(loop(1))(function ($69) {
                                return Control_Monad_Aff_AVar.putVar(v1)(Data_Maybe.Just.create($69));
                            })))(function (v2) {
                                return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.forkAff(Control_Monad_Aff["later'"](policy.timeout.value0)(Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.putVar(v1)(Data_Maybe.Nothing.value))(function () {
                                    return Control_Monad_Aff.cancel(v2)(Control_Monad_Eff_Exception.error("Cancel"));
                                }))))(function (v3) {
                                    return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff_AVar.takeVar(v1))(function (v4) {
                                        if (v4 instanceof Data_Maybe.Nothing) {
                                            return Prelude.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(v)))(function (v5) {
                                                if (v5 instanceof Data_Maybe.Nothing) {
                                                    return Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadErrorAff)(Control_Monad_Eff_Exception.error("Timeout"));
                                                };
                                                if (v5 instanceof Data_Maybe.Just) {
                                                    return Data_Either.either(Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadErrorAff))(Prelude.pure(Control_Monad_Aff.applicativeAff))(v5.value0);
                                                };
                                                throw new Error("Failed pattern match at Network.HTTP.Affjax line 154, column 1 - line 155, column 1: " + [ v5.constructor.name ]);
                                            });
                                        };
                                        if (v4 instanceof Data_Maybe.Just) {
                                            return Prelude.pure(Control_Monad_Aff.applicativeAff)(v4.value0);
                                        };
                                        throw new Error("Failed pattern match at Network.HTTP.Affjax line 154, column 1 - line 155, column 1: " + [ v4.constructor.name ]);
                                    });
                                });
                            });
                        });
                    };
                    throw new Error("Failed pattern match at Network.HTTP.Affjax line 154, column 1 - line 155, column 1: " + [ policy.timeout.constructor.name ]);
                });
            };
        };
    };
};
var defaultRetryPolicy = {
    timeout: Data_Maybe.Nothing.value, 
    delayCurve: function (n) {
        return Data_Int.round($$Math.max(30.0 * 1000.0)(100.0 * $$Math.pow(2.0)(Data_Int.toNumber(n - 1))));
    }, 
    shouldRetryWithStatusCode: function (v) {
        return false;
    }
};
var defaultRequest = {
    method: Network_HTTP_Method.GET.value, 
    url: "/", 
    headers: [  ], 
    content: Data_Maybe.Nothing.value, 
    username: Data_Maybe.Nothing.value, 
    password: Data_Maybe.Nothing.value
};
var cancelAjax = function (xhr) {
    return function (err) {
        return Control_Monad_Aff.makeAff(function (eb) {
            return function (cb) {
                return $foreign._cancelAjax(xhr, err, eb, cb);
            };
        });
    };
};
var affjax$prime = function (dictRequestable) {
    return function (dictRespondable) {
        return function (req) {
            return function (eb) {
                return function (cb) {
                    var reqʹ = {
                        method: Network_HTTP_Method.methodToString(req.method), 
                        url: req.url, 
                        headers: Prelude["<$>"](Prelude.functorArray)(function (h) {
                            return {
                                field: Network_HTTP_RequestHeader.requestHeaderName(h), 
                                value: Network_HTTP_RequestHeader.requestHeaderValue(h)
                            };
                        })(req.headers), 
                        content: Data_Nullable.toNullable(Prelude["<$>"](Data_Maybe.functorMaybe)(Network_HTTP_Affjax_Request.toRequest(dictRequestable))(req.content)), 
                        responseType: Network_HTTP_Affjax_Response.responseTypeToString(Network_HTTP_Affjax_Response.responseType(dictRespondable)), 
                        username: Data_Nullable.toNullable(req.username), 
                        password: Data_Nullable.toNullable(req.password)
                    };
                    var fromResponseʹ = (function () {
                        var $51 = Network_HTTP_Affjax_Response.responseType(dictRespondable);
                        if ($51 instanceof Network_HTTP_Affjax_Response.JSONResponse) {
                            return Control_Bind["<=<"](Data_Either.bindEither)(Network_HTTP_Affjax_Response.fromResponse(dictRespondable))(Control_Bind["<=<"](Data_Either.bindEither)(Data_Foreign.parseJSON)(Data_Foreign.readString));
                        };
                        return Network_HTTP_Affjax_Response.fromResponse(dictRespondable);
                    })();
                    var cbʹ = function (res) {
                        var $54 = Prelude["<$>"](Data_Either.functorEither)(function (v) {
                            var $52 = {};
                            for (var $53 in res) {
                                if (res.hasOwnProperty($53)) {
                                    $52[$53] = res[$53];
                                };
                            };
                            $52.response = v;
                            return $52;
                        })(fromResponseʹ(res.response));
                        if ($54 instanceof Data_Either.Left) {
                            return eb(Control_Monad_Eff_Exception.error(Prelude.show(Data_Foreign.showForeignError)($54.value0)));
                        };
                        if ($54 instanceof Data_Either.Right) {
                            return cb($54.value0);
                        };
                        throw new Error("Failed pattern match at Network.HTTP.Affjax line 197, column 1 - line 202, column 1: " + [ $54.constructor.name ]);
                    };
                    return $foreign._ajax(Network_HTTP_ResponseHeader.responseHeader, reqʹ, cancelAjax, eb, cbʹ);
                };
            };
        };
    };
};
var affjax = function (dictRequestable) {
    return function (dictRespondable) {
        return function ($70) {
            return Control_Monad_Aff["makeAff'"](affjax$prime(dictRequestable)(dictRespondable)($70));
        };
    };
};
var $$delete = function (dictRespondable) {
    return function (u) {
        return affjax(Network_HTTP_Affjax_Request.requestableUnit)(dictRespondable)((function () {
            var $57 = {};
            for (var $58 in defaultRequest) {
                if (defaultRequest.hasOwnProperty($58)) {
                    $57[$58] = defaultRequest[$58];
                };
            };
            $57.method = Network_HTTP_Method.DELETE.value;
            $57.url = u;
            return $57;
        })());
    };
};
var delete_ = $$delete(Network_HTTP_Affjax_Response.responsableUnit);
var get = function (dictRespondable) {
    return function (u) {
        return affjax(Network_HTTP_Affjax_Request.requestableUnit)(dictRespondable)((function () {
            var $59 = {};
            for (var $60 in defaultRequest) {
                if (defaultRequest.hasOwnProperty($60)) {
                    $59[$60] = defaultRequest[$60];
                };
            };
            $59.url = u;
            return $59;
        })());
    };
};
var post = function (dictRequestable) {
    return function (dictRespondable) {
        return function (u) {
            return function (c) {
                return affjax(dictRequestable)(dictRespondable)((function () {
                    var $61 = {};
                    for (var $62 in defaultRequest) {
                        if (defaultRequest.hasOwnProperty($62)) {
                            $61[$62] = defaultRequest[$62];
                        };
                    };
                    $61.method = Network_HTTP_Method.POST.value;
                    $61.url = u;
                    $61.content = new Data_Maybe.Just(c);
                    return $61;
                })());
            };
        };
    };
};
var post_ = function (dictRequestable) {
    return post(dictRequestable)(Network_HTTP_Affjax_Response.responsableUnit);
};
var post$prime = function (dictRequestable) {
    return function (dictRespondable) {
        return function (u) {
            return function (c) {
                return affjax(dictRequestable)(dictRespondable)((function () {
                    var $63 = {};
                    for (var $64 in defaultRequest) {
                        if (defaultRequest.hasOwnProperty($64)) {
                            $63[$64] = defaultRequest[$64];
                        };
                    };
                    $63.method = Network_HTTP_Method.POST.value;
                    $63.url = u;
                    $63.content = c;
                    return $63;
                })());
            };
        };
    };
};
var post_$prime = function (dictRequestable) {
    return post$prime(dictRequestable)(Network_HTTP_Affjax_Response.responsableUnit);
};
var put = function (dictRequestable) {
    return function (dictRespondable) {
        return function (u) {
            return function (c) {
                return affjax(dictRequestable)(dictRespondable)((function () {
                    var $65 = {};
                    for (var $66 in defaultRequest) {
                        if (defaultRequest.hasOwnProperty($66)) {
                            $65[$66] = defaultRequest[$66];
                        };
                    };
                    $65.method = Network_HTTP_Method.PUT.value;
                    $65.url = u;
                    $65.content = new Data_Maybe.Just(c);
                    return $65;
                })());
            };
        };
    };
};
var put_ = function (dictRequestable) {
    return put(dictRequestable)(Network_HTTP_Affjax_Response.responsableUnit);
};
var put$prime = function (dictRequestable) {
    return function (dictRespondable) {
        return function (u) {
            return function (c) {
                return affjax(dictRequestable)(dictRespondable)((function () {
                    var $67 = {};
                    for (var $68 in defaultRequest) {
                        if (defaultRequest.hasOwnProperty($68)) {
                            $67[$68] = defaultRequest[$68];
                        };
                    };
                    $67.method = Network_HTTP_Method.PUT.value;
                    $67.url = u;
                    $67.content = c;
                    return $67;
                })());
            };
        };
    };
};
var put_$prime = function (dictRequestable) {
    return put$prime(dictRequestable)(Network_HTTP_Affjax_Response.responsableUnit);
};
module.exports = {
    retry: retry, 
    defaultRetryPolicy: defaultRetryPolicy, 
    delete_: delete_, 
    "delete": $$delete, 
    "put_'": put_$prime, 
    "put'": put$prime, 
    put_: put_, 
    put: put, 
    "post_'": post_$prime, 
    "post'": post$prime, 
    post_: post_, 
    post: post, 
    get: get, 
    "affjax'": affjax$prime, 
    affjax: affjax, 
    defaultRequest: defaultRequest
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax/foreign.js","Control.Alt":"/Users/anders/dev/ps/halo/ajax/output/Control.Alt/index.js","Control.Bind":"/Users/anders/dev/ps/halo/ajax/output/Control.Bind/index.js","Control.Monad.Aff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff/index.js","Control.Monad.Aff.AVar":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.AVar/index.js","Control.Monad.Aff.Par":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Aff.Par/index.js","Control.Monad.Eff":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff/index.js","Control.Monad.Eff.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Class/index.js","Control.Monad.Eff.Exception":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Exception/index.js","Control.Monad.Eff.Ref":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Eff.Ref/index.js","Control.Monad.Error.Class":"/Users/anders/dev/ps/halo/ajax/output/Control.Monad.Error.Class/index.js","DOM.XHR.Types":"/Users/anders/dev/ps/halo/ajax/output/DOM.XHR.Types/index.js","Data.Either":"/Users/anders/dev/ps/halo/ajax/output/Data.Either/index.js","Data.Foreign":"/Users/anders/dev/ps/halo/ajax/output/Data.Foreign/index.js","Data.Function":"/Users/anders/dev/ps/halo/ajax/output/Data.Function/index.js","Data.Int":"/Users/anders/dev/ps/halo/ajax/output/Data.Int/index.js","Data.Maybe":"/Users/anders/dev/ps/halo/ajax/output/Data.Maybe/index.js","Data.Nullable":"/Users/anders/dev/ps/halo/ajax/output/Data.Nullable/index.js","Math":"/Users/anders/dev/ps/halo/ajax/output/Math/index.js","Network.HTTP.Affjax.Request":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax.Request/index.js","Network.HTTP.Affjax.Response":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Affjax.Response/index.js","Network.HTTP.Method":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Method/index.js","Network.HTTP.RequestHeader":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.RequestHeader/index.js","Network.HTTP.ResponseHeader":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.ResponseHeader/index.js","Network.HTTP.StatusCode":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.StatusCode/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.Method/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var DELETE = (function () {
    function DELETE() {

    };
    DELETE.value = new DELETE();
    return DELETE;
})();
var GET = (function () {
    function GET() {

    };
    GET.value = new GET();
    return GET;
})();
var HEAD = (function () {
    function HEAD() {

    };
    HEAD.value = new HEAD();
    return HEAD;
})();
var OPTIONS = (function () {
    function OPTIONS() {

    };
    OPTIONS.value = new OPTIONS();
    return OPTIONS;
})();
var PATCH = (function () {
    function PATCH() {

    };
    PATCH.value = new PATCH();
    return PATCH;
})();
var POST = (function () {
    function POST() {

    };
    POST.value = new POST();
    return POST;
})();
var PUT = (function () {
    function PUT() {

    };
    PUT.value = new PUT();
    return PUT;
})();
var MOVE = (function () {
    function MOVE() {

    };
    MOVE.value = new MOVE();
    return MOVE;
})();
var COPY = (function () {
    function COPY() {

    };
    COPY.value = new COPY();
    return COPY;
})();
var CustomMethod = (function () {
    function CustomMethod(value0) {
        this.value0 = value0;
    };
    CustomMethod.create = function (value0) {
        return new CustomMethod(value0);
    };
    return CustomMethod;
})();
var showMethod = new Prelude.Show(function (v) {
    if (v instanceof DELETE) {
        return "DELETE";
    };
    if (v instanceof GET) {
        return "GET";
    };
    if (v instanceof HEAD) {
        return "HEAD";
    };
    if (v instanceof OPTIONS) {
        return "OPTIONS";
    };
    if (v instanceof PATCH) {
        return "PATCH";
    };
    if (v instanceof POST) {
        return "POST";
    };
    if (v instanceof PUT) {
        return "PUT";
    };
    if (v instanceof MOVE) {
        return "MOVE";
    };
    if (v instanceof COPY) {
        return "COPY";
    };
    if (v instanceof CustomMethod) {
        return "(CustomMethod " + (Prelude.show(Prelude.showString)(v.value0) + ")");
    };
    throw new Error("Failed pattern match at Network.HTTP.Method line 29, column 1 - line 41, column 1: " + [ v.constructor.name ]);
});
var methodToString = function (v) {
    if (v instanceof CustomMethod) {
        return v.value0;
    };
    return Prelude.show(showMethod)(v);
};
var eqMethod = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof DELETE && v1 instanceof DELETE) {
            return true;
        };
        if (v instanceof GET && v1 instanceof GET) {
            return true;
        };
        if (v instanceof HEAD && v1 instanceof HEAD) {
            return true;
        };
        if (v instanceof OPTIONS && v1 instanceof OPTIONS) {
            return true;
        };
        if (v instanceof PATCH && v1 instanceof PATCH) {
            return true;
        };
        if (v instanceof POST && v1 instanceof POST) {
            return true;
        };
        if (v instanceof PUT && v1 instanceof PUT) {
            return true;
        };
        if (v instanceof MOVE && v1 instanceof MOVE) {
            return true;
        };
        if (v instanceof COPY && v1 instanceof COPY) {
            return true;
        };
        return false;
    };
});
module.exports = {
    DELETE: DELETE, 
    GET: GET, 
    HEAD: HEAD, 
    OPTIONS: OPTIONS, 
    PATCH: PATCH, 
    POST: POST, 
    PUT: PUT, 
    MOVE: MOVE, 
    COPY: COPY, 
    CustomMethod: CustomMethod, 
    methodToString: methodToString, 
    eqMethod: eqMethod, 
    showMethod: showMethod
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.MimeType/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var MimeType = function (x) {
    return x;
};
var showMimeType = new Prelude.Show(function (v) {
    return "(MimeType " + (Prelude.show(Prelude.showString)(v) + ")");
});
var mimeTypeToString = function (v) {
    return v;
};
var eqMimeType = new Prelude.Eq(function (v) {
    return function (v1) {
        return Prelude["=="](Prelude.eqString)(v)(v1);
    };
});
module.exports = {
    MimeType: MimeType, 
    mimeTypeToString: mimeTypeToString, 
    eqMimeType: eqMimeType, 
    showMimeType: showMimeType
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.RequestHeader/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Network_HTTP_MimeType = require("Network.HTTP.MimeType");
var Accept = (function () {
    function Accept(value0) {
        this.value0 = value0;
    };
    Accept.create = function (value0) {
        return new Accept(value0);
    };
    return Accept;
})();
var ContentType = (function () {
    function ContentType(value0) {
        this.value0 = value0;
    };
    ContentType.create = function (value0) {
        return new ContentType(value0);
    };
    return ContentType;
})();
var RequestHeader = (function () {
    function RequestHeader(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    RequestHeader.create = function (value0) {
        return function (value1) {
            return new RequestHeader(value0, value1);
        };
    };
    return RequestHeader;
})();
var showRequestHeader = new Prelude.Show(function (v) {
    if (v instanceof Accept) {
        return "(Accept " + (Prelude.show(Network_HTTP_MimeType.showMimeType)(v.value0) + ")");
    };
    if (v instanceof ContentType) {
        return "(ContentType " + (Prelude.show(Network_HTTP_MimeType.showMimeType)(v.value0) + ")");
    };
    if (v instanceof RequestHeader) {
        return "(RequestHeader " + (Prelude.show(Prelude.showString)(v.value0) + (" " + (Prelude.show(Prelude.showString)(v.value1) + ")")));
    };
    throw new Error("Failed pattern match at Network.HTTP.RequestHeader line 18, column 1 - line 23, column 1: " + [ v.constructor.name ]);
});
var requestHeaderValue = function (v) {
    if (v instanceof Accept) {
        return Network_HTTP_MimeType.mimeTypeToString(v.value0);
    };
    if (v instanceof ContentType) {
        return Network_HTTP_MimeType.mimeTypeToString(v.value0);
    };
    if (v instanceof RequestHeader) {
        return v.value1;
    };
    throw new Error("Failed pattern match at Network.HTTP.RequestHeader line 28, column 1 - line 29, column 1: " + [ v.constructor.name ]);
};
var requestHeaderName = function (v) {
    if (v instanceof Accept) {
        return "Accept";
    };
    if (v instanceof ContentType) {
        return "Content-Type";
    };
    if (v instanceof RequestHeader) {
        return v.value0;
    };
    throw new Error("Failed pattern match at Network.HTTP.RequestHeader line 23, column 1 - line 24, column 1: " + [ v.constructor.name ]);
};
var eqRequestHeader = new Prelude.Eq(function (v) {
    return function (v1) {
        if (v instanceof Accept && v1 instanceof Accept) {
            return Prelude["=="](Network_HTTP_MimeType.eqMimeType)(v.value0)(v1.value0);
        };
        if (v instanceof ContentType && v1 instanceof ContentType) {
            return Prelude["=="](Network_HTTP_MimeType.eqMimeType)(v.value0)(v1.value0);
        };
        if (v instanceof RequestHeader && v1 instanceof RequestHeader) {
            return Prelude["=="](Prelude.eqString)(v.value0)(v1.value0) && Prelude["=="](Prelude.eqString)(v.value1)(v1.value1);
        };
        return false;
    };
});
module.exports = {
    Accept: Accept, 
    ContentType: ContentType, 
    RequestHeader: RequestHeader, 
    requestHeaderValue: requestHeaderValue, 
    requestHeaderName: requestHeaderName, 
    eqRequestHeader: eqRequestHeader, 
    showRequestHeader: showRequestHeader
};

},{"Network.HTTP.MimeType":"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.MimeType/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.ResponseHeader/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var ResponseHeader = (function () {
    function ResponseHeader(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ResponseHeader.create = function (value0) {
        return function (value1) {
            return new ResponseHeader(value0, value1);
        };
    };
    return ResponseHeader;
})();
var showResponseHeader = new Prelude.Show(function (v) {
    return "(ResponseHeader " + (Prelude.show(Prelude.showString)(v.value0) + (" " + (Prelude.show(Prelude.showString)(v.value1) + ")")));
});
var responseHeaderValue = function (v) {
    return v.value1;
};
var responseHeaderName = function (v) {
    return v.value0;
};
var responseHeader = function (field) {
    return function (value) {
        return new ResponseHeader(field, value);
    };
};
var eqResponseHeader = new Prelude.Eq(function (v) {
    return function (v1) {
        return Prelude["=="](Prelude.eqString)(v.value0)(v1.value0) && Prelude["=="](Prelude.eqString)(v.value1)(v1.value1);
    };
});
module.exports = {
    responseHeaderValue: responseHeaderValue, 
    responseHeaderName: responseHeaderName, 
    responseHeader: responseHeader, 
    eqResponseHeader: eqResponseHeader, 
    showResponseHeader: showResponseHeader
};

},{"Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Network.HTTP.StatusCode/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var Prelude = require("Prelude");
var Data_Int = require("Data.Int");
var StatusCode = function (x) {
    return x;
};
var showStatusCode = new Prelude.Show(function (v) {
    return "(StatusCode " + (Prelude.show(Prelude.showInt)(v) + ")");
});
var eqStatusCode = new Prelude.Eq(function (v) {
    return function (v1) {
        return v === v1;
    };
});
module.exports = {
    StatusCode: StatusCode, 
    eqStatusCode: eqStatusCode, 
    showStatusCode: showStatusCode
};

},{"Data.Int":"/Users/anders/dev/ps/halo/ajax/output/Data.Int/index.js","Prelude":"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js"}],"/Users/anders/dev/ps/halo/ajax/output/Prelude/foreign.js":[function(require,module,exports){
/* global exports */
"use strict";

// module Prelude

//- Functor --------------------------------------------------------------------

exports.arrayMap = function (f) {
  return function (arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

//- Bind -----------------------------------------------------------------------

exports.arrayBind = function (arr) {
  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};

//- Monoid ---------------------------------------------------------------------

exports.concatString = function (s1) {
  return function (s2) {
    return s1 + s2;
  };
};

exports.concatArray = function (xs) {
  return function (ys) {
    return xs.concat(ys);
  };
};

//- Semiring -------------------------------------------------------------------

exports.intAdd = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x + y | 0;
  };
};

exports.intMul = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x * y | 0;
  };
};

exports.numAdd = function (n1) {
  return function (n2) {
    return n1 + n2;
  };
};

exports.numMul = function (n1) {
  return function (n2) {
    return n1 * n2;
  };
};

//- ModuloSemiring -------------------------------------------------------------

exports.intDiv = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x / y | 0;
  };
};

exports.intMod = function (x) {
  return function (y) {
    return x % y;
  };
};

exports.numDiv = function (n1) {
  return function (n2) {
    return n1 / n2;
  };
};

//- Ring -----------------------------------------------------------------------

exports.intSub = function (x) {
  return function (y) {
    /* jshint bitwise: false */
    return x - y | 0;
  };
};

exports.numSub = function (n1) {
  return function (n2) {
    return n1 - n2;
  };
};

//- Eq -------------------------------------------------------------------------

exports.refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

exports.refIneq = function (r1) {
  return function (r2) {
    return r1 !== r2;
  };
};

exports.eqArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      if (xs.length !== ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
};

exports.ordArrayImpl = function (f) {
  return function (xs) {
    return function (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o !== 0) {
          return o;
        }
        i++;
      }
      if (xlen === ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};

//- Ord ------------------------------------------------------------------------

exports.unsafeCompareImpl = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (x) {
        return function (y) {
          return x < y ? lt : x > y ? gt : eq;
        };
      };
    };
  };
};

//- Bounded --------------------------------------------------------------------

exports.topInt = 2147483647;
exports.bottomInt = -2147483648;

exports.topChar = String.fromCharCode(65535);
exports.bottomChar = String.fromCharCode(0);

//- BooleanAlgebra -------------------------------------------------------------

exports.boolOr = function (b1) {
  return function (b2) {
    return b1 || b2;
  };
};

exports.boolAnd = function (b1) {
  return function (b2) {
    return b1 && b2;
  };
};

exports.boolNot = function (b) {
  return !b;
};

//- Show -----------------------------------------------------------------------

exports.showIntImpl = function (n) {
  return n.toString();
};

exports.showNumberImpl = function (n) {
  /* jshint bitwise: false */
  return n === (n | 0) ? n + ".0" : n.toString();
};

exports.showCharImpl = function (c) {
  return c === "'" ? "'\\''" : "'" + c + "'";
};

exports.showStringImpl = function (s) {
  return JSON.stringify(s);
};

exports.showArrayImpl = function (f) {
  return function (xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};

},{}],"/Users/anders/dev/ps/halo/ajax/output/Prelude/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
var Unit = function (x) {
    return x;
};
var LT = (function () {
    function LT() {

    };
    LT.value = new LT();
    return LT;
})();
var GT = (function () {
    function GT() {

    };
    GT.value = new GT();
    return GT;
})();
var EQ = (function () {
    function EQ() {

    };
    EQ.value = new EQ();
    return EQ;
})();
var Semigroupoid = function (compose) {
    this.compose = compose;
};
var Category = function (__superclass_Prelude$dotSemigroupoid_0, id) {
    this["__superclass_Prelude.Semigroupoid_0"] = __superclass_Prelude$dotSemigroupoid_0;
    this.id = id;
};
var Functor = function (map) {
    this.map = map;
};
var Apply = function (__superclass_Prelude$dotFunctor_0, apply) {
    this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    this.apply = apply;
};
var Applicative = function (__superclass_Prelude$dotApply_0, pure) {
    this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    this.pure = pure;
};
var Bind = function (__superclass_Prelude$dotApply_0, bind) {
    this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    this.bind = bind;
};
var Monad = function (__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
    this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
    this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
};
var Semigroup = function (append) {
    this.append = append;
};
var Semiring = function (add, mul, one, zero) {
    this.add = add;
    this.mul = mul;
    this.one = one;
    this.zero = zero;
};
var Ring = function (__superclass_Prelude$dotSemiring_0, sub) {
    this["__superclass_Prelude.Semiring_0"] = __superclass_Prelude$dotSemiring_0;
    this.sub = sub;
};
var ModuloSemiring = function (__superclass_Prelude$dotSemiring_0, div, mod) {
    this["__superclass_Prelude.Semiring_0"] = __superclass_Prelude$dotSemiring_0;
    this.div = div;
    this.mod = mod;
};
var DivisionRing = function (__superclass_Prelude$dotModuloSemiring_1, __superclass_Prelude$dotRing_0) {
    this["__superclass_Prelude.ModuloSemiring_1"] = __superclass_Prelude$dotModuloSemiring_1;
    this["__superclass_Prelude.Ring_0"] = __superclass_Prelude$dotRing_0;
};
var Num = function (__superclass_Prelude$dotDivisionRing_0) {
    this["__superclass_Prelude.DivisionRing_0"] = __superclass_Prelude$dotDivisionRing_0;
};
var Eq = function (eq) {
    this.eq = eq;
};
var Ord = function (__superclass_Prelude$dotEq_0, compare) {
    this["__superclass_Prelude.Eq_0"] = __superclass_Prelude$dotEq_0;
    this.compare = compare;
};
var Bounded = function (bottom, top) {
    this.bottom = bottom;
    this.top = top;
};
var BoundedOrd = function (__superclass_Prelude$dotBounded_0, __superclass_Prelude$dotOrd_1) {
    this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
    this["__superclass_Prelude.Ord_1"] = __superclass_Prelude$dotOrd_1;
};
var BooleanAlgebra = function (__superclass_Prelude$dotBounded_0, conj, disj, not) {
    this["__superclass_Prelude.Bounded_0"] = __superclass_Prelude$dotBounded_0;
    this.conj = conj;
    this.disj = disj;
    this.not = not;
};
var Show = function (show) {
    this.show = show;
};
var $dollar = function (f) {
    return function (x) {
        return f(x);
    };
};
var $hash = function (x) {
    return function (f) {
        return f(x);
    };
};
var zero = function (dict) {
    return dict.zero;
};
var unsafeCompare = $foreign.unsafeCompareImpl(LT.value)(EQ.value)(GT.value);
var unit = {};
var top = function (dict) {
    return dict.top;
};
var sub = function (dict) {
    return dict.sub;
};
var $minus = function (dictRing) {
    return sub(dictRing);
};
var showUnit = new Show(function (v) {
    return "unit";
});
var showString = new Show($foreign.showStringImpl);
var showOrdering = new Show(function (v) {
    if (v instanceof LT) {
        return "LT";
    };
    if (v instanceof GT) {
        return "GT";
    };
    if (v instanceof EQ) {
        return "EQ";
    };
    throw new Error("Failed pattern match at Prelude line 863, column 1 - line 868, column 1: " + [ v.constructor.name ]);
});
var showNumber = new Show($foreign.showNumberImpl);
var showInt = new Show($foreign.showIntImpl);
var showChar = new Show($foreign.showCharImpl);
var showBoolean = new Show(function (v) {
    if (v) {
        return "true";
    };
    if (!v) {
        return "false";
    };
    throw new Error("Failed pattern match at Prelude line 841, column 1 - line 845, column 1: " + [ v.constructor.name ]);
});
var show = function (dict) {
    return dict.show;
};
var showArray = function (dictShow) {
    return new Show($foreign.showArrayImpl(show(dictShow)));
};
var semiringUnit = new Semiring(function (v) {
    return function (v1) {
        return unit;
    };
}, function (v) {
    return function (v1) {
        return unit;
    };
}, unit, unit);
var semiringNumber = new Semiring($foreign.numAdd, $foreign.numMul, 1.0, 0.0);
var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
var semigroupoidFn = new Semigroupoid(function (f) {
    return function (g) {
        return function (x) {
            return f(g(x));
        };
    };
});
var semigroupUnit = new Semigroup(function (v) {
    return function (v1) {
        return unit;
    };
});
var semigroupString = new Semigroup($foreign.concatString);
var semigroupOrdering = new Semigroup(function (v) {
    return function (y) {
        if (v instanceof LT) {
            return LT.value;
        };
        if (v instanceof GT) {
            return GT.value;
        };
        if (v instanceof EQ) {
            return y;
        };
        throw new Error("Failed pattern match at Prelude line 413, column 1 - line 418, column 1: " + [ v.constructor.name, y.constructor.name ]);
    };
});
var semigroupArray = new Semigroup($foreign.concatArray);
var ringUnit = new Ring(function () {
    return semiringUnit;
}, function (v) {
    return function (v1) {
        return unit;
    };
});
var ringNumber = new Ring(function () {
    return semiringNumber;
}, $foreign.numSub);
var ringInt = new Ring(function () {
    return semiringInt;
}, $foreign.intSub);
var pure = function (dict) {
    return dict.pure;
};
var $$return = function (dictApplicative) {
    return pure(dictApplicative);
};
var otherwise = true;
var one = function (dict) {
    return dict.one;
};
var not = function (dict) {
    return dict.not;
};
var negate = function (dictRing) {
    return function (a) {
        return $minus(dictRing)(zero(dictRing["__superclass_Prelude.Semiring_0"]()))(a);
    };
};
var mul = function (dict) {
    return dict.mul;
};
var $times = function (dictSemiring) {
    return mul(dictSemiring);
};
var moduloSemiringUnit = new ModuloSemiring(function () {
    return semiringUnit;
}, function (v) {
    return function (v1) {
        return unit;
    };
}, function (v) {
    return function (v1) {
        return unit;
    };
});
var moduloSemiringNumber = new ModuloSemiring(function () {
    return semiringNumber;
}, $foreign.numDiv, function (v) {
    return function (v1) {
        return 0.0;
    };
});
var moduloSemiringInt = new ModuloSemiring(function () {
    return semiringInt;
}, $foreign.intDiv, $foreign.intMod);
var mod = function (dict) {
    return dict.mod;
};
var map = function (dict) {
    return dict.map;
};
var $less$dollar$greater = function (dictFunctor) {
    return map(dictFunctor);
};
var $less$hash$greater = function (dictFunctor) {
    return function (fa) {
        return function (f) {
            return $less$dollar$greater(dictFunctor)(f)(fa);
        };
    };
};
var id = function (dict) {
    return dict.id;
};
var functorArray = new Functor($foreign.arrayMap);
var flip = function (f) {
    return function (b) {
        return function (a) {
            return f(a)(b);
        };
    };
};
var eqUnit = new Eq(function (v) {
    return function (v1) {
        return true;
    };
});
var ordUnit = new Ord(function () {
    return eqUnit;
}, function (v) {
    return function (v1) {
        return EQ.value;
    };
});
var eqString = new Eq($foreign.refEq);
var ordString = new Ord(function () {
    return eqString;
}, unsafeCompare);
var eqOrdering = new Eq(function (v) {
    return function (v1) {
        if (v instanceof LT && v1 instanceof LT) {
            return true;
        };
        if (v instanceof GT && v1 instanceof GT) {
            return true;
        };
        if (v instanceof EQ && v1 instanceof EQ) {
            return true;
        };
        return false;
    };
});
var ordOrdering = new Ord(function () {
    return eqOrdering;
}, function (v) {
    return function (v1) {
        if (v instanceof LT && v1 instanceof LT) {
            return EQ.value;
        };
        if (v instanceof EQ && v1 instanceof EQ) {
            return EQ.value;
        };
        if (v instanceof GT && v1 instanceof GT) {
            return EQ.value;
        };
        if (v instanceof LT) {
            return LT.value;
        };
        if (v instanceof EQ && v1 instanceof LT) {
            return GT.value;
        };
        if (v instanceof EQ && v1 instanceof GT) {
            return LT.value;
        };
        if (v instanceof GT) {
            return GT.value;
        };
        throw new Error("Failed pattern match at Prelude line 668, column 1 - line 677, column 1: " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var eqNumber = new Eq($foreign.refEq);
var ordNumber = new Ord(function () {
    return eqNumber;
}, unsafeCompare);
var eqInt = new Eq($foreign.refEq);
var ordInt = new Ord(function () {
    return eqInt;
}, unsafeCompare);
var eqChar = new Eq($foreign.refEq);
var ordChar = new Ord(function () {
    return eqChar;
}, unsafeCompare);
var eqBoolean = new Eq($foreign.refEq);
var ordBoolean = new Ord(function () {
    return eqBoolean;
}, unsafeCompare);
var eq = function (dict) {
    return dict.eq;
};
var $eq$eq = function (dictEq) {
    return eq(dictEq);
};
var eqArray = function (dictEq) {
    return new Eq($foreign.eqArrayImpl($eq$eq(dictEq)));
};
var divisionRingUnit = new DivisionRing(function () {
    return moduloSemiringUnit;
}, function () {
    return ringUnit;
});
var numUnit = new Num(function () {
    return divisionRingUnit;
});
var divisionRingNumber = new DivisionRing(function () {
    return moduloSemiringNumber;
}, function () {
    return ringNumber;
});
var numNumber = new Num(function () {
    return divisionRingNumber;
});
var div = function (dict) {
    return dict.div;
};
var $div = function (dictModuloSemiring) {
    return div(dictModuloSemiring);
};
var disj = function (dict) {
    return dict.disj;
};
var $bar$bar = function (dictBooleanAlgebra) {
    return disj(dictBooleanAlgebra);
};
var $$const = function (a) {
    return function (v) {
        return a;
    };
};
var $$void = function (dictFunctor) {
    return function (fa) {
        return $less$dollar$greater(dictFunctor)($$const(unit))(fa);
    };
};
var conj = function (dict) {
    return dict.conj;
};
var $amp$amp = function (dictBooleanAlgebra) {
    return conj(dictBooleanAlgebra);
};
var compose = function (dict) {
    return dict.compose;
};
var functorFn = new Functor(compose(semigroupoidFn));
var $less$less$less = function (dictSemigroupoid) {
    return compose(dictSemigroupoid);
};
var $greater$greater$greater = function (dictSemigroupoid) {
    return flip(compose(dictSemigroupoid));
};
var compare = function (dict) {
    return dict.compare;
};
var ordArray = function (dictOrd) {
    return new Ord(function () {
        return eqArray(dictOrd["__superclass_Prelude.Eq_0"]());
    }, function (xs) {
        return function (ys) {
            return $dollar(compare(ordInt)(0))($foreign.ordArrayImpl(function (x) {
                return function (y) {
                    var $78 = compare(dictOrd)(x)(y);
                    if ($78 instanceof EQ) {
                        return 0;
                    };
                    if ($78 instanceof LT) {
                        return 1;
                    };
                    if ($78 instanceof GT) {
                        return negate(ringInt)(1);
                    };
                    throw new Error("Failed pattern match at Prelude line 660, column 1 - line 666, column 1: " + [ $78.constructor.name ]);
                };
            })(xs)(ys));
        };
    });
};
var $less = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $79 = compare(dictOrd)(a1)(a2);
            if ($79 instanceof LT) {
                return true;
            };
            return false;
        };
    };
};
var $less$eq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $80 = compare(dictOrd)(a1)(a2);
            if ($80 instanceof GT) {
                return false;
            };
            return true;
        };
    };
};
var $greater = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $81 = compare(dictOrd)(a1)(a2);
            if ($81 instanceof GT) {
                return true;
            };
            return false;
        };
    };
};
var $greater$eq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var $82 = compare(dictOrd)(a1)(a2);
            if ($82 instanceof LT) {
                return false;
            };
            return true;
        };
    };
};
var categoryFn = new Category(function () {
    return semigroupoidFn;
}, function (x) {
    return x;
});
var boundedUnit = new Bounded(unit, unit);
var boundedOrdering = new Bounded(LT.value, GT.value);
var boundedOrdUnit = new BoundedOrd(function () {
    return boundedUnit;
}, function () {
    return ordUnit;
});
var boundedOrdOrdering = new BoundedOrd(function () {
    return boundedOrdering;
}, function () {
    return ordOrdering;
});
var boundedInt = new Bounded($foreign.bottomInt, $foreign.topInt);
var boundedOrdInt = new BoundedOrd(function () {
    return boundedInt;
}, function () {
    return ordInt;
});
var boundedChar = new Bounded($foreign.bottomChar, $foreign.topChar);
var boundedOrdChar = new BoundedOrd(function () {
    return boundedChar;
}, function () {
    return ordChar;
});
var boundedBoolean = new Bounded(false, true);
var boundedOrdBoolean = new BoundedOrd(function () {
    return boundedBoolean;
}, function () {
    return ordBoolean;
});
var bottom = function (dict) {
    return dict.bottom;
};
var boundedFn = function (dictBounded) {
    return new Bounded(function (v) {
        return bottom(dictBounded);
    }, function (v) {
        return top(dictBounded);
    });
};
var booleanAlgebraUnit = new BooleanAlgebra(function () {
    return boundedUnit;
}, function (v) {
    return function (v1) {
        return unit;
    };
}, function (v) {
    return function (v1) {
        return unit;
    };
}, function (v) {
    return unit;
});
var booleanAlgebraFn = function (dictBooleanAlgebra) {
    return new BooleanAlgebra(function () {
        return boundedFn(dictBooleanAlgebra["__superclass_Prelude.Bounded_0"]());
    }, function (fx) {
        return function (fy) {
            return function (a) {
                return conj(dictBooleanAlgebra)(fx(a))(fy(a));
            };
        };
    }, function (fx) {
        return function (fy) {
            return function (a) {
                return disj(dictBooleanAlgebra)(fx(a))(fy(a));
            };
        };
    }, function (fx) {
        return function (a) {
            return not(dictBooleanAlgebra)(fx(a));
        };
    });
};
var booleanAlgebraBoolean = new BooleanAlgebra(function () {
    return boundedBoolean;
}, $foreign.boolAnd, $foreign.boolOr, $foreign.boolNot);
var $div$eq = function (dictEq) {
    return function (x) {
        return function (y) {
            return not(booleanAlgebraBoolean)($eq$eq(dictEq)(x)(y));
        };
    };
};
var bind = function (dict) {
    return dict.bind;
};
var liftM1 = function (dictMonad) {
    return function (f) {
        return function (a) {
            return bind(dictMonad["__superclass_Prelude.Bind_1"]())(a)(function (v) {
                return $$return(dictMonad["__superclass_Prelude.Applicative_0"]())(f(v));
            });
        };
    };
};
var $greater$greater$eq = function (dictBind) {
    return bind(dictBind);
};
var asTypeOf = function (x) {
    return function (v) {
        return x;
    };
};
var applyFn = new Apply(function () {
    return functorFn;
}, function (f) {
    return function (g) {
        return function (x) {
            return f(x)(g(x));
        };
    };
});
var bindFn = new Bind(function () {
    return applyFn;
}, function (m) {
    return function (f) {
        return function (x) {
            return f(m(x))(x);
        };
    };
});
var apply = function (dict) {
    return dict.apply;
};
var $less$times$greater = function (dictApply) {
    return apply(dictApply);
};
var liftA1 = function (dictApplicative) {
    return function (f) {
        return function (a) {
            return $less$times$greater(dictApplicative["__superclass_Prelude.Apply_0"]())(pure(dictApplicative)(f))(a);
        };
    };
};
var applicativeFn = new Applicative(function () {
    return applyFn;
}, $$const);
var monadFn = new Monad(function () {
    return applicativeFn;
}, function () {
    return bindFn;
});
var append = function (dict) {
    return dict.append;
};
var $plus$plus = function (dictSemigroup) {
    return append(dictSemigroup);
};
var $less$greater = function (dictSemigroup) {
    return append(dictSemigroup);
};
var semigroupFn = function (dictSemigroup) {
    return new Semigroup(function (f) {
        return function (g) {
            return function (x) {
                return $less$greater(dictSemigroup)(f(x))(g(x));
            };
        };
    });
};
var ap = function (dictMonad) {
    return function (f) {
        return function (a) {
            return bind(dictMonad["__superclass_Prelude.Bind_1"]())(f)(function (v) {
                return bind(dictMonad["__superclass_Prelude.Bind_1"]())(a)(function (v1) {
                    return $$return(dictMonad["__superclass_Prelude.Applicative_0"]())(v(v1));
                });
            });
        };
    };
};
var monadArray = new Monad(function () {
    return applicativeArray;
}, function () {
    return bindArray;
});
var bindArray = new Bind(function () {
    return applyArray;
}, $foreign.arrayBind);
var applyArray = new Apply(function () {
    return functorArray;
}, ap(monadArray));
var applicativeArray = new Applicative(function () {
    return applyArray;
}, function (x) {
    return [ x ];
});
var add = function (dict) {
    return dict.add;
};
var $plus = function (dictSemiring) {
    return add(dictSemiring);
};
module.exports = {
    LT: LT, 
    GT: GT, 
    EQ: EQ, 
    Show: Show, 
    BooleanAlgebra: BooleanAlgebra, 
    BoundedOrd: BoundedOrd, 
    Bounded: Bounded, 
    Ord: Ord, 
    Eq: Eq, 
    DivisionRing: DivisionRing, 
    Num: Num, 
    Ring: Ring, 
    ModuloSemiring: ModuloSemiring, 
    Semiring: Semiring, 
    Semigroup: Semigroup, 
    Monad: Monad, 
    Bind: Bind, 
    Applicative: Applicative, 
    Apply: Apply, 
    Functor: Functor, 
    Category: Category, 
    Semigroupoid: Semigroupoid, 
    show: show, 
    "||": $bar$bar, 
    "&&": $amp$amp, 
    not: not, 
    disj: disj, 
    conj: conj, 
    bottom: bottom, 
    top: top, 
    unsafeCompare: unsafeCompare, 
    ">=": $greater$eq, 
    "<=": $less$eq, 
    ">": $greater, 
    "<": $less, 
    compare: compare, 
    "/=": $div$eq, 
    "==": $eq$eq, 
    eq: eq, 
    "-": $minus, 
    negate: negate, 
    sub: sub, 
    "/": $div, 
    mod: mod, 
    div: div, 
    "*": $times, 
    "+": $plus, 
    one: one, 
    mul: mul, 
    zero: zero, 
    add: add, 
    "++": $plus$plus, 
    "<>": $less$greater, 
    append: append, 
    ap: ap, 
    liftM1: liftM1, 
    "return": $$return, 
    ">>=": $greater$greater$eq, 
    bind: bind, 
    liftA1: liftA1, 
    pure: pure, 
    "<*>": $less$times$greater, 
    apply: apply, 
    "void": $$void, 
    "<#>": $less$hash$greater, 
    "<$>": $less$dollar$greater, 
    map: map, 
    id: id, 
    ">>>": $greater$greater$greater, 
    "<<<": $less$less$less, 
    compose: compose, 
    otherwise: otherwise, 
    asTypeOf: asTypeOf, 
    "const": $$const, 
    flip: flip, 
    "#": $hash, 
    "$": $dollar, 
    unit: unit, 
    semigroupoidFn: semigroupoidFn, 
    categoryFn: categoryFn, 
    functorFn: functorFn, 
    functorArray: functorArray, 
    applyFn: applyFn, 
    applyArray: applyArray, 
    applicativeFn: applicativeFn, 
    applicativeArray: applicativeArray, 
    bindFn: bindFn, 
    bindArray: bindArray, 
    monadFn: monadFn, 
    monadArray: monadArray, 
    semigroupString: semigroupString, 
    semigroupUnit: semigroupUnit, 
    semigroupFn: semigroupFn, 
    semigroupOrdering: semigroupOrdering, 
    semigroupArray: semigroupArray, 
    semiringInt: semiringInt, 
    semiringNumber: semiringNumber, 
    semiringUnit: semiringUnit, 
    ringInt: ringInt, 
    ringNumber: ringNumber, 
    ringUnit: ringUnit, 
    moduloSemiringInt: moduloSemiringInt, 
    moduloSemiringNumber: moduloSemiringNumber, 
    moduloSemiringUnit: moduloSemiringUnit, 
    divisionRingNumber: divisionRingNumber, 
    divisionRingUnit: divisionRingUnit, 
    numNumber: numNumber, 
    numUnit: numUnit, 
    eqBoolean: eqBoolean, 
    eqInt: eqInt, 
    eqNumber: eqNumber, 
    eqChar: eqChar, 
    eqString: eqString, 
    eqUnit: eqUnit, 
    eqArray: eqArray, 
    eqOrdering: eqOrdering, 
    ordBoolean: ordBoolean, 
    ordInt: ordInt, 
    ordNumber: ordNumber, 
    ordString: ordString, 
    ordChar: ordChar, 
    ordUnit: ordUnit, 
    ordArray: ordArray, 
    ordOrdering: ordOrdering, 
    boundedBoolean: boundedBoolean, 
    boundedUnit: boundedUnit, 
    boundedOrdering: boundedOrdering, 
    boundedInt: boundedInt, 
    boundedChar: boundedChar, 
    boundedFn: boundedFn, 
    boundedOrdBoolean: boundedOrdBoolean, 
    boundedOrdUnit: boundedOrdUnit, 
    boundedOrdOrdering: boundedOrdOrdering, 
    boundedOrdInt: boundedOrdInt, 
    boundedOrdChar: boundedOrdChar, 
    booleanAlgebraBoolean: booleanAlgebraBoolean, 
    booleanAlgebraUnit: booleanAlgebraUnit, 
    booleanAlgebraFn: booleanAlgebraFn, 
    showBoolean: showBoolean, 
    showInt: showInt, 
    showNumber: showNumber, 
    showChar: showChar, 
    showString: showString, 
    showUnit: showUnit, 
    showArray: showArray, 
    showOrdering: showOrdering
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Prelude/foreign.js"}],"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/foreign.js":[function(require,module,exports){
"use strict";

// module Unsafe.Coerce

exports.unsafeCoerce = function(x) { return x; }

},{}],"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/index.js":[function(require,module,exports){
// Generated by psc version 0.8.0.0
"use strict";
var $foreign = require("./foreign");
module.exports = {
    unsafeCoerce: $foreign.unsafeCoerce
};

},{"./foreign":"/Users/anders/dev/ps/halo/ajax/output/Unsafe.Coerce/foreign.js"}],"/Users/anders/dev/ps/halo/ajax/output/browserify.js":[function(require,module,exports){
require('Main').main();

},{"Main":"/Users/anders/dev/ps/halo/ajax/output/Main/index.js"}],"/usr/local/lib/node_modules/pulp/node_modules/browser-resolve/empty.js":[function(require,module,exports){

},{}]},{},["/Users/anders/dev/ps/halo/ajax/output/browserify.js"]);
