window.indexedDB = window.indexedDB || window.mozIndexedDB || window.webkitIndexedDB || window.msIndexedDB

DB_NAME = "laura-annotations"
STORE_NAME = "annotations"

db = null
req = window.indexedDB.open(DB_NAME, 4)
req.onsuccess = (ev) ->
    db = this.result

    # Update annotations
    $(".annotation").each (i) -> 
        el = $(this)
        ann_id = el.data('query') + el.data('item')
        objectStore = db.transaction([STORE_NAME], "readonly").objectStore(STORE_NAME)
        req = objectStore.get(ann_id)
        req.onsuccess = (ev) ->
            if ev.target.result
                rel = ev.target.result.rel
                $("input[name=group-#{i}][value=#{rel}]").attr('checked', true)

req.onupgradeneeded = (ev) ->
    db = ev.target.result
    db.createObjectStore(STORE_NAME, { keyPath: "ann_id" })

# annotations[ann_id] = [element ids]
annotations = {}
states = ['not-relevant', 'none', 'relevant']

set_annotation = (ev) ->
    el = $(this)
    ann_id = el.attr("data-ann-id")
    val = el.val()
    $("input[data-ann-id=#{ann_id}][value=#{val}]").prop("checked", true)

    objectStore = db.transaction([STORE_NAME], "readwrite").objectStore(STORE_NAME)
    objectStore.put {
        'ann_id': ann_id,
        rel: val
        query: annotations[ann_id].query
        item: annotations[ann_id].item
    }
        
add_annotations = ->
    $(".annotation").each (i) -> 
        el = $(this)
        ann_id = el.data('query') + el.data('item')
        if ann_id not in annotations
            annotations[ann_id] = {
                query: el.data('query'),
                item: el.data('item')
                fields: []
            }
        annotations[ann_id].fields.push i

        el.append($('<label>', {'for': "ann-#{ann_id}-not-relevant"}).html('-'))
        for _, state of states
            do (state) ->
                opt = $("<input>", {
                    type: "radio",
                    id: "ann-#{ann_id}-#{i}",
                    name: "group-#{i}",
                    value: state,
                    'data-ann-id': ann_id,
                })
                opt.click set_annotation
                el.append opt
        el.append($('<label>', {'for': "ann-#{ann_id}-relevant"}).html('+'))

 
add_toolbar = ->
    div = $("<div>").addClass("toolbar")
    status = $("<span>")
    div.append div

    sess = $("<input>", {
        id: "session-name",
        placeholder: "Session name",
        })
    sess.change (ev) ->
        sessionStorage.setItem("session-name", $(this).val())
    sess.val(sessionStorage.getItem("session-name"))
    div.append sess

    export_btn = $("<button>Export</button>")
    export_btn.click ->
        generate_qrel (qrel) ->
            $("#qrel").remove()
            $("body").append($("<pre>", {id: "qrel"}).html(qrel))
    div.append export_btn

    clear_btn = $("<button>Clear</button>")
    clear_btn.click ->
        db.transaction([STORE_NAME], "readwrite").objectStore(STORE_NAME).clear()
    div.append clear_btn

    $("body").prepend div

generate_qrel = (on_done) ->
    objectStore = db.transaction([STORE_NAME], "readonly").objectStore(STORE_NAME)
    accum = ""
    req = objectStore.openCursor()
    sess = $("#session-name").val()
    if sess == ""
        sess = "0"
    req.onsuccess = (ev) ->
        cursor = ev.target.result
        if cursor
            val = cursor.value
            rel = switch val.rel
                when 'not-relevant' then 0
                when 'relevant' then 1
                else null
            if rel isnt null
                accum = accum + "#{val.query}\t#{sess}\t#{val.item}\t#{rel}\n"
            cursor.continue()
        else
            on_done accum

$(document).ready ->
    add_toolbar()
    add_annotations()
