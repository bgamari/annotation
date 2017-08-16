window.indexedDB = window.indexedDB || window.mozIndexedDB || window.webkitIndexedDB || window.msIndexedDB

DB_NAME = "laura-annotations"
STORE_NAME = "annotations"

upload_url = "/annotation"


String.prototype.hashCode = ->
  hash = 0
  if this.length == 0
      return hash
  for _,i in this
    chr = this.charCodeAt(i)
    hash  = ((hash << 5) - hash) + chr
    hash |= 0   # Convert to 32bit integer
  return hash

db = null

# annotations[ann_id] = [element ids]
annotations = {}

parse_options = (str) ->
    if str == undefined
        #return [[['none', null]], [['not-relevant', 0], ['relevant', 1]]]
        return [[['Perfect', 5], ['Must', 4], ['Should', 3], ['Can', 2]],
                [['No', 0], ['<span class="glyphicon glyphicon-trash">', -2]],
                [['<span class="glyphicon glyphicon-erase">', null]]]

    opts = {}
    for _,s in str.split(';')
        group = []
        for _,i in s.split(',')
            [k,v] = s.split('=')
            group.push([k,v])
    return opts


set_annotation = (ev) ->
    el = $(this)
    ann_id = parseInt(el.closest('.annotate').attr("data-ann-id"))
    val = el.attr("data-value")
    toolbar = annotations[ann_id].toolbar
    $("button", $(toolbar)).removeClass("active")
    $("button[data-value=#{val}]", $(toolbar)).addClass("active")

    objectStore = db.transaction([STORE_NAME], "readwrite").objectStore(STORE_NAME)
    if val == undefined
        req = objectStore.delete(ann_id)
    else
        req = objectStore.put {
            ann_id: ann_id,
            rel: val,
            query: annotations[ann_id].query,
            item: annotations[ann_id].item
        }
    req.onerror = (ev) -> console.log("Failed to set annotataion: "+req.error)
    req.onsuccess = (ev) -> console.log("Set annotation "+ann_id+" to "+val)

add_annotations = ->
    $(".annotation").each (i) ->
        el = $(this)
        ann_id = (el.data('query') + el.data('item')).hashCode()
        options = parse_options(el.attr("data-ann-options"))

        toolbar = $("<div>")
                  .addClass('btn-toolbar annotate')
                  .attr('role', 'toolbar')
                  .attr("data-ann-id", ann_id)

        for _, group_opts of options
            group = $("<div class='btn-group' role='toolbar'>")
            toolbar.append(group)
            for _, [name, val] of group_opts
                do (name, val) ->
                    opt = $("<button>")
                          .addClass("btn btn-sm")
                          .attr("data-value", val)
                          .html(name)
                    opt.click set_annotation
                    group.append opt

        el.append(toolbar)
        annotations[ann_id] = {
            query: el.data('query'),
            item: el.data('item'),
            toolbar: toolbar
        }


add_toolbar = ->
    div = $("<div>").attr('id', 'toolbar').addClass("toolbar")
    status = $("<span>")
    div.append status

    sess = $("<input>", {
        id: "session-name",
        placeholder: "Session name",
        })
    sess.change (ev) ->
        sessionStorage.setItem("session-name", $(this).val())
    sessOld = sessionStorage.getItem("session-name")
    sessOld = "NA" if not sessOld 
    sess.val(sessOld )
    div.append sess 

    export_btn = $("<button>Export</button>")
    div.append export_btn
    export_btn.click ->
        generate_qrel (qrel) ->
            $("#qrel").remove()
            area = $("<textarea>", {id: "qrel"}).html(qrel)
            area.css('width', '50em')
            $("#toolbar").after area

    upload_btn = $("<button>Upload</button>")
    div.append upload_btn
    upload_btn.click ->
        generate_qrel (qrel) ->
            $.ajax(upload_url, {
                type: "POST",
                data: {
                    "session": $("#session-name").val(),
                    "qrel": qrel,
                },
            });

    clear_btn = $("<button>Clear</button>")
    clear_btn.click ->
        req = db
            .transaction([STORE_NAME], "readwrite")
            .objectStore(STORE_NAME)
            .clear()
        req.onsuccess = ->
            console.log("Clear successful")
            $(".annotation button").removeClass('active')
        req.onerror = -> console.log("Clear failed: "+req.error)

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
            rel = val.rel
            if rel isnt null
                accum = accum + "#{val.query}\t#{sess}\t#{val.item}\t#{rel}\n"
            cursor.continue()
        else
            on_done accum

load_existing_annotations = ->
    for ann_id,ann of annotations
        do (ann_id, ann) ->
            ann_id = parseInt(ann_id)
            objectStore = db.transaction([STORE_NAME], "readonly").objectStore(STORE_NAME)
            req = objectStore.get(ann_id)
            req.onsuccess = (ev) ->
                if ev.target.result
                    val = ev.target.result.rel
                    $("button[data-value=#{val}]", ann.toolbar).addClass('active')

delay = (ms, func) -> setTimeout func, ms

notify = (msg, klass) ->
    el = $('<li>').html(msg)
    el.addClass(klass)
    $("#notifications").append el
    delay 5000, () -> el.fadeOut()

$(document).ready ->
    add_toolbar()
    add_annotations()

    $("head").append $("<style>
        #notifications { float: right; list-style: none; }
        #notifications li { border-radius: 1em; margin: 1em; padding: 2em; }
        #notifications li.fail { background-color: #fdd; }
        #notifications li.success { background-color: #ded; }
        </style>")
    notifications = $('<ul id="notifications"></ul>')
    notifications = $("body").prepend notifications

    if (!window.indexedDB)
        notify("Your browser doesn't support IndexedDB. Please use a modern browser.", 'error')

    req = window.indexedDB.open(DB_NAME, 4)
    req.onsuccess = (ev) ->
        db = this.result
        load_existing_annotations()
    req.onerror = (ev) ->
        notify("Failed to open annotations database: "+ev)
    req.onupgradeneeded = (ev) ->
        db = ev.target.result
        db.createObjectStore(STORE_NAME, { keyPath: "ann_id" })

$(document).ajaxError (ev, resp) -> notify(resp.responseText, 'fail')
$(document).ajaxSuccess (ev, resp) -> notify("annotation successfully saved", 'success')
