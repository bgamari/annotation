function set_annotation(id, state) {
    $.ajax("http://localhost:8000/"+id, {
	data: {'state': state},
	method: 'POST',
    });
}

function toggle_annotation() {
    if ($(this).hasClass("relevant")) {
	$(this).removeClass("relevant");
	$(this).addClass("not-relevant");
    } else if ($(this).hasClass("not-relevant")) {
	$(this).removeClass("not-relevant");
    } else {
	$(this).addClass("relevant");
    }
}

function on_load() {
    //$(".annotation").click(toggle_annotation);
    $(".annotation").each(function() {
	var el = $(this);
	var ann_id = el.attr('id');
	for (state of ['relevant', 'none', 'not-relevant']) {
	    el.append($("<input>", {
		type: "radio",
		name: ann_id+"-ann",
		value: state,
	    }).click(function() {set_annotation(ann_id, state);}));
	}
    });
}

$(window).load(on_load);
