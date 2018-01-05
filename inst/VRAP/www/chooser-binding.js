(function() {

    function updateChooser(chooser) {
	chooser = $(chooser);
	var left = chooser.find("select.chooser-left");
	var right = chooser.find("select.chooser-right");
	var leftArrow = chooser.find(".left-arrow");
	var rightArrow = chooser.find(".right-arrow");
	
	var canMoveTo = (left.val() || []).length > 0;
	var canMoveFrom = (right.val() || []).length > 0;
	
	leftArrow.toggleClass("text-muted", !canMoveFrom);
	rightArrow.toggleClass("text-muted", !canMoveTo);
    }

    function move(chooser, source, dest) {
	chooser = $(chooser);
	var selected = chooser.find(source).children("option:selected");
	var dest = chooser.find(dest);
	dest.children("option:selected").each(function(i, e) {e.selected = false;});

	for (var i = 0; i < selected.length; i++) {
	    var target = selected[i];
	    var successor = $(dest).children("option").map(function() {
		var targetHTML = $(target).html();
		if ($(this).html() > targetHTML) {
		    return this;
		}
		return null;
	    }).first();

	    if (typeof $(successor).html() === 'undefined') {
		dest.append(target);
	    } else {
		$(successor).before(target);
	    }
	}

	updateChooser(chooser);
	chooser.trigger("change");
    }

    $(document).on("change", ".chooser select", function() {
	updateChooser($(this).parents(".chooser"));
    });

    $(document).on("click", ".chooser .right-arrow", function() {
	move($(this).parents(".chooser"), ".chooser-left", ".chooser-right");
    });

    $(document).on("click", ".chooser .left-arrow", function() {
	move($(this).parents(".chooser"), ".chooser-right", ".chooser-left");
    });

    $(document).on("dblclick", ".chooser select.chooser-left", function() {
    	move($(this).parents(".chooser"), ".chooser-left", ".chooser-right");
    });

    $(document).on("dblclick", ".chooser select.chooser-right", function() {
	move($(this).parents(".chooser"), ".chooser-right", ".chooser-left");
    });

    var binding = new Shiny.InputBinding();

    binding.find = function(scope) {
	return $(scope).find(".chooser");
    };

    binding.initialize = function(el) {
	updateChooser(el);
    };

    binding.getValue = function(el) {
	return {
	    left: $.makeArray($(el).find("select.chooser-left option").map(function(i, e) { return e.value; })),
	    right: $.makeArray($(el).find("select.chooser-right option").map(function(i, e) { return e.value; }))
	}
    };

    binding.setValue = function(el, value) {
	var chooser = $(el);
	var left = chooser.find("select.chooser-left");
	var right = chooser.find("select.chooser-right");

	if (value.hasOwnProperty("leftOptions")) {
	    left.empty();
	    for (var i = 0; i < value.leftOptions.length; i++) {
		left.append('<option title="'
			    + value.leftOptions[i] + '">' 
			    + value.leftOptions[i] + '</option>');
	    }
	}

	if (value.hasOwnProperty("rightOptions")) {
	    right.empty();
	    for (var i = 0; i < value.rightOptions.length; i++) {
		right.append('<option title="'
			     + value.rightOptions[i] + '">' 
			     + value.rightOptions[i] + '</option>');
	    }
	}

    };

    binding.subscribe = function(el, callback) {
	$(el).on("change.chooserBinding", function(e) {
	    callback();
	});
    };

    binding.unsubscribe = function(el) {
	$(el).off(".chooserBinding");
    };

    binding.getType = function() {
	return "shinyjsexamples.chooser";
    };

    binding.receiveMessage = function(el, data) {
	if(data.hasOwnProperty("leftOptions") || 
	   data.hasOwnProperty("rightOptions")) {
	    this.setValue(el,data);
	}
    };

    Shiny.inputBindings.register(binding, "shinyjsexamples.chooser");

})();
