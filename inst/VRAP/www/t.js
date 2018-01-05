(function() {

    $('#recalcButton').click(function() {
//	alert($('a[data-value="resultstab"]').html());
	$('a[data-value="resultstab"]').click();
    });
    // function updateChooser(chooser) {
    // 	chooser = $(chooser);
    // 	var left = chooser.find("select.chooser-left");
    // 	var right = chooser.find("select.chooser-right");
    // 	var leftArrow = chooser.find(".left-arrow");
    // 	var rightArrow = chooser.find(".right-arrow");
	
    // 	var canMoveTo = (left.val() || []).length > 0;
    // 	var canMoveFrom = (right.val() || []).length > 0;
	
    // 	leftArrow.toggleClass("text-muted", !canMoveFrom);
    // 	rightArrow.toggleClass("text-muted", !canMoveTo);
    // }


})();
