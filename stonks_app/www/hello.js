"
shinyjs.toggleBox = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
};
shinyjs.expandBox = function(boxid) {
if (document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}};
shinyjs.collapseBox = function(boxid) {
if (!document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}}"

shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}

