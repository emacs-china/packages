$(document).ready(function() {
	$('table').filterTable({
		label: '',
		placeholder: 'Search <%= JSON.parse(open("all.json").read).length %> packages across 7 ELPA...'
	});
});
