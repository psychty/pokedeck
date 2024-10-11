
// ! Get date label data
var request = new XMLHttpRequest();
request.open("GET", "./Data/collection_values.json", false);
request.send(null);
var poke_df = JSON.parse(request.responseText);
