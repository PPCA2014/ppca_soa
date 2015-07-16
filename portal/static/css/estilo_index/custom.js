$('video').on('ended', function () {
	console.log("Restarting");
  	this.load();
  	this.play();
});