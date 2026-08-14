'use strict';
const e = React.createElement;

var index = 0;

function start() {
	let ismobile = (navigator.userAgent.match(/(iPad)|(iPhone)|(iPod)|(android)|(webOS)/i)) ? true : false;
	let wo = window.innerWidth, 
		ho = window.innerHeight;
	let w, h;
	if (ismobile) {
		changeLayout('mobile', index);
		w = wo;
		h = ho;
	} else {
		changeLayout('default', index);
		w = Math.floor(wo/2);
		h = Math.floor(ho * 0.8);
	}
	w = w + 'px';
	h = h + 'px';
	console.log(">> start. is Mobile=" + ismobile
		+ "; h=" + h + "(" + ho + ")"
		+ "; w=" + w + "(" + wo + ")"
		+ "; index= " + index);
	ReactDOM.render(e(Panel, {h:h, w:w}), document.getElementById('main'));
}

function changeLayout(description){
	var href;
	if (description == "mobile") {
		href = "/mqtt/css/mqtt-mob.css";
	} else {
		href = "/mqtt/css/mqtt.css";
	}
	document.getElementById("link").setAttribute("href", href);
}
