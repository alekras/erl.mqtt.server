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
//	w = w + 'px';
//	h = h + 'px';
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

function browserType() {
	if (navigator.userAgentData) {
// Get high-level brands array
		const brands = navigator.userAgentData.brands;
//		console.log(">> browserType. userAgentData.brands=" + JSON.stringify(brands));

// Check if a specific brand is present
		if (brands.some(b => b.brand === 'Google Chrome')) {
			return 'Chrome';
		} else if (brands.some(b => b.brand === 'Microsoft Edge')) {
			return 'Edge';
		} else if (brands.some(b => b.brand === 'Mozilla Firefox')) {
			return 'FF'
		} else if (brands.some(b => b.brand === 'Apple Safari')) {
			return 'Safari';
		} else {
			return 'Unknown';		
		};
	} else {
		const ua = navigator.userAgent;
//		console.log(">> browserType. userAgent=" + ua);

// Order matters because some browsers include other browser names in their UA string
		if (/edg/i.test(ua)) {
			return 'Edge'; //"Microsoft Edge";
		}
		if (/chrome|crios/i.test(ua) && !/edg/i.test(ua)) {
			return 'Chrome'; //"Google Chrome";
		}
		if (/firefox|fxios/i.test(ua)) {
			return 'FF'; //"Mozilla Firefox";
		}
		if (/safari/i.test(ua) && !/chrome|crios/i.test(ua) && !/edg/i.test(ua)) {
			return 'Safari'; //"Apple Safari";
		}
		if (/trident/i.test(ua)) {
			return 'Expl'; //"Internet Explorer"; // Legacy
		}
		return 'Unknown';
	}
}
