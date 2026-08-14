'use strict';

class RestAPI {

	constructor() {
	}
	
	static myHeaders() {
		let headers = new Headers();
		headers.append('Accept', 'application/json');
		headers.append('Content-Type', 'application/json');
		headers.append('authorization', 'mqtt');
		return headers;
	}

	static loginRequest(state, handleSuccess, handleError) {
		console.log('userName=' + state.userName + '; password=' + state.password);
		let myHeaders = new Headers();
		myHeaders.append('Accept', 'application/json');
		myHeaders.append('Content-Type', 'application/json');
		myHeaders.append('authorization', 'mqtt');
		let req = new Request('/rest/user/login/' + state.userName, {
			method: 'POST', 
			headers: myHeaders, 
			body: JSON.stringify({password:state.password})
			});
		fetch(req)
			.then(res => res.json())
			.then(
				handleSuccess,
				handleError
			);
	}
	
	static checkSession(handleSuccess, handleError) {
		let myHeaders = new Headers();
		myHeaders.append('Accept', 'application/json');
		let req = new Request('/sim/checksession', {
			method: 'GET', 
			headers: myHeaders
			});
		fetch(req)
			.then(res => res.json())
			.then(json => handleSuccess(json))
			.catch(err => handleError(err));
	}
	
	static registerRequest(state, handleSuccess, handleError) {
		let myHeaders = new Headers();
		myHeaders.append('Accept', 'application/json');
		let req = new Request('/sim/register', {
			method: 'POST', 
			headers: myHeaders, 
			body: 'user=' + state.userName 
				+ '&password1=' + state.password1 
				+ '&password2=' + state.password2
			});
		fetch(req)
			.then(res => res.json())
			.then(
				handleSuccess,
				handleError
			);
	}
	
	static getConfig(app, handleSuccess, handleError) {
		let myHeaders = new Headers();
		myHeaders.append('Accept', 'application/json');
		myHeaders.append('Content-Type', 'application/json');
		myHeaders.append('authorization', 'mqtt');
		let req = new Request('/rest/server/config?app=' + app, {
			method: 'GET', 
			headers: myHeaders
			});
		fetch(req)
			.then(res => res.json())
			.then(
				handleSuccess,
				handleError
			);
	}
	
	static get_users(index1, index2, handleSuccess, handleError) {
		let myHeaders = new Headers();
		myHeaders.append('Accept', 'application/json');
		myHeaders.append('Content-Type', 'application/json');
		myHeaders.append('authorization', 'mqtt');
		let req = new Request('/rest/user/list?indexes=' + index1 + ',' + index2, {
			method: 'GET', 
			headers: myHeaders
			});
		fetch(req)
			.then(res => res.json())
			.then(
				handleSuccess,
				handleError
			);
	}

	static add_user(state, handleSuccess, handleError) {
//		console.log('new contact=' + new_contact);
		let myHeaders = new Headers();
		myHeaders.append('Accept', 'application/json');
		myHeaders.append('Content-Type', 'application/json');
		myHeaders.append('authorization', 'mqtt');
		let req = new Request('/rest/user/' + state.user_name, {
			method: 'POST', 
			headers: myHeaders,
			body: JSON.stringify({password:state.password1, roles:state.roles})
			});
		fetch(req)
			.then(res => res.json())
			.then(json => handleSuccess(json, state.user_name))
			.catch(err => handleError(err));
	}
	
	static remove_user(user, handleSuccess, handleError) {
		let headers = RestAPI.myHeaders();
		let req = new Request('/rest/user/' + user, {
			method: 'DELETE', 
			headers: headers,
			body: ''
			});
		fetch(req)
			.then(res => res.json())
			.then(json => handleSuccess(json, user))
			.catch(err => handleError(err));
	}
}

