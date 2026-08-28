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
		let headers = RestAPI.myHeaders();
		let req = new Request('/rest/user/login/' + state.userName, {
			method: 'POST', 
			headers: headers, 
			body: JSON.stringify({password:state.password})
			});
		fetch(req)
			.then(res => res.json())
			.then(json => handleSuccess(json, state.userName))
			.catch(err => handleError(err));
	}
	
	static checkSession(handleSuccess, handleError) {
		let headers = RestAPI.myHeaders();
		let req = new Request('/rest/server/checksession', {
			method: 'GET', 
			headers: headers
			});
		fetch(req)
			.then(res => res.json())
			.then(json => handleSuccess(json))
			.catch(err => handleError(err));
	}
	
	static getConfig(app, handleSuccess, handleError) {
		let headers = RestAPI.myHeaders();
		let req = new Request('/rest/server/config?app=' + app, {
			method: 'GET', 
			headers: headers
			});
		fetch(req)
			.then(res => res.json())
			.then(
				handleSuccess,
				handleError
			);
	}
	
	static get_users(index1, index2, handleSuccess, handleError) {
		let headers = RestAPI.myHeaders();
		let req = new Request('/rest/user/list?indexes=' + index1 + ',' + index2, {
			method: 'GET', 
			headers: headers
			});
		fetch(req)
			.then(res => res.json())
			.then(
				handleSuccess,
				handleError
			);
	}

	static add_user(state, handleSuccess, handleError) {
		let headers = RestAPI.myHeaders();
		let req = new Request('/rest/user/' + state.user_name, {
			method: 'POST', 
			headers: headers,
			body: JSON.stringify({password:state.password, roles:state.roles})
			});
		fetch(req)
			.then(res => res.json())
			.then(json => handleSuccess(json, state.user_name))
			.catch(err => handleError(err));
	}
	
	static update_user(state, handleSuccess, handleError) {
		let headers = RestAPI.myHeaders();
		let req = new Request('/rest/user/' + state.user_name, {
			method: 'PUT', 
			headers: headers,
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

