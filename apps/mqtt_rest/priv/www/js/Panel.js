'use strict';

class Panel extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			activeMenu: 'Land',
			login_user: undefined,
			roles: [],
			auth:false
		};
		RestAPI.checkSession(this.handleCheckSessionSuccess, this.handleCheckSessionError);
		this.parentTd = React.createRef();
		this.warnBoxRef = undefined;
	}
	
	handleCheckSessionSuccess = (json) => {
		console.log('Response GET -> session:: ' + JSON.stringify(json));
		if (json.user) {
			var activeMenu = window.sessionStorage.getItem('activeMenu');

			this.setState({
				auth:true,
				activeMenu: activeMenu,
				login_user: {user_name:json.user, roles:json.roles}
			});

		} else {
			console.log('Cannot retrive session object...')
		}
	}
	
	handleCheckSessionError = (error) => {
		console.log('Error during get session check')
	}

	handleMouseClickMenu(event, command) {
		console.log('Click on menu ' + command);
		switch (command) {
			case 'Logout' :
				this.deleteCookie('sessionid');
				this.setState({
					auth:false,
					activeMenu:'Land',
					login_user: undefined
				});
				command = 'Land';
				break;
			case 'Configuration' :
				this.setState({
					activeMenu:'Configuration',
				});
				break;
			default :
				this.setState({activeMenu: command});
				break;
		};
		window.sessionStorage.setItem('activeMenu', command);
	}

	handleError = (error) => {
		console.log('AJAX error: ' + error);
		this.setState({});
	};

	deleteCookie(name) {
//		console.log('1.Cookie = ' + document.cookie);
		document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/rest;';
//		console.log('2.Cookie = ' + document.cookie);
	}

	handleStateChange = (auth, un, roles) => {
		console.log('Panel state changes: ' + JSON.stringify(this.state) 
				+ ' auth: ' + auth + ' un: ' + un + ' roles: ' + roles);
		var command = 'Land';
		if (auth) {
			command = 'Help';
			this.setState({
				auth: true,
				activeMenu: command,
				login_user: {user_name:un, roles:roles}
			});
		} else {
			command = 'Login';
			this.setState({
				auth: false,
				activeMenu: command,
				login_user: undefined
			});
		};
		window.sessionStorage.setItem('activeMenu', command);
	}
	
	render() {
		var board;
		switch (this.state.activeMenu) {
			case 'Login' :
				board = e(BoardLogin, 
					{
						parent:this.parentTd,
						warnBox:this.warnBoxRef,
						onStateChange:this.handleStateChange
					});
				break;
			case 'Help' :
				board = e(BoardHelp, {key:1});
				break;
			case 'Users' :
				board = e(BoardUsers,
					{
						parent:this.parentTd,
						warnBox:this.warnBoxRef,
						login_user:this.state.login_user
					});
				break;
			case 'Configuration' :
				board = e(BoardConfiguration, 
						{key: 1});
				break;
			case 'Logout' :
				board = e(LandingPage, {key: 1}, null);
				break;
			default : 
				board = e(LandingPage, {key: 1}, null);
				break;
		}
		return e('table', 
			{
				className:'table',
				style:{width:this.props.w, height:this.props.h}
			},
			e('tbody', {}, [
				e('tr', {align:"center", key: 1}, [
					e('td', {key: 1, colSpan:'4', className:'title'}, [
						e(Title, {key: 1, login_user:this.state.login_user})
					])
				]),
				e('tr', {align:"center", className:'menu', key: 2},
					e(Menu, 
					{
						key: 1,
						onMenuClick:(event, command) => this.handleMouseClickMenu(event, command),
						state: this.state.auth,
						active: this.state.activeMenu
					})
				),
				e('tr', {align:"center", key: 3},
					e('td', 
						{
							ref:this.parentTd,
							className:'board-container',
							colSpan:'4'
						}, board
					)
				),
				e('tr', {key: 4}, [
					e('td', {key: 1, colSpan:'1', style:{height: '10px'}}, 
						e('div',
							{
								key: 1 /* for future use */
							}
						)
					),
					e('td', {key: 2, colSpan:'3', style:{height: '10px'}}, [
						e('div',
							{
								id: 'copyright',
								className: 'copyright',
								key: 1
							},
							`©AKrasnopolski 2026 v 0.0.1 (MQTT Monitor)`
						),
						e(WarningBox, {
								key:2,
								ref:(instance) => {this.warnBoxRef = instance;}
						})
					]),
				])
			]
		))
	}
}