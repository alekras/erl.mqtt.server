'use strict';

const Panel = ({h, w}) => {
	const [activeMenu, setActiveMenu] = React.useState();
	const [loginUser, setLoginUser] = React.useState();
	const [roles, setRoles] = React.useState();
	const [auth, setAuth] = React.useState();

	var parentTd = React.useRef(null);
	
	const handleCheckSessionSuccess = (json) => {
		console.log('Response GET -> session:: ' + JSON.stringify(json));
		if (json.user) {
			setAuth(true);
			setActiveMenu(window.sessionStorage.getItem('activeMenu'));
			setLoginUser({user_name:json.user, roles:json.roles});
		} else {
			console.log('Cannot retrive session object...')
		}
	}
	
	const handleCheckSessionError = (error) => {
		console.log('Error during get session check')
	}

	React.useEffect(() => {
			RestAPI.checkSession(handleCheckSessionSuccess, handleCheckSessionError);
		},
		[]
	);

	const handleMouseClickMenu = (event, command) => {
		console.log('Click on menu ' + command);
		switch (command) {
			case 'Logout' :
				deleteCookie('sessionid');
				setAuth(false);
				setActiveMenu('Land');
				setLoginUser(null);
				command = 'Land';
				break;
			default :
				setActiveMenu(command);
				break;
		};
		window.sessionStorage.setItem('activeMenu', command);
	}

	const handleError = (error) => {
		console.log('AJAX error: ' + error);
		setActiveMenu('Land');
	};

	const deleteCookie = (name) => {
//		console.log('1.Cookie = ' + document.cookie);
		document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:01 GMT;path=/rest;';
//		console.log('2.Cookie = ' + document.cookie);
	}

	const handleStateChange = (auth, un, roles) => {
		console.log('Panel state changes: ' 
				+ ' auth: ' + auth + ' un: ' + un + ' roles: ' + roles);
		var command = 'Land';
		if (auth) {
			command = 'Help';
			setAuth(true);
			setActiveMenu(command);
			setLoginUser({user_name:un, roles:roles});
		} else {
			command = 'Login';
			setAuth(false);
			setActiveMenu(command);
			setLoginUser(null);
		};
		window.sessionStorage.setItem('activeMenu', command);
	}
	
	var board;
	switch (activeMenu) {
		case 'Login' :
			board = e(BoardLogin, 
				{
					parent:parentTd,
					onStateChange:handleStateChange
				});
			break;
		case 'Help' :
			board = e(BoardHelp, {key:1});
			break;
		case 'Users' :
			board = e(BoardUsers,
				{
					parent:parentTd,
					loginUser:loginUser
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
			style:{width:w, height:h}
		},
		e('tbody', {}, [
			e('tr', {align:"center", key: 1}, [
				e('td', {key: 1, colSpan:'4', className:'title'}, [
					e(Title, {key: 1, login_user:loginUser})
				])
			]),
			e('tr', {align:"center", className:'menu', key: 2},
				e(Menu, 
				{
					key: 1,
					onMenuClick:(event, command) => handleMouseClickMenu(event, command),
					state: auth,
					active: activeMenu
				})
			),
			e('tr', {align:"center", key: 3},
				e('td', 
					{
						ref:parentTd,
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
						`©AKrasnopolski 2026 v 0.0.2 (MQTT Monitor)`
					)
				]),
			])
		]
	))
}