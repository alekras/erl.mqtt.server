/**
 * 
 */
'use strict';

const BoardUsers = ({parent, loginUser}) => {
	const [newUser, setNewUser] = React.useState('');
	const [roles, setRoles] = React.useState([]);
	const [addUserBoxDisplay, setAddUserBoxDisplay] = React.useState(false);
	const [type, setType] = React.useState('');
	const [warningBoxDisplay, setWarningBoxDisplay] = React.useState(false);
	const [boxType, setBoxType] = React.useState('');
	const [boxText, setBoxText] = React.useState('');
	const [boxFunc, setBoxFunc] = React.useState(() => {});
	const [users, setUsers] = React.useState([]);

	const handleChange = (event) => {
		setNewUser(event.target.value);
//		console.log('BoardUsers Event comes:: ' + event.target.value + ', ' + event.target.name);
		event.preventDefault();
	}

// users: [{"status":"off","roles":["USER","ADMIN"],"user_name":"echo"}, ...]
	const handleSuccessUsers = (json) => {
//		console.log('Response GET -> users:: ' + JSON.stringify(json));
		setUsers(json);
	}
	
	const handleErrorUsers = (error) => {
		console.log('Error during get users')
	}

	React.useEffect(() => {
			RestAPI.get_users(0, 100, handleSuccessUsers, handleErrorUsers);
		},
		[]
	);
	
	const clickToAddUser = (event) => {
		if (loginUser.roles.includes('ADMIN')) {
			setAddUserBoxDisplay(true);
			setType('save');
		} else {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('Only user with "ADMIN" role can add an users.');
		}
	}
	
	const clickToUpdateUser = (event, userName) => {
		if (loginUser.roles.includes('ADMIN')) {
			if (userName == 'echo' || userName == 'guest') {
				setWarningBoxDisplay(true);
				setBoxType('warn');
				setBoxText('You cannot update system defined user "' + userName + '".');
				return;
			}	
// find user's record:
			let user = users.find((record) => {return (record.user_name === userName);});
			setAddUserBoxDisplay(true);
			setType('update');
			setNewUser(user.user_name);
			setRoles(user.roles);
		} else {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('Only user with "ADMIN" role can update an users.');
		}
	}

	const handleSuccessUser = (json, user_name) => {
		if (type === 'save') {
			handleSuccessAddUser(json, user_name);
		} else if (type === 'update') {
			handleSuccessUpdateUser(json, user_name);
		}
	}
	
	const handleSuccessAddUser = (json, user_name) => {
		console.log('Success: user_name=' + user_name + ', ' + JSON.stringify(json));
		if (json.code && json.code === 400) {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('This user name already exists.<br/>Please try another.');
		} else {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('User "' + user_name + '" is successfully added.');
			clickToRefresh();
		}
	}
	
	const handleSuccessUpdateUser = (json, user_name) => {
		console.log('Success Update: user_name=' + user_name + ', ' + JSON.stringify(json));
		if (json.code && json.code === 404) {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('This user name not found.<br/>Please try another.');
		} else {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('User "' + user_name + '" is successfully updated.');
			clickToRefresh();
		}
	}
	
	const handleErrorAddOrUpdateUser = (error) => {
		console.log('Error during add/update new contact: ' + error);
	}
	
	const handleUserBoxClose = (event) => {
		setAddUserBoxDisplay(false);
		setNewUser('');
		setRoles([]);
	}
	
	const handleWarningBoxClose = (event) => {
		setWarningBoxDisplay(false);
		setNewUser('');
		setRoles([]);
	}
	
	const clickToRefresh = (event) => {
		RestAPI.get_users(0, 100, handleSuccessUsers, handleErrorUsers);
		setNewUser('');
		setRoles([]);
	}
	
	const clickToRemove = (event, userName) => {
		if (!loginUser.roles.includes('ADMIN')) {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('Only user with "ADMIN" role can delete an users.');
			return;
		}
		if (userName == 'echo' || userName == 'guest' || userName == 'admin') {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('You cannot remove system defined user "' + userName + '" from users list.');
		} else {
			setWarningBoxDisplay(true);
			setBoxType('confirm');
			setBoxText('Do you want to remove "' + userName + '" from users list?');
			setBoxFunc(() => (arg) => {
					if (arg) {
						RestAPI.remove_user(
						userName, 
						handleSuccessRemoveUser, 
						handleErrorRemoveUser)
					}
				});
		}
	}
	
	const handleSuccessRemoveUser = (json, user) => {
		console.log('Response Remove -> user:' + user + ', json:' + JSON.stringify(json));
		if (json.code && json.code === 404) {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('This user "' + user + '"does not exist.');
		} else {
			setWarningBoxDisplay(true);
			setBoxType('warn');
			setBoxText('User "' + user + '" has successfully removed.');
			clickToRefresh();
		}
	}
	
	const handleErrorRemoveUser = (error) => {
		console.log('Error during remove contact:' + error);
	}

	const renderControlHeader= () => {
		return e('tr', {key:0, align:'center', style:{backgroundColor:'rgb(156,222,228)'}}, [
			e('td', {key:1, className:'td-users-header'}, 
				e('div', {key:1,
					className:'button btn-add',
					onClick:clickToAddUser
				})
			),
			e('td', {key:2}, 
				e('input', {key:1,
					className:'text-input',
					name:'new_user',
					placeholder:'User name to add',
					onChange:handleChange,
					value:newUser
				})
			),
			e('td', {key:3, className:'td-users-header'}, 
				e('div', {key:1,
					className:'button btn-refresh',
					onClick:clickToRefresh}
				)
			)
		]);
	}
	
// users: [{"status":"off","roles":["USER","ADMIN"],"user_name":"echo"}, ...]
	const renderUsersBoard = () => {
		let i = 0;
		let rows = users.map((user) => {
			i++;
//			console.log('i:' + i + ' user name:' + user.user_name + ' status:' + user.status + ' roles:' + user.roles);
			return e(Record, {key:i,
				user:user.user_name,
				status:user.status,
				roles:user.roles,
				clickToRemove:clickToRemove,
				clickToUpdate:clickToUpdateUser});
		});
		return rows;
	}
	
	let r1 = renderControlHeader();
	let r2 = renderUsersBoard();
	var r3 = null;
	var r4 = null;

	if (addUserBoxDisplay) {
		r3 = 
		e(AddUserBox, 
			{
				key:1,
				type: type,
				user_name: newUser,
				par_roles: roles,
				layout: parent.current.getBoundingClientRect(),
				onSuccess: handleSuccessUser,
				onError: handleErrorAddOrUpdateUser,
				onBoxClose: handleUserBoxClose
			});
	};
	if (warningBoxDisplay) {
		r4 =
		e(WarningBox,
			{
				key:1,
				type:boxType,
				warning:boxText,
				layout:parent.current.getBoundingClientRect(),
				yesNoFun:boxFunc,
				onBoxClose:handleWarningBoxClose
			});
	};
	return e(
			'table',
			{
				className:'tbl-users'
			}, 
			e('tbody', {key:1}, [
				r1,
				e('tr', {key:1},
					e('td', {key:1, colSpan:'3', className:'users-header'},
					[
						e('span', {key:0, className:'users-header-name'}, 'User name'),
						e('span', {key:1, className:'users-header-roles'}, 'Roles'),
						e('span', {key:2, className:'users-header-remove'}, 'Remove'),
						e('span', {key:3, className:'users-header-status'}, 'Status')
					])
				),
				e('tr', {key:2}, 
					e('td', {key:1, colSpan:'3'},
						e('div', {key:1, className:'board-users'}, r2)
					)
				),
				e('tr', {key:3, height:'0px'}, 
					e('td', {key:1, colSpan:'3'}, r3)
				),
				e('tr', {key:4, height:'0px'}, 
					e('td', {key:1, colSpan:'3'}, r4)
				)
			])
		);
}

const Record = ({user, roles, status, clickToUpdate, clickToRemove}) => {
		var status, bgColor, bgColorUser;
		if (status === 'on') {
			status = 'online';
			bgColor = 'Aquamarine';
			bgColorUser = 'Aquamarine';
		} else if (status === 'off') {
			status = 'offline';
			bgColor = '#FDD7E4'; //'LightSalmon';
			bgColorUser = '#87CEEB';
		}

	return e('div', {key:1, colSpan:'3', className:'div-user-record'}, [
		e('div', 
			{key:1,
			 className:'user-id-record',
			 onClick:(e)=>clickToUpdate(e, user),
			 style:{backgroundColor:bgColorUser}}, 
			user),
		e('div', {key:2, className:'user-roles-record'},
			JSON.stringify(roles)),
		e('div', 
			{key:4,
			 className:'button btn-remove',
			 onClick:(e)=>clickToRemove(e, user)
			}),
		e('div', 
			{key:3,
			 className:'user-status-record',
			 style:{backgroundColor:bgColor}
			}, status)
		]);
}
