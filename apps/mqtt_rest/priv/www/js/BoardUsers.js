/**
 * 
 */
'use strict';

class BoardUsers extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			newUser:'',
			addUserBoxDisplay: false
		};
		RestAPI.get_users(0, 100, this.handleSuccessUsers, this.handleErrorUsers);
	}
		
	static users = [];

	handleChange(event) {
		this.setState({newUser: event.target.value});
//		console.log('BoardUsers Event comes:: ' + event.target.value + ', ' + event.target.name);
		event.preventDefault();
	}

// users: [{"status":"off","roles":["USER","ADMIN"],"user_name":"echo"}, ...]
	handleSuccessUsers = (json) => {
//		console.log('Response GET -> users:: ' + JSON.stringify(json));
		BoardUsers.users = json;
		this.setState({});
	}
	
	handleErrorUsers = (error) => {
		console.log('Error during get users')
	}
	
	clickToAddUser = (event) => {
		if (this.props.login_user.roles.includes('ADMIN')) {
			this.setState({addUserBoxDisplay:true, type:'save'});
		} else {
			this.props.warnBox.setLayout(
				'warn',
				'Only user with "ADMIN" role can add an users.',
				this.props.parent.current.getBoundingClientRect()
			);
		}
	}
	
	clickToUpdateUser = (event, user_name) => {
		if (this.props.login_user.roles.includes('ADMIN')) {
// find user's record:
			let user = BoardUsers.users.find((record) => {return (record.user_name === user_name);});
			this.setState({addUserBoxDisplay:true, type:'update', newUser:user.user_name, roles:user.roles});
		} else {
			this.props.warnBox.setLayout(
				'warn',
				'Only user with "ADMIN" role can update an users.',
				this.props.parent.current.getBoundingClientRect()
			);
		}
	}

	handleSuccessAddUser = (json, user_name) => {
		console.log('Success: user_name=' + user_name + ', ' + JSON.stringify(json));
		if (json.code && json.code === 400) {
			this.props.warnBox.setLayout(
				'warn', 
				'This user name already exists.<br/>Please try another.', 
				this.props.parent.current.getBoundingClientRect()
			);
		} else {
			this.props.warnBox.setLayout(
				'warn', 
				'User "' + user_name + '" is successfully added.', 
				this.props.parent.current.getBoundingClientRect()
			);
			this.clickToRefresh();
		}
	}
	
	handleErrorAddUser = (error) => {
		console.log('Error during add new contact');
	}
	
	handleAddUserBoxClose = (event) => {
		this.setState({addUserBoxDisplay:false, newUser:''});
	}
	
	clickToRefresh = (event) => {
		RestAPI.get_users(0, 100, this.handleSuccessUsers, this.handleErrorUsers);
		this.setState({newUser:''});
	}
	
	clickToRemove = (event, userName) => {
		if (!this.props.login_user.roles.includes('ADMIN')) {
			this.props.warnBox.setLayout(
				'warn',
				'Only user with "ADMIN" role can delete an users.',
				this.props.parent.current.getBoundingClientRect()
			);
			console.log('login user does not ADMIN');
			return;
		}
		if (userName == 'echo' || userName == 'guest' || userName == 'admin') {
			this.props.warnBox.setLayout(
				'warn',
				'You cannot remove system defined user "' + userName + '" from users list. ',
				this.props.parent.current.getBoundingClientRect()
			);
		} else {
			this.props.warnBox.setLayout(
				'confirm',
				'Do you want to remove "' + userName + '" from users list?',
				this.props.parent.current.getBoundingClientRect(),
				(arg) => {
					if (arg) {
						RestAPI.remove_user(
						userName, 
						this.handleSuccessRemoveUser, 
						this.handleErrorRemoveUser)
					}
				} 
			);
		}
	}
	
	handleSuccessRemoveUser = (json, user) => {
		console.log('Response Remove -> user:' + user + ', json:' + JSON.stringify(json));
		if (json.code && json.code === 404) {
			this.props.warnBox.setLayout(
				'warn', 
				'This user "' + user + '"does not exist.', 
				this.props.parent.current.getBoundingClientRect()
			);
		} else {
			this.props.warnBox.setLayout(
				'warn', 
				'User "' + user + '" has successfully removed.', 
				this.props.parent.current.getBoundingClientRect()
			);
			this.clickToRefresh();
		}
	}
	
	handleErrorRemoveUser = (error) => {
		console.log('Error during remove contact:' + error);
	}
	
	shouldComponentUpdate(nextProps, nextState) {
		if (this.state !== nextState) {
			return true;
		}
		return false;
	}

	renderControlHeader() {
		return e('tr', {key:0, align:'center', style:{backgroundColor:'rgb(156,222,228)'}}, [
			e('td', {key:1, className:'td-users-header'}, 
				e('div', {key:1,
					className:'button btn-add',
					onClick:(e)=>this.clickToAddUser(e)
				})
			),
			e('td', {key:2}, 
				e('input', {key:1,
					className:'text-input',
					name:'new_user',
					placeholder:'User name to add',
					onChange:(e)=>this.handleChange(e),
					value:this.state.newUser
				})
			),
			e('td', {key:3, className:'td-users-header'}, 
				e('div', {key:1,
					className:'button btn-refresh',
					onClick:(e)=>this.clickToRefresh(e)}
				)
			)
		]);
	}
	
// users: [{"status":"off","roles":["USER","ADMIN"],"user_name":"echo"}, ...]
	renderUsersBoard() {
		let i = 0;
		let rows = BoardUsers.users.map((user) => {
			i++;
//			console.log('i:' + i + ' user name:' + user.user_name + ' status:' + user.status + ' roles:' + user.roles);
			return e(Record, {key:i,
				user:user.user_name,
				status:user.status,
				roles:user.roles,
				clickToRemove:this.clickToRemove,
				clickToUpdate:this.clickToUpdateUser});
		});
		return rows;
	}
	
	render() {
		let r1 = this.renderControlHeader();
		let r2 = this.renderUsersBoard();
		var r3;

		if (this.state.addUserBoxDisplay) {
			r3 = 
			e(AddUserBox, 
				{
					key:1,
					type: this.state.type,
					user_name: this.state.newUser,
					roles: this.state.roles,
					layout: this.props.parent.current.getBoundingClientRect(),
					onSuccess: this.handleSuccessAddUser,
					onError: this.handleErrorAddUser,
					onBoxClose: this.handleAddUserBoxClose
				}
			)
		} else {
			r3 = null;
		}
		
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
				)
			])
		);
	}
}

class Record extends React.Component {
	constructor(props) {
		super(props);
		this.state = {};
	}
	
	render() {
		let status;
		let bgColor;
		if (this.props.status === 'on') {
			status = 'online';
			bgColor = 'Aquamarine';
		} else if (this.props.status === 'off') {
			status = 'offline';
			bgColor = '#FDD7E4'; //'LightSalmon';
		}
		return e('div', {key:1, colSpan:'3', className:'div-user-record'}, [
			e('div', 
				{key:1,
				 className:'user-id-record',
				 onClick:(e)=>this.props.clickToUpdate(e, this.props.user),
				 style:{backgroundColor:'#87CEEB'}}, 
				this.props.user),
			e('div', {key:2, className:'user-roles-record'},
				JSON.stringify(this.props.roles)),
			e('div', 
				{key:4,
				 className:'button btn-remove',
				 onClick:(e)=>this.props.clickToRemove(e, this.props.user)
				}),
			e('div', 
				{key:3,
				 className:'user-status-record',
				 style:{backgroundColor:bgColor}
				}, status)
		]);
	}
}
