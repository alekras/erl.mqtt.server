/**
 * 
 */
'use strict';

const Menu = ({state, active, onMenuClick}) => {
	
	if(state) {
		return e(React.Fragment, {}, [
			e(MenuItem, {
				key: 1, 
				onMenuClick: onMenuClick, 
				name:'Configuration',
				active: (active == 'Configuration'),
				command:'Configuration'
			}),
			e(MenuItem, {
				key: 2, 
				onMenuClick: onMenuClick, 
				name:'Users', 
				active: (active == 'Users'),
				command:'Users'
			}),
			e(MenuItem, {
				key: 3, 
				onMenuClick: onMenuClick, 
				name:'Help', 
				active: (active == 'Help'),
				command:'Help'
			}),
			e(MenuItem, {
				key: 4, 
				onMenuClick: onMenuClick, 
				name:'Logout', 
				active: (active == 'Logout'),
				command:'Logout'
			})
		]);
	} else {
		return e(React.Fragment, {}, [
			e(MenuItem, {
				key: 1, 
				onMenuClick: onMenuClick, 
				name:'Login', 
				active: (active == 'Login'),
				command:'Login'
			}),
			e(MenuItem, {
				key: 2, 
				onMenuClick: onMenuClick, 
				name:'Help', 
				active: (active == 'Help'),
				command:'Help'
			}),
			e(MenuItem, {
				key: 3, 
				onMenuClick: () => {}, 
				name:'',
				active: false
			})
		]);
	};

}