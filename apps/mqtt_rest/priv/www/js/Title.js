/**
 * 
 */
'use strict';

class Title extends React.Component {
	constructor(props) {
		super(props);
		this.state = {};
	}
	
	handleMouseClick(event) {
	}
	
	render() {
		var title = 'MQTT SERVER MONITOR';
		var user, bgColor;

		if (this.props.login_user) {
			user = 'You: ' + this.props.login_user.user_name;
			if (this.props.login_user.roles.includes('ADMIN')) {
				bgColor = 'Aquamarine';
				user += ' [ADMIN]';
			} else {
				bgColor = '#FDD7E4'; //'PigPink' 'LightSalmon';
			};
		} else {
			user = '';
		}

		return e(React.Fragment, {}, [
			e('span',
				{key:0, className:'ssl-seal'}
			),
			e('span',
				{key:1, style:{color:'#3e7878', margin:'5px 2px 0 2px', float:'left'}},
				title
			),
			e('span',
				{key:2, className:'title-username', style:{backgroundColor:bgColor}}, 
				user
			),
			e('span', {key:3, style:{clear:'both'}})
		]);
	}
}