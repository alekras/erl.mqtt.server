/**
 * 
 */
'use strict';

class MenuItem extends React.Component {
	constructor(props) {
		super(props);
		this.state = {};
	}
	
	handleMouseOver = (event) => {
		if (this.props.name == '')
			return;
		if (this.props.active) {
			event.target.className = 'menuLabel menuLabel-mover-a';
		} else {
			event.target.className = 'menuLabel menuLabel-mover';
		}
	}
	
	handleMouseLeave = (event) => {
		if (this.props.active) {
			event.target.className = 'menuLabel menuLabel-active';
		} else {
			event.target.className = 'menuLabel menuLabel-initial';
		}
	}
	
	render() {
		var cn = 'menuLabel ' + ((this.props.active) ? 'menuLabel-active' : 'menuLabel-initial');
		return e(
			'td',
			{className:'menuItem'},
			e(
				'div', {
					className:cn,
					onClick:(e) => this.props.onMenuClick(e, this.props.command),
					onMouseOver: this.handleMouseOver,
					onMouseLeave: this.handleMouseLeave
				}, 
				this.props.name
			)
		)
	}
}