/**
 * 
 */
'use strict';

const MenuItem = ({name, active, command, onMenuClick}) => {
	
	const handleMouseOver = (event) => {
		if (name == '')
			return;
		if (active) {
			event.target.className = 'menuLabel menuLabel-mover-a';
		} else {
			event.target.className = 'menuLabel menuLabel-mover';
		}
	}
	
	const handleMouseLeave = (event) => {
		if (active) {
			event.target.className = 'menuLabel menuLabel-active';
		} else {
			event.target.className = 'menuLabel menuLabel-initial';
		}
	};
	
	var cn = 'menuLabel ' + ((active) ? 'menuLabel-active' : 'menuLabel-initial');
	return e('td',
		{className:'menuItem'},
		e('div',
			{
				className:cn,
				onClick:(e) => onMenuClick(e, command),
				onMouseOver: handleMouseOver,
				onMouseLeave: handleMouseLeave
			}, name
		)
	);
};
