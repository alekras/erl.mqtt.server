/**
 * 
 */
'use strict';

const AddUserBox = ({type, user_name, par_roles, layout, onSuccess, onError, onBoxClose}) => {
	const [maskLayout, setMaskLayout] = React.useState({x:0, y:0, w:100, h:100});
	const [boxLayout, setBoxLayout] = React.useState({x:10, y:10, w:50, h:50});
	const [userName, setUserName] = React.useState(user_name);
	const [password, setPassword] = React.useState(['','']);
	const [roles, setRoles] = React.useState(par_roles || []);
	const [errorMessages, setErrorMessages] = React.useState(['','','','']);
	
	React.useEffect(() => {
		let left_space = 30;
		let top_space = 50;
		setMaskLayout({
			x: layout.left,
			y: layout.top - 30,
			w: layout.width,
			h: layout.height + 30,
		});
		setBoxLayout({
			x:left_space,
			y:top_space,
			w:layout.width - 2 * (left_space + 8), // 8 = border+padding1
			h:layout.height - 2 * (top_space + 8)
		});
	}, []); 

	const updateArray = (array, idx, value) => {
		return array.map((c, i) => {
      if (i === idx) {
        return value;
      } else {
        return c;
      }
    });
	}
	
	const handleChange = (event) => {
		switch (event.target.name) {
			case 'user_name' :
				setUserName(event.target.value);
				break;
			case 'password1' :
				setPassword(updateArray(password, 0, event.target.value));
				break;
			case 'password2' :
				setPassword(updateArray(password, 1, event.target.value));
				break;
			case 'role1' :
			case 'role2' :
			case 'role3' :
				var nextRoles = [...roles];
				if (event.target.checked) {
					nextRoles.push(event.target.value);
				} else {
					nextRoles = roles.filter(role => role !== event.target.value);
				}
				setRoles(nextRoles);
				break;
			default:
				break;
		}
		console.log('AddUserBox Event comes:: name=' + event.target.name + ', value=' + event.target.value + ', checked=' + event.target.checked)
		console.log('Password1: ' + password[0] + ' password2: ' + password[1] 
			+ ' roles: ' + roles[0] +', '+ roles[1] +', '+roles[2]);
	};

	const handleSubmit = (event) => {
		if (type === 'save') {
			handleOnSaveSubmit(event);
		} else if (type === 'update') {
			handleOnUpdateSubmit(event);
		}
	}

	const handleOnSaveSubmit = (event) => {
// TODO: block submit until API call is finished
		console.log('A userName was submitted with state: ' + userName + ':' + userName.trim().length + ' psw:' + password[0] + '/' + password[1]
			+ ' errorMessages: 0:' + errorMessages[0] + ' 1:' + errorMessages[1] + ' 2:' + errorMessages[2] + ' 3:' + errorMessages[3]);
// check user_name & password
		let err = 0;
		var nextErrorMsg = [...errorMessages];
		if(userName.trim().length > 0) {
			nextErrorMsg = updateArray(nextErrorMsg, 0, '');
		} else {
			nextErrorMsg = updateArray(nextErrorMsg, 0, 'User name is empty. Please fix it.');
			err++;
		}
		if(password[0] && password[0].length > 4) {
			nextErrorMsg = updateArray(nextErrorMsg, 1, '');
		} else {
			nextErrorMsg = updateArray(nextErrorMsg, 1, 'Password is short or empty. Please fix it.');
			err++;
		}
		if(password[1] && password[0] === password[1]) {
			nextErrorMsg = updateArray(nextErrorMsg, 2, '');
		} else {
			nextErrorMsg = updateArray(nextErrorMsg, 2, 'Password is not confirmed. Please fix it.');
			err++;
		}
		if(roles && roles.length > 0) {
			nextErrorMsg = updateArray(nextErrorMsg, 3, '');
		} else {
			nextErrorMsg = updateArray(nextErrorMsg, 3, 'Select at least one role.');
			err++;
		}
		setErrorMessages(nextErrorMsg);
		if (err === 0) {
			RestAPI.add_user({user_name:userName, password:password[0], roles:roles}, onSuccess, onError);
			onBoxClose(event);
		}
		event.preventDefault();
	}
	
	const handleOnUpdateSubmit = (event) => {
// TODO: block submit until API call is finished
		console.log('A userName was updated with state: ' + userName + ' psw:' + password[0]);
// check user_name & password
		let err = 0;
		var nextErrorMsg = [...errorMessages];
		if(password[0] && password[0].length > 4) {
			nextErrorMsg = updateArray(nextErrorMsg, 1, '');
		} else {
			nextErrorMsg = updateArray(nextErrorMsg, 1, 'Password is short or empty. Please fix it.');
			err++;
		}
		if(password[1] && password[0] === password[1]) {
			nextErrorMsg = updateArray(nextErrorMsg, 2, '');
		} else {
			nextErrorMsg = updateArray(nextErrorMsg, 2, 'Password is not confirmed. Please fix it.');
			err++;
		}
		if(roles && roles.length > 0) {
			nextErrorMsg = updateArray(nextErrorMsg, 3, '');
		} else {
			nextErrorMsg = updateArray(nextErrorMsg, 3, 'Select at least one role.');
			err++;
		}
		setErrorMessages(nextErrorMsg);
		if (err === 0) {
			RestAPI.update_user({user_name:userName, password1:password[0], roles:roles}, onSuccess, onError);
			onBoxClose(event);
		}
		event.preventDefault();
	}

	const handleSubmitByKey = (event) => {
		if (event.code == 'Enter') {
			console.log('onSubmit event: >' + event.code + '<');
			handleSubmit(event);
		}
	};

	const renderButtonArray = () => {
		var buttonTitle;
		switch (type) {
			case 'save':
				buttonTitle = 'SAVE';
				break;
			case 'update':
				buttonTitle = 'UPDATE';
				break;
			default :
				break;
		}
		
		return [
			e('button', 
				{
					key:1, 
					className:'button warning-btn',
					type:'submit',
				}, buttonTitle),
			e('button', 
				{
					key:2, 
					className:'button warning-btn',
					type:'button',
					onClick:(e)=>{onBoxClose(e);}
				}, 'CANCEL')
		];
	};
	
	const renderAddForm = () => {
		var title, ro;
		switch (type) {
			case 'save':
				title = 'ADD USER';
				ro = false;
				break;
			case 'update':
				title = 'UPDATE USER';
				ro = true
				break;
			default :
				break;
		}
		var rl1 = false, rl2 = false, rl3 = false;
		if (roles.includes("ADMIN")) {
			rl1 = true;
		};
		if (roles.includes("USER")) {
			rl2 = true;
		};
		if (roles.includes("GUEST")) {
			rl3 = true;
		};
		
		return e('form', 
				{ style:{padding:'10px', autoComplete:'new-password'},
					onSubmit: (e) => handleSubmit(e),
					onKeyDown: (e) => handleSubmitByKey(e)
				}, [
			e(
			'table',
			{
				key:1,
				style:{width:'100%', height:'100%'}
			}, [
				e('tbody', {key:1}, [
					e('tr',{key:0}, e('td',{key:0, align:'center'}, title)),
					e('tr', {key:1}, [e(TextInput, {key:1,label:'User name:',sendChange:(e)=>handleChange(e),inpName:'user_name',inpType:'text',placeholder:'User name', readOnly:ro, initVal:user_name})]),
					e('tr', {key:11}, e('td',{className:'error-msg'}, errorMessages[0])),
					e('tr', {key:2}, [e(TextInput, {key:1,label:'Password:',sendChange:(e)=>handleChange(e), inpName:'password1',inpType:'password',initVal:''})]),
					e('tr', {key:21}, e('td',{className:'error-msg'}, errorMessages[1])),
					e('tr', {key:3}, [e(TextInput, {key:1,label:'Password confirm:',sendChange:(e)=>handleChange(e), inpName:'password2',inpType:'password',initVal:''})]),
					e('tr', {key:31}, e('td',{className:'error-msg'}, errorMessages[2])),
					e('tr', {key:4}, 
						e('td', {key:1}, 
							e('fieldset', {key:1, style:{textAlign:'left', width:'50%'}},
								[
									e('legend', {key:1, className:'label-text'}, 'User\'s roles:'),
									e('div', {key:2},
										[
											e('input', {key:1, type:'checkbox', id:'rl.1', name:'role1', value:'ADMIN', autoComplete:'off', checked:rl1, onChange:(e)=>handleChange(e)}),
											e('label', {key:2, htmlFor:'rl.1'}, 'ADMIN')
									]),
									e('div', {key:3},
										[
											e('input', {key:1, type:'checkbox', id:'rl.2', name:'role2', value:'USER', autoComplete:'off', checked:rl2, onChange:(e)=>handleChange(e)}),
											e('label', {key:2, htmlFor:'rl.2'}, 'USER')
									]),
									e('div', {key:4},
										[
											e('input', {key:1, type:'checkbox', id:'rl.3', name:'role2', value:'GUEST', autoComplete:'off', checked:rl3, onChange:(e)=>handleChange(e)}),
											e('label', {key:2, htmlFor:'rl.3'}, 'GUEST')
									])
							])
						)
					),	
					e('tr', {key:41}, e('td',{className:'error-msg'}, errorMessages[3])),
					e('tr', {key:5}, [
						e('td',{key:1, style:{paddingTop:'25px'}, align:'center'},[
							e('div', {key:1, className:'warning-btn-container'}, renderButtonArray())
						])
					])
				])
			])
		])
	}

console.log('Render with state: ' + userName + ' psw:' + password[0] + '/' + password[1]
			+ ' errorMessages: 0:' + errorMessages[0] + ' 1:' + errorMessages[1] + ' 2:' + errorMessages[2] + ' 3:' + errorMessages[3]);
	
	return e('div', {
		className:'warning-mask',
		style:{
			width: maskLayout.w + 'px',
			height: maskLayout.h + 'px',
			top: maskLayout.y + 'px',
			left: maskLayout.x + 'px'
		}
	},
		e('div', {key:0,
			className:'warning-box',
			style:{
				width: boxLayout.w + 'px',
				height: boxLayout.h + 'px',
				top: boxLayout.y + 'px',
				left: boxLayout.x + 'px'
			}
		}, e('div', {key:0, className:'warning-inside'},
				renderAddForm()
			)
		)
	);
}
