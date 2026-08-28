'use strict';

const BoardLogin = ({parent, onStateChange}) => {
	const [userName, setUserName] = React.useState('');
	const [password, setPassword] = React.useState('');
	const [roles, setRoles] = React.useState([]);
	const [warningBoxDisplay, setWarningBoxDisplay] = React.useState(false);
	const [boxText, setBoxText] = React.useState('');
	
	const handleChange = (event) => {
		switch (event.target.name) {
			case 'user' :
				setUserName(event.target.value);
				break;
			case 'password' :
				setPassword(event.target.value);
				break;
			default:
				break;
		}
//		console.log('>>> BoardLogin event comes:: ' + event.target.value + ', ' + event.target.name)
	}

	const handleSuccess = (json, usName) => {
		console.log('Login is success. usName:' + usName + ' response: ' + JSON.stringify(json));
		if (json.success) {
			onStateChange(true, usName, json.roles);
		} else {
			onStateChange(false);
			setWarningBoxDisplay(true);
			setBoxText('User name or Password are invalid.<br/>Please try again.');
		}
	};

	const handleError = (error) => {
		console.log('AJAX error: ' + error);
		onStateChange(false);
	};

	const handleSubmit = (event) => {
		if (userName === '') { // For debug TODO: remove
			setUserName('guest');
			RestAPI.loginRequest({userName:'guest', password:'guest'}, handleSuccess, handleError);
			event.preventDefault();
			return;
		}
		doLoginRequest();
		event.preventDefault();
	};

	const handleWarningBoxClose = (event) => {
			setWarningBoxDisplay(false);
	};

	const handleSubmitByKey = (event) => {
		if (event.code == 'Enter') {
//			console.log('onSubmit event: >' + event.code + '<');
			doLoginRequest();
		}
	};
	
	const doLoginRequest = () => {
		if (userName === 'echo') {
			onStateChange(false);
		} else {
			RestAPI.loginRequest({userName:userName, password:password}, handleSuccess, handleError);
		}
	}

//		console.log('RENDER BoardLogin: ' + JSON.stringify(this.state));
	var r4 = null;
	if (warningBoxDisplay) {
		r4 =
		e(WarningBox,
			{
				key:1,
				type:'warn',
				warning:boxText,
				layout:parent.current.getBoundingClientRect(),
				yesNoFun: () => {},
				onBoxClose:handleWarningBoxClose
			});
	};
	
	return e('form', 
		{
			onSubmit: (e) => handleSubmit(e),
			onKeyDown: (e) => handleSubmitByKey(e)
		}, [
			e(
				'table',
				{
					key:1,
				}, 
				e('tbody', {key:1}, [
					e('tr', {key:1}, [e(TextInput, {key:1,label:'User name:',sendChange:(e)=>handleChange(e),inpName:'user',inpType:'text',initVal:userName})]),
					e('tr', {key:2}, [e(TextInput, {key:1,label:'Password:',sendChange:(e)=>handleChange(e),inpName:'password',inpType:'password',initVal:password})]),
					e('tr', {key:3}, [
						e('td',{key:1, style:{paddingTop:'25px'}, align:'center'}, [
							e('button', {
								key:1,
								className:'btn-login button',
								type:'submit'
							}, `LOGIN`)
						])
					]),
					e('tr', {key:4, height:'0px'}, 
						e('td', {key:1}, r4)
					)
				])
			)
		]);
};
