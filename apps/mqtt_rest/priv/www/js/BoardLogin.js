'use strict';

class BoardLogin extends React.Component {

	constructor(props) {
		super(props);
		this.state = {userName:'', password:'', roles:[]};
	}
	
	handleChange(event) {
		switch (event.target.name) {
			case 'user' :
				this.setState({userName: event.target.value});
				break;
			case 'password' :
				this.setState({password: event.target.value});
				break;
			default:
				break;
		}
//		console.log('>>> BoardLogin event comes:: ' + event.target.value + ', ' + event.target.name)
	}

	handleSuccess = (json) => {
		console.log('Login is success: ' + JSON.stringify(json));
		if (json.success) {
			this.props.onStateChange(true, this.state.userName, json.roles);
		} else {
			this.props.warnBox.setLayout(
				'warn', 
				'User name or Password are invalid.<br/>Please try again.', 
				this.props.parent.current.getBoundingClientRect()
			);
			this.props.onStateChange(false);
		}
	};

	handleError = (error) => {
		console.log('AJAX error: ' + error);
		this.props.onStateChange(false);
	};

	handleSubmit(event) {
		if (Config.devMode && this.state.userName == '') { // For debug TODO: remove
			this.setState({userName:'guest', password:'guest'});
			RestAPI.loginRequest({userName:'guest', password:'guest'}, this.handleSuccess, this.handleError);
			event.preventDefault();
			return;
		}
		this.doLoginRequest();
		event.preventDefault();
	};

	handleSubmitByKey(event) {
		if (event.code == 'Enter') {
//			console.log('onSubmit event: >' + event.code + '<');
			this.doLoginRequest();
		}
	};
	
	doLoginRequest() {
		if (this.state.userName == 'echo') {
			this.props.onStateChange(false);
		} else {
			RestAPI.loginRequest(this.state, this.handleSuccess, this.handleError);
		}
	}

	shouldComponentUpdate(nextProps, nextState) {
		return true;
	}

	render() {
//		console.log('RENDER BoardLogin: ' + JSON.stringify(this.state));
		return e('form', 
				{
					onSubmit: (e) => this.handleSubmit(e),
					onKeyDown: (e) => this.handleSubmitByKey(e)
				}, [
			e(
			'table',
			{
				key:1,
				ref:this.parentTable
			}, e('tbody', {key:1}, [
					e('tr', {key:1}, [e(TextInput, {key:1,label:'User name:',sendChange:(e)=>this.handleChange(e),inpName:'user',inpType:'text',initVal:this.state.userName})]),
					e('tr', {key:2}, [e(TextInput, {key:1,label:'Password:',sendChange:(e)=>this.handleChange(e),inpName:'password',inpType:'password',initVal:this.state.password})]),
					e('tr', {key:3}, [
						e('td',{key:1, style:{paddingTop:'25px'}, align:'center'},[
							e('button', {
								key:1,
								className:'btn-login button',
								type:'submit'
							}, `LOGIN`)
						])
					])
				])
			)
		])
	}
}