/**
 * 
 */
'use strict';

/* Properties (from parent cpomponent) :
{
	type
	user_name
	layout
	onSuccess
	onError
	onBoxClose
-- update --
	password1
	password2
	roles

}
*/
class AddUserBox extends React.Component {
	constructor(props) {
		super(props);
		let left_space = 30;
		let top_space = 50;
		this.state = {
			type: props.type,
			display: 'block',
			user_name:props.user_name,
			password1:props.password1 || '',
			password2:props.password2 || '',
			roles: props.roles || [],
			error_message_1: '',
			error_message_2: '',
			error_message_3: '',
			error_message_4: '',
			x: props.layout.left,
			y: props.layout.top - 30, // shift up mask
			w: props.layout.width,
			h: props.layout.height + 30,
			box: {
				x:left_space,
				y:top_space,
				w:props.layout.width - 2 * (left_space + 8), // 8 = border+padding1
				h:props.layout.height - 2 * (top_space + 8)
			},
		};
	}

	handleChange(event) {
		switch (event.target.name) {
			case 'user_name' :
				this.setState({user_name: event.target.value});
				break;
			case 'password1' :
				this.setState({password1: event.target.value});
				break;
			case 'password2' :
				this.setState({password2: event.target.value});
				break;
			case 'role1' :
			case 'role2' :
			case 'role3' :
				if (event.target.checked) {
					this.state.roles.push(event.target.value);
					this.setState({roles:this.state.roles});
				} else {
					let rls = this.state.roles.filter(role => role !== event.target.value);
					this.setState({roles:rls});
				}
				break;
			default:
				break;
		}
//		console.log('AddUserBox Event comes:: name=' + event.target.name + ', value=' + event.target.value + ', checked=' + event.target.checked)
	}

	handleSubmit(event) {
// TODO: block submit until API call is finished
		console.log('A userName was submitted with state: ' + JSON.stringify(this.state));
// check user_name & password
		let err = 0;
		if(this.state.user_name && this.state.user_name.trim().length > 0) {
			this.setState({error_message_1:''});
		} else {
			this.setState({error_message_1:'User name is empty. Please fix it.'});
			err++;
		}
		if(this.state.password1 && this.state.password1.length > 4) {
			this.setState({error_message_2:''});
		} else {
			this.setState({error_message_2:'Password is short or empty. Please fix it.'});
			err++;
		}
		if(this.state.password2 && this.state.password1 === this.state.password2) {
			this.setState({error_message_3:''});
		} else {
			this.setState({error_message_3:'Password is not confirmed. Please fix it.'});
			err++;
		}
		if(this.state.roles && this.state.roles.length > 0) {
			this.setState({error_message_4:''});
		} else {
			this.setState({error_message_4:'Select at least one role.'});
			err++;
		}
		if (err === 0) {
			RestAPI.add_user(this.state, this.props.onSuccess, this.props.onError);
			this.props.onBoxClose(event);
		}
		event.preventDefault();
	}
	
	handleSubmitByKey(event) {
		if (event.code == 'Enter') {
			console.log('onSubmit event: >' + event.code + '<');
//			RestAPI.add_user(this.state, this.props.onSuccess, this.props.onError);
		}
	};

	shouldComponentUpdate(nextProps, nextState) {
		if (this.state !== nextState) {
			return true;
		}
		return false;
	}

	renderButtonArray() {
		var buttonTitle;
		switch (this.state.type) {
			case 'save':
				buttonTitle = 'SAVE';
				break;
			case 'update':
				buttonTitle = 'UPDATE';
				break;
			default :
				break;
		}
		
		var buttonArray = [
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
					onClick:(e)=>{this.props.onBoxClose(e);}
				}, 'CANCEL')
		];
		return buttonArray;
	}
	
	renderAddForm() {
		var title, ro;
		switch (this.state.type) {
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
		if (this.state.roles.includes("ADMIN")) {
			rl1 = true;
		};
		if (this.state.roles.includes("USER")) {
			rl2 = true;
		};
		if (this.state.roles.includes("GUEST")) {
			rl3 = true;
		};
		
		return e('form', 
				{ style:{padding:'10px', autoComplete:'new-password'},
					onSubmit: (e) => this.handleSubmit(e),
					onKeyDown: (e) => this.handleSubmitByKey(e)
				}, [
			e(
			'table',
			{
				key:1,
				style:{width:'100%', height:'100%'}
			}, [
				e('tbody', {key:1}, [
					e('tr',{key:0}, e('td',{key:0, align:'center'}, title)),
					e('tr', {key:1}, [e(TextInput, {key:1,label:'User name:',sendChange:(e)=>this.handleChange(e),inpName:'user_name',inpType:'text',placeholder:'User name', readOnly:ro, initVal:this.state.user_name})]),
					e('tr', {key:11}, e('td',{className:'error-msg'}, this.state.error_message_1)),
					e('tr', {key:2}, [e(TextInput, {key:1,label:'Password:',sendChange:(e)=>this.handleChange(e), inpName:'password1',inpType:'password',initVal:''})]),
					e('tr', {key:21}, e('td',{className:'error-msg'}, this.state.error_message_2)),
					e('tr', {key:3}, [e(TextInput, {key:1,label:'Password confirm:',sendChange:(e)=>this.handleChange(e), inpName:'password2',inpType:'password',initVal:''})]),
					e('tr', {key:31}, e('td',{className:'error-msg'}, this.state.error_message_3)),
					e('tr', {key:4}, 
						e('td', {key:1}, 
							e('fieldset', {key:1, style:{textAlign:'left', width:'50%'}},
								[
									e('legend', {key:1, className:'label-text'}, 'User\'s roles:'),
									e('div', {key:2},
										[
											e('input', {key:1, type:'checkbox', id:'rl.1', name:'role1', value:'ADMIN', autoComplete:'off', checked:rl1, onChange:(e)=>this.handleChange(e)}),
											e('label', {key:2, htmlFor:'rl.1'}, 'ADMIN')
									]),
									e('div', {key:3},
										[
											e('input', {key:1, type:'checkbox', id:'rl.2', name:'role2', value:'USER', autoComplete:'off', checked:rl2, onChange:(e)=>this.handleChange(e)}),
											e('label', {key:2, htmlFor:'rl.2'}, 'USER')
									]),
									e('div', {key:4},
										[
											e('input', {key:1, type:'checkbox', id:'rl.3', name:'role2', value:'GUEST', autoComplete:'off', checked:rl3, onChange:(e)=>this.handleChange(e)}),
											e('label', {key:2, htmlFor:'rl.3'}, 'GUEST')
									])
							])
						)
					),	
					e('tr', {key:41}, e('td',{className:'error-msg'}, this.state.error_message_4)),
					e('tr', {key:5}, [
						e('td',{key:1, style:{paddingTop:'25px'}, align:'center'},[
							e('div', {key:1, className:'warning-btn-container'}, this.renderButtonArray())
						])
					])
				])
			])
		])
	}
	
	render() {
//		console.log('on render AddUserBox: >' + this.state.user_name + '<');
		return e('div', {
				className:'warning-mask',
				style:{
					display:this.state.display,
					width: this.state.w + 'px',
					height: this.state.h + 'px',
					top: this.state.y + 'px',
					left: this.state.x + 'px'
				}
			},
			e('div', {key:0,
				className:'warning-box',
				style:{
					width: this.state.box.w + 'px',
					height: this.state.box.h + 'px',
					top: this.state.box.y + 'px',
					left: this.state.box.x + 'px'
				}
			}, e('div', {key:0, className:'warning-inside'},
					this.renderAddForm()
				 )
			)
		);
	}
}
