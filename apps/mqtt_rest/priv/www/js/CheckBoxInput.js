/**
 * 
 */
'use strict';

class CheckBoxInput extends React.Component {
	constructor(props) {
		super(props);
		this.state = {};
	}

	render() {
		return e(React.Fragment, {}, [
			e('td', 
				{key:1, className:'checkbox-inp-l'}, 
				e('label', 
					{className:'label'},
					[
						e('input', 
						{
							key:1,
							placeholder:this.props.placeholder,
							value:this.props.initValue,
							className:'text-input',
							autoComplete:'off',
							onChange: this.props.onChange,
							name:this.props.inpName,
							type:this.props.inpType,
							size:'30'
						}),
						e('div', {key:2, style:{paddingLeft:'10px'}}, this.props.label)
					]
				)
			),
			e('td', {key:2, className:'checkbox-inp-r'}, null)
		])
	}


}
