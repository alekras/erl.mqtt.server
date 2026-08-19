/**
 * 
 */
'use strict';

const TextInput = ({ placeholder, label, inpName, inpType, initVal, sendChange, readOnly }) => {
	const [updatedValue, setUpdatedValue] = React.useState(initVal);
	const inputRef = React.useRef(null);
	React.useEffect(() => {
			if (inputRef.current) {
				inputRef.current.value = initVal || "";
			}
//			console.log('TextInput "' + inpName + '" useEffect updatedValue: ' + updatedValue
//			 + '; initVal: ' + initVal);
		}, 
		[initVal]
	);

//	console.log('TextInput "' + inpName + '" body updatedValue: ' + updatedValue + '; initVal: ' + initVal);

	const onChange = (event) => {
		if (inputRef.current) {
			setUpdatedValue(inputRef.current.value);
		};
//		console.log('TextInput "' + inpName + '" onChange updatedValue: ' + updatedValue
//		 + '; initVal: ' + initVal);
		sendChange(event);
	}
	
	return e('td', 
		{key:1, className:'text-inp'}, 
		e('label', 
			{className:'label'},
			[
				e('input', 
				{
					key:1,
					ref:inputRef,
					placeholder:placeholder,
					className:'text-input',
					style:{backgroundColor: ((readOnly)? 'yellow' : 'inheret')},
					autoComplete:'new-password',
					onChange: onChange,
					readOnly: ((readOnly)? true : false),
					name:inpName,
					type:inpType,
					size:'30'
				}),
				e('div', {key:2, style:{paddingLeft:'10px'}}, label)
			])
		);
};

