/**
 * 
 */
'use strict';

const BoardConfiguration = ({}) => {
	const [configList, setConfigList] = React.useState([]);

	const handleSuccess = (json) => {
		console.log('get Config is success: ' + JSON.stringify(json));
		setConfigList(json);
	};

	const handleError = (error) => {
		console.log('AJAX error: ' + error);
		this.setState({});
	};
	
	React.useEffect(() => {
			RestAPI.getConfig('mqtt_common,mnesia,mqtt_rest,mqtt_server,mqtt_client', handleSuccess, handleError);
		},
		[]
	);

//	console.log('>>> BoardConfiguration render(). Component state:' + JSON.stringify(this.state));
	let i = 0;
	let rows = configList.map((list_item) => {
		i++;
		let appRow = e('tr', {key:i, className:'board-config-app'}, [
				e('td', {key:1, className:'board-config-app-name'}, list_item.app + ':'),
				e('td', {key:2}, '')
			]);
		let lines = Object.entries(list_item.config).map(([prop, value]) => {
			i++;
			return e(Line, {key:i,
					order: i,
					name: prop,
					val: value});
			});

		return [
			appRow,
			...lines
		];
	}).flat();
	
	rows.push(
		e('tr', {key:++i}, 
				e('td', {key:1,colSpan:'2',}, '')
		)
	);
		
	return e(
		'table',
		{
			className:'tbl-config'
		}, 
		e('tbody', {key:1}, rows)
	);
}

const Line = ({name, val, order}) => {
	
	let bgColor;
	if (order % 2 == 0) {
		bgColor = 'Aquamarine';
	} else {
		bgColor = 'White';
	}

	return e('tr',
		{ className:'board-config-line',
			style:{backgroundColor:bgColor}
		}, [
			e('td',
				{key:1,
					className:'board-config-line-name'
				},
				name + ': '
			),
			e('td',
				{key:2,
					className:'board-config-line-value'
				}, val
			)
		]);
}
