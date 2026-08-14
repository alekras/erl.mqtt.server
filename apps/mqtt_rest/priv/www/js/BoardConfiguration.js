/**
 * 
 */
'use strict';

class BoardConfiguration extends React.Component {
	constructor(props) {
		super(props);
		this.state = {};
		console.log('<<< BoardConfiguration constructor.');
//		BoardConfiguration.configList = [{app:'mqtt_common', config:{a:'a1',b:'b1',c:'c1'}},{app:'mqtt_rest', config:{r:'r1',q:'q1',x:'x1'}}];
		RestAPI.getConfig('mqtt_common,mnesia,mqtt_rest,mqtt_server,mqtt_client', this.handleSuccess, this.handleError);
	}
	
	static configList = [];

	handleSuccess = (json) => {
		console.log('get Config is success: ' + JSON.stringify(json));
		BoardConfiguration.configList = json;
		this.setState({});
	};

	handleError = (error) => {
		console.log('AJAX error: ' + error);
		this.setState({});
	};
	
	handleChange(event) {
		event.preventDefault();
	}

	shouldComponentUpdate(nextProps, nextState) {
		if (this.state !== nextState) {
			return true;
		}
		return true;
	}

	render() {
		console.log('>>> BoardConfiguration render(). Component state:' + JSON.stringify(this.state));
		let i = 0;
		let rows = BoardConfiguration.configList.map((list_item) => {
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
				//style:{maxWidth:this.props.w}
			}, 
			e('tbody', {key:1}, rows)
		);
	}	
}

class Line extends React.Component {
	constructor(props) {
		super(props);
		this.state = {};
	}
	
	render() {
		let bgColor;
		if (this.props.order % 2 == 0) {
			bgColor = 'Aquamarine';
		} else {
			bgColor = 'White';
		}

		return e('tr', {className:'board-config-line', style:{backgroundColor:bgColor}}, [
			e('td', {key:1,
				className:'board-config-line-name'
			}, this.props.name + ': '),
			e('td', {key:2,
				className:'board-config-line-value'
			}, this.props.val)
		]);
	}
}
