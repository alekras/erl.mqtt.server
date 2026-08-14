/**
 * 
 */
'use strict';

class WarningBox extends React.Component {
	constructor(props) {
		super(props);
		this.state = {
			display:'none',
			x:0,
			y:0,
			w:100,
			h:100,
			box: {
				x:10,
				y:10,
				w:50,
				h:50
			},
			yesNoFun: () => {}
		};
		this.boxRef = React.createRef();
	}
	
	setLayout = (type, warning, layout, func) => {
		let boxLayout = this.boxRef.current.getBoundingClientRect()
		this.setState({
			type: type,
			textWarning: warning,
			display: 'block',
			x: layout.left,
			y: layout.top - 30,
			w: layout.width,
			h: layout.height + 30,
			box: {
				x:(layout.width - layout.width/2)/2,
				y:(layout.height + 30 - (layout.height + 30)/2)/2,
				w:layout.width/2,
				h:(layout.height + 30)/2
			},
			yesNoFun: func
		})
	}
	
	handleWarningBoxClose = (event) => {
		this.setState({
			display:'none'
		});
	}

	render() {
		var buttonArray;
		if (this.state.type == 'warn') {
			buttonArray = [
				e('button', 
					{
						key:1, 
						className:'button warning-btn',
						onClick:(e)=>this.handleWarningBoxClose(e)
					}, 'Close')
			]
		} else {
			buttonArray = [
				e('button', 
					{
						key:1, 
						className:'button warning-btn',
						onClick:(e)=>{this.state.yesNoFun(true); this.handleWarningBoxClose(e);}
					}, 'YES'),
				e('button', 
					{
						key:2, 
						className:'button warning-btn',
						onClick:(e)=>{this.state.yesNoFun(false); this.handleWarningBoxClose(e);}
					}, 'NO')
			]
		};
		
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
				ref:this.boxRef,
				style:{
					width: this.state.box.w + 'px',
					height: this.state.box.h + 'px',
					top: this.state.box.y + 'px',
					left: this.state.box.x + 'px'
				}
			}, e('div', {key:0, className:'warning-inside'}, [
					e('div',
						{
							key:0,
							className:'warning-msg',
							dangerouslySetInnerHTML:{ __html: this.state.textWarning}
						}),
					e('div', {key:1, className:'warning-btn-container'}, buttonArray)
				])
			)
		);
	}
}
