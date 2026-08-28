/**
 * 
 */
'use strict';

const WarningBox = ({type, warning, layout, yesNoFun, onBoxClose}) => {
	const [maskLayout, setMaskLayout] = React.useState({x:0, y:0, w:100, h:100});
	const [boxLayout, setBoxLayout] = React.useState({x:10, y:10, w:50, h:50});
	
	React.useEffect(() => {
		setMaskLayout({
			x: layout.left,
			y: layout.top - 30,
			w: layout.width,
			h: layout.height + 30,
		});
		setBoxLayout({
			x:(layout.width - layout.width/2)/2,
			y:(layout.height + 30 - (layout.height + 30)/2)/2,
			w:layout.width/2,
			h:(layout.height + 30)/2
		});
	}, []); 
	
	var buttonArray;
	if (type == 'warn') {
		buttonArray = [
			e('button', 
				{
					key:1, 
					className:'button warning-btn',
					onClick:(e) => onBoxClose(e)
				}, 'Close')
			]
	} else {
		buttonArray = [
			e('button', 
				{
					key:1, 
					className:'button warning-btn',
					onClick:(e) => {yesNoFun(true); onBoxClose(e);}
				}, 'YES'),
			e('button', 
				{
					key:2, 
					className:'button warning-btn',
					onClick:(e) => {yesNoFun(false); onBoxClose(e);}
				}, 'NO')
		]
	};
		
	return e('div', 
		{
			className:'warning-mask',
			style:{
				width: maskLayout.w + 'px',
				height: maskLayout.h + 'px',
				top: maskLayout.y + 'px',
				left: maskLayout.x + 'px'
			}
		}, 
		e('div', 
			{
				key:0,
				className:'warning-box',
				style:{
					width: boxLayout.w + 'px',
					height: boxLayout.h + 'px',
					top: boxLayout.y + 'px',
					left: boxLayout.x + 'px'
				}
			}, 
			e('div', 
				{
					key:0, className:'warning-inside'
				},
				[
					e('div', 
						{
							key:0,
							className:'warning-msg',
							dangerouslySetInnerHTML:{ __html: warning}
						}
					),
					e('div', 
						{
							key:1,
							className:'warning-btn-container'
						}, buttonArray
					)
				]
			)
		)
	);
}
