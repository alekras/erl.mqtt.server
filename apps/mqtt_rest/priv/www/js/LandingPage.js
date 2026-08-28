'use strict';

const LandingPage = () => {

	const getHtmlText = () => {
		return `<h3>MQTT server monitoring tools</h3>
				<h5>Features:</h5>
				<dt>Login page</dt>
				<dd>Start here to enter in the system.</dd>
				<dt>Configuration page</dt>
				<dd>shows environment variables fron sys.config file.</dd>
				<dt>Users page</dt>
				<dd>shows list of users and allows manipulate user's records in DB.</dd>
			</dl>
			`;
	}

	return e('div',
		{
			className:'help',
			dangerouslySetInnerHTML:{ __html: getHtmlText() }
		}
	);
}
