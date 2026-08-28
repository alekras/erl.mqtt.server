/**
 * 
 */
'use strict';

const BoardHelp = () => {
	
const getHtmlText = () => {
		return `<h3>1. Lets getting starting.</h3>
			<h5>1.1 Login</h5>
			<p>Click on "Login" menu item to open Login page. Then type user name and password.
			</p>
			<p>MQTT server has a few preset system users:
			<dl>
				<dt>admin</dt>
				<dd>Administrator with password 'admin' has unlimited ability to change/update users database. </dd>
				<dt>echo</dt>
				<dd>Service user 'echo' is using in some applications (do not change password).</dd>
				<dt>guest</dt>
				<dd>Service user 'guest' is using for testing (do not change password)</dd>
			</dl>
			</p>
			<p> One or more roles are assigned for each user:
			<dl>
				<dt>ADMIN</dt>
				<dd>User can add new users, remove or update already existed ones.</dd>
				<dt>USER</dt>
				<dd>User can not change users database</dd>
				<dt>GUEST</dt>
				<dd>User can not change users database</dd>
			</dl>
			</p>
			<h5>1.2 Configuration page</h5>
			<p>The page shows content of sys.config file, that is container of environment variables
			for MQTT and Web server. Content of config file is divided in five parts -
			configurations for mqtt_common, mqtt_rest and mqtt_server apps, mnesia and mqtt_client
			(if exist)
			</p>
			<dl>
				<dt>mqtt_common</dt>
				<dd><b>storage</b> - type of backend DB</dd>
				<dd><b>cluster_nodes</b> - list of nodes in cluster</dd>
				<dd><b>mnesia_master</b> - is it master of mnesia nodes?</dd>
				<dt>mqtt_rest</dt>
				<dd><b>port</b> - Rest Web server port</dd>
				<dd><b>host_name</b> - domain name of Rest Web server</dd>
				<dt>mqtt_server</dt>
				<dd><b>port</b> - port for clear connection</dd>
				<dd><b>port_tls</b> - port for TLS encripted connection</dd>
				<dd><b>port_ws</b> - port for websocket connection</dd>
				<dd><b>port_wss</b> - port for secured TLS web socket connection</dd>
				<dd><b>cacertfile</b> - TLS connection parameter</dd>
				<dd><b>certfile</b> - TLS connection parameter</dd>
				<dd><b>keyfile</b> - TLS connection parameter</dd>
				<dd><b>verify</b> - TLS connection parameter</dd>
				<dt>mnesia</dt>
				<dd><b>dir</b> - mnesia storage dir</dd>
				<dd><b>schema_location</b> - location of mnesia schema</dd>
				<dt>mqtt_client</dt>
				<dd><b> </b> - </dd>
			</dl>
			<p>
			</p>
			<h5>1.3 Users page</h5>
			<p>The 'Users' page contains list of registered users in MQTT server. The page allows to
			manipulate user's records.
			</p>
			<p> If user with role 'ADMIN' login to the system, he/she can issue follows operations:
			<dl>
				<dt>add user</dt>
				<dd>Click on button <img src="/mqtt/img/add-user.png" style="height: 25px;vertical-align: middle;">.
				Then new window 'ADD USER' is open. Fill out fields 'User name', 'Password', 
				'Password confirm' and 'User\'s roles'. Click button 'Save' to save user's record in DB.
				</dd>
				<dt>delete user</dt>
				<dd>Click on button <img src="/mqtt/img/remove-user.png" style="height: 25px;vertical-align: middle;">.
				Then confirm window is appear. Click 'YES' to confirm to remove this user's record from DB.</dd>
				<dt>update user</dt>
				<dd>Click on name of the user.
				Then new window 'UPDATE USER' is open. You can change fields 'Password', 
				'Password confirm' and 'User\'s roles'. If you leave both passwords fields blank then 
				password of the user does not changed. Click button 'UPDATE' to save updated 
				user's record in DB.
				</dd>
			</dl>
			</p>
			<p>
			</p>`;
	}
	
	return e('div',
		{
			className:'help',
			dangerouslySetInnerHTML:{ __html: getHtmlText() }
		}
	)
}
