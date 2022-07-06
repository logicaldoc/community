package com.logicaldoc.web.websockets;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.automation.AutomationDictionary;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.gui.common.client.websockets.WebsocketMessage;

/**
 * Utility functions to interact with the user interface from within the
 * Automation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
@AutomationDictionary
public class WebsocketTool {

	protected static Logger log = LoggerFactory.getLogger(WebsocketTool.class);

	/**
	 * Send to the Interface the command to display a message
	 * 
	 * @param session the current session
	 * @param message the message to display
	 * @param level the priority, it could be: <b>info</b>, <b>warn</b> or
	 *        <b>error</b>
	 */
	public void showMessage(Session session, String message, String level) {
		WebsocketMessage command = new WebsocketMessage(session.getSid(), "command");
		command.setCommand("message");
		command.setUserId(session.getUserId());
		command.setUsername(session.getUsername());
		command.setPayload(message);
		command.setTarget(level);

		EventEndpoint.distributeMessage(command);
	}

	/**
	 * Send to the Interface the command to open an URL
	 * 
	 * @param session the current session
	 * @param url the url to open
	 * @param target the target windows, if not specified the '_blank' target will be
	 *        used
	 */
	public void openUrl(Session session, String url, String target) {
		WebsocketMessage command = new WebsocketMessage(session.getSid(), "command");
		command.setCommand("openurl");
		command.setUserId(session.getUserId());
		command.setUsername(session.getUsername());
		command.setPayload(url);
		command.setTarget(StringUtils.isNotEmpty(target) ? target : "_blank");

		EventEndpoint.distributeMessage(command);
	}
}