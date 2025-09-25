package com.logicaldoc.gui.frontend.client.chatgpt;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;

/**
 * A box showing a ChatGPT message in a thread
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
class MessageBox extends Label {

	private static final String CHATGPT = "chatgpt";

	private String avatarIcon;

	private String role;

	public MessageBox(String message, String role) {
		setAlign(Alignment.LEFT);
		setCanSelectText(true);

		this.role = role;

		String url = CHATGPT.equals(role) ? Util.imageUrl("chatgpt.png")
				: Util.avatarUrl(Session.get().getUser().getId());
		avatarIcon = "<img src='" + url
				+ "' style='border: 0px; height: 32px; width: 32px; margin-right: 5px; margin-bottom: 3px' />";

		String color = CHATGPT.equals(role) ? "white" : "MintCream";
		setBackgroundColor(color);
		setHeight("*");

		setContents(prepareContent(message));
	}

	private String prepareContent(String message) {
		if (message.isEmpty()) {
			return "<table border='0' width='100%'><tr><td style='width:36px; align:center; vertical-align: top;'>"
					+ avatarIcon + "</td><td>" + AwesomeFactory.getSpinnerIconHtml("pulse", "") + "</td></tr></table>";
		} else {
			String messageTextElementId = getID() + "_message";

			String copyButton = "<button onclick=\"copy(document.getElementById('" + messageTextElementId
					+ "').innerText);\" style='width:32px'>" + AwesomeFactory.getIconHtml("paste") + "</button>";

			return "<table border='0' width='100%'><tr><td style='width:36px; align:center; vertical-align: top;'>"
					+ avatarIcon + (CHATGPT.equals(role) ? copyButton : "") + "</td><td><span id='"
					+ messageTextElementId + "'>" + message + "</span></td></tr></table>";
		}
	}

	public void updateMessage(String message) {
		setContents(prepareContent(message));
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof MessageBox)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}