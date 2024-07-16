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

	private String avatarIcon;

	private String role;

	public MessageBox(String message, String role) {
		setAlign(Alignment.LEFT);
		setCanSelectText(true);

		this.role = role;

		String url = "chatgpt".equals(role) ? Util.imageUrl("chatgpt.png")
				: Util.avatarUrl(Session.get().getUser().getId());
		avatarIcon = "<img src='" + url
				+ "' style='border: 0px; height: 32px; width: 32px  margin-right: 5px; margin-bottom: 3px' />";

		String color = "chatgpt".equals(role) ? "white" : "MintCream";
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
			String copyIcon = "<img src='" + Util.imageUrl("page_paste.png")
					+ "' style='border: 0px; height: 16px; width: 16px;' />";
			String copyButton = "<button onclick=\"copy(document.getElementById('" + messageTextElementId
					+ "').innerText);\" style='width:32px'>" + copyIcon + "</button>";

			return "<table border='0' width='100%'><tr><td style='width:36px; align:center; vertical-align: top;'>"
					+ avatarIcon + ("chatgpt".equals(role) ? copyButton : "") + "</td><td><span id='"
					+ messageTextElementId + "'>" + message + "</span></td></tr></table>";
		}
	}

	public void updateMessage(String message) {
		setContents(prepareContent(message));
	}
}