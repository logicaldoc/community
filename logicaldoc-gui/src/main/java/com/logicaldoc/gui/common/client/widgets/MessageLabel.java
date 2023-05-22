package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.user.client.Window;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.widgets.Label;

/**
 * Simple label showing a warning message.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MessageLabel extends Label {

	public MessageLabel(final GUIMessage message) {
		this(message, true);
	}

	public MessageLabel(final GUIMessage message, boolean showLinks) {
		if (message.getUrl() == null || message.getUrl().isEmpty() || !showLinks)
			setContents("<span>" + message.getMessage() + "</span>");
		else
			setContents("<span style='text-decoration: underline'>" + message.getMessage() + "</span>");
		setHeight(25);
		setWrap(true);
		if (message.getPriority() == GUIMessage.PRIO_INFO)
			setIcon("[SKIN]/Dialog/notify.png");
		else if (message.getPriority() == GUIMessage.PRIO_WARN)
			setIcon("[SKIN]/Dialog/warn.png");
		if (showLinks && message.getUrl() != null && !message.getUrl().isEmpty()) {
			setCursor(Cursor.HAND);
			addClickHandler(event -> Window.open(message.getUrl(), "_self", ""));
		}
	}
}