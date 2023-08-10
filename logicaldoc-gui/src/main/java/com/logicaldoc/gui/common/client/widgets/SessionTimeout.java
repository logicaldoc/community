package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.Label;

/**
 * This is the panel shown when the session timeout occurs.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.4.1
 */
public class SessionTimeout extends Dialog {

	private static final String WHITE = "white";

	private static SessionTimeout instance;

	public static SessionTimeout get() {
		if (instance == null)
			instance = new SessionTimeout();
		return instance;
	}

	private SessionTimeout() {
		setShowEdges(false);
		setShowHeader(false);
		centerInPage();
		setIsModal(true);
		setVertical(true);
		setAlign(Alignment.CENTER);
		setMargin(2);
		setMembersMargin(0);
		setBodyColor(WHITE);
		setBackgroundColor(WHITE);
		setOverflow(Overflow.HIDDEN);
		setHeight100();
		setWidth100();

		Label message = new Label(I18N.message("sessiontimeout"));
		message.setWrap(false);
		message.setAlign(Alignment.CENTER);
		message.setStyleName("sessiontimeout");
		message.setLayoutAlign(Alignment.CENTER);
		message.setLayoutAlign(VerticalAlignment.CENTER);
		message.setHeight(50);
		message.setBackgroundColor(WHITE);

		addMember(message);
	}
}