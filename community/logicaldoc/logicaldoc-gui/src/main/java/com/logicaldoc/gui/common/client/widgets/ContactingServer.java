package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;

/**
 * This is the window that must be showed to the user during a long LogicalDOC
 * computation.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class ContactingServer extends Window {

	public static ContactingServer instance;

	public static ContactingServer get() {
		if (instance == null)
			instance = new ContactingServer();
		return instance;
	}

	private ContactingServer() {
		setShowEdges(false);
		setShowHeader(false);
		setShowHeaderBackground(false);
		setShowHeaderIcon(false);
		setShowResizer(false);
		setShowFooter(false);
		setAlign(Alignment.CENTER);
		setMargin(0);
		setMembersMargin(3);
		setPadding(0);
		setBodyColor("white");
		setBackgroundColor("white");
		setBorder("1px solid DarkBlue");
		setOverflow(Overflow.HIDDEN);
		setAutoSize(true);
		centerInPage();
		setIsModal(true);
		setVertical(true);

		Label message = new Label(I18N.message("contactingserver") + "...");
		message.setWrap(false);
		message.setAlign(Alignment.CENTER);
		message.setIcon(Util.imageUrl("loading32.gif"));
		message.setIconSize(32);
		message.setStyleName("contactingserver");
		message.setLayoutAlign(Alignment.CENTER);
		message.setLayoutAlign(VerticalAlignment.TOP);
		message.setBackgroundColor("white");
		message.setShowEdges(false);
		message.setAutoFit(true);
		message.setHeight(50);
		message.setMargin(0);

		addItem(message);
	}
}