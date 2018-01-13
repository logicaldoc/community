package com.logicaldoc.gui.frontend.client.panels;

import com.logicaldoc.gui.common.client.log.EventPanel;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * The program footer
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class StatusBar extends HLayout {

	public StatusBar(boolean includeIcons) {
		setHeight(20);
		setWidth100();
		setMembersMargin(2);
		setStyleName("footer");

		HLayout events = EventPanel.get();
		events.setWidth100();

		addMember(events);

		if (includeIcons)
			addMember(StatusBarIcons.get());
	}
}