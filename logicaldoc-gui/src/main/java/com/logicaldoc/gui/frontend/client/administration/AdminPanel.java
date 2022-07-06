package com.logicaldoc.gui.frontend.client.administration;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * Represents a panel to be shown in the right part of the administration
 * screen.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public abstract class AdminPanel extends VLayout {
	protected VLayout body = new VLayout();
	
	protected TabSet tabs = new TabSet();

	protected Tab tab = new Tab();
	
	public AdminPanel(String title) {
		setWidth100();
		setMembersMargin(5);
		setMargin(5);

		body.setWidth100();
		body.setHeight100();

		tab.setTitle(I18N.message(title));
		tab.setPane(body);

		tabs.setWidth100();
		tabs.setHeight100();
		tabs.setTabs(tab);

		setMembers(tabs);
	}
}