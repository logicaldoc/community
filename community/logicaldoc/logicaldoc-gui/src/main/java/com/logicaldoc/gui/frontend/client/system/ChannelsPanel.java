package com.logicaldoc.gui.frontend.client.system;

import com.logicaldoc.gui.common.client.data.ChannelsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Displays a list of cluster channels.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.5
 */
public class ChannelsPanel extends VLayout {
	private ListGrid list;

	private Layout listing = new VLayout();

	public ChannelsPanel() {
		setMembersMargin(3);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				initListGrid();
			}
		});
		toolStrip.addFill();

		setMembers(toolStrip, listing);
		
		initListGrid();
	}

	private void initListGrid() {
		if (list != null) {
			listing.removeMember(list);
			list.destroy();
		}

		ListGridField name = new ListGridField("name", I18N.message("channel"), 150);
		name.setCanEdit(false);

		ListGridField members = new ListGridField("members", I18N.message("members"));
		members.setCanEdit(false);
		members.setWidth("*");

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanEdit(false);
		list.setWidth100();
		list.setHeight100();
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setFields(name, members);
		list.setDataSource(new ChannelsDS());
		
		listing.addMember(list);
	}
}