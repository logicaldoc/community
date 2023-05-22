package com.logicaldoc.gui.frontend.client.account;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.UserHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the list of the user's last succesful logins.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class LastLogins extends com.smartgwt.client.widgets.Window {

	private ListGrid list;

	public LastLogins() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("lastlogins"));
		setWidth(600);
		setHeight(400);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		centerInPage();
		setAutoSize(true);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		refresh.addClickHandler(event -> refresh());

		toolStrip.addSeparator();

		ToolStripButton export = new ToolStripButton();
		export.setTitle(I18N.message("export"));
		export.addClickHandler(event -> GridUtil.exportCSV(list, true));
		if (Feature.visible(Feature.EXPORT_CSV)) {
			toolStrip.addButton(export);
			if (!Feature.enabled(Feature.EXPORT_CSV)) {
				export.setDisabled(true);
				export.setTooltip(I18N.message("featuredisabled"));
			}
		}

		toolStrip.addFill();
		addItem(toolStrip);

		prepareGrid();
		addItem(list);

		list.fetchData();
	}

	private void prepareGrid() {
		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField date = new DateListGridField("date", "date");

		ListGridField ip = new ListGridField("ip", I18N.message("ip"), 80);
		ip.setCanFilter(true);

		ListGridField device = new ListGridField("device", I18N.message("device"), 200);
		device.setCanFilter(true);

		ListGridField geolocation = new ListGridField("geolocation", I18N.message("geolocation"));
		geolocation.setCanFilter(true);
		geolocation.setWidth("*");

		list = new ListGrid();
		list.setWidth100();
		list.setHeight(getHeight());
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.SINGLE);
		list.setDataSource(new UserHistoryDS(Session.get().getUser().getId(), "event.user.login", 300));
		list.setFields(id, date, ip, device, geolocation);

		addResizedHandler(event ->list.setHeight(getHeight() - 68));
	}

	public void refresh() {
		list.setDataSource(new UserHistoryDS(Session.get().getUser().getId(), "event.user.login", 300));
		list.fetchData();
	}
}