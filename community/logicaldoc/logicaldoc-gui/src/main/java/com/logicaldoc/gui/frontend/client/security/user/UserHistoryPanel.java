package com.logicaldoc.gui.frontend.client.security.user;

import com.logicaldoc.gui.common.client.data.UserHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the history of a user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UserHistoryPanel extends VLayout {

	private long userId;

	public UserHistoryPanel(long userId) {
		this.userId = userId;
	}

	@Override
	public void onDraw() {
		ListGridField event = new ListGridField("event", I18N.message("event"), 200);
		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);
		ListGridField comment = new ListGridField("comment", I18N.message("comment"));
		ListGridField sid = new ListGridField("sid", I18N.message("sid"), 200);
		ListGridField ip = new ListGridField("ip", I18N.message("ip"), 100);
		ListGridField geolocation = new ListGridField("geolocation", I18N.message("geolocation"), 200);
		ListGridField device = new ListGridField("device", I18N.message("device"), 200);

		ListGrid listGrid = new ListGrid();
		listGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		listGrid.setCanFreezeFields(true);
		listGrid.setAutoFetchData(true);
		listGrid.setDataSource(new UserHistoryDS(userId));
		listGrid.setFields(event, date, ip, geolocation, device, sid, comment);
		addMember(listGrid);
	}
}