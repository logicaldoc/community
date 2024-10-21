package com.logicaldoc.gui.frontend.client.security.user;

import com.logicaldoc.gui.common.client.data.UserHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.grid.CopyCellClickHandler;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

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

		RefreshableListGrid list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new UserHistoryDS(userId));
		list.setFields(event, date, ip, geolocation, device, sid, comment);
		list.addCellClickHandler(new CopyCellClickHandler());

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();

		SpinnerItem maxItem = ItemFactory.newSpinnerItem("max", "display", UserHistoryDS.getDefaultMaxHistories(), 1,
				(Integer) null);
		maxItem.setWidth(70);
		maxItem.setStep(20);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.setHint(I18N.message("elements"));
		buttons.addFormItem(maxItem);
		maxItem.addChangedHandler(
				evn -> list.refresh(new UserHistoryDS(userId, Integer.parseInt(maxItem.getValueAsString()))));

		buttons.addSeparator();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		buttons.addButton(export);
		export.addClickHandler(evn -> GridUtil.exportCSV(list, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		buttons.addButton(print);
		print.addClickHandler(evn -> GridUtil.print(list));

		addMember(list);
		addMember(buttons);
	}
}