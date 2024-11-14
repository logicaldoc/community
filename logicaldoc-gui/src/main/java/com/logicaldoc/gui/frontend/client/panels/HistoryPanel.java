package com.logicaldoc.gui.frontend.client.panels;

import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.grid.CopyCellClickHandler;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileVersionListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * A geneneric panel for displaying history records
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public abstract class HistoryPanel extends VLayout {

	private static final String COMMENT = "comment";

	private boolean versionFields = false;

	protected HistoryPanel() {
		setMembersMargin(3);
	}

	protected HistoryPanel(boolean versionFields) {
		this();
		this.versionFields = versionFields;
	}

	@Override
	protected void onDraw() {
		ListGridField user = new UserListGridField("user", "userId", "user");
		ListGridField event = new ListGridField("event", I18N.message("event"), 200);
		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);
		ListGridField comment = new ListGridField(COMMENT, I18N.message(COMMENT));
		FileNameListGridField fileName = new FileNameListGridField();
		ListGridField path = new ListGridField("path", I18N.message("path"));
		ListGridField sid = new ListGridField("sid", I18N.message("sid"));
		ListGridField ip = new ListGridField("ip", I18N.message("ip"));
		ip.setAutoFitWidth(true);
		ListGridField device = new ListGridField("device", I18N.message("device"));
		device.setAutoFitWidth(true);
		device.setHidden(true);
		ListGridField geolocation = new ListGridField("geolocation", I18N.message("geolocation"));
		geolocation.setAutoFitWidth(true);
		geolocation.setHidden(true);

		ListGridField version = new VersionListGridField();
		ListGridField fileVersion = new FileVersionListGridField();

		final RefreshableListGrid list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(getDataSource(null));
		list.addCellDoubleClickHandler(new CopyCellClickHandler());

		if (Menu.enabled(Menu.SESSIONS)) {
			if (versionFields)
				list.setFields(user, event, date, comment, version, fileVersion, fileName, path, sid, ip, device,
						geolocation);
			else
				list.setFields(user, event, date, comment, fileName, path, sid, ip, device, geolocation);
		} else {
			if (versionFields)
				list.setFields(user, event, date, comment, version, fileVersion, fileName, path);
			else
				list.setFields(user, event, date, comment, fileName, path);
		}

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();

		SpinnerItem maxItem = ItemFactory.newSpinnerItem("max", "display",
				Session.get().getConfigAsInt("gui.maxhistories"), 1, (Integer) null);
		maxItem.setWidth(70);
		maxItem.setStep(20);
		maxItem.setSaveOnEnter(true);
		maxItem.setImplicitSave(true);
		maxItem.setHint(I18N.message("elements"));
		buttons.addFormItem(maxItem);
		maxItem.addChangedHandler(evnt -> list.refresh(getDataSource(Integer.parseInt(maxItem.getValueAsString()))));

		buttons.addSeparator();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		buttons.addButton(export);
		export.addClickHandler(evnt -> GridUtil.exportCSV(list, true));

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		buttons.addButton(print);
		print.addClickHandler(evnt -> GridUtil.print(list));

		addMember(list);
		addMember(buttons);
	}

	/**
	 * Concrete implementations must put here the retrieval of the data source
	 * 
	 * @param maxItems maximum number of events
	 * 
	 * @return the data source to use
	 */
	protected abstract DataSource getDataSource(Integer maxItems);
}
