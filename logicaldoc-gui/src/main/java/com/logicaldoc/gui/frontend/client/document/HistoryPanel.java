package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentHistoryDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.widgets.grid.FileNameListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.FileVersionListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.VersionListGridField;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel shows the history of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class HistoryPanel extends DocumentDetailTab {

	public HistoryPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		ListGridField id = new ListGridField("id");
		id.setHidden(true);

		ListGridField user = new UserListGridField("user", "userId", "user");
		ListGridField event = new ListGridField("event", I18N.message("event"), 200);
		ListGridField version = new VersionListGridField();
		ListGridField fileVersion = new FileVersionListGridField();
		ListGridField date = new DateListGridField("date", "date", DateCellFormatter.FORMAT_LONG);

		ListGridField comment = new ListGridField("comment", I18N.message("comment"));
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

		final RefreshableListGrid list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new DocumentHistoryDS(document.getId(), null));
		if (Menu.enabled(Menu.SESSIONS))
			list.setFields(user, event, date, comment, version, fileVersion, fileName, path, sid, ip, device,
					geolocation);
		else
			list.setFields(user, event, date, comment, version, fileVersion, fileName, path);

		list.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				LD.askForValue(I18N.message("comment"), I18N.message("comment"),
						list.getSelectedRecord().getAttributeAsString("comment"), new ValueCallback() {
							@Override
							public void execute(final String value) {
								// Nothing to do
							}
						});
				event.cancel();
			}
		});

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
		maxItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				list.refresh(new DocumentHistoryDS(document.getId(), Integer.parseInt(maxItem.getValueAsString())));
			}
		});
		
		buttons.addSeparator();

		ToolStripButton export = new ToolStripButton(I18N.message("export"));
		buttons.addButton(export);
		export.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				GridUtil.exportCSV(list, true);
			}
		});

		ToolStripButton print = new ToolStripButton(I18N.message("print"));
		buttons.addButton(print);
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				GridUtil.print(list);
			}
		});

		VLayout container = new VLayout();
		container.setMembersMargin(3);
		container.addMember(list);
		container.addMember(buttons);
		addMember(container);
	}
}