package com.logicaldoc.gui.frontend.client.document;

import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.DocumentHistoryDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

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

		ListGridField user = new ListGridField("user", I18N.message("user"), 100);
		ListGridField event = new ListGridField("event", I18N.message("event"), 200);
		ListGridField version = new ListGridField("version", I18N.message("version"), 70);
		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.CENTER);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);
		ListGridField comment = new ListGridField("comment", I18N.message("comment"));
		ListGridField fileName = new ListGridField("filename", I18N.message("filename"));
		ListGridField path = new ListGridField("path", I18N.message("path"));
		ListGridField sid = new ListGridField("sid", I18N.message("sid"));
		ListGridField ip = new ListGridField("ip", I18N.message("ip"));

		final ListGrid list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new DocumentHistoryDS(document.getId(), null));
		if (Menu.enabled(Menu.SESSIONS))
			list.setFields(user, event, date, comment, version, fileName, path, sid, ip);
		else
			list.setFields(user, event, date, comment, version, fileName, path);

		list.addDoubleClickHandler(new DoubleClickHandler() {
			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				LD.askForValue(I18N.message("comment"), I18N.message("comment"), list.getSelectedRecord()
						.getAttributeAsString("comment"), new ValueCallback() {
					@Override
					public void execute(final String value) {
					}
				});
				event.cancel();
			}
		});

		VLayout container = new VLayout();
		container.setMembersMargin(3);
		container.addMember(list);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);

		Button exportButton = new Button(I18N.message("export"));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Util.exportCSV(list, true);
			}
		});

		Button print = new Button(I18N.message("print"));
		print.setAutoFit(true);
		buttons.addMember(print);
		print.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Canvas.printComponents(new Object[] { list });
			}
		});

		container.addMember(buttons);
		addMember(container);
	}
}