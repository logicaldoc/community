package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.FolderHistoryDS;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the history of a folder
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class HistoryPanel extends FolderDetailTab {

	public HistoryPanel(final GUIFolder folder) {
		super(folder, null);

		ListGridField user = new ListGridField("user", I18N.message("user"), 100);
		ListGridField event = new ListGridField("event", I18N.message("event"), 200);
		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.CENTER);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);
		ListGridField comment = new ListGridField("comment", I18N.message("comment"));
		ListGridField fileName = new ListGridField("filename", I18N.message("name"));
		ListGridField path = new ListGridField("path", I18N.message("path"));
		ListGridField sid = new ListGridField("sid", I18N.message("sid"));

		final ListGrid list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setDataSource(new FolderHistoryDS(folder.getId()));
		if (Menu.enabled(Menu.SESSIONS))
			list.setFields(user, event, date, comment, fileName, path, sid);
		else
			list.setFields(user, event, date, comment, fileName, path);

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