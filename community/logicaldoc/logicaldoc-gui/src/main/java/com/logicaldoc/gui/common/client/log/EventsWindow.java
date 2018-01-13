package com.logicaldoc.gui.common.client.log;

import com.logicaldoc.gui.common.client.beans.GUIEvent;
import com.logicaldoc.gui.common.client.formatters.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Shows the events list
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class EventsWindow extends Window {

	private static EventsWindow instance = new EventsWindow();

	private ListGrid grid;

	public EventsWindow() {
		super();

		HeaderControl trash = new HeaderControl(HeaderControl.TRASH, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				grid.setData(new ListGridRecord[0]);
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, trash, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("lastevents"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setWidth(580);
		setHeight(300);
		centerInPage();

		grid = new ListGrid() {

			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				if (GUIEvent.ERROR.equals(record.getAttribute("severity")))
					return "color: #EF4A4A";
				if (GUIEvent.WARNING.equals(record.getAttribute("severity")))
					return "color: #FF8723";
				else
					return "color: #577ED0";
			}
		};
		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setWidth100();
		grid.setHeight100();
		grid.setCanReorderFields(false);
		grid.setCanFreezeFields(false);
		grid.setCanGroupBy(false);

		ListGridField date = new ListGridField("date", I18N.message("date"), 110);
		date.setAlign(Alignment.CENTER);
		date.setType(ListGridFieldType.DATE);
		date.setCellFormatter(new DateCellFormatter(false));
		date.setCanFilter(false);

		ListGridField detail = new ListGridField("detail", I18N.message("detail"));
		detail.setWidth("*");
		detail.setCanSort(false);

		ListGridField severityLabel = new ListGridField("severityLabel", I18N.message("severity"), 80);

		grid.setFields(date, severityLabel, detail);
		grid.setCanResizeFields(true);
		addItem(grid);
	}

	public void addEvent(GUIEvent event) {
		ListGridRecord record = new ListGridRecord();
		record.setAttribute("date", event.getDate());
		record.setAttribute("detail", event.getDetail());
		record.setAttribute("severity", event.getSeverity());
		record.setAttribute("severityLabel", I18N.message(event.getSeverity()));
		grid.addData(record);
		grid.sort("date", SortDirection.DESCENDING);
	}

	public static EventsWindow get() {
		return instance;
	}
}