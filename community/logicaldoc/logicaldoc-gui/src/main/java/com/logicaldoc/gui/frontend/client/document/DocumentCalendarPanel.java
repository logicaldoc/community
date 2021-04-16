package com.logicaldoc.gui.frontend.client.document;

import java.util.Date;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.calendar.EventsCalendar;
import com.smartgwt.client.types.ViewName;
import com.smartgwt.client.widgets.calendar.Calendar;

/**
 * This panel shows the calendar events on a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class DocumentCalendarPanel extends DocumentDetailTab {

	protected Calendar calendar = null;

	private Date choosenDate = null;

	private ViewName choosenView = null;

	public DocumentCalendarPanel(final GUIDocument document) {
		super(document, null);
		setMembersMargin(1);
	}

	@Override
	protected void onDraw() {
		long docId = document.getId();
		if (document.getDocRef() != null)
			docId = document.getDocRef();

		calendar = new EventsCalendar(docId, null, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				refresh();
			}
		});

		calendar.setChosenDate(choosenDate);
		calendar.setCurrentViewName(choosenView);
		setMembers(calendar);
	}

	public void refresh() {
		if (calendar != null) {
			removeMember(calendar);
			choosenDate = calendar.getChosenDate();
			choosenView = calendar.getCurrentViewName();
		}

		onTabSelected();
	}
}