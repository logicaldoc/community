package com.logicaldoc.gui.frontend.client.document;

import java.util.Date;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
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

		calendar = new EventsCalendar(docId, null, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(Void arg0) {
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
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}