package com.logicaldoc.gui.frontend.client.services;

import java.util.Date;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;

public interface CalendarServiceAsync {
	void saveEvent(GUICalendarEvent event, AsyncCallback<Void> callback);

	void getEvent(long eventId, AsyncCallback<GUICalendarEvent> callback);

	void deleteEvent(long eventId, AsyncCallback<Void> callback);

	void countUserEvents(String username, Date end, AsyncCallback<Integer> callback);

	void find(Date startDate, Date endDate, Date expireFrom, Date expireTo, Integer frequency,
			String title, String type, String subtype, Integer status, Integer maxRecords, AsyncCallback<GUICalendarEvent[]> callback);
}