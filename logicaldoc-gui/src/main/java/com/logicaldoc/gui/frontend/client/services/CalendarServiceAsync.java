package com.logicaldoc.gui.frontend.client.services;

import java.util.Date;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.CalendarEventSearchCriteria;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;

public interface CalendarServiceAsync {
	void saveEvent(GUICalendarEvent event, AsyncCallback<Void> callback);

	void getEvent(long eventId, AsyncCallback<GUICalendarEvent> callback);

	void deleteEvent(long eventId, AsyncCallback<Void> callback);

	void countUserEvents(String username, Date end, AsyncCallback<Integer> callback);

	void find(CalendarEventSearchCriteria criteria, AsyncCallback<GUICalendarEvent[]> callback);
}