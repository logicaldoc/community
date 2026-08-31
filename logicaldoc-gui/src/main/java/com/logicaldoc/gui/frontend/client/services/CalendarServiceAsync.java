package com.logicaldoc.gui.frontend.client.services;

import java.util.Date;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUICalendarEventSearchCriteria;

public interface CalendarServiceAsync {
	void saveEvent(GUICalendarEvent event, AsyncCallback<Void> callback);

	void getEvent(long eventId, AsyncCallback<GUICalendarEvent> callback);

	void deleteEvent(long eventId, boolean alertCancelation, AsyncCallback<Void> callback);

	void countUserEvents(String username, Date end, AsyncCallback<Integer> callback);

	void find(GUICalendarEventSearchCriteria criteria, AsyncCallback<List<GUICalendarEvent>> callback);
}