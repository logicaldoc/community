package com.logicaldoc.gui.frontend.client.services;

import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUICalendarEventSearchCriteria;

/**
 * The client side stub for the Calendar Service. This service allows the
 * handling of calendar events.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
@RemoteServiceRelativePath("calendar")
public interface CalendarService extends RemoteService {

	/**
	 * Saves an event into the calendar
	 * 
	 * @param event the event to save
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveEvent(GUICalendarEvent event) throws ServerException;

	/**
	 * Gets an event
	 * 
	 * @param eventId identifier of the event
	 * 
	 * @return the calendar event
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUICalendarEvent getEvent(long eventId) throws ServerException;

	/**
	 * Searches for events
	 * 
	 * @param criteria the search criteria
	 * 
	 * @return The list of events ordered by ascending date
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUICalendarEvent> find(GUICalendarEventSearchCriteria criteria) throws ServerException;

	/**
	 * Deletes an event. If the event is a master, in any case all the
	 * occurrences will be deleted too
	 * 
	 * @param eventId identifier of the event
	 * @param alertCancelation flat to alert attendees about the cancelation
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void deleteEvent(long eventId, boolean alertCancelation) throws ServerException;

	/**
	 * Counts the number of events that start from now until a given date
	 * 
	 * @param username The user to be processed
	 * @param end The and date
	 * 
	 * @return The number of found events
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public int countUserEvents(String username, Date end) throws ServerException;

	public static class Instance {
		private static CalendarServiceAsync inst;

		private Instance() {
		}

		public static CalendarServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(CalendarService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}