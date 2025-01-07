package com.logicaldoc.gui.frontend.client.calendar;

import java.util.Date;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.CalendarEventsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.CalendarService;
import com.smartgwt.client.types.TimeDisplayFormat;
import com.smartgwt.client.widgets.calendar.Calendar;

/**
 * Represents a calendar containing events related to documents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.3
 */
public class EventsCalendar extends Calendar {

	public EventsCalendar(Long docId, Date date, final AsyncCallback<Void> onChangeCallback) {
		setDataSource(new CalendarEventsDS(docId));
		setAutoFetchData(true);
		setScrollToWorkday(true);
		setShowDayView(false);
		setDayViewTitle(I18N.message("day"));
		setWeekViewTitle(I18N.message("week"));
		setMonthViewTitle(I18N.message("month"));
		setPreviousButtonHoverText(I18N.message("previous"));
		setNextButtonHoverText(I18N.message("next"));
		setCancelButtonTitle(I18N.message("cancel"));
		setDatePickerHoverText(I18N.message("choosedate"));
		setDateFormatter(I18N.getDateDisplayFormat(false));
		setTimeFormatter(TimeDisplayFormat.TOSHORT24HOURTIME);
		setCanCreateEvents(false);
		setCanResizeEvents(false);
		setCanDragEvents(false);
		setCanDragReposition(false);
		setCanDragResize(false);
		setCanDrop(false);
		setCanDrag(false);
		setCanAcceptDrop(false);
		setCanDragScroll(false);
		setCanEditLane(false);
		setCanEditEvents(false);
		setCanRemoveEvents(false);
		if (date != null)
			setChosenDate(date);
		else
			setChosenDate(new Date());

		addEventClickHandler(event -> {
			CalendarService.Instance.get().getEvent(event.getEvent().getAttributeAsLong("eventId"),
					new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(final GUICalendarEvent ev) {
							long organizerId = Long.parseLong(event.getEvent().getAttribute("organizerId"));
							GUIUser currentUser = Session.get().getUser();

							if (ev.getParentId() != null && (currentUser.getId() == organizerId
									|| currentUser.isMemberOf(Constants.GROUP_ADMIN))) {
								LD.ask(I18N.message("editevent"), I18N.message("douwantmodifyalloccurrences"),
										editAllOccurrences -> {
											if (Boolean.FALSE.equals(editAllOccurrences)) {
												CalendarEventDialog eventDialog = new CalendarEventDialog(ev,
														onChangeCallback);
												eventDialog.show();
											} else {
												CalendarService.Instance.get().getEvent(
														Long.parseLong(event.getEvent().getAttribute("parentId")),
														new DefaultAsyncCallback<>() {
															@Override
															public void onSuccess(GUICalendarEvent calEv) {
																new CalendarEventDialog(calEv, onChangeCallback).show();
															}
														});
											}
										});
							} else {
								new CalendarEventDialog(ev, onChangeCallback).show();
							}
						}
					});
			event.cancel();
		});
	}
}
