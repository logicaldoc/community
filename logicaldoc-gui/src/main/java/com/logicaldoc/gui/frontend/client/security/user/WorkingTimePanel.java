package com.logicaldoc.gui.frontend.client.security.user;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.beans.GUIWorkingTime;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.ValuesCallback;
import com.logicaldoc.gui.common.client.widgets.GroupSelectorCombo;
import com.logicaldoc.gui.common.client.widgets.UserSelectorCombo;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TimeDisplayFormat;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.calendar.Calendar;
import com.smartgwt.client.widgets.calendar.CalendarEvent;
import com.smartgwt.client.widgets.calendar.CalendarView;
import com.smartgwt.client.widgets.calendar.DateHeaderCustomizer;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Allows the configuration of the user's working time
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.2
 */
public class WorkingTimePanel extends VLayout {
	private GUIUser user;

	private ChangedHandler changedHandler;

	private Calendar calendar = null;

	public WorkingTimePanel(GUIUser user, ChangedHandler changedHandler) {
		if (user == null) {
			setMembers(UsersPanel.SELECT_USER);
		} else {
			this.user = user;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();
			setMembersMargin(20);
			setAlign(Alignment.LEFT);
		}
	}

	@Override
	protected void onDraw() {
		prepareGUI();
	}

	public void prepareGUI() {
		ToolStripButton clone = new ToolStripButton(I18N.message("clone"));
		clone.addClickHandler(event -> {
			final UserSelectorCombo usersSelector = new UserSelectorCombo("users", "users", null, true, true);

			final GroupSelectorCombo groupsSelector = new GroupSelectorCombo("groups", "groups");

			LD.askForValues("cloneworktime", null, Arrays.asList(new FormItem[] { usersSelector, groupsSelector }), 350,
					new ValuesCallback() {
						@Override
						public void execute(String value) {
							// Nothing to do
						}

						@Override
						public void execute(Map<String, Object> values) {
							LD.contactingServer();
							SecurityService.Instance.get().cloneWorkTimes(user.getId(), usersSelector.getUserIds(),
									groupsSelector.getGroupIds(), new AsyncCallback<Void>() {

										@Override
										public void onFailure(Throwable caught) {
											LD.clearPrompt();
											GuiLog.serverError(caught);
										}

										@Override
										public void onSuccess(Void arg0) {
											LD.clearPrompt();
										}
									});
						}
					});
		});
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addButton(clone);

		// We do not want the event dialog to display a title, so prepare a
		// model window without title and specify it's usage through the
		// AutoChild
		Window eventDialogModel = new Window();
		eventDialogModel.setShowTitle(false);

		calendar = new Calendar();
		calendar.setAutoChildProperties("eventDialog", eventDialogModel);
		calendar.setAutoChildProperties("eventEditorLayout", eventDialogModel);

		calendar.setStyleName("calendar-without-label");
		calendar.setDateFormatter(I18N.getDateDisplayFormat(false));
		calendar.setTimeFormatter(TimeDisplayFormat.TOSHORT24HOURTIME);
		calendar.setShowWeekView(true);
		calendar.setShowDayView(false);
		calendar.setShowMonthView(false);
		calendar.setShowDatePickerButton(false);
		calendar.setShowDateChooser(false);
		calendar.setShowNextButton(false);
		calendar.setShowPreviousButton(false);
		calendar.setShowAddEventButton(false);
		calendar.setShowEventHovers(false);
		calendar.setEventAutoArrange(true);
		calendar.setEventOverlap(false);
		calendar.setScrollToWorkday(true);
		calendar.setDisableWeekends(false);

		calendar.setEventNameFieldTitle(I18N.message("label"));
		calendar.setEventDescriptionFieldTitle(I18N.message("comment"));
		calendar.setSaveButtonTitle(I18N.message("save"));
		calendar.setDetailsButtonTitle(I18N.message("edit"));
		calendar.setRemoveButtonTitle(I18N.message("remove"));
		calendar.setCancelButtonTitle(I18N.message("cancel"));

		if (user.getWorkingTimes() != null && user.getWorkingTimes().length > 0) {
			calendar.setChosenDate(user.getWorkingTimes()[0].getStart());
			for (GUIWorkingTime wt : user.getWorkingTimes()) {
				try {
					calendar.addEvent(wt.getStart(), wt.getEnd(), wt.getLabel(), wt.getDescription());
				} catch (Exception t) {
					// Nothing to do
				}
			}
		}

		calendar.setDateHeaderCustomizer(new DateHeaderCustomizer() {

			@Override
			public String getHeaderTitle(Date date, int dayOfWeek, String defaultValue, CalendarView calendarView) {
				return I18N.message("dayname_" + dayOfWeek);
			}
		});

		calendar.setCanCreateEvents(changedHandler != null);
		calendar.setCanRemoveEvents(changedHandler != null);
		if (changedHandler != null) {
			calendar.addEventRemovedHandler(event -> changedHandler.onChanged(null));

			calendar.addEventAddedHandler(event -> changedHandler.onChanged(null));

			calendar.addEventChangedHandler(event -> changedHandler.onChanged(null));
		}

		addMember(calendar);
		addMember(toolStrip);
	}

	boolean validate() {
		if (calendar != null) {
			CalendarEvent[] events = calendar.getData();
			if (events != null && events.length > 0) {
				ArrayList<GUIWorkingTime> wts = new ArrayList<>();
				for (CalendarEvent calendarEvent : events) {
					GUIWorkingTime wt = new GUIWorkingTime(calendarEvent.getName(), calendarEvent.getStartDate(),
							calendarEvent.getEndDate());
					wt.setDescription(calendarEvent.getDescription());
					wts.add(wt);
				}
				if (wts.isEmpty())
					user.setWorkingTimes(new GUIWorkingTime[0]);
				else
					user.setWorkingTimes(wts.toArray(new GUIWorkingTime[0]));
			} else
				user.setWorkingTimes(new GUIWorkingTime[0]);
		}

		return true;
	}
}