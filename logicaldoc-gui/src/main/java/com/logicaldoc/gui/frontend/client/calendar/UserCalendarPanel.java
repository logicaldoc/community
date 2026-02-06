package com.logicaldoc.gui.frontend.client.calendar;

import java.util.Date;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.EmptyAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttendee;
import com.logicaldoc.gui.common.client.beans.GUICalendarEvent;
import com.logicaldoc.gui.common.client.beans.GUIReminder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.google.GoogleApiAuthorization;
import com.logicaldoc.gui.frontend.client.google.GoogleAsyncCallback;
import com.logicaldoc.gui.frontend.client.google.GoogleService;
import com.smartgwt.client.types.ViewName;
import com.smartgwt.client.widgets.calendar.Calendar;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Calendar dashboard that displays the events in which the user is involved
 * into.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class UserCalendarPanel extends VLayout {

	protected Calendar calendar = null;

	private static UserCalendarPanel instance;

	private Date choosenDate = null;

	private ViewName choosenView = null;

	public static UserCalendarPanel get() {
		if (instance == null)
			instance = new UserCalendarPanel();
		return instance;
	}

	public UserCalendarPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(click -> refresh());
		toolStrip.addButton(refresh);

		ToolStripButton newEvent = new ToolStripButton();
		newEvent.setTitle(I18N.message("newevent"));
		newEvent.addClickHandler(click -> newEvent());
		toolStrip.addButton(newEvent);

		if (Feature.enabled(Feature.GOOGLE_CALENDAR) && Menu.enabled(Menu.GOOGLE_CALENDAR)) {
			toolStrip.addSeparator();

			ToolStripButton authorize = new ToolStripButton();
			authorize.setTitle(I18N.message("authorize"));
			authorize.setTooltip(I18N.message("authorizegooglecal"));
			authorize.addClickHandler(click -> new GoogleApiAuthorization().show());
			toolStrip.addButton(authorize);

			ToolStripButton synchronize = new ToolStripButton();
			synchronize.setTitle(I18N.message("synchronize"));
			synchronize.setTooltip(I18N.message("synchronizegooglecal"));
			synchronize.addClickHandler(click -> synchronize());
			toolStrip.addButton(synchronize);

			CheckboxItem enableSynchronization = ItemFactory.newCheckbox("enabled");
			enableSynchronization.setTooltip(I18N.message("calendarenabledtip"));
			toolStrip.addFormItem(enableSynchronization);

			GoogleService.Instance.get().loadSettings(Session.get().getUser().getUsername(),
					new DefaultAsyncCallback<List<String>>() {

						@Override
						public void handleSuccess(List<String> settings) {
							enableSynchronization.setValue("1".equals(settings.get(2)));
							enableSynchronization
									.addChangedHandler(changed -> GoogleService.Instance.get().enableCalendar(
											enableSynchronization.getValueAsBoolean(), new EmptyAsyncCallback<>()));
						}
					});
		}

		toolStrip.addFill();
		addMember(toolStrip);

		initCalendar();
	}

	private void newEvent() {
		GUICalendarEvent calEvent = new GUICalendarEvent();
		calEvent.setOrganizer(Session.get().getUser().getFullName());
		calEvent.setOrganizerId(Session.get().getUser().getId());

		GUIAttendee user = new GUIAttendee();
		user.setId(Session.get().getUser().getId());
		user.setUsername(Session.get().getUser().getUsername());
		user.setFirstName(Session.get().getUser().getFirstName());
		user.setName(Session.get().getUser().getName());
		user.setEmail(Session.get().getUser().getEmail());
		calEvent.addAttendee(user);

		calEvent.addReminder(new GUIReminder(0, GUIReminder.TIME_UNIT_MINUTE));
		new CalendarEventDialog(calEvent, new RefreshCallback()).show();
	}

	private void initCalendar() {
		calendar = new EventsCalendar(null, choosenDate, new RefreshCallback());
		calendar.setChosenDate(choosenDate);
		calendar.setCurrentViewName(choosenView);
		addMember(calendar);
	}

	public void synchronize() {
		LD.contactingServer();
		GoogleService.Instance.get().synchronizeCalendar(new RefreshCallback());
	}

	public void refresh() {
		if (calendar != null) {
			removeMember(calendar);
			choosenDate = calendar.getChosenDate();
			choosenView = calendar.getCurrentViewName();
		}
		initCalendar();
	}

	private final class RefreshCallback extends GoogleAsyncCallback<Void> {
		@Override
		public void onSuccess(Void arg0) {
			refresh();
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