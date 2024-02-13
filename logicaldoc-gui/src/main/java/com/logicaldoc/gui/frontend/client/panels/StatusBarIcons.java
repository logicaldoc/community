package com.logicaldoc.gui.frontend.client.panels;

import java.util.List;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIReadingRequest;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.ReadingRequestController;
import com.logicaldoc.gui.common.client.controllers.ReadingRequestObserver;
import com.logicaldoc.gui.common.client.controllers.UserController;
import com.logicaldoc.gui.common.client.controllers.UserObserver;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.clipboard.ClipboardObserver;
import com.logicaldoc.gui.frontend.client.clipboard.ClipboardWindow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Container for a set of clickable icons representing the program state.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class StatusBarIcons extends HLayout implements ClipboardObserver, UserObserver, ReadingRequestObserver {
	private static final String CLIPBOARD = "clipboard";

	private static StatusBarIcons instance;

	private Button clipboardCounter = AwesomeFactory.newIconButton(CLIPBOARD, CLIPBOARD, "0");

	private Button lockedCounter = AwesomeFactory.newIconButton("lock-alt", "event.lockeddocs", "0");

	private Button checkoutCounter = AwesomeFactory.newIconButton("edit", "event.checkedoutdocs", "0");

	private Button messagesCounter = AwesomeFactory.newIconButton("envelope", "messages");

	private Button workflowsCounter = AwesomeFactory.newIconButton("tasks", "workflowtasksassigned");

	private Button eventsCounter = AwesomeFactory.newIconButton("calendar", "upcomingevents");

	private Button readingsCounter = AwesomeFactory.newIconButton("glasses", "readingrequests", "0");

	private StatusBarIcons() {

		prepareIconsClickHandlers();

		addMember(clipboardCounter);
		addMember(lockedCounter);
		addMember(checkoutCounter);

		if (Feature.enabled(Feature.MESSAGES) && Menu.enabled(Menu.MESSAGES)) {
			addMember(messagesCounter);
		}

		if (Feature.enabled(Feature.CALENDAR) && Menu.enabled(Menu.DASHBOARD_CALENDAR)) {
			addMember(eventsCounter);
		}

		if (Feature.enabled(Feature.WORKFLOW)) {
			addMember(workflowsCounter);
		}

		if (Feature.enabled(Feature.READING_CONFIRMATION)) {
			addMember(readingsCounter);
		}

		Clipboard.getInstance().addObserver(this);
		UserController.get().addObserver(this);
		ReadingRequestController.get().addObserver(this);
		onUserChanged(Session.get().getUser());
		onNewReadingRequests(null);
	}

	private void prepareIconsClickHandlers() {
		clipboardCounter.addClickHandler(event -> {
			if (!Clipboard.getInstance().isEmpty())
				ClipboardWindow.getInstance().show();
		});

		lockedCounter.addClickHandler(event -> MainPanel.get().selectUserTab());

		checkoutCounter.addClickHandler(event -> MainPanel.get().selectUserTab());

		messagesCounter.addClickHandler(event -> {
			if (Menu.enabled(Menu.MESSAGES))
				MainPanel.get().selectMessagesTab();
		});

		workflowsCounter.addClickHandler(event -> {
			if (Feature.enabled(Feature.WORKFLOW))
				MainPanel.get().selectWorkflowTab();
		});

		eventsCounter.addClickHandler(event -> {
			if (Menu.enabled(Menu.DASHBOARD_CALENDAR))
				MainPanel.get().selectCalendarTab();
		});

		readingsCounter.addClickHandler(event -> {
			if (Menu.enabled(Menu.DASHBOARD_READINGS))
				MainPanel.get().selectReadingsTab();
		});
	}

	public static StatusBarIcons get() {
		if (instance == null)
			instance = new StatusBarIcons();
		return instance;
	}

	@Override
	public void onAdd(GUIDocument entry) {
		onRemove(entry);
	}

	@Override
	public void onRemove(GUIDocument entry) {
		clipboardCounter
				.setTitle(AwesomeFactory.getIconHtml(CLIPBOARD, Integer.toString(Clipboard.getInstance().size())));
	}

	@Override
	public void onUserChanged(GUIUser user) {
		lockedCounter.setTitle(AwesomeFactory.getIconHtml("lock-alt", Integer.toString(user.getLockedDocs())));
		checkoutCounter.setTitle(AwesomeFactory.getIconHtml("edit", Integer.toString(user.getCheckedOutDocs())));
		messagesCounter.setTitle(AwesomeFactory.getIconHtml("envelope", Integer.toString(user.getUnreadMessages())));
		workflowsCounter.setTitle(AwesomeFactory.getIconHtml("tasks", Integer.toString(user.getAssignedTasks())));
		eventsCounter.setTitle(AwesomeFactory.getIconHtml("calendar", Integer.toString(user.getUpcomingEvents())));
	}

	@Override
	public void onUserLogin(String username) {
		// Nothing to do
	}

	@Override
	public void onUserLogout(String username) {
		// Nothing to do
	}

	@Override
	public void destroy() {
		UserController.get().removeObserver(this);
	}

	@Override
	protected void onUnload() {
		destroy();
		super.onUnload();
	}

	@Override
	protected void onDestroy() {
		destroy();
		super.onDestroy();
	}

	@Override
	public void onConfirmReading(long docId) {
		readingsCounter.setTitle(AwesomeFactory.getIconHtml("glasses",
				Integer.toString(ReadingRequestController.get().countUnconfirmedReadings())));
	}

	@Override
	public void onNewReadingRequests(List<GUIReadingRequest> readings) {
		onConfirmReading(0L);
	}
}