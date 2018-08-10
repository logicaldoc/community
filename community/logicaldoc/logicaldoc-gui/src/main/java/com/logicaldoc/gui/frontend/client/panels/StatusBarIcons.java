package com.logicaldoc.gui.frontend.client.panels;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.observer.UserController;
import com.logicaldoc.gui.common.client.observer.UserObserver;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.frontend.client.clipboard.Clipboard;
import com.logicaldoc.gui.frontend.client.clipboard.ClipboardObserver;
import com.logicaldoc.gui.frontend.client.clipboard.ClipboardWindow;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Container for a set of clickable icons representing the program state.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class StatusBarIcons extends HLayout implements ClipboardObserver, UserObserver {
	private static StatusBarIcons instance;

	private ToolStripButton clipboardCounter = AwesomeFactory.newIconButton("clipboard", "clipboard", "0");

	private ToolStripButton lockedCounter = AwesomeFactory.newIconButton("lock-alt", "event.lockeddocs", "0");

	private ToolStripButton checkoutCounter = AwesomeFactory.newIconButton("edit", "event.checkedoutdocs", "0");

	private ToolStripButton messagesCounter = AwesomeFactory.newIconButton("envelope", "messages");

	private ToolStripButton workflowsCounter = AwesomeFactory.newIconButton("tasks", "workflowtasksassigned");

	private ToolStripButton eventsCounter = AwesomeFactory.newIconButton("calendar", "upcomingevents");

	private StatusBarIcons() {

		clipboardCounter.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (!Clipboard.getInstance().isEmpty())
					ClipboardWindow.getInstance().show();
			}
		});

		lockedCounter.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				MainPanel.get().selectUserTab();
			}
		});

		checkoutCounter.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				MainPanel.get().selectUserTab();
			}
		});

		messagesCounter.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				MainPanel.get().selectMessagesTab();
			}
		});

		workflowsCounter.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				MainPanel.get().selectWorkflowTab();
			}
		});

		eventsCounter.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				MainPanel.get().selectCalendarTab();
			}
		});

		addMember(clipboardCounter);
		addMember(lockedCounter);
		addMember(checkoutCounter);

		if (Feature.enabled(Feature.MESSAGES)) {
			addMember(messagesCounter);
		}

		if (Feature.enabled(Feature.CALENDAR)) {
			addMember(eventsCounter);
		}

		if (Feature.enabled(Feature.WORKFLOW)) {
			addMember(workflowsCounter);
		}

		Clipboard.getInstance().addObserver(this);
		UserController.get().addObserver(this);
		onUserChanged(Session.get().getUser());
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
		clipboardCounter.setTitle(AwesomeFactory.getIconHtml("clipboard",
				Integer.toString(Clipboard.getInstance().size())));
	}

	@Override
	public void onUserChanged(GUIUser user) {
		lockedCounter.setTitle(AwesomeFactory.getIconHtml("lock-alt", Integer.toString(user.getLockedDocs())));
		checkoutCounter.setTitle(AwesomeFactory.getIconHtml("edit", Integer.toString(user.getCheckedOutDocs())));
		messagesCounter.setTitle(AwesomeFactory.getIconHtml("envelope", Integer.toString(user.getUnreadMessages())));
		workflowsCounter.setTitle(AwesomeFactory.getIconHtml("tasks", Integer.toString(user.getActiveTasks())));
		eventsCounter.setTitle(AwesomeFactory.getIconHtml("calendar", Integer.toString(user.getUpcomingEvents())));
	}

	@Override
	public void destroy() {
		UserController.get().removeObserver(this);
	}

	@Override
	protected void finalize() throws Throwable {
		destroy();
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
}