package com.logicaldoc.gui.frontend.client.dashboard.chat;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.ChatMessagesDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays the messages in the chat.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public class ChatMessagesPanel extends VLayout implements ChatObserver {
	private static final String USERNAME = "username";

	private static final String MESSAGE = "message";

	private RefreshableListGrid messages = null;

	private Timer timer;

	public ChatMessagesPanel() {
		setWidth100();
		setHeight100();
		ChatController.get().addObserver(this);
	}

	@Override
	public void onDraw() {
		ListGridField message = new ListGridField(MESSAGE, I18N.message(MESSAGE));
		message.setWidth("*");
		message.setShowTitle(false);

		ListGridField user = new UserListGridField("user", "userId", "user",
				Session.get().getConfigAsBoolean("gui.avatar.showingrids"));
		user.setShowTitle(false);

		ListGridField username = new ListGridField(USERNAME, I18N.message(USERNAME));
		username.setShowTitle(false);
		username.setWidth(80);
		username.setHidden(true);

		ListGridField date = new DateListGridField("date", "date");
		date.setHidden(true);
		date.setShowTitle(false);

		messages = new RefreshableListGrid(new ChatMessagesDS());
		messages.setEmptyMessage(I18N.message("nomessages"));
		messages.setAutoFetchData(true);
		messages.setSelectionType(SelectionStyle.NONE);
		messages.setFields(date, user, username, message);
		messages.setSortField("date");
		messages.addVisibilityChangedHandler(visibilityChanged -> {
			if (timer != null) {
				if (!visibilityChanged.getIsVisible())
					stopTimer();
				else if (!timer.isRunning())
					startTimer();
			}
		});
		messages.addDataArrivedHandler(event -> messages.scrollToRow(messages.getTotalRows() - 1));

		setMembers(messages);
		startTimer();
	}

	/**
	 * Takes care of installing the timer responsible of opening the tree
	 */
	private void startTimer() {
		stopTimer();
		timer = new Timer() {
			public void run() {
				if (isVisible()) {
					messages.refresh(new ChatMessagesDS());
				}
			}
		};
		timer.scheduleRepeating(120 * 1000);
	}

	/**
	 * Stops the timer timer responsible of opening the tree
	 */
	private void stopTimer() {
		if (timer != null)
			timer.cancel();
	}

	@Override
	public void onMessage(long id, Date date, String username, String message) {
		Record rec = messages.find(new AdvancedCriteria("id", OperatorId.EQUALS, id));
		if (rec == null) {
			List<ListGridRecord> recd = new ArrayList<>();

			Record[] records = messages.getDataAsRecordList().toArray();
			if (records != null && records.length > 0) {
				for (Record r : records)
					recd.add(new ListGridRecord(r));
			}

			ListGridRecord r = new ListGridRecord();
			r.setAttribute("id", id);
			r.setAttribute(USERNAME, username);
			r.setAttribute("date", date);
			r.setAttribute(MESSAGE, message);
			recd.add(r);

			messages.setRecords(recd.toArray(new ListGridRecord[0]));
			messages.scrollToRow(recd.size() - 1);
		}
	}

	@Override
	public void destroy() {
		ChatController.get().removeObserver(this);
		stopTimer();
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
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}