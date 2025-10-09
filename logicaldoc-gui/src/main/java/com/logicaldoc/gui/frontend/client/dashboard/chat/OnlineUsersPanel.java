package com.logicaldoc.gui.frontend.client.dashboard.chat;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.UserController;
import com.logicaldoc.gui.common.client.controllers.UserObserver;
import com.logicaldoc.gui.common.client.data.OnlineUsersDS;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.ChatService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Displays the online users
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public class OnlineUsersPanel extends VLayout implements UserObserver {
	private static final String USERNAME = "username";

	private RefreshableListGrid onlineUsers = null;

	private Timer timer;

	public OnlineUsersPanel() {
		setWidth100();
		setHeight100();
		UserController.get().addObserver(this);
	}

	@Override
	public void onDraw() {
		ListGridField user = new UserListGridField("user", "id", "user",
				Session.get().getConfigAsBoolean("gui.avatar.showingrids"));
		user.setShowTitle(false);
		user.setWidth("100%");

		ListGridField username = new ListGridField(USERNAME, I18N.message(USERNAME));
		username.setShowTitle(false);
		username.setWidth(80);
		username.setHidden(true);

		onlineUsers = new RefreshableListGrid(new OnlineUsersDS());
		onlineUsers.setEmptyMessage(I18N.message("nousers"));
		onlineUsers.setAutoFetchData(true);
		onlineUsers.setFields(user, username);
		onlineUsers.setSortField(USERNAME);

		onlineUsers.addVisibilityChangedHandler(event -> {
			if (timer != null) {
				if (!event.getIsVisible())
					stopTimer();
				else if (!timer.isRunning())
					startTimer();
			}
		});

		onlineUsers.addCellContextClickHandler(click -> {
			prepareContextMenu().showContextMenu();
			click.cancel();
		});

		setMembers(onlineUsers);

		startTimer();
	}

	/**
	 * Takes care of installing the timer responsible of opening the tree
	 */
	private void startTimer() {
		stopTimer();
		timer = new Timer() {
			public void run() {
				if (isVisible())
					onlineUsers.refresh(new OnlineUsersDS());
			}
		};
		timer.scheduleRepeating(60 * 1000);
	}

	/**
	 * Stops the timer timer responsible of opening the tree
	 */
	private void stopTimer() {
		if (timer != null)
			timer.cancel();
	}

	@Override
	public void onUserLogin(String username) {
		Record rec = onlineUsers.find(new AdvancedCriteria(USERNAME, OperatorId.EQUALS, username));
		if (rec == null) {
			List<ListGridRecord> recds = new ArrayList<>();

			Record[] records = onlineUsers.getDataAsRecordList().toArray();
			if (records != null && records.length > 0) {
				for (Record r : records)
					recds.add(new ListGridRecord(r));
			}

			ListGridRecord r = new ListGridRecord();
			r.setAttribute(USERNAME, username);
			recds.add(r);

			onlineUsers.setRecords(recds.toArray(new ListGridRecord[0]));
		}
	}

	@Override
	public void onUserLogout(String username) {
		Record rec = onlineUsers.find(new AdvancedCriteria(USERNAME, OperatorId.EQUALS, username));
		if (rec != null) {
			List<ListGridRecord> recds = new ArrayList<>();

			Record[] records = onlineUsers.getDataAsRecordList().toArray();
			if (records != null && records.length > 0) {
				for (Record r : records) {
					if (r.getAttributeAsString(USERNAME).equals(username))
						continue;
					recds.add(new ListGridRecord(r));
				}
			}

			onlineUsers.setRecords(recds.toArray(new ListGridRecord[0]));
		}
	}

	@Override
	public void onUserChanged(GUIUser user) {
		// Nothing to do
	}

	@Override
	public void destroy() {
		UserController.get().removeObserver(this);
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

	/**
	 * Prepares the context menu.
	 */
	private Menu prepareContextMenu() {
		final ListGridRecord[] selection = onlineUsers.getSelectedRecords();

		Menu contextMenu = new Menu();
		MenuItem inviteToChat = new MenuItem();
		inviteToChat.setTitle(I18N.message("invitetochat"));
		inviteToChat.addClickHandler(
				event -> LD.askForValue(I18N.message("invitetochat"), I18N.message("message"), null, answer -> {
					List<String> users = new ArrayList<>();
					for (int i = 0; i < selection.length; i++)
						users.add(selection[i].getAttributeAsString(USERNAME));

					ChatService.Instance.get().invite(users, answer, new DefaultAsyncCallback<>() {

						@Override
						public void handleSuccess(Void arg) {
							GuiLog.info(I18N.message("invitationsent"));
						}
					});
				}));

		contextMenu.setItems(inviteToChat);
		return contextMenu;
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