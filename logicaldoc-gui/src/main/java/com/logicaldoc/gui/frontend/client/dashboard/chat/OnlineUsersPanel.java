package com.logicaldoc.gui.frontend.client.dashboard.chat;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.data.OnlineUsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.UserController;
import com.logicaldoc.gui.common.client.observer.UserObserver;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.ChatService;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.events.VisibilityChangedEvent;
import com.smartgwt.client.widgets.events.VisibilityChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * Displays the online users
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public class OnlineUsersPanel extends VLayout implements UserObserver {
	private RefreshableListGrid onlineUsers = null;

	private Timer timer;

	public OnlineUsersPanel() {
		setWidth100();
		setHeight100();
		UserController.get().addObserver(this);
	}

	@Override
	public void onDraw() {
		ListGridField username = new ListGridField("username", I18N.message("onlineusers"), 150);
		username.setCanFilter(true);
		username.setWidth("100%");

		UserListGridField avatar = new UserListGridField();

		onlineUsers = new RefreshableListGrid(new OnlineUsersDS());
		onlineUsers.setEmptyMessage(I18N.message("nousers"));
		onlineUsers.setAutoFetchData(true);
		onlineUsers.setFields(avatar, username);
		onlineUsers.setSortField("username");

		onlineUsers.addVisibilityChangedHandler(new VisibilityChangedHandler() {

			@Override
			public void onVisibilityChanged(VisibilityChangedEvent event) {
				if (timer != null) {
					if (!event.getIsVisible())
						stopTimer();
					else if (!timer.isRunning())
						startTimer();
				}
			}
		});

		onlineUsers.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				prepareContextMenu().showContextMenu();
				event.cancel();
			}
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
		Record rec = onlineUsers.find(new AdvancedCriteria("username", OperatorId.EQUALS, username));
		if (rec == null) {
			List<ListGridRecord> recds = new ArrayList<>();

			Record[] records = onlineUsers.getDataAsRecordList().toArray();
			if (records != null && records.length > 0) {
				for (Record r : records)
					recds.add(new ListGridRecord(r));
			}

			ListGridRecord r = new ListGridRecord();
			r.setAttribute("username", username);
			recds.add(r);

			onlineUsers.setRecords(recds.toArray(new ListGridRecord[0]));
		}
	}

	@Override
	public void onUserLogout(String username) {
		Record rec = onlineUsers.find(new AdvancedCriteria("username", OperatorId.EQUALS, username));
		if (rec != null) {
			List<ListGridRecord> recds = new ArrayList<>();

			Record[] records = onlineUsers.getDataAsRecordList().toArray();
			if (records != null && records.length > 0) {
				for (Record r : records) {
					if (r.getAttributeAsString("username").equals(username))
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
		inviteToChat.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.askForValue(I18N.message("invitetochat"), I18N.message("message"), null, new ValueCallback() {

					@Override
					public void execute(String value) {
						final String[] users = new String[selection.length];
						for (int i = 0; i < selection.length; i++)
							users[i] = selection[i].getAttributeAsString("username");

						ChatService.Instance.get().invite(users, value, new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {
								GuiLog.info(I18N.message("invitationsent"));
							}
						});
					}
				});
			}
		});

		contextMenu.setItems(inviteToChat);
		return contextMenu;
	}
}