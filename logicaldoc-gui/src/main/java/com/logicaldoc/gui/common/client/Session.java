package com.logicaldoc.gui.common.client;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
import com.logicaldoc.gui.common.client.observer.UserController;
import com.logicaldoc.gui.common.client.services.InfoService;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.SessionTimeout;
import com.smartgwt.client.util.SC;

/**
 * Represents a client work session
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Session implements DocumentObserver {
	private static Session instance;

	private GUIInfo info;

	private GUISession session;

	private Long hiliteDocId;

	private Set<SessionObserver> sessionObservers = new HashSet<SessionObserver>();

	private Timer timer;

	private boolean showThumbnail = true;

	private int missedPingCount = 0;

	public static Session get() {
		if (instance == null)
			instance = new Session();
		return instance;
	}

	public boolean isDemo() {
		return "demo".equals(info.getRunLevel());
	}

	public boolean isDevel() {
		return "devel".equals(info.getRunLevel());
	}

	public String getSid() {
		if (session != null)
			return session.getSid();
		else
			return null;
	}

	public String getIncomingMessage() {
		if (session != null)
			return session.getWelcomeMessage();
		else
			return null;
	}

	public boolean isFolderPagination() {
		return getConfigAsBoolean("gui.folder.pagination");
	}

	public void close() {
		session = null;
		sessionObservers.clear();
		if (timer != null)
			timer.cancel();
		Util.uninstallCloseWindowAlert();
	}

	public GUIUser getUser() {
		return session.getUser();
	}

	public void setUser(GUIUser user) {
		session.setUser(user);
		I18N.init(user);
	}

	public void init(final GUISession session) {
		InfoService.Instance.get().getSessionInfo(new AsyncCallback<GUIParameter[]>() {

			@Override
			public void onFailure(Throwable caught) {
				SC.warn(caught.getMessage());
			}

			@Override
			public void onSuccess(GUIParameter[] parameters) {
				Session.get().session = session;
				Session.get().info = session.getInfo();

				I18N.init(session.getUser());

				Menu.init(session.getUser());

				if (session.isLoggedIn()) {
					for (SessionObserver listener : sessionObservers) {
						listener.onUserLoggedIn(session.getUser());
					}
					boolean validSession = true;
					GUIUser user = getUser();
					for (GUIParameter parameter : parameters) {
						if (parameter.getName().equals("messages"))
							user.setMessages(Integer.parseInt(parameter.getValue()));
						else if (parameter.getName().equals("workflows"))
							user.setAssignedTasks(Integer.parseInt(parameter.getValue()));
						else if (parameter.getName().equals("events"))
							user.setUpcomingEvents(Integer.parseInt(parameter.getValue()));
						else if (parameter.getName().equals("valid"))
							validSession = Boolean.parseBoolean(parameter.getValue());
					}

					UserController.get().changed(user);

					if (!validSession)
						onInvalidSession();

					Util.installCloseWindowAlert();

					if (session.getInfo().getSessionHeartbeat() > 0) {
						/*
						 * Create the timer that synchronizes the session info
						 */
						timer = new Timer() {
							public void run() {
								InfoService.Instance.get().ping(new AsyncCallback<Boolean>() {
									@Override
									public void onFailure(Throwable caught) {
										missedPingCount++;
										if (missedPingCount >= 3)
											onInvalidSession();
									}

									@Override
									public void onSuccess(Boolean active) {
										missedPingCount = 0;
										if (!active)
											onInvalidSession();
									}
								});
							}
						};

						timer.scheduleRepeating(session.getInfo().getSessionHeartbeat() * 1000);
					}
				}
			}
		});
	}

	public void onInvalidSession() {
		timer.cancel();
		SessionTimeout.get().show();
		Util.uninstallCloseWindowAlert();
	}

	public void addObserver(SessionObserver observer) {
		sessionObservers.add(observer);
	}

	public void removeObserver(SessionObserver observer) {
		sessionObservers.remove(observer);
	}

	public GUIInfo getInfo() {
		return info;
	}

	public void setInfo(GUIInfo info) {
		this.info = info;
	}

	public GUISession getSession() {
		return session;
	}

	public void setSession(GUISession session) {
		this.session = session;
	}

	public String getTenantName() {
		return info.getTenant().getName();
	}

	public long getTenantId() {
		return info.getTenant().getId();
	}

	public boolean isDefaultTenant() {
		return info.getTenant().getId() == Constants.TENANT_DEFAULTID;
	}

	/**
	 * Checks if the current user belongs to the <b>admin</b> group
	 * 
	 * @return true if the current user belongs to the <b>admin</b> group
	 */
	public boolean isAdmin() {
		return getUser() != null && getUser().isMemberOf(Constants.GROUP_ADMIN);
	}

	public String getConfig(String name) {
		if (info != null)
			return info.getConfig(name);
		else
			return null;
	}

	public String getTenantConfig(String name) {
		String val = getConfig(getTenantName() + "." + name);
		if (val == null)
			val = getConfig(name);
		return val;
	}

	public int getConfigAsInt(String name) {
		return Integer.parseInt(getConfig(name));
	}

	public long getConfigAsLong(String name) {
		return Long.parseLong(getConfig(name));
	}

	public boolean getConfigAsBoolean(String name) {
		return Boolean.parseBoolean(getConfig(name));
	}

	public boolean getTenantConfigAsBoolean(String name) {
		return Boolean.parseBoolean(getTenantConfig(name));
	}

	public void setConfig(String name, String value) {
		info.setConfig(name, value);
	}

	public void updateConfig(List<GUIParameter> params) {
		for (GUIParameter param : params) {
			setConfig(param.getName(), param.getValue());
		}
	}

	public boolean isServerPushEnabled() {
		return "true".equals(Session.get().getConfig("gui.serverpush"));
	}

	public boolean isShowThumbnail() {
		return showThumbnail;
	}

	public void setShowThumbnail(boolean showThumbnail) {
		this.showThumbnail = showThumbnail;
	}

	public void logout() {
		SecurityService.Instance.get().logout(new AsyncCallback<Void>() {
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
				SC.warn(caught.getMessage());
			}

			@Override
			public void onSuccess(Void result) {
				CookiesManager.removeSid();

				try {
					String tenant = Session.get().getUser().getTenant().getName();
					Session.get().close();
					Util.redirectToLoginUrl(tenant);
				} catch (Throwable t) {
					// Nothing to do
				}
			}
		});
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentModified(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentStored(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentCheckedIn(GUIDocument document) {
		getUser().setCheckedOutDocs(Session.get().getUser().getCheckedOutDocs() - 1);
	}

	@Override
	public void onDocumentCheckedOut(GUIDocument document) {
		getUser().setCheckedOutDocs(Session.get().getUser().getCheckedOutDocs() + 1);
	}

	@Override
	public void onDocumentsDeleted(GUIDocument[] documents) {
		// Nothing to do
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentLocked(GUIDocument document) {
		getUser().setLockedDocs(Session.get().getUser().getLockedDocs() + 1);
	}

	@Override
	public void onDocumentUnlocked(GUIDocument document) {
		getUser().setLockedDocs(Session.get().getUser().getLockedDocs() - 1);
	}

	@Override
	public void onDocumentBeginEditing(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		// Nothing to do
	}

	public Long getHiliteDocId() {
		return hiliteDocId;
	}

	public void setHiliteDocId(Long hiliteDocId) {
		this.hiliteDocId = hiliteDocId;
	}
}