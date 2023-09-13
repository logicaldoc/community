package com.logicaldoc.gui.frontend.client.security.user;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.UserController;
import com.logicaldoc.gui.common.client.controllers.UserObserver;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.security.FirewallPanel;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects all user details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 * 
 */
public class UserDetailsPanel extends VLayout implements UserObserver {

	private static final String USERNAMEALREADYINUSE = "usernamealreadyinuse";

	private static final String ADMIN = "admin";

	private static final String TABID_USERINTERFACE = "userinterface";

	private static final String TABID_SECURITY = "security";

	private static final String TABID_WORKINGTIME = "workingtime";

	private static final String TABID_FIREWALL = "firewall";

	private static final String TABID_QUOTA = "quota";

	private static final String TABID_PROPERTIES = "properties";

	private GUIUser user;

	private Layout propertiesTabPanel;

	private Layout quotaTabPanel;

	private Layout historyTabPanel;

	private Layout firewallTabPanel;

	private Layout securityTabPanel;

	private Layout workingTimeTabPanel;

	private Layout guiTabPanel;

	private UserPropertiesPanel propertiesPanel;

	private UserSecurityPanel securityPanel;

	private WorkingTimePanel workingTimePanel;

	private EditingTabSet tabSet;

	private UsersPanel usersPanel;

	private UserQuotaPanel quotaPanel;

	private UserHistoryPanel historyPanel;

	private FirewallPanel firewallPanel;

	private UserInterfacePanel guiPanel;

	public UserDetailsPanel(UsersPanel usersPanel) {
		super();
		UserController.get().addObserver(this);
		this.usersPanel = usersPanel;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), canceEvent -> {
			if (user.getId() != 0) {
				SecurityService.Instance.get().getUser(user.getId(), new AsyncCallback<GUIUser>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIUser user) {
						setUser(user);
					}
				});
			} else {
				setUser(new GUIUser());
			}
			tabSet.hideSave();
		});

		Tab propertiesTab = new Tab(I18N.message(TABID_PROPERTIES));
		propertiesTab.setID(TABID_PROPERTIES);
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		Tab securityTab = new Tab(I18N.message(TABID_SECURITY));
		securityTab.setID(TABID_SECURITY);
		securityTabPanel = new HLayout();
		securityTabPanel.setWidth100();
		securityTabPanel.setHeight100();
		securityTab.setPane(securityTabPanel);
		tabSet.addTab(securityTab);

		Tab workingTimeTab = new Tab(I18N.message(TABID_WORKINGTIME));
		workingTimeTab.setID(TABID_WORKINGTIME);
		workingTimeTabPanel = new HLayout();
		workingTimeTabPanel.setWidth100();
		workingTimeTabPanel.setHeight100();
		workingTimeTab.setPane(workingTimeTabPanel);
		tabSet.addTab(workingTimeTab);

		Tab quotaTab = new Tab(I18N.message(TABID_QUOTA));
		quotaTab.setID(TABID_QUOTA);
		if (Feature.visible(Feature.QUOTAS)) {
			if (Feature.enabled(Feature.QUOTAS)) {
				quotaTabPanel = new HLayout();
				quotaTabPanel.setWidth100();
				quotaTabPanel.setHeight100();
			} else {
				quotaTabPanel = new FeatureDisabled();
			}
			quotaTab.setPane(quotaTabPanel);
			tabSet.addTab(quotaTab);
		}

		Tab firewallTab = new Tab(I18N.message(TABID_FIREWALL));
		firewallTab.setID(TABID_FIREWALL);
		if (Feature.visible(Feature.FIREWALL)) {
			if (Feature.enabled(Feature.FIREWALL)) {
				firewallTabPanel = new HLayout();
				firewallTabPanel.setWidth100();
				firewallTabPanel.setHeight100();
			} else {
				firewallTabPanel = new FeatureDisabled();
			}
			firewallTab.setPane(firewallTabPanel);
			tabSet.addTab(firewallTab);
		}

		Tab guiTab = new Tab(I18N.message(TABID_USERINTERFACE));
		guiTab.setID(TABID_USERINTERFACE);
		guiTabPanel = new HLayout();
		guiTabPanel.setWidth100();
		guiTabPanel.setHeight100();
		guiTab.setPane(guiTabPanel);
		tabSet.addTab(guiTab);

		Tab historyTab = new Tab(I18N.message("history"));
		historyTabPanel = new HLayout();
		historyTabPanel.setWidth100();
		historyTabPanel.setHeight100();
		historyTab.setPane(historyTabPanel);
		tabSet.addTab(historyTab);

		addMember(tabSet);
	}

	public UsersPanel getUsersPanel() {
		return usersPanel;
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the change handler
		 */
		ChangedHandler changeHandler = prepareChangeHandler();

		/*
		 * Prepare the standard properties tab
		 */
		addPropertiesTab(changeHandler);

		/*
		 * Prepare the security tab
		 */
		addSecurityTab(changeHandler);

		/*
		 * Prepare the working time tab
		 */
		addWorkingTimeTab(changeHandler);

		/*
		 * Prepare the quota tab
		 */
		addQuotaTab(changeHandler);

		/*
		 * Prepare the firewall tab
		 */
		addFirewallTab(changeHandler);

		/*
		 * Prepare the user interface tab
		 */
		addGuiTab(changeHandler);

		/*
		 * Prepare the history tab
		 */
		addHistoryTab();
	}

	private void addHistoryTab() {
		if (historyPanel != null) {
			historyPanel.destroy();
			if (Boolean.TRUE.equals(historyTabPanel.contains(historyPanel)))
				historyTabPanel.removeMember(historyPanel);
		}
		historyPanel = new UserHistoryPanel(user.getId());
		historyTabPanel.addMember(historyPanel);
	}

	private void addGuiTab(ChangedHandler changeHandler) {
		if (guiPanel != null) {
			guiPanel.destroy();
			if (Boolean.TRUE.equals(guiTabPanel.contains(guiPanel)))
				guiTabPanel.removeMember(guiPanel);
		}
		guiPanel = new UserInterfacePanel(user, changeHandler);
		guiTabPanel.addMember(guiPanel);
	}

	private void addFirewallTab(ChangedHandler changeHandler) {
		if (Feature.enabled(Feature.FIREWALL)) {
			if (firewallPanel != null) {
				firewallPanel.destroy();
				if (Boolean.TRUE.equals(firewallTabPanel.contains(firewallPanel)))
					firewallTabPanel.removeMember(firewallPanel);
			}
			firewallPanel = new FirewallPanel(user, changeHandler);
			firewallTabPanel.addMember(firewallPanel);
		}
	}

	private void addQuotaTab(ChangedHandler changeHandler) {
		if (Feature.enabled(Feature.QUOTAS)) {
			if (quotaPanel != null) {
				quotaPanel.destroy();
				if (Boolean.TRUE.equals(quotaTabPanel.contains(quotaPanel)))
					quotaTabPanel.removeMember(quotaPanel);
			}
			quotaPanel = new UserQuotaPanel(user, changeHandler);
			quotaTabPanel.addMember(quotaPanel);
		}
	}

	private void addWorkingTimeTab(ChangedHandler changeHandler) {
		if (workingTimePanel != null) {
			workingTimePanel.destroy();
			if (Boolean.TRUE.equals(workingTimeTabPanel.contains(workingTimePanel)))
				workingTimeTabPanel.removeMember(workingTimePanel);
		}
		workingTimePanel = new WorkingTimePanel(user, changeHandler);
		workingTimeTabPanel.addMember(workingTimePanel);
		workingTimePanel.setDisabled(ADMIN.equals(user.getUsername()));
	}

	private void addSecurityTab(ChangedHandler changeHandler) {
		if (securityPanel != null) {
			securityPanel.destroy();
			if (Boolean.TRUE.equals(securityTabPanel.contains(securityPanel)))
				securityTabPanel.removeMember(securityPanel);
		}
		securityPanel = new UserSecurityPanel(user, changeHandler);
		securityTabPanel.addMember(securityPanel);
	}

	private void addPropertiesTab(ChangedHandler changeHandler) {
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}
		propertiesPanel = new UserPropertiesPanel(user, changeHandler, usersPanel);
		propertiesTabPanel.addMember(propertiesPanel);
	}

	private ChangedHandler prepareChangeHandler() {
		ChangedHandler changeHandler = (ChangedEvent event) -> onModified();

		// Admin only can change the 'admin' user
		if (user.getUsername().equalsIgnoreCase(ADMIN)
				&& !Session.get().getUser().getUsername().equalsIgnoreCase(ADMIN))
			changeHandler = null;
		return changeHandler;
	}

	public GUIUser getUser() {
		return user;
	}

	public void setUser(GUIUser user) {
		this.user = user;
		refresh();
	}

	public void onModified() {
		// Admin only can change the admin user
		if (user.getUsername().equalsIgnoreCase(ADMIN)
				&& !Session.get().getUser().getUsername().equalsIgnoreCase(ADMIN))
			tabSet.hideSave();
		else
			tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = propertiesPanel.validate();
		boolean securityValid = securityPanel.validate();
		boolean workingTimeValid = workingTimePanel.validate();

		boolean quotaValid = true;
		if (quotaPanel != null)
			quotaValid = quotaPanel.validate();
		boolean firewallValid = true;
		if (firewallPanel != null)
			firewallValid = firewallPanel.validate();
		boolean guiValid = true;
		if (guiPanel != null)
			guiValid = guiPanel.validate();

		if (!stdValid)
			tabSet.selectTab(TABID_PROPERTIES);
		else if (!securityValid)
			tabSet.selectTab(TABID_SECURITY);
		else if (!workingTimeValid)
			tabSet.selectTab(TABID_WORKINGTIME);
		else if (quotaPanel != null && !quotaValid)
			tabSet.selectTab(TABID_QUOTA);
		else if (firewallPanel != null && !firewallValid)
			tabSet.selectTab(TABID_FIREWALL);
		else if (guiPanel != null && !guiValid)
			tabSet.selectTab(TABID_USERINTERFACE);

		return stdValid && securityValid && workingTimeValid && quotaValid && firewallValid && guiValid;
	}

	public void onSave() {
		if (validate()) {
			final boolean createNew = user.getId() == 0;
			final boolean notifyCredentials = user.isNotifyCredentials();

			LD.contactingServer();
			SecurityService.Instance.get().saveUser(user, Session.get().getInfo(), new AsyncCallback<GUIUser>() {
				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIUser user) {
					LD.clearPrompt();
					tabSet.hideSave();
					if (createNew && user.getWelcomeScreen() == -99) {
						GuiLog.warn(I18N.message(USERNAMEALREADYINUSE), I18N.message(USERNAMEALREADYINUSE));
						SC.warn(I18N.message(USERNAMEALREADYINUSE));
						return;
					}

					if (createNew && notifyCredentials)
						GuiLog.info(I18N.message("emailnotifyaccountsent"), I18N.message("emailnotifyaccountsent"));
					if (user != null) {
						usersPanel.updateRecord(user);
						usersPanel.showUserDetails(user);
					}
				}
			});
		}
	}

	@Override
	public void onUserChanged(GUIUser user) {
		if (user.getId() == this.user.getId()) {
			this.user = user;
			refresh();
		}
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
}