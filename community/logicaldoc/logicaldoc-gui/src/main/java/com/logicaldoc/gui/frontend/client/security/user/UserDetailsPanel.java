package com.logicaldoc.gui.frontend.client.security.user;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.security.FirewallPanel;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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
public class UserDetailsPanel extends VLayout {

	private static final String TABID_USERINTERFACE = "userinterface";

	private static final String TABID_FIREWALL = "firewall";

	private static final String TABID_QUOTA = "quota";

	private static final String TABID_PROPERTIES = "properties";

	private GUIUser user;

	private Layout propertiesTabPanel;

	private Layout quotaTabPanel;

	private Layout historyTabPanel;

	private Layout firewallTabPanel;

	private Layout guiTabPanel;

	private UserPropertiesPanel propertiesPanel;

	private EditingTabSet tabSet;

	private UsersPanel usersPanel;

	private UserQuotaPanel quotaPanel;

	private UserHistoryPanel historyPanel;

	private FirewallPanel firewallPanel;

	private UserInterfacePanel guiPanel;

	public UserDetailsPanel(UsersPanel usersPanel) {
		super();
		this.usersPanel = usersPanel;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		}, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (user.getId() != 0) {
					SecurityService.Instance.get().getUser(user.getId(), new AsyncCallback<GUIUser>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
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
			}
		});

		Tab propertiesTab = new Tab(I18N.message(TABID_PROPERTIES));
		propertiesTab.setID(TABID_PROPERTIES);
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

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
		 * Prepare the standard properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (propertiesTabPanel.contains(propertiesPanel))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};

		// Admin only can change the 'admin' user
		if (user.getUsername().equalsIgnoreCase("admin")
				&& !Session.get().getUser().getUsername().equalsIgnoreCase("admin"))
			changeHandler = null;

		propertiesPanel = new UserPropertiesPanel(user, changeHandler);
		propertiesTabPanel.addMember(propertiesPanel);

		/*
		 * Prepare the quota tab
		 */
		if (Feature.enabled(Feature.QUOTAS)) {
			if (quotaPanel != null) {
				quotaPanel.destroy();
				if (quotaTabPanel.contains(quotaPanel))
					quotaTabPanel.removeMember(quotaPanel);
			}
			quotaPanel = new UserQuotaPanel(user, changeHandler);
			quotaTabPanel.addMember(quotaPanel);
		}

		/*
		 * Prepare the firewall tab
		 */
		if (Feature.enabled(Feature.FIREWALL)) {
			if (firewallPanel != null) {
				firewallPanel.destroy();
				if (firewallTabPanel.contains(firewallPanel))
					firewallTabPanel.removeMember(firewallPanel);
			}
			firewallPanel = new FirewallPanel(user, changeHandler);
			firewallTabPanel.addMember(firewallPanel);
		}

		/*
		 * Prepare the user interface tab
		 */
		if (guiPanel != null) {
			guiPanel.destroy();
			if (guiTabPanel.contains(guiPanel))
				guiTabPanel.removeMember(guiPanel);
		}
		guiPanel = new UserInterfacePanel(user, changeHandler);
		guiTabPanel.addMember(guiPanel);

		/*
		 * Prepare the history tab
		 */
		if (historyPanel != null) {
			historyPanel.destroy();
			if (historyTabPanel.contains(historyPanel))
				historyTabPanel.removeMember(historyPanel);
		}
		historyPanel = new UserHistoryPanel(user.getId());
		historyTabPanel.addMember(historyPanel);

		tabSet.selectTab(0);
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
		if (user.getUsername().equalsIgnoreCase("admin")
				&& !Session.get().getUser().getUsername().equalsIgnoreCase("admin"))
			tabSet.hideSave();
		else
			tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = propertiesPanel.validate();
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
		else if (quotaPanel != null && !quotaValid)
			tabSet.selectTab(TABID_QUOTA);
		else if (firewallPanel != null && !firewallValid)
			tabSet.selectTab(TABID_FIREWALL);
		else if (guiPanel != null && !guiValid)
			tabSet.selectTab(TABID_USERINTERFACE);

		return stdValid && quotaValid && firewallValid && guiValid;
	}

	public void onSave() {
		if (validate()) {
			final boolean createNew = user.getId() == 0;

			ContactingServer.get().show();
			SecurityService.Instance.get().saveUser(user, Session.get().getInfo(), new AsyncCallback<GUIUser>() {
				@Override
				public void onFailure(Throwable caught) {
					ContactingServer.get().hide();
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIUser user) {
					ContactingServer.get().hide();
					tabSet.hideSave();
					if (createNew && user.getWelcomeScreen() == -99) {
						Log.warn(I18N.message("usernamealreadyinuse"), I18N.message("usernamealreadyinuse"));
						SC.warn(I18N.message("usernamealreadyinuse"));
						return;
					}

					if (createNew && user.isNotifyCredentials())
						Log.info(I18N.message("emailnotifyaccountsent"), I18N.message("emailnotifyaccountsent"));
					if (user != null) {
						usersPanel.updateRecord(user);
						usersPanel.showUserDetails(user);
					}
				}
			});
		}
	}
}