package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel collects all user details
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 * 
 */
public class UserDetailsPanel extends VLayout {

	private GUIUser user;

	private Layout propertiesTabPanel;

	private Layout quotaTabPanel;

	private Layout historyTabPanel;

	private Layout firewallTabPanel;

	private UserPropertiesPanel propertiesPanel;

	private HLayout savePanel;

	private TabSet tabSet = new TabSet();

	private UsersPanel usersPanel;

	private UserQuotaPanel quotaPanel;

	private UserHistoryPanel historyPanel;

	private FirewallPanel firewallPanel;

	private Button saveButton;

	public UserDetailsPanel(UsersPanel usersPanel) {
		super();
		this.usersPanel = usersPanel;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		savePanel = new HLayout();
		savePanel.setHeight(20);
		savePanel.setVisible(false);
		savePanel.setStyleName("warn");
		savePanel.setWidth100();
		saveButton = new Button(I18N.message("save"));
		saveButton.setAutoFit(true);
		saveButton.setMargin(2);
		saveButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("70%");
		spacer.setOverflow(Overflow.HIDDEN);

		Img closeImage = ItemFactory.newImgIcon("delete.png");
		closeImage.setHeight("16px");
		closeImage.addClickHandler(new ClickHandler() {
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
				savePanel.setVisible(false);
			}
		});
		closeImage.setCursor(Cursor.HAND);
		closeImage.setTooltip(I18N.message("close"));
		closeImage.setLayoutAlign(Alignment.RIGHT);
		closeImage.setLayoutAlign(VerticalAlignment.CENTER);

		savePanel.addMember(saveButton);
		savePanel.addMember(spacer);
		savePanel.addMember(closeImage);
		addMember(savePanel);

		tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		Tab quotaTab = new Tab(I18N.message("quota"));
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

		Tab firewallTab = new Tab(I18N.message("firewall"));
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
		if (savePanel != null)
			savePanel.setVisible(false);

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
		if (user.getUserName().equalsIgnoreCase("admin")
				&& !Session.get().getUser().getUserName().equalsIgnoreCase("admin"))
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
		if (user.getUserName().equalsIgnoreCase("admin")
				&& !Session.get().getUser().getUserName().equalsIgnoreCase("admin"))
			savePanel.setVisible(false);
		else
			savePanel.setVisible(true);
	}

	private boolean validate() {
		boolean stdValid = propertiesPanel.validate();
		boolean quotaValid = true;
		if (quotaPanel != null)
			quotaValid = quotaPanel.validate();
		boolean firewallValid = true;
		if (firewallPanel != null)
			firewallValid = firewallPanel.validate();

		if (!stdValid)
			tabSet.selectTab(0);
		else if (quotaPanel != null && !quotaValid)
			tabSet.selectTab(1);
		else if (firewallPanel != null && !firewallValid)
			tabSet.selectTab(2);

		return stdValid && quotaValid && firewallValid;
	}

	public void onSave() {
		if (validate()) {
			final boolean createNew = user.getId() == 0;
			saveButton.setDisabled(true);

			ContactingServer.get().show();
			SecurityService.Instance.get().saveUser(user, Session.get().getInfo(), new AsyncCallback<GUIUser>() {
				@Override
				public void onFailure(Throwable caught) {
					ContactingServer.get().hide();
					Log.serverError(caught);
					saveButton.setDisabled(false);
				}

				@Override
				public void onSuccess(GUIUser user) {
					ContactingServer.get().hide();
					saveButton.setDisabled(false);
					if (createNew && user.getWelcomeScreen() == -99) {
						Log.warn(I18N.message("usernamealreadyinuse"), I18N.message("usernamealreadyinuse"));
						SC.warn(I18N.message("usernamealreadyinuse"));
						return;
					}

					savePanel.setVisible(false);
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