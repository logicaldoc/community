package com.logicaldoc.gui.frontend.client.impex.email;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.EmailAccountService;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects all account details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class EmailAccountDetailsPanel extends VLayout {
	private GUIEmailAccount account;

	private Layout standardTabPanel;

	private Layout advancedTabPanel;

	private Layout automationTabPanel;

	private Layout filtersTabPanel;

	private EmailAccountStandardProperties standardPanel;

	private EmailAccountAdvancedProperties advancedPanel;

	private EmailAccountAutomationPanel automationPanel;

	private EmailAccountFiltersPanel filtersPanel;

	private EditingTabSet tabSet;

	private EmailAccountsPanel accountsPanel;

	public EmailAccountDetailsPanel(EmailAccountsPanel accountsPanel) {
		super();

		this.accountsPanel = accountsPanel;
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
				if (account.getId() != 0) {
					EmailAccountService.Instance.get().get(account.getId(), new AsyncCallback<GUIEmailAccount>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIEmailAccount account) {
							setAccount(account);
						}
					});
				} else {
					setAccount(new GUIEmailAccount());
				}
				tabSet.hideSave();
			}
		});

		Tab propertiesTab = new Tab(I18N.message("properties"));
		standardTabPanel = new HLayout();
		standardTabPanel.setWidth100();
		standardTabPanel.setHeight100();
		propertiesTab.setPane(standardTabPanel);
		tabSet.addTab(propertiesTab);

		Tab extendedPropertiesTab = new Tab(I18N.message("propertiesext"));
		advancedTabPanel = new HLayout();
		advancedTabPanel.setWidth100();
		advancedTabPanel.setHeight100();
		extendedPropertiesTab.setPane(advancedTabPanel);
		tabSet.addTab(extendedPropertiesTab);

		Tab filtersTab = new Tab(I18N.message("filters"));
		filtersTabPanel = new HLayout();
		filtersTabPanel.setWidth100();
		filtersTabPanel.setHeight100();
		filtersTab.setPane(filtersTabPanel);
		tabSet.addTab(filtersTab);

		Tab automationTab = new Tab(I18N.message("automation"));
		automationTabPanel = new HLayout();
		automationTabPanel.setWidth100();
		automationTabPanel.setHeight100();
		automationTab.setPane(automationTabPanel);
		tabSet.addTab(automationTab);

		addMember(tabSet);
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the standard properties tab
		 */
		if (standardPanel != null) {
			standardPanel.destroy();
			if (standardTabPanel.contains(standardPanel))
				standardTabPanel.removeMember(standardPanel);
		}

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};
		standardPanel = new EmailAccountStandardProperties(account, changeHandler);
		standardTabPanel.addMember(standardPanel);

		/*
		 * Prepare the extended properties tab
		 */
		if (advancedPanel != null) {
			advancedPanel.destroy();
			if (advancedTabPanel.contains(advancedPanel))
				advancedTabPanel.removeMember(advancedPanel);
		}
		advancedPanel = new EmailAccountAdvancedProperties(account, changeHandler);
		advancedTabPanel.addMember(advancedPanel);

		/*
		 * Prepare the filters tab
		 */
		if (filtersPanel != null) {
			filtersPanel.destroy();
			if (filtersTabPanel.contains(filtersPanel))
				filtersTabPanel.removeMember(filtersPanel);
		}
		filtersPanel = new EmailAccountFiltersPanel(account, changeHandler);
		filtersTabPanel.addMember(filtersPanel);

		/*
		 * Prepare the automation tab
		 */
		if (automationPanel != null) {
			automationPanel.destroy();
			if (automationTabPanel.contains(automationPanel))
				automationTabPanel.removeMember(automationPanel);
		}
		automationPanel = new EmailAccountAutomationPanel(account, changeHandler);
		automationTabPanel.addMember(automationPanel);
	}

	public GUIEmailAccount getAccount() {
		return account;
	}

	public void setAccount(GUIEmailAccount account) {
		this.account = account;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = standardPanel.validate();
		boolean extValid = advancedPanel.validate();
		boolean filtValid = filtersPanel.validate();
		boolean automationValid = automationPanel.validate();

		if (!stdValid)
			tabSet.selectTab(0);
		else if (!extValid)
			tabSet.selectTab(1);
		else if (!filtValid)
			tabSet.selectTab(2);
		else if (!automationValid)
			tabSet.selectTab(3);
		return stdValid && extValid && filtValid && automationValid;
	}

	public void onSave() {
		if (validate()) {
			EmailAccountService.Instance.get().save(account, new AsyncCallback<GUIEmailAccount>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(GUIEmailAccount account) {
					tabSet.hideSave();
					if (account != null) {
						accountsPanel.updateRecord(account);
						accountsPanel.showDetails(account);
					}
				}
			});
		}
	}
}