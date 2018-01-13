package com.logicaldoc.gui.frontend.client.impex.accounts;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmailAccount;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.EmailAccountService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.VerticalAlignment;
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
 * This panel collects all account details
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class AccountDetailsPanel extends VLayout {
	private GUIEmailAccount account;

	private Layout standardTabPanel;

	private Layout advancedTabPanel;

	private Layout filtersTabPanel;

	private AccountStandardProperties standardPanel;

	private AccountAdvancedProperties advancedPanel;

	private AccountFiltersPanel filtersPanel;

	private HLayout savePanel;

	private TabSet tabSet = new TabSet();

	private EmailAccountsPanel accountsPanel;

	public AccountDetailsPanel(EmailAccountsPanel accountsPanel) {
		super();

		this.accountsPanel = accountsPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		savePanel = new HLayout();
		savePanel.setHeight(20);
		savePanel.setVisible(false);
		savePanel.setStyleName("warn");
		savePanel.setWidth100();
		Button saveButton = new Button(I18N.message("save"));
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

		addMember(tabSet);
	}

	private void refresh() {
		if (savePanel != null)
			savePanel.setVisible(false);

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
		standardPanel = new AccountStandardProperties(account, changeHandler);
		standardTabPanel.addMember(standardPanel);

		/*
		 * Prepare the extended properties tab
		 */
		if (advancedPanel != null) {
			advancedPanel.destroy();
			if (advancedTabPanel.contains(advancedPanel))
				advancedTabPanel.removeMember(advancedPanel);
		}
		advancedPanel = new AccountAdvancedProperties(account, changeHandler);
		advancedTabPanel.addMember(advancedPanel);

		/*
		 * Prepare the filters tab
		 */
		if (filtersPanel != null) {
			filtersPanel.destroy();
			if (filtersTabPanel.contains(filtersPanel))
				filtersTabPanel.removeMember(filtersPanel);
		}
		filtersPanel = new AccountFiltersPanel(account, changeHandler);
		filtersTabPanel.addMember(filtersPanel);
	}

	public GUIEmailAccount getAccount() {
		return account;
	}

	public void setAccount(GUIEmailAccount account) {
		this.account = account;
		refresh();
	}

	public void onModified() {
		savePanel.setVisible(true);
	}

	private boolean validate() {
		boolean stdValid = standardPanel.validate();
		boolean extValid = advancedPanel.validate();
		boolean filtValid = filtersPanel.validate();

		if (!stdValid)
			tabSet.selectTab(0);
		else if (!extValid)
			tabSet.selectTab(1);
		else if (!filtValid)
			tabSet.selectTab(2);
		return stdValid && extValid && filtValid;
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
					savePanel.setVisible(false);
					if (account != null) {
						accountsPanel.updateRecord(account);
						accountsPanel.showDetails(account);
					}
				}
			});
		}
	}
}