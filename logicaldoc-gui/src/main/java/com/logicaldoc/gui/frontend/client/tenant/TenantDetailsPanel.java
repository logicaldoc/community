package com.logicaldoc.gui.frontend.client.tenant;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects all tenant details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.9
 */
public class TenantDetailsPanel extends VLayout {
	private GUITenant tenant;

	private Layout propertiesTabPanel;

	private Layout quotaTabPanel;

	private Layout brandingTabPanel;

	private Layout keystoreTabPanel;

	private TenantPropertiesPanel propertiesPanel;

	private TenantQuotaPanel quotaPanel;

	private TenantBrandingPanel brandingPanel;

	private TenantKeystorePanel keystorePanel;

	private EditingTabSet tabSet;

	private TenantsPanel tenantsPanel;

	public TenantDetailsPanel(TenantsPanel tenantsPanel) {
		super();
		this.tenantsPanel = tenantsPanel;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (tenant.getId() != 0) {
				TenantService.Instance.get().load(tenant.getId(), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUITenant tenant) {
						setTenant(tenant);
					}
				});
			} else {
				setTenant(new GUITenant());
			}
			tabSet.hideSave();
		});

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		Tab quotaTab = new Tab(I18N.message("quota"));
		quotaTabPanel = new HLayout();
		quotaTabPanel.setWidth100();
		quotaTabPanel.setHeight100();
		quotaTab.setPane(quotaTabPanel);
		tabSet.addTab(quotaTab);

		if (isBrandingEnabled()) {
			Tab brandingTab = new Tab(I18N.message("branding"));
			brandingTabPanel = new HLayout();
			brandingTabPanel.setWidth100();
			brandingTabPanel.setHeight100();
			brandingTab.setPane(brandingTabPanel);
			tabSet.addTab(brandingTab);
		}

		if (isKeystoreEnabled()) {
			Tab keystoreTab = new Tab(I18N.message("keystore"));
			keystoreTabPanel = new HLayout();
			keystoreTabPanel.setWidth100();
			keystoreTabPanel.setHeight100();
			keystoreTab.setPane(keystoreTabPanel);
			tabSet.addTab(keystoreTab);
		}

		addMember(tabSet);
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		/*
		 * Prepare the quota tab
		 */
		if (quotaPanel != null) {
			quotaPanel.destroy();
			if (Boolean.TRUE.equals(quotaTabPanel.contains(quotaPanel)))
				quotaTabPanel.removeMember(quotaPanel);
		}

		/*
		 * Prepare the branding tab
		 */
		if (brandingPanel != null) {
			brandingPanel.destroy();
			if (Boolean.TRUE.equals(brandingTabPanel.contains(brandingPanel)))
				brandingTabPanel.removeMember(brandingPanel);
		}

		/*
		 * Prepare the keystore tab
		 */
		if (keystorePanel != null) {
			keystorePanel.destroy();
			if (Boolean.TRUE.equals(keystoreTabPanel.contains(keystorePanel)))
				keystoreTabPanel.removeMember(keystorePanel);
		}

		ChangedHandler changeHandler = (ChangedEvent event) -> onModified();

		propertiesPanel = new TenantPropertiesPanel(this.tenant, changeHandler);
		propertiesTabPanel.addMember(propertiesPanel);
		quotaPanel = new TenantQuotaPanel(this.tenant, changeHandler);
		quotaTabPanel.addMember(quotaPanel);

		if (isBrandingEnabled()) {
			brandingPanel = new TenantBrandingPanel(tenant, changeHandler);
			brandingTabPanel.addMember(brandingPanel);
		}

		if (isKeystoreEnabled()) {
			keystorePanel = new TenantKeystorePanel(tenant);
			keystoreTabPanel.addMember(keystorePanel);
		}
	}

	public GUITenant getTenant() {
		return tenant;
	}

	public void setTenant(GUITenant tenant) {
		this.tenant = tenant;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		if (!propertiesPanel.validate()) {
			tabSet.selectTab(0);
			return false;
		}

		if (!quotaPanel.validate()) {
			tabSet.selectTab(1);
			return false;
		}

		if (isBrandingEnabled() && !brandingPanel.validate()) {
			tabSet.selectTab(2);
			return false;
		}

		return true;
	}

	private boolean isKeystoreEnabled() {
		return Feature.enabled(Feature.DIGITAL_SIGNATURE) && Menu.enabled(Menu.KEYSTORE);
	}

	private boolean isBrandingEnabled() {
		return (Feature.enabled(Feature.BRANDING_LOGO) || Feature.enabled(Feature.BRANDING_FULL)
				|| Feature.enabled(Feature.BRANDING_STANDARD)) && Menu.enabled(Menu.BRANDING);
	}

	public void onSave() {
		if (validate()) {
			final boolean newTenant = TenantDetailsPanel.this.tenant.getId() == 0L;

			TenantService.Instance.get().save(tenant, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUITenant tenant) {
					tabSet.hideSave();
					if (tenant != null) {
						if (newTenant) {
							SC.say(I18N.message("newtenantresume", tenant.getName(), tenant.getAdminUsername(),
									"admin"));
						}

						TenantDetailsPanel.this.tenant = tenant;
						TenantDetailsPanel.this.tenantsPanel.updateRecord(tenant);
						TenantDetailsPanel.this.tenantsPanel.loadTenant(tenant.getId());
					}
				}
			});
		}
	}
}