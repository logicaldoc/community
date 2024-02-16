package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.logicaldoc.gui.frontend.client.tenant.TenantBrandingPanel;
import com.smartgwt.client.widgets.IButton;

/**
 * Shows the branding details.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class BrandingPanel extends AdminPanel {

	private GUITenant tenant;

	public BrandingPanel(long tenantId) {
		super("branding");
		TenantService.Instance.get().load(tenantId, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUITenant ten) {
				tenant = ten;
				initGUI();
			}
		});
	}

	private void initGUI() {
		TenantBrandingPanel tenantBranding = new TenantBrandingPanel(tenant, event -> {
			// Nothing to do
		});

		IButton save = new IButton(I18N.message("save"));
		save.setMinWidth(80);
		save.addClickHandler(event -> {
			if (tenantBranding.validate())
				onSave();
		});

		body.setMembers(tenantBranding);
		addMember(save);
	}

	private void onSave() {
		TenantService.Instance.get().save(tenant, new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUITenant ten) {
				BrandingPanel.this.tenant = ten;
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}
}