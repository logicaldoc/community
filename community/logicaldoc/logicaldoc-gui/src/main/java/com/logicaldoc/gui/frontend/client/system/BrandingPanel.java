package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.logicaldoc.gui.frontend.client.tenant.TenantBrandingPanel;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows the branding details.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class BrandingPanel extends AdminPanel {

	private TenantBrandingPanel tenantBranding;

	private GUITenant tenant;

	public BrandingPanel(long tenantId) {
		super("branding");
		TenantService.Instance.get().load(tenantId, new AsyncCallback<GUITenant>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUITenant ten) {
				tenant = ten;
				initGUI();
			}
		});
	}

	private void initGUI() {
		tenantBranding = new TenantBrandingPanel(tenant, new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {

			}
		});

		IButton save = new IButton(I18N.message("save"));
		save.setAutoDraw(true);
		save.setMinWidth(80);
		save.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				if (tenantBranding.validate())
					onSave();
			}
		});

		body.setMembers(tenantBranding);
		addMember(save);
	}

	private void onSave() {
		TenantService.Instance.get().save(tenant, new AsyncCallback<GUITenant>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUITenant ten) {
				BrandingPanel.this.tenant = ten;
				Log.info(I18N.message("settingssaved"), null);
			}
		});
	}
}