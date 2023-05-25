package com.logicaldoc.gui.frontend.client.settings;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.logicaldoc.gui.frontend.client.tenant.TenantQuotaPanel;
import com.smartgwt.client.widgets.IButton;

/**
 * Shows the quota details.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class QuotaPanel extends AdminPanel {

	private TenantQuotaPanel tenantQuota;

	private GUITenant tenant;

	public QuotaPanel(long tenantId) {
		super("quota");

		TenantService.Instance.get().load(tenantId, new AsyncCallback<GUITenant>() {
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
		tenantQuota = new TenantQuotaPanel(tenant, event -> {
			// Nothing to do
		});

		IButton save = new IButton(I18N.message("save"));
		save.setMinWidth(80);
		save.addClickHandler(event -> {
			if (tenantQuota.validate())
				TenantService.Instance.get().save(tenant, new AsyncCallback<GUITenant>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUITenant ten) {
						QuotaPanel.this.tenant = ten;
						GuiLog.info(I18N.message("settingssaved"), null);
					}
				});
		});

		body.setMembers(tenantQuota);
		addMember(save);
	}
}