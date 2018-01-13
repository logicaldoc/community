package com.logicaldoc.gui.frontend.client.settings;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.logicaldoc.gui.frontend.client.tenant.TenantQuotaPanel;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

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
		tenantQuota = new TenantQuotaPanel(tenant, new ChangedHandler() {

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
				if (tenantQuota.validate())
					TenantService.Instance.get().save(tenant, new AsyncCallback<GUITenant>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUITenant ten) {
							QuotaPanel.this.tenant = ten;
							Log.info(I18N.message("settingssaved"), null);
						}
					});
			}
		});

		body.setMembers(tenantQuota);
		addMember(save);
	}
}