package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.LdapService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to import a user in the selected tenant.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.9
 */
public class LdapImportDialog extends Window {

	private DynamicForm form = new DynamicForm();

	private SelectItem tenant;

	public LdapImportDialog(final String[] usernames) {
		VLayout layout = new VLayout();
		layout.setMargin(2);

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("tenantselection"));
		setWidth(380);
		setHeight(100);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		layout.addMember(form);

		tenant = ItemFactory.newTenantSelector();
		tenant.setTitle(I18N.message("tenant"));
		tenant.setWrapTitle(false);
		tenant.setRequired(true);

		ButtonItem start = new ButtonItem();
		start.setStartRow(false);
		start.setTitle(I18N.message("select"));
		start.setAutoFit(true);
		start.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (!form.validate())
					return;
				long tnt = Long.parseLong(tenant.getSelectedRecord().getAttributeAsString("id"));

				ContactingServer.get().show();
				LdapService.Instance.get().importUsers(usernames, tnt, new AsyncCallback<GUIValue[]>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
						ContactingServer.get().hide();
					}

					@Override
					public void onSuccess(GUIValue[] report) {
						ContactingServer.get().hide();
						String message = I18N.message("importreport",
								new String[] { report[0].getValue(), report[1].getValue(), report[2].getValue() });
						if ("0".equals(report[2].getValue()))
							Log.info(I18N.message("importcompleted"), message);
						else
							Log.error(I18N.message("importerrors"), message, null);
						SC.warn(message);
						destroy();
					}
				});
			}
		});

		form.setFields(tenant, start);
		addItem(layout);
	}
}