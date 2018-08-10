package com.logicaldoc.gui.frontend.client.sharefile;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.ShareFileService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This popup window is used to handle ShareFile settings.
 * 
 * @author Marco Meschieri - LogicalDOC since 7.2.1
 */
public class ShareFileSettings extends Window {
	private SubmitItem save;

	private ValuesManager vm;

	public ShareFileSettings(String[] settings) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("sharefile"));
		setWidth(400);
		setHeight(150);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(2);
		setMembersMargin(2);

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		TextItem username = ItemFactory.newEmailItem("username", "username", false);
		username.setValue(settings[0]);
		username.setRequired(true);
		username.setWidth(220);

		TextItem password = ItemFactory.newPasswordItem("password", "password", null);
		password.setRequired(true);
		password.setWidth(150);

		TextItem host = ItemFactory.newTextItem("host", "host", null);
		host.setValue(settings[2]);
		host.setRequired(true);
		host.setWidth(220);

		save = new SubmitItem();
		save.setTitle(I18N.message("save"));
		save.setAlign(Alignment.RIGHT);
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		form.setItems(host, username, password, save);

		addItem(form);
	}

	public void onSave() {
		if (!vm.validate())
			return;

		ContactingServer.get().show();
		ShareFileService.Instance.get().saveSettings(vm.getValueAsString("host"), vm.getValueAsString("username"),
				vm.getValueAsString("password"), new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						SC.warn(I18N.message("connectionfailed"));
					}

					@Override
					public void onSuccess(Void result) {
						ContactingServer.get().hide();
						Log.info(I18N.message("connectionestablished"), null);
						destroy();
					}
				});
	}
}