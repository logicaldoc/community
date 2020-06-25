package com.logicaldoc.gui.frontend.client.security.user;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This is the form used to replicate some user settings to the selected users.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class ReplicateUserSettings extends Window {

	public ReplicateUserSettings(List<Long> userIds, final UsersPanel panel) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("replicatesettings"));
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		final ValuesManager vm = new ValuesManager();
		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWidth(350);

		SelectItem masterUser = ItemFactory.newUserSelector("user", "masteruser", null, true);
		masterUser.setHint(I18N.message("masteruserhint"));

		final CheckboxItem userInterface = ItemFactory.newCheckbox("userinterface", "userinterface");
		userInterface.setValue(true);

		final CheckboxItem groups = ItemFactory.newCheckbox("groups", "groups");
		groups.setValue(false);

		final ButtonItem confirm = new ButtonItem();
		confirm.setTitle(I18N.message("confirm"));
		confirm.setAutoFit(true);
		confirm.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				vm.validate();
				if (!vm.hasErrors()) {
					long masterUserId = Long.parseLong(vm.getValueAsString("user"));
					ContactingServer.get().show();
					SecurityService.Instance.get().replicateUsersSettings(masterUserId, userIds.toArray(new Long[0]),
							userInterface.getValueAsBoolean(), groups.getValueAsBoolean(), new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									ContactingServer.get().hide();
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void arg0) {
									ContactingServer.get().hide();
									Log.info(I18N.message("userssaved"));
									destroy();
									panel.refresh();
								}
							});
				}
			}
		});

		form.setFields(masterUser, userInterface, groups, confirm);

		addItem(form);
	}
}