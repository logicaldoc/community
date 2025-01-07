package com.logicaldoc.gui.frontend.client.security.user;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;

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

		SelectItem masterUser = ItemFactory.newUserSelector("user", "masteruser", null, true, false);
		masterUser.setHint(I18N.message("masteruserhint"));

		final CheckboxItem userInterface = ItemFactory.newCheckbox("userinterface", "userinterface");
		userInterface.setValue(true);

		final CheckboxItem groups = ItemFactory.newCheckbox("groups", "groups");
		groups.setValue(false);

		final ButtonItem confirm = new ButtonItem();
		confirm.setTitle(I18N.message("confirm"));
		confirm.setAutoFit(true);
		confirm.addClickHandler(event -> {
			vm.validate();
			if (Boolean.FALSE.equals(vm.hasErrors())) {
				long masterUserId = Long.parseLong(vm.getValueAsString("user"));
				LD.contactingServer();
				SecurityService.Instance.get().replicateUsersSettings(masterUserId, userIds,
						userInterface.getValueAsBoolean(), groups.getValueAsBoolean(), new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void arg0) {
								LD.clearPrompt();
								GuiLog.info(I18N.message("userssaved"));
								destroy();
								panel.refresh();
							}
						});
			}
		});

		form.setFields(masterUser, userInterface, groups, confirm);

		addItem(form);
	}
}