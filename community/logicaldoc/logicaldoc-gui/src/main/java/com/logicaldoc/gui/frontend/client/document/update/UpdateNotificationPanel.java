package com.logicaldoc.gui.frontend.client.document.update;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.UserSelectorCombo;
import com.logicaldoc.gui.frontend.client.document.DocumentDetailTab;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows a list of users and a brief message to notify them about the document
 * creation or checkin
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6
 */
public class UpdateNotificationPanel extends DocumentDetailTab {
	private DynamicForm form = new DynamicForm();

	private VLayout container = new VLayout();

	private HLayout formContainer = new HLayout();

	private ValuesManager vm = new ValuesManager();

	protected boolean tagsInitialized = false;

	private UserSelectorCombo usersItem;

	public UpdateNotificationPanel(GUIDocument document) {
		super(document, null);
		setWidth100();
		setHeight100();
		container.setWidth100();
		container.setMembersMargin(5);
		addMember(container);

		formContainer.setWidth100();
		formContainer.setMembersMargin(10);

		container.setMembers(formContainer);

		refresh();
	}

	private void refresh() {
		vm.clearErrors(false);

		/*
		 * Prepare the second form for the tags
		 */
		prepareForm();
		formContainer.addMember(form);
	}

	private void prepareForm() {
		if (formContainer.contains(form)) {
			formContainer.removeMember(form);
			form.destroy();
		}

		form = new DynamicForm();
		form.setWidth100();
		form.setValuesManager(vm);

		usersItem = new UserSelectorCombo("users", "users", null, true, true);

		usersItem.setDisabled(!updateEnabled);

		TextAreaItem message = ItemFactory.newTextAreaItem("message", "message", null);
		message.setWidth("*");
		form.setItems(usersItem, message);
	}

	public boolean validate() {
		vm.validate();

		if (!vm.hasErrors()) {
			document.setNotifyUsers(usersItem.getUserIds());
			document.setNotifyMessage(vm.getValueAsString("message"));
		}

		return !vm.hasErrors();
	}
}