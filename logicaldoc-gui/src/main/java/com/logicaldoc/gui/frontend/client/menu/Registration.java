package com.logicaldoc.gui.frontend.client.menu;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This is the form used to change registration data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Registration extends Window {

	protected ValuesManager vm = new ValuesManager();

	public Registration(List<String> reg) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("registration"));
		setWidth(350);
		setHeight(150);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setMargin(5);
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem regName = ItemFactory.newTextItem("reg_name", "name", null);
		regName.setWrapTitle(false);
		regName.setValue(reg.get(0));

		TextItem regEmail = ItemFactory.newEmailItem("reg_email", "email", false);
		regEmail.setWrapTitle(false);
		regEmail.setValue(reg.get(1));

		TextItem regOrganization = ItemFactory.newTextItem("reg_organization", "organization", null);
		regOrganization.setWrapTitle(false);
		regOrganization.setValue(reg.get(2));

		TextItem regWebsite = ItemFactory.newTextItem("reg_website", "website", null);
		regWebsite.setWidth(180);
		regWebsite.setWrapTitle(false);
		regWebsite.setValue(reg.get(3));

		ButtonItem apply = new ButtonItem();
		apply.setTitle(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler(event -> {
			vm.validate();
			if (Boolean.FALSE.equals(vm.hasErrors())) {
				SettingService.Instance.get().saveRegistration(form.getValueAsString("reg_name"),
						form.getValueAsString("reg_email"), form.getValueAsString("reg_organization"),
						form.getValueAsString("reg_website"), new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								SC.warn(caught.getMessage());
							}

							@Override
							public void onSuccess(Void ret) {
								afterSave();
								Registration.this.destroy();
							}
						});
			}
		});

		form.setFields(regName, regEmail, regOrganization, regWebsite, apply);

		addItem(form);
	}

	/**
	 * Invoked after the data was saved on the server. Override to include your
	 * own logic.
	 */
	public void afterSave() {
		// Nothing to do
	}
}
