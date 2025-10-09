package com.logicaldoc.gui.frontend.client.metadata.form;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.dialogs.AbstractEmailDialog;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * This is the form used to send form invitations
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WebFormInvitationDialog extends AbstractEmailDialog {

	private long formId;

	private CheckboxItem prefillFields;

	public WebFormInvitationDialog(long formId) {
		super();
		this.formId = formId;
		setTitle(I18N.message("invitationtofillform"));
		defaultMessage = I18N.message("invitedyoutofillform");
		setHeight(480);
	}

	@Override
	protected void onSubmit(GUIEmail mail) {
		if (Boolean.TRUE.equals(prefillFields.getValueAsBoolean())) {
			new WebFormPrefilledEmail(mail, formId).show();
			destroy();
		} else {
			LD.contactingServer();
			GUIForm gform = new GUIForm();
			gform.setId(formId);
			FormService.Instance.get().invite(gform, mail, I18N.getLocale(), new DefaultAsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					super.onFailure(caught);
					destroy();
				}

				@Override
				public void handleSuccess(Void arg) {
					GuiLog.info(I18N.message("messagesent"));
					destroy();
				}
			});
		}
	}

	@Override
	protected HLayout prepareButtons() {
		HLayout buttons = super.prepareButtons();

		prefillFields = ItemFactory.newCheckbox("prefillfileds", "prefillfileds");

		DynamicForm hintForm = new DynamicForm();
		hintForm.setTitleOrientation(TitleOrientation.LEFT);
		hintForm.setNumCols(2);
		hintForm.setWidth100();
		hintForm.setItems(prefillFields);

		buttons.addMember(hintForm);

		return buttons;
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}