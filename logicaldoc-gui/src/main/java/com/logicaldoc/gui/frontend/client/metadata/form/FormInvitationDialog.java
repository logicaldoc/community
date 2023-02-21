package com.logicaldoc.gui.frontend.client.metadata.form;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.dialogs.AbstractEmailDialog;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.FormService;

/**
 * This is the form used to send form invitations
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class FormInvitationDialog extends AbstractEmailDialog {

	private long formId;

	public FormInvitationDialog(long formId) {
		super();
		this.formId = formId;
		setTitle(I18N.message("invitationtofillform"));
		defaultMessage=I18N.message("invitedyoutofillform");
	}

	@Override
	protected void onSend(GUIEmail mail) {
		LD.contactingServer();
		FormService.Instance.get().invite(formId, mail, I18N.getLocale(), new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
				sendButton.enable();
				destroy();
			}

			@Override
			public void onSuccess(Void arg0) {
				LD.clearPrompt();
				sendButton.enable();
				GuiLog.info(I18N.message("messagesent"));
				destroy();
			}
		});
	}
}