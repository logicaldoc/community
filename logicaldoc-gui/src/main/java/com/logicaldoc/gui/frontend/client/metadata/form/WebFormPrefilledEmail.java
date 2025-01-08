package com.logicaldoc.gui.frontend.client.metadata.form;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIEmail;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;

/**
 * Collects the pre-filled fields to use to invite some responders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class WebFormPrefilledEmail extends StickyWindow {

	private ExtendedPropertiesPanel extPanel;

	private GUIEmail mail;

	public WebFormPrefilledEmail(GUIEmail mail, long formId) {
		super("prefilledfields");

		this.mail = mail;

		centerInPage();
		setWidth(600);
		setAutoHeight();

		initGUI(formId);
	}

	private void initGUI(long formId) {
		StaticTextItem hint = ItemFactory.newStaticTextItem("hint", null, I18N.message("prefilledfieldshint"));
		hint.setWidth(200);
		hint.setShowTitle(false);

		DynamicForm hintForm = new DynamicForm();
		hintForm.setTitleOrientation(TitleOrientation.TOP);
		hintForm.setNumCols(1);
		hintForm.setWidth100();
		hintForm.setItems(hint);

		addItem(hintForm);

		FormService.Instance.get().getById(formId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIForm frm) {
				extPanel = new ExtendedPropertiesPanel(frm, null, true, false, false, true);
				addItem(extPanel);

				SubmitItem send = new SubmitItem();
				send.setTitle(I18N.message("submit"));
				send.addClickHandler(event -> onSubmit());

				DynamicForm sendForm = new DynamicForm();
				sendForm.setTitleOrientation(TitleOrientation.TOP);
				sendForm.setNumCols(1);
				sendForm.setWidth100();
				sendForm.setItems(send);

				addItem(sendForm);
			}
		});

	}

	public void onSubmit() {
		if (!extPanel.validate())
			return;

		LD.contactingServer();
		FormService.Instance.get().invite((GUIForm) extPanel.getObject(), mail, I18N.getLocale(),
				new DefaultAsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						destroy();
					}

					@Override
					public void onSuccess(Void arg) {
						LD.clearPrompt();
						GuiLog.info(I18N.message("messagesent"));
						destroy();
					}
				});
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