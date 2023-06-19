package com.logicaldoc.gui.frontend.client.metadata.form;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * Helps to create pre-filled web form link
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class WebFormPrefilledLink extends StickyWindow {

	private static final String RESPONDEREMAIL = "responderemail";

	private ValuesManager vm;

	private ExtendedPropertiesPanel extPanel;

	private StaticTextItem prefilledLink;

	public WebFormPrefilledLink(long formId) {
		super("prefilledlink");

		centerInPage();
		setWidth(500);
		setAutoHeight();

		initGUI(formId);
	}

	private void initGUI(long formId) {
		DynamicForm responderForm = new DynamicForm();
		vm = new ValuesManager();
		responderForm.setValuesManager(vm);
		responderForm.setTitleOrientation(TitleOrientation.TOP);
		responderForm.setNumCols(1);

		TextItem responder = ItemFactory.newEmailItem(RESPONDEREMAIL, RESPONDEREMAIL, false);
		responder.setWidth(200);

		responderForm.setItems(responder);

		addItem(responderForm);

		FormService.Instance.get().getById(formId, new AsyncCallback<GUIForm>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIForm frm) {
				extPanel = new ExtendedPropertiesPanel(frm, null, true, false, false);
				addItem(extPanel);

				prefilledLink = ItemFactory.newStaticTextItem("prefilledlink", null);
				prefilledLink.setWidth(200);

				SubmitItem getLink = new SubmitItem();
				getLink.setTitle(I18N.message("getlink"));
				getLink.addClickHandler(event -> onGetLink());

				DynamicForm linkForm = new DynamicForm();
				linkForm.setTitleOrientation(TitleOrientation.TOP);
				linkForm.setNumCols(1);
				linkForm.setWidth100();
				linkForm.setItems(getLink, prefilledLink);
				
				addItem(linkForm);
			}
		});

	}

	public void onGetLink() {
		if (!vm.validate())
			return;

		if (!extPanel.validate())
			return;

		FormService.Instance.get().getPreFilledLink((GUIForm) extPanel.getObject(),
				vm.getValueAsString(RESPONDEREMAIL), new AsyncCallback<String>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String link) {
						prefilledLink.setValue("<a href='" + link + "' target=='_blank'>" + link + "</a>");
					}
				});
	}
}