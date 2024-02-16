package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.AttributeSetService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SubmitItem;

/**
 * This popup window is used to upload a new options file to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.2
 */
public class OptionsUploader extends Window {

	private SubmitItem submitButton;

	private Upload uploader;

	private ValuesManager vm;

	private DynamicForm form;

	private Options options;

	public OptionsUploader(Options options) {
		this.options = options;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadoptions"));
		setWidth(400);
		setHeight(125);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		submitButton = new SubmitItem();
		submitButton.setTitle(I18N.message("submit"));
		submitButton.setDisabled(true);
		submitButton.setAlign(Alignment.RIGHT);
		submitButton.addClickHandler(event -> onSubmit());

		form.setItems(submitButton);

		uploader = new Upload(submitButton);
		addItem(uploader);
		addItem(form);
	}

	public void onSubmit() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		LD.contactingServer();
		AttributeSetService.Instance.get().parseOptions(options.getSetId(), options.getAttribute(),
				new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						options.refresh();
						LD.clearPrompt();
					}

					@Override
					public void onSuccess(List<GUIValue> ret) {
						options.refresh();
						LD.clearPrompt();
						destroy();
					}
				});
	}
}