package com.logicaldoc.gui.frontend.client.ai.model;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.IgnoreAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new AI model.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelImporter extends Window {
	private IButton save;

	private Upload uploader;

	DynamicForm form = new DynamicForm();

	private ChangedHandler changedHandler;

	public ModelImporter(String modelName, ChangedHandler changedHandler) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadmodel"));
		setWidth(430);
		setHeight(200);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		this.changedHandler = changedHandler;

		save = new IButton(I18N.message("import"));
		save.addClickHandler(click -> onUpload());

		TextItem name = ItemFactory.newSimpleTextItem("name", "name", modelName != null ? modelName : "newmodel");
		name.setRequired(true);
		name.setVisible(modelName == null);

		form.setItems(name);

		uploader = new Upload(save);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(2);
		buttons.setMembers(save);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);
		layout.addMember(form);
		layout.addMember(uploader);
		layout.addMember(buttons);
		addItem(layout);

		addCloseClickHandler(click -> cleanUploads());
	}

	public ModelImporter(ChangedHandler changedHandler) {
		this(null, changedHandler);
	}

	private void cleanUploads() {
		DocumentService.Instance.get().cleanUploadedFileFolder(new IgnoreAsyncCallback<>());
	}

	private void onUpload() {
		if (!form.validate())
			return;

		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		LD.contactingServer();
		save.setDisabled(true);
		AIService.Instance.get().importModel(form.getValueAsString("name"), new DefaultAsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				super.onFailure(caught);
				save.setDisabled(false);
				cleanUploads();
			}

			@Override
			public void onSuccess(GUIModel model) {
				super.onSuccess(model);
				save.setDisabled(false);
				if (changedHandler != null)
					changedHandler.onChanged(null);
				cleanUploads();
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