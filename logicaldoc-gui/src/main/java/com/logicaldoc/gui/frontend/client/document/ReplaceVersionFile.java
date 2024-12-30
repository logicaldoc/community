package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new version's file.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class ReplaceVersionFile extends Window {
	private IButton saveButton;

	private Upload uploader;

	private ValuesManager vm;

	private GUIDocument document;

	private String fileVersion;

	public ReplaceVersionFile(GUIDocument document, String fileVersion) {
		this.document = document;
		this.fileVersion = fileVersion;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("replacefile"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		TextItem commentItem = ItemFactory.newTextItem("comment", null);
		commentItem.setRequired(true);
		commentItem.setWidth(250);

		form.setItems(commentItem);

		saveButton = new IButton(I18N.message("save"));
		saveButton.addClickHandler(event -> onSave());
		saveButton.setDisabled(true);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);

		layout.addMember(form);
		uploader = new Upload(saveButton);
		layout.addMember(uploader);
		layout.addMember(saveButton);

		addItem(layout);
	}

	public void onSave() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		DocumentService.Instance.get().replaceFile(document.getId(), fileVersion, vm.getValueAsString("comment"),
				new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						destroy();
					}
				});
	}

	public String getLanguage() {
		return vm.getValueAsString("language");
	}

	public boolean getImportZip() {
		return Boolean.valueOf(vm.getValueAsString("zip"));
	}
}