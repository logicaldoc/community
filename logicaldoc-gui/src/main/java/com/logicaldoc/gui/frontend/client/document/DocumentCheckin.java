package com.logicaldoc.gui.frontend.client.document;

import java.util.Arrays;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.document.update.UpdateDialog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.BooleanItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a checked-out document to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentCheckin extends Window {
	private static final String CHECKFILENAME = "checkfilename";

	private static final String MAJORVERSION = "majorversion";

	private IButton submitButton;

	private Upload uploader;

	private ValuesManager vm;

	private GUIDocument document;

	private String fileName;

	public DocumentCheckin(GUIDocument document, String filename) {
		this.document = document;
		this.fileName = filename;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("checkin"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		BooleanItem versionItem = new BooleanItem();
		versionItem.setName(MAJORVERSION);
		versionItem.setTitle(I18N.message(MAJORVERSION));

		final BooleanItem filenameItem = new BooleanItem();
		filenameItem.setName(CHECKFILENAME);
		filenameItem.setTitle(I18N.message(CHECKFILENAME));
		filenameItem.setDefaultValue(true);
		filenameItem.setWrapTitle(false);
		filenameItem.addChangedHandler((ChangedEvent event) -> {
			if (Boolean.FALSE.equals(filenameItem.getValueAsBoolean()))
				submitButton.setDisabled(false);
		});

		TextItem commentItem = ItemFactory.newTextItem("comment", null);
		commentItem.setRequired(true);
		commentItem.setBrowserSpellCheck(true);
		commentItem.setWidth(250);

		form.setItems(versionItem, filenameItem, commentItem);

		submitButton = new IButton(I18N.message("submit"));
		submitButton.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> onSubmit());
		submitButton.setDisabled(true);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();

		layout.addMember(form);
		uploader = new Upload(submitButton);
		layout.addMember(uploader);
		layout.addMember(submitButton);

		addItem(layout);

		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				// Nothing to do
			}

			@Override
			public void onSuccess(Void result) {
				// Nothing to do
			}
		});
	}

	public void onSubmit() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		if (Boolean.FALSE.equals(vm.validate()))
			return;

		if ("true".equals(vm.getValueAsString(CHECKFILENAME)) && !uploader.getUploadedFile().equals(fileName)) {
			submitButton.setDisabled(true);
			SC.warn(I18N.message("nosamefilename"));
			return;
		}

		document.setComment(vm.getValueAsString("comment"));
		UpdateDialog bulk = new UpdateDialog(Arrays.asList(document.getId()), document, UpdateDialog.CHECKIN,
				"true".equals(vm.getValueAsString(MAJORVERSION)));
		bulk.show();
		destroy();
	}

	public String getLanguage() {
		return vm.getValueAsString("language");
	}

	public boolean getImportZip() {
		return "true".equals(vm.getValueAsString("zip"));
	}
}