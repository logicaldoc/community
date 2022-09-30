package com.logicaldoc.gui.frontend.client.document;

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
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a checked-out document to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentCheckin extends Window {
	private IButton sendButton;

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
		versionItem.setName("majorversion");
		versionItem.setTitle(I18N.message("majorversion"));

		final BooleanItem filenameItem = new BooleanItem();
		filenameItem.setName("checkfilename");
		filenameItem.setTitle(I18N.message("checkfilename"));
		filenameItem.setDefaultValue(true);
		filenameItem.setWrapTitle(false);
		filenameItem.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (!filenameItem.getValueAsBoolean())
					sendButton.setDisabled(false);
			}
		});

		TextItem commentItem = ItemFactory.newTextItem("comment", "comment", null);
		commentItem.setRequired(true);
		commentItem.setBrowserSpellCheck(true);
		commentItem.setWidth(250);

		form.setItems(versionItem, filenameItem, commentItem);

		sendButton = new IButton(I18N.message("send"));
		sendButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSend();
			}
		});
		sendButton.setDisabled(true);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();

		layout.addMember(form);
		uploader = new Upload(sendButton);
		layout.addMember(uploader);
		layout.addMember(sendButton);

		addItem(layout);

		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
			}

			@Override
			public void onSuccess(Void result) {
			}
		});
	}

	public void onSend() {
		if (uploader.getUploadedFile()==null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		if (!vm.validate())
			return;

		if ("true".equals(vm.getValueAsString("checkfilename"))
				&& !uploader.getUploadedFile().equals(fileName)) {
			sendButton.setDisabled(true);
			SC.warn(I18N.message("nosamefilename"));
			return;
		}

		document.setComment(vm.getValueAsString("comment"));
		UpdateDialog bulk = new UpdateDialog(new long[] { document.getId() }, document, UpdateDialog.CONTEXT_CHECKIN,
				"true".equals(vm.getValueAsString("majorversion")));
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