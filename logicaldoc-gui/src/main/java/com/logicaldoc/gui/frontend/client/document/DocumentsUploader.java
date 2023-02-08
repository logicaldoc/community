package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.MultipleUpload;
import com.logicaldoc.gui.frontend.client.document.update.UpdateDialog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangeEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload documents to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsUploader extends Window {

	private static final String CHARSET = "charset";

	private IButton sendButton;

	private ValuesManager vm;

	private boolean zipImport = true;

	private DynamicForm form;

	private MultipleUpload uploader;

	public DocumentsUploader() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("adddocuments"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMinWidth(450);
		setAutoSize(true);

		sendButton = new IButton(I18N.message("send"));
		sendButton.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> onSend());
		sendButton.setDisabled(true);

		prepareForm();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();

		layout.addMember(form);

		uploader = new MultipleUpload(sendButton);
		layout.addMember(uploader);
		layout.addMember(sendButton);

		// Clean the upload folder if the window is closed
		addCloseClickHandler((CloseClickEvent event) -> {
			DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					destroy();
				}
			});
		});

		addItem(layout);

		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

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

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px, 100%");
		vm = new ValuesManager();
		form.setValuesManager(vm);

		final StaticTextItem fileNameWaring = ItemFactory.newStaticTextItem("fileNameWarning",
				I18N.message("attention"), I18N.message("filenamewarning"));
		fileNameWaring.setRequired(true);

		final SelectItem charset = ItemFactory.newCharsetSelector(CHARSET);
		charset.setValue(Session.get().getConfig(CHARSET) != null ? Session.get().getConfig(CHARSET) : "UTF-8");
		charset.setHidden(true);

		final CheckboxItem zipItem = new CheckboxItem();
		zipItem.setName("zip");
		zipItem.setTitle(I18N.message("importfromzip"));
		zipItem.setValue(!zipImport);
		zipItem.setTitleAlign(Alignment.LEFT);
		zipItem.addChangedHandler((ChangedEvent event) -> {
			if (Boolean.TRUE.equals((Boolean) event.getValue()))
				charset.show();
			else
				charset.hide();
		});

		final CheckboxItem immediateIndexing = new CheckboxItem();
		immediateIndexing.setName("immediateIndexing");
		immediateIndexing.setTitle(I18N.message("immediateindexing"));
		immediateIndexing.setValue(false);
		immediateIndexing.setTitleAlign(Alignment.LEFT);

		if (!FolderController.get().getCurrentFolder().hasPermission(Constants.PERMISSION_IMPORT)) {
			zipItem.setDisabled(true);
			zipItem.setValue(false);
		}

		zipItem.addChangeHandler((ChangeEvent event) -> zipImport = !zipImport);

		form.setItems(zipItem, charset, immediateIndexing, fileNameWaring);
	}

	public void onSend() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		if (uploader.getUploadedFiles().isEmpty()) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		GUIFolder folder = FolderController.get().getCurrentFolder();
		GUIDocument metadata = new GUIDocument();
		metadata.setFolder(FolderController.get().getCurrentFolder());
		metadata.setLanguage(I18N.getDefaultLocaleForDoc());
		metadata.setTemplateId(folder.getTemplateId());
		metadata.setTemplate(folder.getTemplate());
		metadata.setAttributes(folder.getAttributes());
		metadata.setTags(folder.getTags());
		metadata.setOcrTemplateId(folder.getOcrTemplateId());

		UpdateDialog bulk = new UpdateDialog(null, metadata, UpdateDialog.CONTEXT_UPLOAD, false,
				vm.getValueAsString(CHARSET));
		bulk.setZip(getImportZip());
		bulk.setCharset(getCharset());
		bulk.setImmediateIndexing(getImmediateIndexing());
		bulk.show();

		destroy();
	}

	public String getCharset() {
		return vm.getValueAsString(CHARSET);
	}

	public boolean getImportZip() {
		return "true".equals(vm.getValueAsString("zip"));
	}

	public boolean getImmediateIndexing() {
		return "true".equals(vm.getValueAsString("immediateIndexing"));
	}
}