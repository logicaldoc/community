package com.logicaldoc.gui.frontend.client.document;

import gwtupload.client.IFileInput.FileInputType;
import gwtupload.client.IUploadStatus.Status;
import gwtupload.client.IUploader;
import gwtupload.client.MultiUploader;

import com.google.gwt.dom.client.Style.BorderStyle;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.document.update.UpdateDialog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangeEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangeHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload documents to the server.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class DocumentsUploader extends Window {

	private IButton sendButton;

	private MultiUploader multiUploader;

	private ValuesManager vm;

	private boolean zipImport = true;

	private DynamicForm form;

	private Label customExternalDropZone;

	public DocumentsUploader() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("adddocuments"));
		setWidth(460);
		setHeight(245);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		customExternalDropZone = new Label();
		customExternalDropZone.setText(I18N.message("dropfileshere"));
		customExternalDropZone.setWidth((getWidth() - 5) + "px");
		customExternalDropZone.setHeight("40px");
		customExternalDropZone.getElement().getStyle().setBorderStyle(BorderStyle.DASHED);
		customExternalDropZone.getElement().getStyle().setBorderWidth(1, Unit.PX);
		customExternalDropZone.getElement().getStyle().setPadding(10, Unit.PX);
		multiUploader = new MultiUploader(FileInputType.CUSTOM.with(
				(Widget) new Button(I18N.message("clickmeordropfiles"))).withZone(customExternalDropZone));
		multiUploader.addOnFinishUploadHandler(onFinishUploaderHandler);
		multiUploader.addOnStartUploadHandler(onStartUploaderHandler);
		multiUploader.setFileInputPrefix("LDOC");
		multiUploader.reset();

		sendButton = new IButton(I18N.message("send"));
		sendButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSend();
			}
		});

		prepareForm();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setMargin(2);
		layout.setWidth100();

		layout.addMember(form);
		layout.addMember(customExternalDropZone);
		layout.addMember(multiUploader);
		layout.addMember(sendButton);

		// Clean the upload folder if the window is closed
		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						destroy();
					}
				});
			}
		});

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

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px, 100%");
		vm = new ValuesManager();
		form.setValuesManager(vm);

		final SelectItem charset = ItemFactory.newCharsetSelector("charset");
		charset.setHidden(true);

		final CheckboxItem zipItem = new CheckboxItem();
		zipItem.setName("zip");
		zipItem.setTitle(I18N.message("importfromzip"));
		zipItem.setValue(!zipImport);
		zipItem.setTitleAlign(Alignment.LEFT);
		zipItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				if ((Boolean) event.getValue())
					charset.show();
				else
					charset.hide();
			}
		});

		final CheckboxItem immediateIndexing = new CheckboxItem();
		immediateIndexing.setName("immediateIndexing");
		immediateIndexing.setTitle(I18N.message("immediateindexing"));
		immediateIndexing.setValue(false);
		immediateIndexing.setTitleAlign(Alignment.LEFT);

		if (!Session.get().getCurrentFolder().hasPermission(Constants.PERMISSION_IMPORT)) {
			zipItem.setDisabled(true);
			zipItem.setValue(false);
		}

		SelectItem template = ItemFactory.newTemplateSelector(true, null);
		template.setMultiple(false);

		zipItem.addChangeHandler(new ChangeHandler() {
			public void onChange(ChangeEvent event) {
				zipImport = !zipImport;
			}
		});

		form.setItems(zipItem, charset, immediateIndexing);
	}

	private IUploader.OnFinishUploaderHandler onFinishUploaderHandler = new IUploader.OnFinishUploaderHandler() {
		public void onFinish(IUploader uploader) {
			if (uploader.getStatus() == Status.SUCCESS || multiUploader.getSuccessUploads() > 0) {
				sendButton.setDisabled(false);
			}
		}
	};

	private IUploader.OnStartUploaderHandler onStartUploaderHandler = new IUploader.OnStartUploaderHandler() {
		public void onStart(IUploader uploader) {
			sendButton.setDisabled(true);
		}
	};

	public void onSend() {
		if (multiUploader.getSuccessUploads() <= 0) {
			SC.warn(I18N.message("filerequired"));
			return;
		}
		if (!vm.validate())
			return;

		GUIFolder folder = Session.get().getCurrentFolder();
		GUIDocument metadata = new GUIDocument();
		metadata.setFolder(Session.get().getCurrentFolder());
		metadata.setLanguage(I18N.getDefaultLocaleForDoc());
		metadata.setTemplateId(folder.getTemplateId());
		metadata.setTemplate(folder.getTemplate());
		metadata.setAttributes(folder.getAttributes());
		metadata.setTags(folder.getTags());

		UpdateDialog bulk = new UpdateDialog(null, metadata, UpdateDialog.CONTEXT_UPLOAD, false);
		bulk.setZip(getImportZip());
		bulk.setCharset(getCharset());
		bulk.setImmediateIndexing(getImmediateIndexing());

		bulk.show();
		destroy();
	}

	public String getCharset() {
		return vm.getValueAsString("charset");
	}

	public boolean getImportZip() {
		return "true".equals(vm.getValueAsString("zip"));
	}

	public boolean getImmediateIndexing() {
		return "true".equals(vm.getValueAsString("immediateIndexing"));
	}
}