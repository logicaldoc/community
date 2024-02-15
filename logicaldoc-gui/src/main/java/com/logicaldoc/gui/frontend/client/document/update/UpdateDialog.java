package com.logicaldoc.gui.frontend.client.document.update;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.ServerValidationException;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to allow the users to input the data for a bulk
 * update or other update operations
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class UpdateDialog extends StickyWindow {

	private static final String IGNOREEMPTYFIELDS = "ignoreemptyfields";

	private static final String VERSIONCOMMENT = "versioncomment";

	public static final String CHECKIN = "checkin";

	public static final String CONTEXT_UPLOAD = "adddocuments";

	public static final String BULKUPDATE = "bulkupdate";

	private UpdatePanel bulkPanel;

	private boolean zip = false;

	private boolean immediteIndexing = false;

	private String charset = "UTF-8";

	private String context;

	private GUIDocument metadata;

	private boolean majorVersion = false;

	private final List<Long> ids;

	public UpdateDialog(List<Long> ids, GUIDocument metadata, String context, boolean majorVersion, String charset) {
		super(context);
		this.context = context;
		this.metadata = metadata;
		this.majorVersion = majorVersion;
		this.ids = ids;
		this.charset = charset != null ? charset : Session.get().getConfig("charset");
	}

	public UpdateDialog(List<Long> ids, GUIDocument metadata, String context, boolean majorVersion) {
		this(ids, metadata, context, majorVersion, null);
	}

	@Override
	protected WindowStatus getDefaultStatus() {
		return new WindowStatus(800, 350);
	}

	@Override
	protected void onDraw() {
		super.onDraw();

		DocumentService.Instance.get().getEnabledPermissions(ids, new AsyncCallback<GUIAccessControlEntry>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIAccessControlEntry permissions) {
				/*
				 * Since the document is locked, temporarily alter the status to
				 * have the editing enabled.
				 */
				int originalStatus = 0;
				if (CHECKIN.equals(context) && metadata != null) {
					originalStatus = metadata.getStatus();
					metadata.setStatus(0);
				}

				bulkPanel = new UpdatePanel(metadata, CONTEXT_UPLOAD.equals(context) || CHECKIN.equals(context),
						permissions.isSecurity());
				bulkPanel.setWidth100();
				bulkPanel.setHeight("*");
				bulkPanel.setShowResizeBar(false);

				if (CHECKIN.equals(context) && metadata != null)
					metadata.setStatus(originalStatus);

				HTMLPane spacer = new HTMLPane();
				spacer.setContents("<div>&nbsp;</div>");
				spacer.setWidth("60%");
				spacer.setOverflow(Overflow.HIDDEN);

				TextItem versionComment = ItemFactory.newTextItem(VERSIONCOMMENT, null);
				versionComment.setWidth(350);

				CheckboxItem ignoreEmptyFields = ItemFactory.newCheckbox(IGNOREEMPTYFIELDS, IGNOREEMPTYFIELDS);
				ignoreEmptyFields.setValue(true);

				final DynamicForm saveForm = new DynamicForm();
				saveForm.setMargin(3);
				saveForm.setTitleOrientation(TitleOrientation.LEFT);
				saveForm.setNumCols(4);
				saveForm.setShowResizeBar(false);
				if (BULKUPDATE.equals(context))
					saveForm.setItems(versionComment, ignoreEmptyFields);
				else
					saveForm.setItems(versionComment);

				Button saveButton = prepareSaveButton(saveForm);

				HLayout savePanel = new HLayout();
				savePanel.addMember(saveButton);
				if (!CHECKIN.equals(context))
					savePanel.addMember(saveForm);
				savePanel.addMember(spacer);
				savePanel.setWidth100();
				savePanel.setHeight(30);
				savePanel.setMargin(2);
				savePanel.setMembersMargin(10);

				VLayout content = new VLayout();
				content.setTop(10);
				content.setWidth100();
				content.setHeight100();
				content.setMembersMargin(3);
				content.setMembers(bulkPanel, savePanel);

				addItem(content);
			}
		});
	}

	private Button prepareSaveButton(final DynamicForm saveForm) {
		Button saveButton = new Button(CHECKIN.equals(context) ? I18N.message(CHECKIN) : I18N.message("save"));
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);
		saveButton.addClickHandler((ClickEvent event) -> {
			if (!bulkPanel.validate())
				return;

			DocumentService.Instance.get().validate(metadata, new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					if (caught instanceof ServerValidationException)
						bulkPanel.extendedPropertiesPanel.handleErrors((ServerValidationException) caught);
					else
						GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void arg0) {
					if (CHECKIN.equals(context)) {
						doCheckin();
					} else if (!ids.isEmpty())
						doBulkUpdate(saveForm);
					else {
						doAddDocuments(saveForm);
					}
				}
			});
		});
		return saveButton;
	}

	private void doAddDocuments(final DynamicForm saveForm) {
		bulkPanel.getDocument().setComment(saveForm.getValueAsString(VERSIONCOMMENT));
		LD.contactingServer();
		hide();
		DocumentService.Instance.get().addDocuments(zip, charset, immediteIndexing, bulkPanel.getDocument(),
				new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable error) {
						LD.clearPrompt();
						GuiLog.serverError(error);

						// We have to refresh the documents
						// list
						// because maybe
						// some documents have been stored.
						DocumentsPanel.get().refresh();
					}

					@Override
					public void onSuccess(List<GUIDocument> docs) {
						DocumentsPanel.get().refresh();
						LD.clearPrompt();
					}
				});
	}

	private void doBulkUpdate(final DynamicForm saveForm) {
		LD.ask(I18N.message(BULKUPDATE), I18N.message("bulkwarning"), (Boolean yes) -> {
			if (Boolean.TRUE.equals(yes)) {
				bulkPanel.getDocument().setComment(saveForm.getValueAsString(VERSIONCOMMENT));
				LD.contactingServer();
				DocumentService.Instance.get().bulkUpdate(ids, bulkPanel.getDocument(),
						"true".equals(saveForm.getValueAsString(IGNOREEMPTYFIELDS)), new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable error) {
								LD.clearPrompt();
								GuiLog.serverError(error);
							}

							@Override
							public void onSuccess(List<GUIDocument> updatedDocs) {
								LD.clearPrompt();
								GuiLog.info(I18N.message("bulkapplied"), null);
								for (GUIDocument updatedDoc : updatedDocs)
									DocumentController.get().modified(updatedDoc);
								destroy();
							}
						});
			}
		});
	}

	private void doCheckin() {
		LD.contactingServer();
		DocumentService.Instance.get().checkin(metadata, majorVersion, new AsyncCallback<GUIDocument>() {

			@Override
			public void onFailure(Throwable error) {
				LD.clearPrompt();
				GuiLog.serverError(error);
			}

			@Override
			public void onSuccess(GUIDocument doc) {
				LD.clearPrompt();
				DocUtil.markCheckedIn(doc);
				destroy();
			}
		});
	}

	public boolean isZip() {
		return zip;
	}

	public void setZip(boolean zip) {
		this.zip = zip;
	}

	public void setImmediateIndexing(boolean immediteIndexing) {
		this.immediteIndexing = immediteIndexing;
	}

	public void setCharset(String charset) {
		this.charset = charset;
	}
}