package com.logicaldoc.gui.frontend.client.document.update;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
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
public class UpdateDialog extends Window {

	public final static String CONTEXT_CHECKIN = "checkin";

	public final static String CONTEXT_UPLOAD = "adddocuments";

	public final static String CONTEXT_UPDATE = "bulkupdate";

	private UpdatePanel bulkPanel;

	private boolean zip = false;

	private boolean immediteIndexing = false;

	private String charset = "UTF-8";

	public UpdateDialog(final long[] ids, final GUIDocument metadata, final String context,
			final boolean majorVersion) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		setTitle(I18N.message(context));
		setWidth(800);
		setHeight(300);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		/*
		 * Since the document is locked, temporarily alter the status to have
		 * the editing enabled.
		 */
		int originalStatus = 0;
		if (CONTEXT_CHECKIN.equals(context) && metadata != null) {
			originalStatus = metadata.getStatus();
			metadata.setStatus(0);
		}
		bulkPanel = new UpdatePanel(metadata, CONTEXT_UPLOAD.equals(context));
		bulkPanel.setWidth100();
		bulkPanel.setHeight("*");
		bulkPanel.setShowResizeBar(false);
		if (CONTEXT_CHECKIN.equals(context) && metadata != null)
			metadata.setStatus(originalStatus);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("60%");
		spacer.setOverflow(Overflow.HIDDEN);

		TextItem versionComment = ItemFactory.newTextItem("versionComment", "versioncomment", null);
		versionComment.setWidth(350);

		CheckboxItem ignoreEmptyFields = ItemFactory.newCheckbox("ignoreemptyfields", "ignoreemptyfields");
		ignoreEmptyFields.setValue(true);

		final DynamicForm saveForm = new DynamicForm();
		saveForm.setMargin(3);
		saveForm.setTitleOrientation(TitleOrientation.LEFT);
		saveForm.setNumCols(4);
		saveForm.setShowResizeBar(false);
		if (CONTEXT_UPDATE.equals(context))
			saveForm.setItems(versionComment, ignoreEmptyFields);
		else
			saveForm.setItems(versionComment);

		Button saveButton = new Button(
				CONTEXT_CHECKIN.equals(context) ? I18N.message("checkin") : I18N.message("save"));
		saveButton.setLayoutAlign(VerticalAlignment.CENTER);
		saveButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (!bulkPanel.validate())
					return;

				if (CONTEXT_CHECKIN.equals(context)) {
					ContactingServer.get().show();
					DocumentService.Instance.get().checkin(metadata, majorVersion, new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable error) {
							ContactingServer.get().hide();
							GuiLog.serverError(error);
						}

						@Override
						public void onSuccess(GUIDocument doc) {
							ContactingServer.get().hide();
							DocUtil.markCheckedIn(doc);
							destroy();
						}
					});
				} else if (ids != null && ids.length > 0)
					LD.ask(I18N.message("bulkupdate"), I18N.message("bulkwarning"), new BooleanCallback() {
						@Override
						public void execute(Boolean value) {
							if (value) {
								bulkPanel.getDocument().setComment(saveForm.getValueAsString("versionComment"));
								ContactingServer.get().show();
								DocumentService.Instance.get().bulkUpdate(ids, bulkPanel.getDocument(),
										"true".equals(saveForm.getValueAsString("ignoreemptyfields")),
										new AsyncCallback<Void>() {
											@Override
											public void onFailure(Throwable error) {
												ContactingServer.get().hide();
												GuiLog.serverError(error);
											}

											@Override
											public void onSuccess(Void arg) {
												ContactingServer.get().hide();
												GuiLog.info(I18N.message("bulkapplied"), null);
												if (!Session.get().isServerPushEnabled())
													DocumentsPanel.get().refresh();
												destroy();
											}
										});
							}
						}
					});
				else {
					bulkPanel.getDocument().setComment(saveForm.getValueAsString("versionComment"));
					ContactingServer.get().show();
					hide();
					DocumentService.Instance.get().addDocuments(zip, charset, immediteIndexing, bulkPanel.getDocument(),
							new AsyncCallback<GUIDocument[]>() {
								@Override
								public void onFailure(Throwable error) {
									ContactingServer.get().hide();
									GuiLog.serverError(error);

									// We have to refresh the documents list
									// because maybe
									// some documents have been stored.
									DocumentsPanel.get().refresh();
								}
								
								@Override
								public void onSuccess(GUIDocument[] doc) {
									DocumentsPanel.get().refresh();
									ContactingServer.get().hide();
								}
							});
				}
			}
		});

		HLayout savePanel = new HLayout();
		savePanel.addMember(saveButton);
		if (!CONTEXT_CHECKIN.equals(context))
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