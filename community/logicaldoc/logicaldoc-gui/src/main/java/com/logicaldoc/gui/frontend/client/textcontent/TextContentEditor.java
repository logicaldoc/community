package com.logicaldoc.gui.frontend.client.textcontent;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.DocumentServiceAsync;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to show the text document in a text area.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.1
 */
public class TextContentEditor extends Window {

	private DynamicForm form = null;

	private VLayout layout = null;

	private GUIDocument document;

	public TextContentEditor(final GUIDocument document, String content) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (document.getId() > 0)
			setTitle(I18N.message("editdoc") + ": " + document.getFileName());
		else
			setTitle(I18N.message("createdoc") + ": " + document.getFileName());

		this.document = document;

		setWidth(com.google.gwt.user.client.Window.getClientWidth());
		setHeight(com.google.gwt.user.client.Window.getClientHeight());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				unlockAndClose();
			}
		});

		layout = new VLayout();
		layout.setWidth100();
		addItem(layout);

		if (content != null) {
			prepareBody(content);
		} else {
			DocumentService.Instance.get().getContentAsString(TextContentEditor.this.document.getId(), new AsyncCallback<String>() {

				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(String content) {
					prepareBody(content);
				}
			});
		}
	}

	/**
	 * Prepares the popup body
	 */
	private void prepareBody(String content) {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.setAlign(Alignment.RIGHT);
		toolStrip.addFill();

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		toolStrip.addButton(save);
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		toolStrip.addButton(close);
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				unlockAndClose();
			}
		});

		TextAreaItem contentItem = ItemFactory.newTextAreaItem("content", I18N.message("content"), content);
		contentItem.setShowTitle(false);
		contentItem.setWidth(getWidth() - 6);
		contentItem.setHeight("*");

		form = new DynamicForm();
		form.setWidth100();
		form.setHeight100();
		form.setItems(contentItem);

		layout.setMembers(toolStrip, form);
	}

	private void unlockAndClose() {
		if (document.getId() != 0)
			DocumentService.Instance.get().unlock(new long[] { TextContentEditor.this.document.getId() }, new AsyncCallback<Void>() {
				@Override
				public void onFailure(Throwable caught) {
					Log.serverError(caught);
					destroy();
				}

				@Override
				public void onSuccess(Void result) {
					DocumentsPanel.get().refresh();
					destroy();
				}
			});
		else
			destroy();
	}

	private void onSave() {
		if (document.getId() != 0L) {
			// We are editing an existing file
			DocumentService.Instance.get().checkinContent(document.getId(), form.getValueAsString("content"),
					new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument doc) {
							TextContentEditor.this.document = doc;
							DocumentsPanel.get().refresh();
							destroy();
						}
					});
		} else {
			// We are creating a new file
			DocumentService.Instance.get().createDocument(document, form.getValueAsString("content"),
					new AsyncCallback<GUIDocument>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument doc) {
							TextContentEditor.this.document = doc;
							DocumentsPanel.get().refresh();
							destroy();
						}
					});
		}
	}
}