package com.logicaldoc.gui.frontend.client.document;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.ImpexService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;

/**
 * This popup window is used to send documents or folders to an archive.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SendToArchiveDialog extends Window {

	private DynamicForm form = new DynamicForm();

	/**
	 * Constructor
	 * 
	 * @param ids Identifiers of the elements that have to be archived
	 * @param document True if the ids refers to documents, False in case of
	 *        folders
	 */
	public SendToArchiveDialog(List<Long> ids, boolean document) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("sendtoexparchive"));
		setWidth(380);
		setHeight(100);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		SelectItem archive = ItemFactory.newArchiveSelector(GUIArchive.MODE_EXPORT, GUIArchive.STATUS_OPENED);
		archive.setTitle(I18N.message("selectopenarchive"));
		archive.setWrapTitle(false);
		archive.setRequired(true);

		ButtonItem send = new ButtonItem();
		send.setStartRow(false);
		send.setTitle(I18N.message("sendtoexparchive"));
		send.setAutoFit(true);
		send.addClickHandler(event -> onSubmit(ids, document));

		form.setFields(archive, send);
		addItem(form);
	}

	public void onSubmit(List<Long> ids, boolean document) {
		if (!form.validate())
			return;

		if (document)
			ImpexService.Instance.get().addDocuments(Long.parseLong(form.getValueAsString("archive")), ids,
					new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							GuiLog.info(I18N.message("documentsaddedtoarchive"), null);
							destroy();
						}
					});
		else
			ImpexService.Instance.get().addFolder(Long.parseLong(form.getValueAsString("archive")), ids.get(0),
					new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							GuiLog.info(I18N.message("documentsaddedtoarchive"), null);
							destroy();
						}
					});
	}
}