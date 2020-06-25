package com.logicaldoc.gui.frontend.client.document.stamp;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentsGrid;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This popup window is used to apply a stamp
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 7.3
 */
public class StampDialog extends Window {

	private DynamicForm form = new DynamicForm();

	private CheckboxItem visualPositioning;

	private SelectItem stamp;

	public StampDialog(final DocumentsGrid sourceGrid) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("applystamp"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		stamp = ItemFactory.newStampSelector();
		stamp.setTitle(I18N.message("choosestamp"));
		stamp.setWrapTitle(false);
		stamp.setRequired(true);

		visualPositioning = new CheckboxItem();
		visualPositioning.setName("visualpositioning");
		visualPositioning.setTitle(I18N.message("visualpositioning"));

		ButtonItem apply = new ButtonItem();
		apply.setTitle(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onApply(sourceGrid);
			}
		});

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(stamp, visualPositioning, apply);

		addItem(form);
	}

	public void onApply(DocumentsGrid sourceGrid) {
		if (!form.validate())
			return;
		ListGridRecord selection = stamp.getSelectedRecord();

		if (visualPositioning.getValueAsBoolean()) {
			VisualPositioningStampDialog dialog = new VisualPositioningStampDialog(sourceGrid,
					selection.getAttributeAsLong("id"));
			dialog.show();
			destroy();
		} else {
			ContactingServer.get().show();
			GUIStamp dummyStamp = new GUIStamp(selection.getAttributeAsLong("id"));
			dummyStamp.setPageOption(-1);
			dummyStamp.setPageSelection(null);
			StampService.Instance.get().applyStamp(sourceGrid.getSelectedIds(), dummyStamp, new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					ContactingServer.get().hide();
					Log.serverError(caught);
				}

				@Override
				public void onSuccess(Void result) {
					ContactingServer.get().hide();
					Log.info(I18N.message("event.stamped"), null);
					GUIDocument[] docs = sourceGrid.getSelectedDocuments();
					for (GUIDocument doc : docs) {
						DocumentService.Instance.get().getById(doc.getId(), new AsyncCallback<GUIDocument>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUIDocument document) {
								sourceGrid.updateDocument(document);
								DocumentController.get().modified(document);
							}
						});
					}
					destroy();
				}
			});
		}
	}
}