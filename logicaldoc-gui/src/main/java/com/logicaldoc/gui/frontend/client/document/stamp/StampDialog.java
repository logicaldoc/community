package com.logicaldoc.gui.frontend.client.document.stamp;

import java.util.Arrays;
import java.util.stream.Collectors;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This popup window is used to apply a stamp
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 7.3
 */
public class StampDialog extends StickyWindow {

	private DynamicForm form = new DynamicForm();

	private CheckboxItem visualPositioning;

	private SelectItem stampSelector;

	protected GUIDocument[] documents;

	public StampDialog(GUIDocument[] documents) {
		super("applystamp");
		this.documents = documents;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		stampSelector = ItemFactory.newStampSelector();
		stampSelector.setTitle(I18N.message("choosestamp"));
		stampSelector.setWrapTitle(false);
		stampSelector.setRequired(true);

		visualPositioning = new CheckboxItem();
		visualPositioning.setName("visualpositioning");
		visualPositioning.setTitle(I18N.message("visualpositioning"));

		ButtonItem apply = new ButtonItem();
		apply.setTitle(I18N.message("apply"));
		apply.setAutoFit(true);
		apply.addClickHandler(event -> onApply());

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(stampSelector, visualPositioning, apply);

		addItem(form);
	}

	public void onApply() {
		if (!form.validate())
			return;

		ListGridRecord selection = stampSelector.getSelectedRecord();
		long stampId = selection.getAttributeAsLong("id");

		LD.contactingServer();
		StampService.Instance.get().getStamp(stampId, new AsyncCallback<GUIStamp>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIStamp stamp) {
				LD.clearPrompt();

				Boolean visualPositioningFlag = visualPositioning.getValueAsBoolean();
				if (stamp.getTemplateId() != null) {
					// Display the form for filling the form parameters
					FillStamp fillStamp = new FillStamp(documents, stamp, visualPositioningFlag);
					fillStamp.show();
					destroy();
				} else {
					applyStamp(stamp, visualPositioningFlag, StampDialog.this, documents);
				}
			}
		});
	}

	static void applyStamp(GUIStamp stamp, Boolean visualPositioningFlag, Window popup, GUIDocument[] documents) {
		if (Boolean.TRUE.equals(visualPositioningFlag)) {
			VisualPositioningStampDialog dialog = new VisualPositioningStampDialog(documents, stamp);
			dialog.show();
			popup.destroy();
		} else {
			LD.contactingServer();

			StampService.Instance.get().applyStamp(Arrays.asList(documents).stream().map(d -> d.getId())
					.collect(Collectors.toList()).toArray(new Long[0]), stamp, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							LD.clearPrompt();
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							LD.clearPrompt();
							GuiLog.info(I18N.message("event.stamped"), null);
							for (GUIDocument doc : documents) {
								DocumentService.Instance.get().getById(doc.getId(), new AsyncCallback<GUIDocument>() {

									@Override
									public void onFailure(Throwable caught) {
										GuiLog.serverError(caught);
									}

									@Override
									public void onSuccess(GUIDocument document) {
										DocumentController.get().modified(document);
									}
								});
							}
							popup.destroy();
						}
					});
		}
	}
}