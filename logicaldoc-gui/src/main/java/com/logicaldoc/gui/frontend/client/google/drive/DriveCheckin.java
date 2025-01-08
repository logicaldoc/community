package com.logicaldoc.gui.frontend.client.google.drive;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.google.GoogleAsyncCallback;
import com.logicaldoc.gui.frontend.client.google.GoogleService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.BooleanItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to perform the checkin of a Google Drive document
 * into LogicalDOC.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class DriveCheckin extends Window {
	private static final String MAJORVERSION = "majorversion";

	private SubmitItem checkin;

	private ValuesManager vm;

	public DriveCheckin(final GUIDocument document, final DriveEditor parentDialog) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("checkin"));
		setWidth(400);
		setHeight(140);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMembersMargin(2);

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);

		BooleanItem versionItem = new BooleanItem();
		versionItem.setName(MAJORVERSION);
		versionItem.setTitle(I18N.message(MAJORVERSION));

		TextItem commentItem = ItemFactory.newTextItem("comment", null);
		commentItem.setRequired(true);
		commentItem.setWidth(240);

		checkin = new SubmitItem();
		checkin.setTitle(I18N.message("checkin"));
		checkin.setAlign(Alignment.RIGHT);
		checkin.addClickHandler(event -> onCheckin(document, parentDialog));

		form.setItems(versionItem, commentItem, checkin);

		addItem(form);
	}

	public void onCheckin(final GUIDocument document, final DriveEditor parentDialog) {
		if (Boolean.FALSE.equals(vm.validate()))
			return;
		LD.contactingServer();
		GoogleService.Instance.get().checkin(document.getId(), vm.getValueAsString("comment"),
				Boolean.valueOf(vm.getValueAsString(MAJORVERSION)), new GoogleAsyncCallback<>() {
					@Override
					public void onSuccess(GUIDocument result) {
						LD.clearPrompt();
						destroy();
						parentDialog.destroy();
						DocumentController.get().modified(result);
						DocumentController.get().setCurrentDocument(result);
					}
				});
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}