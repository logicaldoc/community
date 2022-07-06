package com.logicaldoc.gui.frontend.client.document.signature;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.AutoComplete;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the form used to gather informations to sign more documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class SignatureDialog extends Window {

	private CheckboxItem visualPositioning;

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form = new DynamicForm();

	private long[] docIds;

	public SignatureDialog(long[] docIds) {
		super();

		this.docIds = docIds;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("signature"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight(40);

		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);

		TextItem reason = ItemFactory.newTextItem("reason", "reason", null);
		reason.setRequired(true);
		reason.setWrapTitle(false);
		reason.setWidth(250);
		reason.setAutoComplete(AutoComplete.NONE);

		visualPositioning = new CheckboxItem();
		visualPositioning.setName("visualpositioning");
		visualPositioning.setTitle(I18N.message("visualpositioning"));
		visualPositioning.setDisabled(true);

		form.setItems(reason, visualPositioning);

		IButton sign = new IButton(I18N.message("sign"));
		sign.setAutoFit(true);
		sign.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSign();
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(5);
		buttons.setMargin(3);
		buttons.addMember(sign);

		layout.addMember(form);
		layout.addMember(buttons);

		addItem(layout);

		SignService.Instance.get().isVisualSignatureEnabled(new AsyncCallback<Boolean>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Boolean enabled) {
				visualPositioning.setDisabled(!enabled);
			}
		});
	}

	private void onSign() {
		if (!form.validate())
			return;

		if (visualPositioning.getValueAsBoolean()) {
			VisualPositioningSignatureDialog dialog = new VisualPositioningSignatureDialog(docIds,
					vm.getValueAsString("reason"));
			dialog.show();
			destroy();
		} else {
			destroy();
			LD.contactingServer();
			SignService.Instance.get().signDocuments(docIds, vm.getValueAsString("reason"), 1, null, null, null,
					new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							LD.clearPrompt();
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void arg) {
							GuiLog.info(I18N.message("event.signed"), null);
							LD.clearPrompt();
						}
					});
		}
	}
}