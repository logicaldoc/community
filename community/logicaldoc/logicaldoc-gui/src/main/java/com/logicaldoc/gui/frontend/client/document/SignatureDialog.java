package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
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

	public SignatureDialog(final long[] docIds) {
		super();

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

		final ValuesManager vm = new ValuesManager();
		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);

		TextItem reason = ItemFactory.newTextItem("reason", "reason", null);
		reason.setRequired(true);
		reason.setWrapTitle(false);
		reason.setWidth(250);
		reason.setAutoComplete(AutoComplete.NONE);

		TextItem password = ItemFactory.newPasswordItem("password", "password", null);
		password.setRequired(true);
		password.setWrapTitle(false);
		password.setAutoComplete(AutoComplete.NONE);

		form.setItems(reason, password);

		IButton sign = new IButton(I18N.message("sign"));
		sign.setAutoFit(true);
		sign.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (vm.validate()) {
					ContactingServer.get().show();
					SignService.Instance.get().signDocuments(docIds, vm.getValueAsString("reason"),
							vm.getValueAsString("password"), new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									ContactingServer.get().hide();
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void arg) {
									destroy();
									ContactingServer.get().hide();
								}
							});
				}
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(5);
		buttons.setMargin(3);
		buttons.addMember(sign);
		
		layout.addMember(form);
		layout.addMember(buttons);
		
		addItem(layout);
	}
}